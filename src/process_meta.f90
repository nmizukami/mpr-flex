module process_meta

  use nrtype
  use public_var

  implicit none

  private

  public::read_inParList
  public::get_parm_meta
  public::param_setup
  public::print_config

contains

! ************************************************************************************************
! Public subroutine: Read 'inParList' input file
! ************************************************************************************************
subroutine read_inParList(infile, err, message)
  use globalData, only:inParMeta      ! meta for inParList nml input
  use data_type,  only:input_meta
  use ascii_util, only:file_open
  use var_lookup, only:nBeta
  implicit none
  ! input
  character(*),intent(in)              :: infile            ! input filename
  ! output
  integer(i4b),intent(out)             :: err               ! error code
  character(*),intent(out)             :: message           ! error message
  ! local variables
  type(input_meta),allocatable         :: tempCalParMeta(:) ! temp data structure for inParList input meta
  character(len=strLen)                :: cmessage          ! error message subroutine
  integer(i4b)                         :: ixLocal           ! index for calibrationg parameter list
  integer(i4b),parameter               :: maxLines=1000     ! maximum lines in the file
  integer(i4b)                         :: iend              ! check for the end of the file
  integer(i4b)                         :: unt               ! DK: need to either define units globally, or use getSpareUnit
  character(LEN=strLen)                :: temp              ! single lime of information
  character(LEN=strLen)                :: ffmt              ! file format
  character(len=1)                     :: dLim(1)           ! column delimiter
  integer(i4b)                         :: iline             ! loop through lines in the file

  ! initialize error handling
  err=0; message="read_inParList/"
  allocate(tempCalParMeta(nBeta))
  call file_open(trim(infile), unt, err, cmessage)
  if(err/=0)then; message=trim(message)//trim(cmessage); return; endif

  ! get to the start of the variable descriptions
  do iline=1,maxLines
    read(unt,'(a)',iostat=iend)temp; if (iend/=0)exit    ! read line of data
    if (temp(1:1)/='!') exit                             ! assume first line not comment is format code
  end do ! looping through file to find the format code
  read(temp,*)ffmt ! to get format

  ixLocal=0_i4b
  line:do iline=1,maxLines
    ! read a line of data and exit iif an error code (character read, so only possible error is end of file)
    read(unt,'(a)',iostat=iend)temp; if (iend/=0)exit
    ! check that the line is not a comment
    if (temp(1:1)=='!')cycle
    ! save data into a temporary structure
    ixLocal = ixLocal+1_i4b
    read(temp,trim(ffmt),iostat=err) tempCalParMeta(ixLocal)%betaname, dLim(1),&  ! beta parameter name
                                     tempCalParMeta(ixLocal)%TF                     ! Transfer function type
    if (err/=0) then; err=30; message=trim(message)//"errorReadLine"; return; endif
    ! check that the delimiters are in the correct place
    if(any(dLim /= '|'))then
      message=trim(message)//'delimiter is not in the correct place; line = ['//trim(temp)//']; filename = '//trim(infile)
      err=32; return
    endif
  enddo line

  ! close file unit
  close(unt)

  ! save 'inParMeta'
  allocate(inParMeta(ixLocal))
  inParMeta=tempCalParMeta(1:ixLocal)

end subroutine

! ************************************************************************************************
! Public subroutine: Prepare calibrating parameter metadata from a meta file
! ************************************************************************************************
subroutine get_parm_meta( err, message)
  ! Process inParMeta along with betaMeta and gammaMeta (popMeta.f90)
  ! Saved data:  calGammaMeta, calBetaName
  use data_type,  only:par_meta, cpar_meta, scale_meta
  use globalData, only:inParMeta,      & ! meta for beta parameter listed in 'inParList' nml input
                       gammaMeta,      & ! meta for all gamma parameters
                       betaMeta,       & ! meta for all beta parameters
                       calGammaMeta,   & ! meta for only gamma parameters listed in input
                       calBetaName,    & ! list of beta parameters calibrated with MPR
                       calScaleMeta,   & ! meta for beta parameter whose scaling operator(s) is calibrated
                       nCalGamma,      & ! number of gamma parameters to be calibrated
                       nSoilBetaModel, & ! number of soil beta parameters to be calibrated via MPR
                       nVegBetaModel,  & ! number of veg beta parameters to be calibrated via MPR
                       soilBetaCalName,& ! list of Soil parameters to be estimated via MPR
                       vegBetaCalName    ! list of vege parameters to be estimated via MPR
  use get_ixname, only:get_ixBeta, get_ixGamma
  use var_lookup, only:nBeta,nGamma
  implicit none
  ! input
  ! output
  integer(i4b),intent(out)             :: err              ! error code
  character(*),intent(out)             :: message          ! error message
  ! local variables
  character(len=strLen)                :: cmessage         ! error message from subroutine
  integer(i4b)                         :: iBeta            ! loop index of lines in inParList nml input
  integer(i4b)                         :: ivar             ! loop index of master parameter
  integer(i4b)                         :: iPar             ! loop index of master parameter

  err=0; message="get_param_meta/"

  ! update betaMeta(:)%tftype (transfer function type to be used)
  do iBeta=1,size(inParMeta)
    ivar=get_ixBeta(inParMeta(iBeta)%betaname)
    if(ivar<=0)then; err=40; message=trim(message)//"1.variableNotFound[var="//trim(inParMeta(iBeta)%betaname)//"]"; return; endif
    betaMeta(ivar)%tftype=inParMeta(iBeta)%TF
  enddo

  call get_parMetaSubset( err, cmessage ); if(err/=0)then;message=trim(message)//trim(cmessage); return; endif
  call get_calBetaName  ( err, cmessage ); if(err/=0)then;message=trim(message)//trim(cmessage); return; endif
  call get_calScaleMeta ( err, cmessage ); if(err/=0)then;message=trim(message)//trim(cmessage); return; endif

  contains

  ! Private subroutine:
  subroutine get_parMetaSubset( err, message )
   ! Saved data:  calGammaMeta, nCalGamma
    !input
    !output
    character(len=strLen),intent(out)   :: message           ! error message for current routine
    integer(i4b),         intent(out)   :: err               ! error code
    !local
    integer(i4b)                         :: ivar             ! loop index of master parameter
    integer(i4b)                         :: iPar            ! loop index of lines in "inParList" nml input
    integer(i4b)                         :: iGamma           ! loop index of all the gamma parameters in master
    type(cpar_meta),allocatable          :: tempParSubset(:)

    err=0; message="get_parMetaSubset/"

    allocate(tempParSubset(nGamma+nBeta))

    nCalGamma=0     ! Count and Save number of gamma parameter to be calibrated
    do iPar=1,size(inParMeta)
      do iGamma=1,nGamma  ! look for gammma parameters associated with this beta parameter
        if (gammaMeta(iGamma)%beta==inParMeta(iPar)%betaname .and. gammaMeta(iGamma)%tftype==inParMeta(iPar)%TF) then
          nCalGamma = nCalGamma+1
          ivar=get_ixGamma(gammaMeta(iGamma)%pname)
          tempParSubset(nCalGamma)%ixMaster = ivar
          tempParSubset(nCalGamma)%pname    = gammaMeta(iGamma)%pname
          tempParSubset(nCalGamma)%val      = gammaMeta(iGamma)%val
          tempParSubset(nCalGamma)%lwr      = gammaMeta(iGamma)%lwr
          tempParSubset(nCalGamma)%upr      = gammaMeta(iGamma)%upr
          tempParSubset(nCalGamma)%beta     = gammaMeta(iGamma)%beta
          tempParSubset(nCalGamma)%tftype   = gammaMeta(iGamma)%tftype
          tempParSubset(nCalGamma)%ptype    = gammaMeta(iGamma)%ptype
          tempParSubset(nCalGamma)%flag     = .True.
          tempParSubset(nCalGamma)%hups     = gammaMeta(iGamma)%hups
          tempParSubset(nCalGamma)%hpnorm   = gammaMeta(iGamma)%hpnorm
          tempParSubset(nCalGamma)%vups     = gammaMeta(iGamma)%vups
          tempParSubset(nCalGamma)%vpnorm   = gammaMeta(iGamma)%vpnorm
          tempParSubset(nCalGamma)%perLyr   = gammaMeta(iGamma)%perLyr
        endif
      end do
    enddo

    if (nCalGamma > 0) then              ! Save'calGammaMeta'
      allocate(calGammaMeta(nCalGamma))
      calGammaMeta=tempParSubset(1:nCalGamma)
    endif

  end subroutine

  ! Private subroutine: Get list of beta parameters (excluding z and h) computed with MPR.
  ! populated global data: calBetaName, soilBetaCalName, vegBetaCalName
  subroutine get_calBetaName( err, message )
    !input
    !output
    character(len=strLen),intent(out)    :: message                ! error message for current routine
    integer(i4b),         intent(out)    :: err                    ! error code
    !local
    integer(i4b)                         :: iPar                   ! loop index of parameter
    integer(i4b)                         :: ivar                   ! index in master parameter
    integer(i4b)                         :: iSoil                  ! counter for soil beta parameters to be computed with MPR
    integer(i4b)                         :: iVeg                   ! counter for vege beta parameters to be computed with MPR
    character(len=strLen),allocatable    :: tempSoilBetaInGamma(:) !
    character(len=strLen),allocatable    :: tempVegBetaInGamma(:)  !
    character(len=strLen),allocatable    :: res(:)                 !
    integer(i4b)                         :: counter                 ! counter

    err=0; message="get_calBetaName/"

    allocate(res(size(betaMeta)))

    counter = 0
    do iPar=1,size(inParMeta)
      ivar = get_ixBeta(inParMeta(iPar)%betaname)
      if(ivar<=0)then; err=40; message=trim(message)//"BetaNotFoundInMasterMeta[var="//trim(inParMeta(iPar)%betaname)//"]"; return; endif
      if ( trim(inParMeta(iPar)%betaname) == 'h1' ) cycle
      if ( trim(inParMeta(iPar)%betaname) == 'h2' ) cycle
      if ( trim(inParMeta(iPar)%betaname) == 'h3' ) cycle
      if ( trim(inParMeta(iPar)%betaname) == 'h4' ) cycle
      if ( trim(inParMeta(iPar)%betaname) == 'h5' ) cycle
      if ( trim(inParMeta(iPar)%betaname) == 'z' )  cycle
      counter=counter+1
      res(counter)=betaMeta(ivar)%pname
    end do

    ! Count number of soil and Vege parameters to be computed with MPR excluding h and z parameters
    nSoilBetaModel=0
    nVegBetaModel=0

    if ( counter > 0 ) then

      allocate(calBetaName(counter))
      calBetaName=res(1:counter)
      do iPar=1,size(calBetaName)
        ivar=get_ixBeta(calBetaName(iPar))
        if (betaMeta(ivar)%ptype=='soil') nSoilBetaModel = nSoilBetaModel+1
        if (betaMeta(ivar)%ptype=='veg')  nVegBetaModel  = nVegBetaModel +1
      enddo

      iSoil=0;iVeg=0

      allocate(tempSoilBetaInGamma(nBeta),stat=err)
      allocate(tempVegBetaInGamma(nBeta),stat=err)

      do iPar=1,size(calBetaName)
        ivar = get_ixBeta(calBetaName(iPar))
        if(ivar<=0)then; err=40; message=trim(message)//"2.variableNotFound[var="//trim(calBetaName(iPar))//"]"; return; endif
        if (betaMeta(ivar)%ptype=='soil')then; iSoil=iSoil+1; tempSoilBetaInGamma(iSoil) = betaMeta(ivar)%pname; endif
        if (betaMeta(ivar)%ptype=='veg') then; iVeg=iVeg+1;   tempVegBetaInGamma(iVeg)   = betaMeta(ivar)%pname; endif
      enddo

      allocate(soilBetaCalName(nSoilBetaModel))
      soilBetaCalName=tempSoilBetaInGamma(1:nSoilBetaModel)

      allocate(vegBetaCalName(nVegBetaModel))
      vegBetaCalName=tempVegBetaInGamma(1:nVegBetaModel)

    else

      print*, 'NO gamma parameters included in the list'
    endif

  end subroutine

  ! Private subroutine: to populate scaling parameter meta for beta parameter calibrated with MPR
  ! populated global data: calScaleMeta
  subroutine get_calScaleMeta( err, message )
    !input
    !output
    character(len=strLen),intent(out)   :: message               ! error message for current routine
    integer(i4b),         intent(out)   :: err                   ! error code
    !local
    type(scale_meta),     allocatable   :: tempBetaCalScale(:)   !
    integer(i4b)                        :: nScaleBeta            ! counter

    err=0; message="get_scaleInBeta/"

    allocate(tempBetaCalScale(size(inParMeta)),stat=err);if(err/=0)then;message=trim(message)//'error allocating tempBetaCalScale';return;endif

    nScaleBeta=0
    do iPar=1,size(inParMeta)!if beta parameter is estimated with MPR or even not calibrated, calibrate scaling parameter

      associate( hups      => betaMeta(get_ixBeta(inParMeta(iPar)%betaname))%hups,   &
                 vups      => betaMeta(get_ixBeta(inParMeta(iPar)%betaname))%vups,   &
                 hpower    => betaMeta(get_ixBeta(inParMeta(iPar)%betaname))%hpnorm, &
                 vpower    => betaMeta(get_ixBeta(inParMeta(iPar)%betaname))%vpnorm, &
                 hscaleMask=> inParMeta(iPar)%isScaleCalH, &
                 vscaleMask=> inParMeta(iPar)%isScaleCalV )

      nScaleBeta=nScaleBeta+1
      tempBetaCalScale(nScaleBeta)%betaname    = inParMeta(iPar)%betaname
      tempBetaCalScale(nScaleBeta)%pdefault(1) = hpower
      tempBetaCalScale(nScaleBeta)%pdefault(2) = vpower
      tempBetaCalScale(nScaleBeta)%mask(1)     = hscaleMask
      tempBetaCalScale(nScaleBeta)%mask(2)     = vscaleMask

      ! Check beta meta data for scaling and if value is -999 or na, turns off calibration
      if (hscaleMask .and. (hpower==-999.0_dp .or. hups=='na'))then
        print*,'Switch horizontal upscale calibration to False for',inParMeta(iPar)%betaname
        tempBetaCalScale(nScaleBeta)%mask(1)=.False.
      endif

      if (vscaleMask .and. (vpower==-999.0_dp .or. vups=='na'))then
        print*,'Switch vertical upscale calibration to False for',inParMeta(iPar)%betaname
        tempBetaCalScale(nScaleBeta)%mask(2)=.False.
      endif

      end associate

    enddo

    allocate(calScaleMeta(nScaleBeta),stat=err);if(err/=0)then;message=trim(message)//'error allocating calScaleMeta';return;endif
    calScaleMeta=tempBetaCalScale(1:nScaleBeta)

  end subroutine

end subroutine

! ************************************************************************************************
! Public subroutine: convert parameter data structure to simple arrays
! ************************************************************************************************
subroutine param_setup( err, message )
  use globalData,  only:parArray, parMask, calGammaMeta, nCalGamma, calScaleMeta
  implicit none
  !output variables
  integer(i4b),     intent(out) :: err                    ! error code
  character(*),     intent(out) :: message                ! error message
  ! local variables
  integer(i4b)                  :: nCalParSum             ! number of total parameters (gamma and scaling parameters) involved
  integer(i4b)                  :: iPar                   ! loop indices
  integer(i4b)                  :: idx                    ! count of calibrating parameter including per layer parameter
  integer(i4b)                  :: ixHV                   ! count of calibrating parameter including per layer parameter

  ! initialize error control
  err=0; message='param_setput/'

  nCalParSum = nCalGamma+size(calScaleMeta)*2

  allocate(parArray(nCalParSum,3),stat=err);if(err/=0)then;message=trim(message)//'error allocating parArray';return;endif
  allocate(parMask(nCalParSum),stat=err);if(err/=0)then;message=trim(message)//'error allocating parMask';return;endif
  idx=0
  do iPar=1,nCalGamma
    idx=idx+1
    parArray(idx,1) = calGammaMeta(iPar)%val
    parArray(idx,2) = calGammaMeta(iPar)%lwr
    parArray(idx,3) = calGammaMeta(iPar)%upr
    parMask (idx)   = calGammaMeta(iPar)%flag
  enddo

  do iPar=1,size(calScaleMeta)
    do ixHV=1,2
      idx=idx+1
      parArray(idx,1) = calScaleMeta(iPar)%pdefault(ixHV)        ! default value of pnorm value
      parArray(idx,2) = -100.0_dp                                ! lower bound or pnorm value
      parArray(idx,3) =  100.0_dp                                ! upper bound of pnorm value
      parMask (idx)   = calScaleMeta(iPar)%mask(ixHV)
    enddo
  enddo

end subroutine

!*********************************************************
! Public subroutine: print out calibrating parameter data
!*********************************************************
subroutine print_config()
  use globaldata,  only: inParMeta,      &
                         betaMeta,       &
                         calGammaMeta,    &
                         calScaleMeta,   &
                         calBetaName,    &
                         calBetaOrderIdx,&
                         parMask,        &
                         parArray,       &
                         nCalGamma,  &
                         nBetaNeed
  implicit none

  integer(i4b) :: i,j   ! loop index for writing

  write(*,*) '!-----------------------------------------------------------'
  write(*,*) '!    MPR-flex - configurations of parameter estimations     '
  write(*,*) '!-----------------------------------------------------------'
  write(*,'(A,1X,A)') new_line(' '),'! Beta parameters listed in input'
  write(*,*) '!-----------------------------------------------------------'
  do i=1,size(inParMeta)
    write(*,*) trim(adjustl(inParMeta(i)%betaname))
  end do
  write(*,'(A,1X,A)') new_line(' '),'! Calibrating Gamma parameters'
  write(*,*) '!-----------------------------------------------------------'
  do i=1,size(calGammaMeta)
    write(*,*) ( trim(adjustl(calGammaMeta(i)%pname)) )
  end do
  if (size(calBetaName)/=0)then
    write(*,'(A,1X,A)') new_line(' '),'! Beta parameters to be estimated with MPR excluding z and h'
    write(*,*) '!-----------------------------------------------------------'
    do i=1,size(calBetaName)
      write(*,*) ( trim(adjustl(calBetaName(i))) )
    end do
    write(*,'(A,1X,A)') new_line(' '),'! List of gamma parameters calibrated'
    write(*,*) '!-----------------------------------------------------------'
    do i=1,size(calGammaMeta)
      write(*,*) ( trim(adjustl(calGammaMeta(i)%pname)) )
    end do
    write(*,'(A,1X,A)') new_line(' '),'! All beta parameters computed with MPR including dependent beta parameters'
    write(*,*) '!-----------------------------------------------------------'
    do i=1,nBetaNeed
      write(*,*) ( trim(adjustl(betaMeta(calBetaOrderIdx(i))%pname)) )
    end do
  else
    write(*,'(A,1X,A)') new_line(' '), '! No beta parameters estimated with MPR'
  endif
  write(*,'(A,1X,A)') new_line(' '),'! Parameter Array input to optimization routine'
  write(*,*) '!-----------------------------------------------------------'
  write(*,*) 'Parameter Name        (initial)value    cal.flag   Note'
  do i=1,nCalGamma
    write(*,200) calGammaMeta(i)%pname(1:20), parArray(i,1), parMask(i)
    200 format(1X,A,1X,ES17.10,1X,L9)
  enddo
  do i=1,size(calScaleMeta)
     write(*,300) calScaleMeta(i)%betaname(1:20), parArray(nCalGamma+2*i-1,1), parMask(nCalGamma+2*i-1), 'Horizontal scaling parameter'
     write(*,300) calScaleMeta(i)%betaname(1:20), parArray(nCalGamma+2*i  ,1), parMask(nCalGamma+2*i),   'Vertical scaling parameter'
     300 format(1X,A,1X,ES17.10,1X,L9,1X,A30)
  end do
  print*,"!-----------------------------------------------------------"
  print*,"!-----------------------------------------------------------"

end subroutine

!**********************************
! Not Used---  Public subroutine: check if h parameters exist in gamma parameter
!**********************************
subroutine check_gammaH( err, message)
  use globalData,   only: calGammaMeta
  implicit none
  !output variables
  integer(i4b),         intent(out) :: err         ! error code
  character(*),         intent(out) :: message     ! error message
  !local variables
  integer(i4b)                      :: id(20)
  logical(lgc)                      :: mask(20)
  logical(lgc),allocatable          :: checkH(:)
  integer(i4b)                      :: i

  ! initialize error control
  err=0; message='check_gammaH/'
  allocate(checkH(nLyr-1))
  id=-999
  if ( allocated(calGammaMeta) )then
    !check h parameters - now can chcek up to 5 layers
    do i=1,size(calGammaMeta)
      if (calGammaMeta(i)%pname=="h1gamma1")then;id(1)=1;cycle;endif
      if (calGammaMeta(i)%pname=="h2gamma1")then;id(2)=1;cycle;endif
      if (calGammaMeta(i)%pname=="h3gamma1")then;id(3)=1;cycle;endif
      if (calGammaMeta(i)%pname=="h4gamma1")then;id(4)=1;cycle;endif
    enddo
    mask=(id>0)
    checkH=mask(1:nLyr-1)
    if ( any(.not. checkH) )then;err=40;message=trim(message)//"Calibrating gamma parameter require (nLyr-1) hgamma parameters"; return;endif
  else
    print*, 'No gamma parameters listed in CalPar->No MPR computation'
  endif

end subroutine

end module process_meta
