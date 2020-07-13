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

END SUBROUTINE

! ************************************************************************************************
! Public subroutine: Prepare calibrating parameter metadata from a meta file
! ************************************************************************************************
SUBROUTINE get_parm_meta( err, message)
  ! Process inParMeta along with betaMeta and gammaMeta (popMeta.f90)
  ! Saved data:  calGammaMeta, calBetaName
  use data_type,  only: cpar_meta, scale_meta
  use globalData, only: inParMeta,      & ! meta for beta parameter listed in 'inParList' nml input
                        gammaMeta,      & ! meta for all gamma parameters
                        betaMeta,       & ! meta for all beta parameters
                        calGammaMeta,   & ! meta for only gamma parameters listed in input
                        calBetaName,    & ! list of beta parameters calibrated with MPR
                        calScaleMeta,   & ! meta for beta parameter whose scaling operator(s) is calibrated
                        nCalGamma,      & ! number of gamma parameters to be computed
                        nCalScale,      & ! number of scaling parameters to be computed
                        nSoilBetaModel, & ! number of soil beta parameters to be computed via MPR
                        nVegBetaModel,  & ! number of veg beta parameters to be computed via MPR
                        soilBetaCalName,& ! list of Soil parameters to be omputed via MPR
                        vegBetaCalName    ! list of vege parameters to be omputed via MPR
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
  integer(i4b)                         :: idxBeta          ! loop index of master parameter
  integer(i4b)                         :: iPar             ! loop index of master parameter

  err=0; message="get_param_meta/"

  ! update betaMeta(:)%tftype (transfer function type to be used)
  do iBeta=1,size(inParMeta)
    idxBeta=get_ixBeta(inParMeta(iBeta)%betaname)
    if(idxBeta<=0)then; err=40; message=trim(message)//"1.variableNotFound[var="//trim(inParMeta(iBeta)%betaname)//"]"; return; endif
    betaMeta(idxBeta)%tftype=inParMeta(iBeta)%TF
  enddo

  call get_parMetaSubset( err, cmessage ); if(err/=0)then;message=trim(message)//trim(cmessage); return; endif
  call get_calBetaName  ( err, cmessage ); if(err/=0)then;message=trim(message)//trim(cmessage); return; endif
  call get_calScaleMeta ( err, cmessage ); if(err/=0)then;message=trim(message)//trim(cmessage); return; endif

  CONTAINS

  ! Private subroutine:
  subroutine get_parMetaSubset( err, message )
   ! Saved data:  calGammaMeta, nCalGamma
    !input
    !output
    character(len=strLen),intent(out)   :: message          ! error message for current routine
    integer(i4b),         intent(out)   :: err              ! error code
    !local
    integer(i4b)                        :: ivar             ! loop index of master parameter
    integer(i4b)                        :: iPar             ! loop index of lines in "inParList" nml input
    integer(i4b)                        :: iGamma           ! loop index of all the gamma parameters in master
    type(cpar_meta),allocatable         :: tempParSubset(:)

    err=0; message="get_parMetaSubset/"

    allocate(tempParSubset(nGamma+nBeta))

    nCalGamma=0     ! Count and save number of gamma parameter to be used
    do iPar=1,size(inParMeta)
      do iGamma=1,nGamma
        if (gammaMeta(iGamma)%beta==inParMeta(iPar)%betaname .and. gammaMeta(iGamma)%tftype==inParMeta(iPar)%TF) then
          nCalGamma = nCalGamma+1
          ivar=get_ixGamma(gammaMeta(iGamma)%pname)
          tempParSubset(nCalGamma)%ixMaster = ivar
          tempParSubset(nCalGamma)%pname    = gammaMeta(iGamma)%pname
          tempParSubset(nCalGamma)%val      = gammaMeta(iGamma)%val
          tempParSubset(nCalGamma)%beta     = gammaMeta(iGamma)%beta
          tempParSubset(nCalGamma)%tftype   = gammaMeta(iGamma)%tftype
          tempParSubset(nCalGamma)%ptype    = gammaMeta(iGamma)%ptype
          tempParSubset(nCalGamma)%flag     = .True.
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
    integer(i4b)                         :: idxBeta                ! index in beta parameter
    integer(i4b)                         :: iSoil                  ! counter for soil beta parameters to be computed with MPR
    integer(i4b)                         :: iVeg                   ! counter for vege beta parameters to be computed with MPR
    character(len=strLen),allocatable    :: tempSoilBetaInGamma(:) !
    character(len=strLen),allocatable    :: tempVegBetaInGamma(:)  !
    character(len=strLen),allocatable    :: res(:)                 ! beta parameter name array
    integer(i4b)                         :: counter                ! counter

    err=0; message="get_calBetaName/"

    allocate(res(size(betaMeta)))

    counter = 0
    do iPar=1,size(inParMeta)
      idxBeta = get_ixBeta(inParMeta(iPar)%betaname)
      if(idxBeta<=0)then; err=40; message=trim(message)//"BetaNotFoundInMasterMeta[var="//trim(inParMeta(iPar)%betaname)//"]"; return; endif
      if ( trim(inParMeta(iPar)%betaname) == 'z' )  cycle
      counter=counter+1
      res(counter)=betaMeta(idxBeta)%parName
    end do

    ! Count number of soil and Vege parameters to be computed with MPR excluding h and z parameters
    nSoilBetaModel=0
    nVegBetaModel=0

    if ( counter > 0 ) then

      allocate(calBetaName(counter))
      calBetaName=res(1:counter)

      do iPar=1,size(calBetaName)
        idxBeta=get_ixBeta(calBetaName(iPar))
        if (betaMeta(idxBeta)%parType=='soil') nSoilBetaModel = nSoilBetaModel+1
        if (betaMeta(idxBeta)%parType=='veg')  nVegBetaModel  = nVegBetaModel +1
      enddo

      iSoil=0;iVeg=0

      allocate(tempSoilBetaInGamma(nBeta),stat=err)
      allocate(tempVegBetaInGamma(nBeta),stat=err)

      do iPar=1,size(calBetaName)
        idxBeta = get_ixBeta(calBetaName(iPar))
        if(idxBeta<=0)then; err=40; message=trim(message)//"2.variableNotFound[var="//trim(calBetaName(iPar))//"]"; return; endif
        if (betaMeta(idxBeta)%parType=='soil')then; iSoil=iSoil+1; tempSoilBetaInGamma(iSoil) = betaMeta(idxBeta)%parName; endif
        if (betaMeta(idxBeta)%parType=='veg') then; iVeg=iVeg+1;   tempVegBetaInGamma(iVeg)   = betaMeta(idxBeta)%parName; endif
      enddo

      allocate(soilBetaCalName(nSoilBetaModel))
      soilBetaCalName=tempSoilBetaInGamma(1:nSoilBetaModel)

      allocate(vegBetaCalName(nVegBetaModel))
      vegBetaCalName=tempVegBetaInGamma(1:nVegBetaModel)

    else

      print*, 'No gamma parameters included in the list'

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
    integer(i4b)                        :: ix                    ! loop index
    type(scale_meta),     allocatable   :: tempBetaCalScale(:)   ! temporal holder for scale_meta data

    err=0; message="get_scaleInBeta/"

    allocate(tempBetaCalScale(size(inParMeta)),stat=err)
    if(err/=0)then;message=trim(message)//'error allocating tempBetaCalScale';return;endif

    nCalScale = 0
    do iPar=1,size(inParMeta)!if beta parameter is estimated with MPR or even not calibrated, calibrate scaling parameter
      associate( scaler => betaMeta(get_ixBeta(inParMeta(iPar)%betaname))%parScale,   &
                 pnorm  => betaMeta(get_ixBeta(inParMeta(iPar)%betaname))%parPnorm)

      allocate(tempBetaCalScale(iPar)%pdefault(size(scaler)), tempBetaCalScale(iPar)%mask(size(scaler)), stat=err)
      if(err/=0)then;message=trim(message)//'error allocating tempBetaCalScale(iPar)%pdefault';return;endif

      tempBetaCalScale(iPar)%betaname    = inParMeta(iPar)%betaname
      do ix = 1, size(pnorm)
        nCalScale = nCalScale + 1
        tempBetaCalScale(iPar)%pdefault(ix) = pnorm(ix)
        tempBetaCalScale(iPar)%mask(ix)     = .true.
      end do

      end associate
    enddo

    allocate(calScaleMeta(size(inParMeta)),stat=err)
    if(err/=0)then;message=trim(message)//'error allocating calScaleMeta';return;endif
    calScaleMeta=tempBetaCalScale(1:size(inParMeta))


  end subroutine

end subroutine

! ************************************************************************************************
! Public subroutine: convert parameter data structure to simple arrays
! ************************************************************************************************
SUBROUTINE param_setup( err, message )
  use globalData,  only:parArray, parMask, calGammaMeta, nCalGamma, nCalScale, calScaleMeta
  implicit none
  !output variables
  integer(i4b),     intent(out) :: err                    ! error code
  character(*),     intent(out) :: message                ! error message
  ! local variables
  integer(i4b)                  :: nCalParSum             ! number of total parameters (gamma and scaling parameters) involved
  integer(i4b)                  :: iPar                   ! loop indices
  integer(i4b)                  :: idx                    ! count of calibrating parameter including per layer parameter
  integer(i4b)                  :: ixScale                ! index of beta parameter dimensions

  ! initialize error control
  err=0; message='param_setput/'

  nCalParSum = nCalGamma+nCalScale

  allocate(parArray(nCalParSum,1),stat=err);if(err/=0)then;message=trim(message)//'error allocating parArray';return;endif
  allocate(parMask(nCalParSum),stat=err);if(err/=0)then;message=trim(message)//'error allocating parMask';return;endif
  idx=0
  do iPar=1,nCalGamma
    idx=idx+1
    parArray(idx,1) = calGammaMeta(iPar)%val
    parMask (idx)   = calGammaMeta(iPar)%flag
  enddo

  do iPar=1,size(calScaleMeta)
    do ixScale=1,size(calScaleMeta(iPar)%mask)
      idx=idx+1
      parArray(idx,1) = calScaleMeta(iPar)%pdefault(ixScale)        ! default value of pnorm value
      parMask (idx)   = calScaleMeta(iPar)%mask(ixScale)
    enddo
  enddo

END SUBROUTINE

!*********************************************************
! Public subroutine: print out calibrating parameter data
!*********************************************************
SUBROUTINE print_config()

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

  integer(i4b) :: i,j,cc  ! loop index for writing

  write(*,*) '!-----------------------------------------------------------'
  write(*,*) '!    MPR-flex - configurations of parameter estimations     '
  write(*,*) '!-----------------------------------------------------------'
  write(*,'(A,1X,A)') new_line(' '),'! Beta parameters listed in input'
  write(*,*) '!-----------------------------------------------------------'
  do i=1,size(inParMeta)
    write(*,*) trim(adjustl(inParMeta(i)%betaname))
  end do

  if (size(calBetaName)/=0)then

    write(*,'(A,1X,A)') new_line(' '),'! Beta parameters to be estimated with MPR excluding z'
    write(*,*) '!-----------------------------------------------------------'
    do i=1,size(calBetaName)
      write(*,*) ( trim(adjustl(calBetaName(i))) )
    end do

    write(*,'(A,1X,A)') new_line(' '),'! List of gamma parameters to be used'
    write(*,*) '!-----------------------------------------------------------'
    do i=1,size(calGammaMeta)
      write(*,*) ( trim(adjustl(calGammaMeta(i)%pName)) )
    end do

    write(*,'(A,1X,A)') new_line(' '),'! All beta parameters computed with MPR including dependent beta parameters'
    write(*,*) '!-----------------------------------------------------------'
    do i=1,nBetaNeed
      write(*,*) ( trim(adjustl(betaMeta(calBetaOrderIdx(i))%parName)) )
    end do

  else

    write(*,'(A,1X,A)') new_line(' '), '! No beta parameters estimated with MPR'

  endif

  write(*,'(A,1X,A)') new_line(' '),'! Parameter Array input to optimization routine'
  write(*,*) '!-----------------------------------------------------------'
  write(*,*) 'Parameter Name        (initial)value    cal.flag   Note'
  do i=1,nCalGamma
    write(*,200) calGammaMeta(i)%pName(1:20), parArray(i,1), parMask(i)
    200 format(1X,A,1X,ES17.10,1X,L9)
  enddo
  cc = 1
  do i=1,size(calScaleMeta)
    do j=1,size(calScaleMeta(i)%pdefault)
      write(*,300) calScaleMeta(i)%betaname, parArray(nCalGamma+cc,1), parMask(nCalGamma+cc-1), 'scaling parameter-',j
      300 format(1X,A20,1X,ES17.10,1X,L9,1X,A18,I1)
      cc = cc + 1
    end do
  end do
  print*,"!-----------------------------------------------------------"
  print*,"!-----------------------------------------------------------"

END SUBROUTINE

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

END MODULE process_meta
