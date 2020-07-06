module write_param_nc

  ! netCDF writing routines

  use io_netcdf
  use public_var
  use nrtype
  use data_type                                    ! data strucutre definition

  implicit none

  private

  public::write_nc_beta

contains

  ! *********************************************************************
  ! public subroutine: write model parameter in netCDF
  ! *********************************************************************
  subroutine write_nc_beta(fname,           & ! input: output nc filename
                           hruID,           & ! input: hruID array
                           hModel,          & ! input: model layer thickness
                           parMxyMz,        & ! input: parameter data structure
                           vegParMxy,       & ! input: parameter data structure
                           ierr, message)     ! output: error control

    use globalData,    only: nSoilBetaModel, soilBetaCalName
    use globalData,    only: nVegBetaModel, vegBetaCalName

    implicit none
    ! input variables
    character(*),   intent(in)            :: fname        ! input: filename
    integer(i4b),   intent(in)            :: hruID(:)     ! Hru ID
    real(dp),       intent(in)            :: hModel(:,:)  ! input: array of model layer thickness at model layer x model hru
    type(namedvar2),intent(in)            :: parMxyMz(:)  ! input: data structure for model soil parameter at model layer x model hru
    type(namedvar2),intent(in)            :: vegParMxy(:) ! input: data structure for model veg parameter at model layer x model hru
    ! output variables
    integer(i4b), intent(out)             :: ierr         ! error code
    character(*), intent(out)             :: message      ! error message
    ! local variables
    type(defDim)                          :: soilDim(2)
    type(defDim)                          :: vegDim(2)
    type(defDim)                          :: hruDim
    real(dp),                 allocatable :: zModel(:,:)  ! array of model layer depth at model layer x model hru
    character(len=strLen),    allocatable :: betaNames(:) ! parameter name array
    integer(i4b)                          :: ncid         ! netcdf id
    integer(i4b)                          :: dimid        ! dimension id (dummy)
    integer(i4b)                          :: iPar         ! loop index for parameter
    integer(i4b)                          :: iLyr         ! loop index for model layer
    integer(i4b)                          :: iMon         ! loop index for month
    character(len=strLen)                 :: cmessage     ! error message of downwind routine

    ! initialize error control
    ierr=0; message='write_nc_beta/'

    ! define netCDF
    call def_nc(fname, ncid, ierr, cmessage, nctype='netcdf4')

    ! Define dimension
    hruDim%dname = 'hruid'
    hruDim%desc = 'Model HRU ID'
    hruDim%unt = '-'

    call def_dim(ncid, hruDim%dname, nHru, dimid, ierr, cmessage)
    if(ierr/=0)then;message=trim(message)//trim(cmessage);return;endif

    call def_var(ncid, hruDim%dname, [hruDim%dname], ncd_int, ierr, cmessage, vdesc=hruDim%desc, vunit=hruDim%unt)
    if(ierr/=0)then;message=trim(message)//trim(cmessage);return;endif

    if (nSoilBetaModel>0) then

      soilDim(1)%dname = hruDim%dname
      soilDim(1)%desc = hruDim%desc
      soilDim(1)%unt = hruDim%unt

      soilDim(2)%dname='lyr'
      soilDim(2)%desc='Soil Layer ID'
      soilDim(2)%unt='-'

      call def_dim(ncid, soilDim(2)%dname, nLyr, dimid, ierr, cmessage)
      if(ierr/=0)then;message=trim(message)//trim(cmessage);return;endif

      call def_var(ncid, soilDim(2)%dname, [soilDim(2)%dname], ncd_int, ierr, cmessage, vdesc=soilDim(2)%desc, vunit=soilDim(2)%unt)
      if(ierr/=0)then;message=trim(message)//trim(cmessage);return;endif

    end if

    if (nVegBetaModel>0) then

      vegDim(1)%dname = hruDim%dname
      vegDim(1)%desc = hruDim%desc
      vegDim(1)%unt = hruDim%unt

      vegDim(2)%dname='mon'
      vegDim(2)%desc='Month'
      vegDim(2)%unt='-'

      call def_dim(ncid, vegDim(2)%dname, nMonth, dimid, ierr, cmessage)
      if(ierr/=0)then;message=trim(message)//trim(cmessage);return;endif

      call def_var(ncid, vegDim(2)%dname, [vegDim(2)%dname], ncd_int, ierr, cmessage, vdesc=vegDim(2)%desc, vunit=vegDim(2)%unt)
      if(ierr/=0)then;message=trim(message)//trim(cmessage);return;endif

    end if

    ! Define variables
    if (nSoilBetaModel>0) then
      ! construct z array
      allocate(zModel,source=hModel)
      do iLyr=2,nLyr
        zModel(iLyr,:)=zModel(iLyr-1,:)+zModel(iLyr,:)
      enddo

      ! construct soil beta parameter name array
      allocate(betaNames(nSoilBetaModel+1),stat=ierr);if(ierr/=0)then;message=trim(message)//'error allocating betaNames';return;endif
      betaNames(1:nSoilBetaModel)=soilBetaCalName
      betaNames(nSoilBetaModel+1)='z'

      call defNetCDF(ncid, betaNames, soilDim, ierr, cmessage)
      if(ierr/=0)then;message=trim(message)//trim(cmessage);return;endif
    end if

    if (nVegBetaModel>0) then
      call defNetCDF(ncid, vegBetaCalName, vegDim, ierr, cmessage)
      if(ierr/=0)then;message=trim(message)//trim(cmessage);return;endif
    end if

    ! end definitions
    call end_def(ncid, ierr, cmessage)
    if(ierr/=0)then;message=trim(message)//trim(cmessage);return;endif

    ! close NetCDF file
    call close_nc(ncid, ierr, cmessage)
    if(ierr/=0)then;message=trim(message)//trim(cmessage);return;endif


    ! write hru
    call write_nc(fname, hruDim%dname,hruID, 1, size(hruID), ierr,cmessage)
    if(ierr/=0)then;message=trim(message)//trim(cmessage);return;endif

    if (nSoilBetaModel>0) then

      ! write 2nd Dimension
      call write_nc(fname, soilDim(2)%dname, (/(iLyr,iLyr=1,nLyr)/), 1, nLyr, ierr,cmessage)
      if(ierr/=0)then;message=trim(message)//trim(cmessage);return;endif

      ! Write soil parameters
      do iPar=1,nSoilBetaModel
        call write_nc(fname,trim(soilBetaCalName(iPar)),parMxyMz(iPar)%varData,(/1,1/),(/nLyr,nHru/),ierr,cmessage)
        if(ierr/=0)then;message=trim(message)//trim(cmessage);return;endif
      enddo

      ! Write soil layers depth
      call write_nc(fname,'z',zModel,(/1,1/),(/nLyr,nHru/),ierr,cmessage)
      if(ierr/=0)then;message=trim(message)//trim(cmessage);return;endif

    end if

    if (nVegBetaModel>0) then
      ! write 2nd Dimension
      call write_nc(fname, vegDim(2)%dname,(/(iMon,iMon=1,nMonth)/), 1, nMonth, ierr,cmessage)
      if(ierr/=0)then;message=trim(message)//trim(cmessage);return;endif

      ! Write veg parameters
      do iPar=1,nVegBetaModel
        call write_nc(fname,vegBetaCalName(iPar),vegParMxy(iPar)%varData,(/1,1/),(/nMonth,nHru/),ierr,cmessage)
        if(ierr/=0)then;message=trim(message)//trim(cmessage);return;endif
      end do
    end if

  end subroutine

  !private subroutine
  subroutine defNetCDF(ncid,            &  ! input: netcdf ID
                       betaNames,       &  ! input: character array for beta parameter name
                       dimMeta,         &  ! input: dimension meta
                       ierr, message)      ! output: error control

    use globalData,    only: betaMeta
    use var_lookup,    only: nBeta
    implicit none
    ! input variables
    integer(i4b), intent(in)          :: ncid            ! NetCDF file ID
    character(*), intent(in)          :: betaNames(:)    ! parameter name array
    type(defDim), intent(in)          :: dimMeta(2)      ! dimension name
    ! output variables
    integer(i4b), intent(out)         :: ierr            ! error code
    character(*), intent(out)         :: message         ! error message
    ! local variables
    type(betaPar_meta), allocatable   :: betaMetaTemp(:) ! meta data for beta parameter estimated via MPR
    integer(i4b)                      :: nBetaOut        ! number of beta parameter to be output
    integer(i4b)                      :: iPar            ! variable index
    integer(i4b)                      :: ixDim           ! dimension index
    integer(i4b)                      :: nDim            ! number of dimensions
    integer(i4b)                      :: iBeta           ! variable index
    character(len=strLen)             :: cmessage        ! error message of downwind routine

    ! initialize error control
    ierr=0; message='defNetCDF/'

    nBetaOut=size(betaNames)
    allocate(betaMetaTemp(nBetaOut))

    do iBeta=1,nBetaOut
      do iPar=1,nBeta
        if ( betaMeta(iPar)%pname==betaNames(iBeta) )then
           betaMetaTemp(iBeta)=betaMeta(iPar)
           exit
        endif
      enddo
      ! define parameter values
      call def_var(ncid, betaMetaTemp(iBeta)%pname, [dimMeta(2)%dname, dimMeta(1)%dname], ncd_float, &
                  ierr, cmessage, &
                  vdesc=betaMetaTemp(iBeta)%desc, &
                  vunit=betaMetaTemp(iBeta)%unt)
      if(ierr/=0)then;message=trim(message)//trim(cmessage);return;endif
    end do

  end subroutine

end module write_param_nc
