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
                           snowParMxy,      & ! input: parameter data structure
                           ierr, message)     ! output: error control

    use globalData,    only: nSoilBetaModel, soilBetaCalName
    use globalData,    only: nVegBetaModel,  vegBetaCalName
    use globalData,    only: nSnowBetaModel, snowBetaCalName
    use globalData,    only: dimMeta
    use var_lookup,    only: ixDim

    implicit none
    ! input variables
    character(*),   intent(in)            :: fname        ! input: filename
    integer(i4b),   intent(in)            :: hruID(:)     ! Hru ID
    real(dp),       intent(in)            :: hModel(:,:)  ! input: array of model layer thickness
    type(namevar),  intent(in)            :: parMxyMz(:)  ! input: data structure for model soil parameter at model space
    type(namevar),  intent(in)            :: vegParMxy(:) ! input: data structure for model veg parameter at model space
    type(namevar),  intent(in)            :: snowParMxy(:)! input: data structure for model snow parameter at model space
    ! output variables
    integer(i4b), intent(out)             :: ierr         ! error code
    character(*), intent(out)             :: message      ! error message
    ! local variables
    character(len=strLen),    allocatable :: betaNames(:) ! parameter name array
    integer(i4b)                          :: ncid         ! netcdf id
    character(len=strLen)                 :: cmessage     ! error message of downwind routine

    ! initialize error control
    ierr=0; message='write_nc_beta/'

    ! define netCDF
    call def_nc(fname, ncid, ierr, cmessage, nctype='netcdf4')

    ! -----------------------------------------------------------
    ! define dimension - populate dimMeta%dimId
    call def_dim(ncid, dimMeta(ixDim%hru)%dname, nHru, dimMeta(ixDim%hru)%dimId, ierr, cmessage)
    if(ierr/=0)then;message=trim(message)//trim(cmessage);return;endif

    call def_var(ncid, dimMeta(ixDim%hru)%dname, [dimMeta(ixDim%hru)%dname], ncd_int, ierr, cmessage, vdesc=dimMeta(ixDim%hru)%desc, vunit=dimMeta(ixDim%hru)%unt)
    if(ierr/=0)then;message=trim(message)//trim(cmessage);return;endif

    if (nSoilBetaModel>0) then
      call def_dim(ncid, dimMeta(ixDim%lyr)%dname, nLyr, dimMeta(ixDim%lyr)%dimId, ierr, cmessage)
      if(ierr/=0)then;message=trim(message)//trim(cmessage);return;endif
    end if

    if (nVegBetaModel>0) then
      call def_dim(ncid, dimMeta(ixDim%mon)%dname, nMonth, dimMeta(ixDim%mon)%dimId, ierr, cmessage)
      if(ierr/=0)then;message=trim(message)//trim(cmessage);return;endif
    end if

    ! -----------------------------------------------------------
    ! Define variables
    if (nSoilBetaModel>0) then

      ! construct soil beta parameter name array
      allocate(betaNames(nSoilBetaModel+1),stat=ierr)
      if(ierr/=0)then;message=trim(message)//'error allocating betaNames';return;endif
      betaNames(1:nSoilBetaModel)=soilBetaCalName
      betaNames(nSoilBetaModel+1)='h'

      call defNetCDF(ncid, betaNames, ierr, cmessage)
      if(ierr/=0)then;message=trim(message)//trim(cmessage);return;endif

    end if

    if (nVegBetaModel>0) then
      call defNetCDF(ncid, vegBetaCalName, ierr, cmessage)
      if(ierr/=0)then;message=trim(message)//trim(cmessage);return;endif
    end if

    if (nSnowBetaModel>0) then
      call defNetCDF(ncid, snowBetaCalName, ierr, cmessage)
      if(ierr/=0)then;message=trim(message)//trim(cmessage);return;endif
    end if

    ! end definitions
    call end_def(ncid, ierr, cmessage)
    if(ierr/=0)then;message=trim(message)//trim(cmessage);return;endif

    ! close NetCDF file
    call close_nc(ncid, ierr, cmessage)
    if(ierr/=0)then;message=trim(message)//trim(cmessage);return;endif

    ! -----------------------------------------------------------
    ! Define variables
    ! write hru
    call write_nc(fname, dimMeta(ixDim%hru)%dname, hruID, 1, size(hruID), ierr,cmessage)
    if(ierr/=0)then;message=trim(message)//trim(cmessage);return;endif

    ! Write soil parameters
    if (nSoilBetaModel>0) then

      call write_par(fname, soilBetaCalName, parMxyMz, ierr, cmessage)
      if(ierr/=0)then;message=trim(message)//trim(cmessage);return;endif

      ! Write soil layers depth
      call write_nc(fname,'h',hModel,(/1,1/),(/nHru, nLyr/),ierr,cmessage)
      if(ierr/=0)then;message=trim(message)//trim(cmessage);return;endif
    end if

    ! Write veg parameters
    if (nVegBetaModel>0) then
      call write_par(fname, vegBetaCalName, vegParMxy, ierr, cmessage)
      if(ierr/=0)then;message=trim(message)//trim(cmessage);return;endif
    end if

    ! Write veg parameters
    if (nSnowBetaModel>0) then
      call write_par(fname, snowBetaCalName, snowParMxy, ierr, cmessage)
      if(ierr/=0)then;message=trim(message)//trim(cmessage);return;endif
    end if

  end subroutine

  !private subroutine
  subroutine defNetCDF(ncid,            &  ! input: netcdf ID
                       betaNames,       &  ! input: character array for beta parameter name
                       ierr, message)      ! output: error control

    use globalData,    only: betaMeta
    use globalData,    only: dimMeta
    use get_ixname,    only: get_ixBeta

    implicit none
    ! input variables
    integer(i4b), intent(in)          :: ncid            ! NetCDF file ID
    character(*), intent(in)          :: betaNames(:)    ! parameter name array
    ! output variables
    integer(i4b), intent(out)         :: ierr            ! error code
    character(*), intent(out)         :: message         ! error message
    ! local variables
    character(len=strLen),allocatable :: dimName(:)      ! dimension name array
    integer(i4b)                      :: ixPar           ! variable index
    integer(i4b)                      :: ixDime          ! dimension index
    integer(i4b)                      :: ixBeta          ! beta parameter variable index
    character(len=strLen)             :: cmessage        ! error message of downwind routine

    ! initialize error control
    ierr=0; message='defNetCDF/'

    do ixPar = 1, size(betaNames)

      ixBeta = get_ixBeta(betaNames(ixPar))

      allocate(dimName(size(betaMeta(ixBeta)%parDim)), stat=ierr)
      if(ierr/=0)then;message=trim(message)//'error allocating betaNames';return;endif

      do  ixDime =1, size(betaMeta(ixBeta)%parDim)
       dimName(ixDime) = dimMeta(betaMeta(ixBeta)%parDim(ixDime))%dname
      end do

      call def_var(ncid, betaNames(ixPar), dimName, ncd_float, &
                  ierr, cmessage,              &
                  vdesc=betaMeta(ixBeta)%parDesc, &
                  vunit=betaMeta(ixBeta)%parUnit)
      if(ierr/=0)then;message=trim(message)//trim(cmessage);return;endif

      deallocate(dimName, stat=ierr)
      if(ierr/=0)then;message=trim(message)//'error deallocating dimLens/dimStartIndex';return;endif

    end do

  end subroutine

  !private subroutine
  subroutine write_par(fname,           &  ! input: netcdf name
                       betaNames,       &  ! input: character array for beta parameter name
                       parDataStruct,   &  ! input: parameter data structure
                       ierr, message)      ! output: error control

    use globalData,    only: betaMeta
    use globalData,    only: dimMeta
    use get_ixname,    only: get_ixBeta

    implicit none
    ! input variables
    character(*), intent(in)          :: fname           ! NetCDF file name
    character(*), intent(in)          :: betaNames(:)    ! parameter name array
    type(namevar),intent(in)          :: parDataStruct(:)! input: data structure for model soil parameter at model layer x model hru
    ! output variables
    integer(i4b), intent(out)         :: ierr            ! error code
    character(*), intent(out)         :: message         ! error message
    ! local variables
    integer(i4b),allocatable          :: dimLens(:)      ! dimension length
    integer(i4b),allocatable          :: dimStartIndex(:)! dimension start index
    integer(i4b)                      :: ixPar           ! variable index
    integer(i4b)                      :: ixDime          ! dimension index
    integer(i4b)                      :: ixBeta          ! beta parameter variable index
    character(len=strLen)             :: cmessage        ! error message of downwind routine

    ! initialize error control
    ierr=0; message='defNetCDF/'

    do ixPar = 1, size(betaNames)

      ixBeta = get_ixBeta(betaNames(ixPar))

      allocate(dimLens(size(betaMeta(ixBeta)%parDim)), dimStartIndex(size(betaMeta(ixBeta)%parDim)), stat=ierr)
      if(ierr/=0)then;message=trim(message)//'error allocating dimLens/dimStartIndex';return;endif

      do  ixDime =1, size(betaMeta(ixBeta)%parDim)
       dimStartIndex(ixDime) = 1
       dimLens(ixDime)       = dimMeta(betaMeta(ixBeta)%parDim(ixDime))%dimLength
      end do

      if (size(betaMeta(ixBeta)%parDim)==1) then
        call write_nc(fname, betaNames(ixPar), parDataStruct(ixPar)%dvar1, dimStartIndex(1), dimLens(1), ierr,cmessage)
      else if (size(betaMeta(ixBeta)%parDim)==2) then
        call write_nc(fname, betaNames(ixPar), parDataStruct(ixPar)%dvar2, dimStartIndex, dimLens, ierr,cmessage)
      endif
      if(ierr/=0)then;message=trim(message)//trim(cmessage);return;endif

      deallocate(dimLens, dimStartIndex, stat=ierr)
      if(ierr/=0)then;message=trim(message)//'error deallocating dimLens/dimStartIndex';return;endif

    end do

  end subroutine

end module write_param_nc
