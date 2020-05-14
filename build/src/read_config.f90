module read_config
  use nrtype
  use public_var

  implicit none

  private

  public :: read_nml

! Main configuration
  namelist / runconfig / mpr_param_file,          &
                         inParList

! MPR configuration
  namelist / mprconfig /  mpr_input_dir,          &
                          mpr_output_dir,         &
                          soil_param_nc,          &
                          veg_param_nc,           &
                          fname_soil,             &
                          fname_topo,             &
                          fname_veg,              &
                          fname_mapping,          &
                          dname_overPoly,         &
                          dname_hru,              &
                          sclass_table,           &
                          vclass_table,           &
                          nVclass,                &
                          dname_spoly,            &
                          dname_slyrs,            &
                          dname_tpoly,            &
                          dname_vpoly,            &
                          nHru
  namelist /space / nHru, &
                    nLyr

  namelist / soil / hfrac

contains

! --------------------------
subroutine read_nml(nmlfile, err, message)

  implicit none

  ! input
  character(*), intent(in)  :: nmlfile
  ! output variables
  integer(i4b)              :: err
  character(len=strLen)     :: message    ! error message for downwind routine

  ! Start procedure here
  err=0; message="read_nml/"

  ! Open namelist file
  open(UNIT=30, file=trim(nmlfile),status="old", action="read", iostat=err )
  if(err/=0)then; message=trim(message)//"Error:Open namelist"; return; endif

  ! read "runconfig" group
  read(unit=30, NML=runconfig, iostat=err)
  if (err/=0)then; message=trim(message)//"Error:Read runconfig"; return; endif

  ! read "mprconfig" group
  read(unit=30, NML=mprconfig, iostat=err)
  if (err/=0)then; message=trim(message)//"Error:Read mprconfig"; return; endif

  ! read "space" group
  read(unit=30, NML=space, iostat=err)
  if (err/=0)then; message=trim(message)//"Error:Read space"; return; endif

  ! read "soil" group
  allocate(hfrac(nLyr))
  read(unit=30, NML=soil, iostat=err)
  if (err/=0)then; message=trim(message)//"Error:Read soil"; return; endif

  ! check
  if (abs(sum(hfrac)-1._dp)>verySmall) then
    err=10; message=trim(message)//"sum of layer frac is not one"; return
  endif

  close(UNIT=30)

  print *, 'Namelist file has been successfully processed'

end subroutine

end module read_config
