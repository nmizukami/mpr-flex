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
                          nLyr,                   &
                          nHru

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

  close(UNIT=30)

  print *, 'Namelist file has been successfully processed'

end subroutine

end module read_config
