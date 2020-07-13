PROGRAM main_mpr

  use nrtype
  use public_var
  use read_config,     only: read_nml
  use popMeta,         only: paramMaster
  use globaldata,      only: calBetaName, parArray
  use process_meta,    only: read_inParList, get_parm_meta, param_setup, print_config
  use process_meta,    only: restartIO
  use tf,              only: betaDependency, betaCompOrder
  use mpr_routine,     only: run_mpr
  use read_soildata,   only: check_polyID

  implicit none

  character(len=strLen)             :: nmlfile         ! namelist containing configuration
  integer(i4b)                      :: ierr            ! error code
  character(len=strLen)             :: cmessage        ! error message from suroutine

   call getarg(1,nmlfile)
   if(len_trim(nmlfile)==0) call handle_err(50,'Need to supply name of the namelist file as a command-line argument')

  ! Read configuration namelists and save variables
  call read_nml( trim(nmlfile), ierr, cmessage ); call handle_err(ierr,cmessage)

  ! Populate master parameter meta.  Saved data: dimMeta, betaMeta, gammaMeta
  call paramMaster( ierr, cmessage ); call handle_err(ierr,cmessage)

  ! Read 'inParList' input listing metadata of beta parameters to be estimated. Saved data: 'inParMeta'
  call read_inParList( trim(inParList), ierr,  cmessage ); call handle_err(ierr,cmessage)

  ! Process 'inParMeta' along with master parameter meta data.
  ! Saved data: 'calGammaMeta', 'calBetaName', calScaleMeta, nCalGamma
  call get_parm_meta(ierr,cmessage); call handle_err(ierr,cmessage)

  ! Compute beta parameter dependency. Saved data: betaAncilMeta
  call betaDependency (ierr, cmessage); call handle_err(ierr,cmessage)

  ! Compute computing order of beta parameters including dependent parameters. Saved data: 'calBetaOrderIdx', nBetaNeed
  call betaCompOrder (calBetaName, ierr, cmessage); call handle_err(ierr,cmessage)

  call check_polyID(trim(mpr_input_dir)//trim(fname_soil), dname_spoly , ierr, cmessage); call handle_err(ierr, cmessage)

  ! updata gamma/scaling parameter based on restart
  call restartIO( mpr_param_file )

  ! initialize parameter and mask arrays
  call param_setup( mpr_param_file, ierr, cmessage ); call handle_err(ierr,cmessage)

  ! Print out calibration configuration
  call print_config()

  ! main routine starts depending on option
  call run_mpr( parArray(:,1), ierr, cmessage ); call handle_err(ierr,cmessage)
  stop

CONTAINS

  SUBROUTINE handle_err(err,message)
    ! handle error codes
    implicit none
    integer(i4b),intent(in)::err             ! error code
    character(*),intent(in)::message         ! error message
    if(err/=0)then
     print*,'FATAL ERROR: '//trim(message)
     stop
    endif
  END SUBROUTINE

END PROGRAM main_mpr
