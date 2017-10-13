module vic_routines
! Routines specific to VIC model
  use nrtype 
  use public_var
  use data_type 

  implicit none

  private

  public :: adj_soil_param_vic 
  public :: adj_vege_param_vic 
  public :: replace_soil_param_vic 
  public :: vic_hru_id
  public :: vic_soil_layer
  public :: read_vic_sim
  public :: read_soil_param_vic
  public :: write_soil_param_vic

contains

!***************************
! write VIC soil parameters 
!***************************
subroutine write_soil_param_vic(hruid, param, err, message)
  implicit none
  !input variables
  integer(i4b),intent(in)            :: hruid(:)     ! hru ID
  real(dp),    intent(in)            :: param(:,:)   ! 
  ! output
  integer(i4b),intent(out)           :: err          ! error code
  character(*),intent(out)           :: message      ! error message
  ! local variables
  integer(i4b)                       :: iHru         ! loop index
  integer(i4b)                       :: i            ! loop index

  ! initialize error control
  err=0; message='write_soil_param_vic/'
  if(size(param,2)/=TotNpar)then;err=10;message=trim(message)//"params 2nd dimension size different than TotNpar";return;endif
  if(size(param,1)/=nHru)then;err=11;message=trim(message)//'params 1st dimension size different than nHru';return;endif
  if(size(hruid)/=nHru)then;err=12;message=trim(message)//'hruid size different than nHru';return;endif
  open(UNIT=51,file=trim(calibparam_name),action='write',status='unknown' )
  hru:do iHru = 1,nHru
    write(51,'(I1,2X)',advance='no') 1
    write(51,'(I8,2X)',advance='no')       hruid(iHru)
    write(51,'(2(f9.4,X))',advance='no')  (param(iHru,i),i=3,4)
    write(51,'(f9.5,X)',advance='no')      param(iHru,5)
    write(51,'(4(f9.4,X))',advance='no')  (param(iHru,i),i=6,9)
    write(51,'(3(f9.4,X))',advance='no')  (param(iHru,i),i=10,12)
    write(51,'(3(f10.4,X))',advance='no') (param(iHru,i),i=13,15)
    write(51,'(3(f7.1,X))',advance='no')  (param(iHru,i),i=16,18)
    write(51,'(34(f10.4,X))',advance='no')(param(iHru,i),i=19,52)
    write(51,'(I2,X)',advance='no')       int(param(iHru,53))
    write(51,'(f9.4)')                     param(iHru,54)
  enddo hru 
  close(UNIT=51)
  return
end subroutine

!***************************
! Read VIC hru IDs 
!***************************
subroutine vic_hru_id(hruid, err, message)
  implicit none
  ! input 
  ! output
  integer(i4b),intent(out)                   :: hruid(:)     ! list of hru where calibration is performed 
  integer(i4b),intent(out)                   :: err          ! error code
  character(*),intent(out)                   :: message      ! error message
  ! local variables
  real(dp),dimension(TotNpar)                :: realline
  integer(i4b)                               :: iPar,iHru    ! loop index
  integer(i4b)                               :: stat

  ! initialize error control
  err=0; message='vic_hru_id/'
  open (UNIT=50,file=origparam_name,form='formatted',status='old',IOSTAT=stat)
 ! Read original soil parameter file
  do iHru = 1,nHru
    read(unit=50,fmt=*) (realline(iPar), iPar=1,TotNpar)
    hruid(iHru)=int(realline(2))
  end do
  close(UNIT=50)
  return
end subroutine

!***************************
! Read VIC soil layer parameters 
!***************************
subroutine vic_soil_layer(hlyr, err, message)
  implicit none
  ! input 
  ! output
  real(dp),    intent(out)           :: hlyr(:,:) ! soil layer thickness (bucket size) matrix (nHru x nLyr)
  integer(i4b),intent(out)           :: err       ! error code
  character(*),intent(out)           :: message   ! error message
  ! local variables
  real(dp),dimension(TotNpar)        :: realline
  integer(i4b)                       :: ipar,iHru  ! loop index
  integer(i4b)                       :: stat

  ! initialize error control
  err=0; message='vic_soil_layer/'
  open (UNIT=50,file=origparam_name,form='formatted',status='old',IOSTAT=stat)
 ! Read original soil parameter file
  do iHru = 1,nHru
    read(unit=50,fmt=*) (realline(ipar), ipar=1,TotNpar)
    hlyr(iHru,:)=realline(4*nLyr+11:5*nLyr+10)
  end do
  close(UNIT=50)
  return
end subroutine

!***************************
! Read VIC soil parameters 
!***************************
subroutine read_soil_param_vic(param, err, message)
  implicit none
  ! input 
  ! output
  real(dp),    intent(out)                   :: param(:,:)   ! calibrating parameter list 
  integer(i4b),intent(out)                   :: err          ! error code
  character(*),intent(out)                   :: message      ! error message
  ! local variables
  integer(i4b)                               :: ipar,iHru    ! loop index
  integer(i4b)                               :: stat

  ! initialize error control
  err=0; message='vic_soil_param/'
  open (UNIT=50,file=origparam_name,form='formatted',status='old',IOSTAT=stat)
 ! Read original soil parameter file
  do iHru = 1,nHru
    read(unit=50,fmt=*) (param(iHru,ipar), ipar=1,TotNpar)
  end do
  close(UNIT=50)
  return
end subroutine

!***************************
! replace VIC soil parameters 
!***************************
subroutine replace_soil_param_vic(param, hModel, parMxyMz, adjParam, err, message)
  use globalData, only: betaMeta, calBetaName, nSoilBetaModel 
  use get_ixname, only: get_ixBeta
  implicit none
  !input variables
  real(dp),         intent(in)   :: param(:,:)    ! original soil parameters 
  real(dp),         intent(in)   :: hModel(:,:)   ! Model layer thickness at model hrus 
  type(namedvar2),  intent(in)   :: parMxyMz(:)   ! soil model parameter at model layer x model hrus
  ! output
  real(dp),         intent(out)  :: adjParam(:,:) ! adjusted soil parameter
  integer(i4b),     intent(out)  :: err          ! error code
  character(*),     intent(out)  :: message       ! error message
  ! local variables
  integer(i4b)                   :: ipar,iHru     ! loop index

  ! initialize error control
  err=0; message='replace_soil_param_vic/'
  adjParam=param
  hru: do iHru = 1,nHru
  ! replace parameter values
    adjParam(iHru,23:25)    = hModel(:,iHru)
    do iPar=1,nSoilBetaModel
      associate( ix=>get_ixBeta(trim(calBetaName(iPar))) )
      select case( betaMeta(ix)%pname )
        case('binfilt');  adjParam(iHru,5)     = parMxyMz(iPar)%varData(1, iHru) 
        case('D1');       adjParam(iHru,6)     = parMxyMz(iPar)%varData(nLyr,iHru)
        case('D2');       adjParam(iHru,7)     = parMxyMz(iPar)%varData(nLyr,iHru)
        case('D3');       adjParam(iHru,8)     = parMxyMz(iPar)%varData(nLyr,iHru)
        case('D4');       adjParam(iHru,9)     = parMxyMz(iPar)%varData(nLyr,iHru)
        case('expt');     adjParam(iHru,10:12) = parMxyMz(iPar)%varData(:,iHru)
        case('ks');       adjParam(iHru,13:15) = parMxyMz(iPar)%varData(:,iHru)*60*60*24
        case('bbl');      adjParam(iHru,28:30) = parMxyMz(iPar)%varData(:,iHru)
        case('bd');       adjParam(iHru,34:36) = parMxyMz(iPar)%varData(:,iHru)
        case('sd');       adjParam(iHru,37:39) = parMxyMz(iPar)%varData(:,iHru)
        case('WcrFrac');  adjParam(iHru,41:43) = parMxyMz(iPar)%varData(:,iHru)
        case('WpwpFrac'); adjParam(iHru,44:46) = parMxyMz(iPar)%varData(:,iHru)
      end select
      end associate
    end do
  enddo hru
  return
end subroutine

!***************************
! Adjust VIC soil parameters with multipliers 
!***************************
subroutine adj_soil_param_vic(param, multiplier, adjParam,  err, message)
!! This routine takes the adjustable parameter set "param" from namelist, reads into "origparam_name",
!! computes the new parameters, writes them into "calibparam_name" 
  use globalData, only: calParMeta, nCalPar
  implicit none
  !input variables
  real(dp),    intent(in)    :: param(:,:)    ! original soil parameters 
  type(var_d), intent(in)    :: multiplier(:) ! mulitpliers for calibrating soil parameter 
  ! output
  real(dp),    intent(out)   :: adjParam(:,:) ! adjusted soil parameter
  integer(i4b),intent(out)   :: err           ! error code
  character(*),intent(out)   :: message       ! error message
  ! local variables
  integer(i4b)               :: iPar,iHru     ! loop index

  ! initialize error control
  err=0; message='adj_soil_param_vic/'
  adjParam=param
  do iHru = 1,nHru
    ! Modify parameter values
    do iPar=1,nCalPar
      select case( calParMeta(iPar)%pname )
        case('binfilt');  adjParam(iHru,5)     = multiplier( iPar )%var(1)!*Param(iHru,5)
        case('D1');       adjParam(iHru,6)     = multiplier( iPar )%var(1)!*Param(iHru,6)
        case('D2');       adjParam(iHru,7)     = multiplier( iPar )%var(1)!*Param(iHru,7)
        case('D3');       adjParam(iHru,8)     = multiplier( iPar )%var(1)!*Param(iHru,8)
        case('D4');       adjParam(iHru,9)     = multiplier( iPar )%var(1)!*Param(iHru,9)
        case('expt');     adjParam(iHru,10:12) = multiplier( iPar )%var(1:nLyr)!*Param(iHru,10:12)
        case('ks');       adjParam(iHru,13:15) = multiplier( iPar )%var(1:nLyr)!*Param(iHru,13:15)
        case('h1');       adjParam(iHru,23)    = multiplier( iPar )%var(1)!*Param(iHru,23)
        case('h2');       adjParam(iHru,24)    = multiplier( iPar )%var(1)!*Param(iHru,24)
        case('h3');       adjParam(iHru,25)    = multiplier( iPar )%var(1)!*Param(iHru,25)
        case('bbl');      adjParam(iHru,28:30) = multiplier( iPar )%var(1:nLyr)!*Param(iHru,28:30)
        case('bd');       adjParam(iHru,34:36) = multiplier( iPar )%var(1:nLyr)!*Param(iHru,34:36)
        case('sd');       adjParam(iHru,37:39) = multiplier( iPar )%var(1:nLyr)!*Param(iHru,37:39)
        case('WcrFrac');  adjParam(iHru,41:43) = multiplier( iPar )%var(1:nLyr)!*Param(iHru,41:43)
        case('WpwpFrac'); adjParam(iHru,44:46) = multiplier( iPar )%var(1:nLyr)!*Param(iHru,44:46)
       end select
    end do
    ! Limit parameters to correct possible values without physical meaning: this applies for all configurations
    !binfilt
    if(adjParam(iHru,5) .lt. 0.001) then
      adjParam(iHru,5) = 0.001
    elseif(adjParam(iHru,5) .gt. 0.5) then
      adjParam(iHru,5) = 0.5
    endif
    !Ds
    if(adjParam(iHru,6) .lt. 0.0001) then
      adjParam(iHru,6) = 0.0001
    elseif(adjParam(iHru,6) .gt. 1.0) then
      adjParam(iHru,6) = 1.0
    endif
    !Dsmax
    if(adjParam(iHru,7) .lt. 0.0001) then
      adjParam(iHru,7) = 0.00001
    elseif(adjParam(iHru,7) .gt. 1.0) then
      adjParam(iHru,7) = 1.0
    endif
    !Ws
    if(adjParam(iHru,8) .lt. 0.0001) then
      adjParam(iHru,8) = 0.0001
    elseif(adjParam(iHru,8) .gt. 1000) then
      adjParam(iHru,8) = 1000.0 
    endif
    !bulk density for each layer
    do iPar = 34,36
      if(adjParam(iHru,iPar) .lt. 805.) then
        adjParam(iHru,iPar) = 805.
      elseif(adjParam(iHru,iPar) .gt. adjParam(iHru,iPar+3)) then
        adjParam(iHru,iPar) = adjParam(iHru,iPar+3)*0.9 
      endif
    enddo
    if(adjParam(iHru,23) .gt. adjParam(iHru,24)) then
      adjParam(iHru,23)=adjParam(iHru,24)
    endif
  enddo 
  return
end subroutine

!***************************
! Adjust VIC vege parameters with multiplier
!***************************
subroutine adj_vege_param_vic(multiplier, err, message)
  use globalData, only: calParMeta, nCalPar
  implicit none

  ! input variables
  type(var_d),          intent(in) :: multiplier(:)             ! list of calibratin parameters 
  ! output
  integer(i4b),intent(out)         :: err                       ! error code
  character(*),intent(out)         :: message                   ! error message
  ! local variables
  integer(i4b)                     :: vegClass                  ! vegetation class 
  real(dp)                         :: vegFrac                   ! fraction of vage class
  real(dp),dimension(nLyr)         :: rootDepth                 ! root zone depth
  real(dp),dimension(nLyr)         :: rootFrac                  ! root zone fraction
  real(dp),dimension(12)           :: laiMonth                  ! monthly LAI
  integer(i4b)                     :: hruID                     ! hru ID
  integer(i4b)                     :: nTile                     ! number of vege tile 
  integer(i4b)                     :: iPar,iHru,iTile,iMon,iLyr ! loop index
  character(50)                    :: rowfmt                    ! string specifying write format for real value
  integer(i4b)                     :: stat

  ! initialize error control
  err=0; message='adj_vege_param_vic/'
  !Open original and modified vege parameter files
  open (UNIT=50,file=origvege_name,form='formatted',status='old',IOSTAT=stat)
  open (UNIT=51,file=calivege_name,action='write',status='replace' )
  write(rowfmt,'(A,I2,A)') '(',nLyr,'(1X,F4.2))'
 ! Read original vege parameter file
  hru:do iHru = 1,nHru
    read(unit=50,fmt=*) hruID,nTile
    write(51,'(I10,1X,I2)') hruID, nTile
    tile:do iTile = 1,nTile
      read(unit=50,fmt=*) vegClass,vegFrac,(rootDepth(iLyr), iLyr=1,nLyr),(rootFrac(iLyr), iLyr=1,nLyr)
      read(unit=50,fmt=*) (laiMonth(iMon), iMon=1,12)
      ! Modify parameter values
      par:do iPar=1,nCalPar
        select case( calParMeta(iPar)%pname )
          case('lai');    laiMonth = multiplier( iPar )%var(1)*laiMonth
        end select
      enddo par
      ! Write the modified parameter file for the entire basin/region for traditional upscaling
      write(51,'(3X,I2,1X,F8.6)',advance='no') vegClass, vegFrac
      write(51,rowfmt,advance='no')            (rootDepth(iLyr), iLyr=1,nLyr)
      write(51,rowfmt)                         (rootFrac(iLyr), iLyr=1,nLyr)
      write(51,'(5X,12(1X,F6.3))')             (laiMonth(iMon), iMon=1,12)
    enddo tile 
  enddo hru
  ! Close original and modified basin parameter files
  close(UNIT=50)
  close(UNIT=51)
  return
end subroutine

!***************************
! Read VIC output file
!***************************
subroutine read_vic_sim(err, message, prec, qsim )
  implicit none
  !output variables
  real(dp),optional,     intent(out) :: prec(:,:)
  real(dp),optional,     intent(out) :: qsim(:,:)
  integer(i4b),          intent(out) :: err            ! error code
  character(*),          intent(out) :: message        ! error message
  !local variables
  character(len=strLen)              :: filename
  real(dp)                           :: cellfraction,basin_area
  real(dp)                           :: auxflux(6)                 ! This is only in case of water balance mode
  integer(i4b)                       :: ibasin, itime, ivar, icell ! index 
  integer(i4b)                       :: ncell
  integer(i4b)                       :: dum,c_cell

  ! initialize error control
  err=0; message='read_vic_sim/'
  !set output variable to zero
  !cell counter
  c_cell = 1
  !open a few files
  open (UNIT=53,file=trim(filelist_name),form='formatted',status='old')
  open (UNIT=54,file=trim(cellfrac_name),form='formatted',status='old')
  open (UNIT=51,file=trim(region_info),form='formatted',status='old')
  do ibasin = 1,nbasin
    read (UNIT=51,fmt=*) dum,dum,basin_area,ncell
    do icell = 1,ncell
      read (UNIT=53,fmt=*) filename
      read (UNIT=54,fmt=*) cellfraction
      filename=trim(sim_dir)//trim(filename)
      open (UNIT=55,file= filename,form='formatted',status='old')
      do itime = 1,sim_len
        read (UNIT=55,fmt=*) (auxflux(ivar), ivar=1,6)
        if (present(prec)) then 
          prec(c_cell,itime) = (auxflux(4))*cellfraction
        end if 
        if (present(qsim)) then 
          qsim(c_cell,itime) = (auxflux(5) + auxflux(6))*cellfraction
        end if 
      enddo
      close(UNIT=55)
      c_cell = c_cell + 1
    enddo
  enddo
  close(UNIT=51)
  close(UNIT=53)
  close(UNIT=54)
  return
end subroutine

end module vic_routines
