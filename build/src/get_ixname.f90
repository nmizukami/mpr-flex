module get_ixname
! Define functions to get the index of a named variable
use nrtype                  ! variable types, etc.
use public_var

implicit none

private

public::get_ixGamma         ! assign variable index to gamma parameter
public::get_ixBeta          ! assign variable index to beta parameter
public::get_ixDataMap       ! assign variable index to variables in mapping netCDF data
public::get_ixDataVeg       ! assign variable index to variables in veg netCDF data
public::get_ixDataSoil      ! assign variable index to variables in soil netCDF data
public::get_ixDataTopo      ! assign variable index to variables in topo netCDF data
public::get_ixPrpVeg

contains

! *******************************************************************************************************************
! function: get the index of the named variables for gamma parameters
! *******************************************************************************************************************
 function get_ixGamma(varName)
   use var_lookup,only:ixGamma             ! indices of the named variables
   implicit none
   ! define dummy variables
   character(*), intent(in) :: varName     ! variable name
   integer(i4b)             :: get_ixGamma ! index of the named variable
   ! get the index of the named variables
   select case(trim(varName))
     case('ks1gamma1');        get_ixGamma = ixGamma%ks1gamma1         ! gamma parameter-1 for 1st ksat transfer function (Cosby et al. 1984)
     case('ks1gamma2');        get_ixGamma = ixGamma%ks1gamma2         ! gamma parameter-2 for 1st ksat transfer function (Cosby et al. 1984)
     case('ks1gamma3');        get_ixGamma = ixGamma%ks1gamma3         ! gamma parameter-3 for 1st ksat transfer function (Cosby et al. 1984)
     case('ks2gamma1');        get_ixGamma = ixGamma%ks2gamma1         ! gamma parameter-1 for 2nd ksat transfer function
     case('ks2gamma2');        get_ixGamma = ixGamma%ks2gamma2         ! gamma parameter-2 for 2nd ksat transfer function
     case('ks2gamma3');        get_ixGamma = ixGamma%ks2gamma3         ! gamma parameter-3 for 2nd ksat transfer function
     case('phi1gamma1');       get_ixGamma = ixGamma%phi1gamma1        ! gamma parameter-1 for 1st porosity transfer function (Cosby et al. 1984)
     case('phi1gamma2');       get_ixGamma = ixGamma%phi1gamma2        ! gamma parameter-2 for 1st porosity transfer function (Cosby et al. 1984)
     case('phi1gamma3');       get_ixGamma = ixGamma%phi1gamma3        ! gamma parameter-3 for 1st porosity transfer function (Cosby et al. 1984)
     case('phi2gamma1');       get_ixGamma = ixGamma%phi2gamma1        ! gamma parameter-1 for 2nd porosity transfer function
     case('phi2gamma2');       get_ixGamma = ixGamma%phi2gamma2        ! gamma parameter-2 for 2nd porosity transfer function
     case('phi2gamma3');       get_ixGamma = ixGamma%phi2gamma3        ! gamma parameter-3 for 2nd porosity transfer function
     case('phi2gamma4');       get_ixGamma = ixGamma%phi2gamma4        ! gamma parameter-4 for 2nd porosity transfer function
     case('phi2gamma5');       get_ixGamma = ixGamma%phi2gamma5        ! gamma parameter-5 for 2nd porosity transfer function
     case('phi2gamma6');       get_ixGamma = ixGamma%phi2gamma6        ! gamma parameter-6 for 2nd porosity transfer function
     case('fc1gamma1');        get_ixGamma = ixGamma%fc1gamma1         ! gamma parameter-1 for 1st field capacity transfer function (koren et al. 2003)
     case('wp1gamma1');        get_ixGamma = ixGamma%wp1gamma1         ! gamma parameter-1 for 1st wilting point transfer function (koren et al. 2003)
     case('resid1gamma1');     get_ixGamma = ixGamma%resid1gamma1      ! gamma parameter-1 for 1st residual transfer function
     case('transp1gamma1');    get_ixGamma = ixGamma%transp1gamma1     ! gamma parameter-1 for 1st soil transpire transfer function
     case('b1gamma1');         get_ixGamma = ixGamma%b1gamma1          ! gamma parameter-1 for 1st retention curve slope transfer function (cosby et al. 1984)
     case('b1gamma2');         get_ixGamma = ixGamma%b1gamma2          ! gamma parameter-2 for 1st retention curve slope transfer function (cosby et al. 1984)
     case('b1gamma3');         get_ixGamma = ixGamma%b1gamma3          ! gamma parameter-3 for 1st retention curve slope transfer function (Cosby et al. 1984)
     case('psis1gamma1');      get_ixGamma = ixGamma%psis1gamma1       ! gamma parameter-1 for 1st saturation potential matric transfer function (koren et al. 2003)
     case('psis1gamma2');      get_ixGamma = ixGamma%psis1gamma2       ! gamma parameter-2 for 1st saturation potential matric transfer function (koren et al. 2003)
     case('psis1gamma3');      get_ixGamma = ixGamma%psis1gamma3       ! gamma parameter-3 for 1st saturation potential matric transfer function (koren et al. 2003)
     case('myu1gamma1');       get_ixGamma = ixGamma%myu1gamma1        ! gamma parameter-1 for 1st specific yield transfer function (koren et al. 2003)
     case('myu1gamma2');       get_ixGamma = ixGamma%myu1gamma2        ! gamma parameter-2 for 1st specific yield transfer function (koren et al. 2003)
     case('sof1gamma1');       get_ixGamma = ixGamma%sof1gamma1        ! gamma parameter-1 for 1st organic matter content transfer function
     case('h1gamma1');         get_ixGamma = ixGamma%h1gamma1          ! gamma parameter-1 for top layer thickness [m] (fraction of z)
     case('h2gamma1');         get_ixGamma = ixGamma%h2gamma1          ! gamma parameter-1 2nd layer thickness [m] (franction of z)
     case('binfilt1gamma1');   get_ixGamma = ixGamma%binfilt1gamma1    ! gamma parameter-1 for variable infilitration curve parameter
     case('binfilt1gamma2');   get_ixGamma = ixGamma%binfilt1gamma2    ! gamma parameter-2 for variable infilitration curve parameter
     case('D11gamma1');        get_ixGamma = ixGamma%D11gamma1         ! gamma parameter-1 for linear reservoir coefficient
     case('D21gamma1');        get_ixGamma = ixGamma%D21gamma1         ! gamma parameter-1 for nonlinear reservoir coefficient
     case('D31gamma1');        get_ixGamma = ixGamma%D31gamma1         ! gamma parameter-1 for nonlinear reservoir water content adjustment
     case('D41gamma1');        get_ixGamma = ixGamma%D41gamma1         ! gamma parameter-1 for exponent of nonlinear reservoir
     case('expt1gamma1');      get_ixGamma = ixGamma%expt1gamma1       ! gamma parameter-1 for exponent in Campbell equatin for Kh
     case('expt1gamma2');      get_ixGamma = ixGamma%expt1gamma2       ! gamma parameter-2 for exponent in Campbell equatin for Kh
     case('bbl1gamma1');       get_ixGamma = ixGamma%bbl1gamma1        ! gamma parameter-1 for bubbling pressure of soil [cm]
     case('bbl1gamma2');       get_ixGamma = ixGamma%bbl1gamma2        ! gamma parameter-2 for bubbling pressure of soil [cm]
     case('sd1gamma1');        get_ixGamma = ixGamma%sd1gamma1         ! gamma parameter-1 for soil particle density [kg/m^3]
     case('bd1gamma1');        get_ixGamma = ixGamma%bd1gamma1         ! gamma parameter-1 for bulk density [kg/m^3]
     case('WcrFrac1gamma1');   get_ixGamma = ixGamma%WcrFrac1gamma1    ! gamma parameter-1 for Fractional soil moisture content at critical point [-]
     case('WpwpFrac1gamma1');  get_ixGamma = ixGamma%WpwpFrac1gamma1   ! gamma parameter-1 for Fractional soil moisture content at wilting point [-]
     case('bfr1gamma1');       get_ixGamma = ixGamma%bfr1gamma1        ! gamma parameter-1 for nonliear reservoir coefficient with normalized strage
     case('fsm1gamma1');       get_ixGamma = ixGamma%fsm1gamma1        !
     case('zk1gamma1');        get_ixGamma = ixGamma%zk1gamma1         !
     case('zsk1gamma1');       get_ixGamma = ixGamma%zsk1gamma1        !
     case('zsk1gamma2');       get_ixGamma = ixGamma%zsk1gamma2        !
     case('zpk1gamma1');       get_ixGamma = ixGamma%zpk1gamma1        !
     case('pfree1gamma1');     get_ixGamma = ixGamma%pfree1gamma1      !
     case('rexp1gamma1');      get_ixGamma = ixGamma%rexp1gamma1       !
     case('lai1gamma1');       get_ixGamma = ixGamma%lai1gamma1        ! gamma parameter-1 for monthly lai (multiplier)
     case('cht1gamma1');       get_ixGamma = ixGamma%cht1gamma1        ! gamma parameter-1 for height of canopy top
     case('chb1gamma1');       get_ixGamma = ixGamma%chb1gamma1        ! gamma parameter-1 for height of canopy bottom
     case('scf1gamma1');       get_ixGamma = ixGamma%scf1gamma1        ! gamma parameter-1 for soil precipitation correction factor
      ! get to here if cannot find the variable
      case default;             get_ixGamma = imiss
   end select
 end function

! *******************************************************************************************************************
! function: get the index of the named variables for beta parameters
! *******************************************************************************************************************
 function get_ixBeta(varName)
   use var_lookup,only:ixBeta             ! indices of the named variables
   implicit none
   ! define dummy variables
   character(*), intent(in) :: varName     ! variable name
   integer(i4b)             :: get_ixBeta ! index of the named variable
   ! get the index of the named variables
   select case(trim(varName))
     ! Beta parameters
     case('uhshape');          get_ixBeta = ixBeta%uhshape           ! gamma pdf uh shape parameter [-]
     case('uhscale');          get_ixBeta = ixBeta%uhscale           ! gamma pdf uh scale parameter [-]
     case('ks');               get_ixBeta = ixBeta%ks                ! saturated hydrologic conductivity [m/s]
     case('bd');               get_ixBeta = ixBeta%bd                ! soil particle density [kg/m^3]
     case('sd');               get_ixBeta = ixBeta%sd                ! soil particle density [kg/m^3]
     case('psis');             get_ixBeta = ixBeta%psis              ! matric potential [kPa]
     case('b');                get_ixBeta = ixBeta%b                 ! Fractional soil moisture content at wilting point [-]
     case('phi');              get_ixBeta = ixBeta%phi               ! porosity [-]
     case('fc');               get_ixBeta = ixBeta%fc                ! field capacity[-]
     case('wp');               get_ixBeta = ixBeta%wp                ! wilting point [-]
     case('resid');            get_ixBeta = ixBeta%resid             ! residual [-]
     case('transp');           get_ixBeta = ixBeta%transp            ! transpire [-]
     case('myu');              get_ixBeta = ixBeta%myu               ! Fractional soil moisture content at wilting point [-]
     case('sof');              get_ixBeta = ixBeta%sof               ! Fractional soil organic content [-]
     case('binfilt');          get_ixBeta = ixBeta%binfilt           ! variable infilitration curve parameter
     case('D1');               get_ixBeta = ixBeta%D1                !
     case('D4');               get_ixBeta = ixBeta%D4                !
     case('D2');               get_ixBeta = ixBeta%D2                !
     case('D3');               get_ixBeta = ixBeta%D3                !
     case('Dsmax');            get_ixBeta = ixBeta%Dsmax             !
     case('Ds');               get_ixBeta = ixBeta%Ds                !
     case('Ws');               get_ixBeta = ixBeta%Ws                !
     case('expt');             get_ixBeta = ixBeta%expt              ! exponent in Campbell equatin for Kh
     case('bbl');              get_ixBeta = ixBeta%bbl               ! bubbling pressure of soil [cm]
     case('h');                get_ixBeta = ixBeta%h                 ! layer thickness
     case('WcrFrac');          get_ixBeta = ixBeta%WcrFrac           ! Fractional soil moisture content at critical point [-]
     case('WpwpFrac');         get_ixBeta = ixBeta%WpwpFrac          ! Fractional soil moisture content at wilting point [-]
     case('bfr');              get_ixBeta = ixBeta%bfr               ! nonliear reservoir coefficient with normalized strage [m/s]
     case('twm');              get_ixBeta = ixBeta%twm               ! Tention water content [mm]
     case('fwm');              get_ixBeta = ixBeta%fwm               ! Free water content [mm]
     case('fsm');              get_ixBeta = ixBeta%fsm               ! Supplemental free water content [mm]
     case('fpm');              get_ixBeta = ixBeta%fpm               ! Primary free water content [mm]
     case('zk');               get_ixBeta = ixBeta%zk                ! draw coefficient from free water content [/day]
     case('zsk');              get_ixBeta = ixBeta%zsk               ! draw coefficient from supplemental free water content [/day]
     case('zpk');              get_ixBeta = ixBeta%zpk               ! draw coefficient form primary free water content [/day]
     case('pfree');            get_ixBeta = ixBeta%pfree             !
     case('zperc');            get_ixBeta = ixBeta%zperc             !
     case('rexp');             get_ixBeta = ixBeta%rexp              !
     case('rmin');             get_ixBeta = ixBeta%rmin              ! minimum stomatal resistance
     case('lai');              get_ixBeta = ixBeta%lai               ! monthly lai [m2/m2]
     case('cht');              get_ixBeta = ixBeta%cht               ! height of canopy top [m]
     case('chb');              get_ixBeta = ixBeta%chb               ! height of canopy bottom [m]
     case('scf');              get_ixBeta = ixBeta%scf               ! soil precipitation correction factor [fraction]
     case('mfmax');            get_ixBeta = ixBeta%mfmax             !
     case('mfmin');            get_ixBeta = ixBeta%mfmin             !
     case('uadj');             get_ixBeta = ixBeta%uadj              !
     case('si');               get_ixBeta = ixBeta%si                !
     case('pxtemp');           get_ixBeta = ixBeta%pxtemp            !
     case('nmf');              get_ixBeta = ixBeta%nmf               !
     case('tipm');             get_ixBeta = ixBeta%tipm              !
     case('plwhc');            get_ixBeta = ixBeta%plwhc             !
     case('daygm');            get_ixBeta = ixBeta%daygm             !
     ! get to here if cannot find the variable
     case default;             get_ixBeta = imiss
   end select
 end function

! *******************************************************************************************************************
! function: get the index of the named variables for mapping data
! *******************************************************************************************************************
 function get_ixDataMap(varName)
   USE var_lookup,only:ixVarMapData               ! indices of the named variables
   implicit none
   ! define dummy variables
   character(*), intent(in) :: varName            ! variable name
   integer(i4b)             :: get_ixDataMap      ! index of the named variable
   ! get the index of the named variables
   select case(trim(varName))
     case('hru_id');         get_ixDataMap = ixVarMapData%hru_id         ! hru ID
     case('weight');         get_ixDataMap = ixVarMapData%weight         ! weight of intersecting geophysical polygon
     case('intersector');    get_ixDataMap = ixVarMapData%intersector    ! ID of intersecting geophysical polygon
     case('overlaps');       get_ixDataMap = ixVarMapData%overlaps       ! number of intersecting geophysical polygon
     ! get to here if cannot find the variable
     case default;           get_ixDataMap = imiss
   end select
 end function

! *******************************************************************************************************************
! function: get the index of the named variables for veg data
! *******************************************************************************************************************
 function get_ixDataVeg(varName)
   USE var_lookup,only:ixVarVegData               ! indices of the named variables
   implicit none
   ! define dummy variables
   character(*), intent(in) :: varName            ! variable name
   integer(i4b)             :: get_ixDataVeg      ! index of the named variable
   ! get the index of the named variables
   select case(trim(varName))
     case('polyid');     get_ixDataVeg = ixVarVegData%polyid     ! veg polygon ID
     case('lai');        get_ixDataVeg = ixVarVegData%lai        ! monthly lai *10 [m2 m-2]
     case('ch');         get_ixDataVeg = ixVarVegData%ch         ! canopy height [m]
     case('vegclass');   get_ixDataVeg = ixVarVegData%vegclass   ! veg class in veg polygon and layer
     ! get to here if cannot find the variable
     case default;     get_ixDataVeg = imiss
   endselect
 end function

! *******************************************************************************************************************
! function: get the index of the named variables for soil data
! *******************************************************************************************************************
 function get_ixDataSoil(varName)
  USE var_lookup,only:ixVarSoilData                 ! indices of the named variables
  implicit none
  ! define dummy variables
  character(*), intent(in) :: varName            ! variable name
  integer(i4b)             :: get_ixDataSoil     ! index of the named variable
  ! get the index of the named variables
  select case(trim(varName))
   case('polyid');       get_ixDataSoil = ixVarSoilData%polyid        ! soil polygon ID
   case('hslyrs');       get_ixDataSoil = ixVarSoilData%hslyrs        ! soil layer thickness [m]
   case('sand_pct');     get_ixDataSoil = ixVarSoilData%sand_pct      ! sand percent in soil polygon and layer [%]
   case('silt_pct');     get_ixDataSoil = ixVarSoilData%silt_pct      ! silt percent in soil polygon and layer [%]
   case('clay_pct');     get_ixDataSoil = ixVarSoilData%clay_pct      ! clay percent in soil polygon and layer [%]
   case('bulk_density'); get_ixDataSoil = ixVarSoilData%bulk_density  ! bulk density in soil polygon and layer [kg/m3]
   case('soc');          get_ixDataSoil = ixVarSoilData%soc           ! soil carbon content in soil polygon and layer [%]
   ! get to here if cannot find the variable
   case default;     get_ixdataSoil = imiss
  endselect
 end function

! *******************************************************************************************************************
! function: get the index of the named variables for veg data
! *******************************************************************************************************************
 function get_ixDataTopo(varName)
   USE var_lookup,only:ixVarTopoData              ! indices of the named variables
   implicit none
   ! define dummy variables
   character(*), intent(in) :: varName            ! variable name
   integer(i4b)             :: get_ixDataTopo     ! index of the named variable
   ! get the index of the named variables
   select case(trim(varName))
     case('polyid');     get_ixDataTopo = ixVarTopoData%polyid     ! polygon ID
     case('ele_mean');   get_ixDataTopo = ixVarTopoData%ele_mean   ! average elev over soil polygon [m]
     case('ele_std');    get_ixDataTopo = ixVarTopoData%ele_std    ! std elev over soil polygon [m]
     case('slp_mean');   get_ixDataTopo = ixVarTopoData%slp_mean   ! average slope over soil polygon [percent]
     ! get to here if cannot find the variable
     case default;     get_ixDataTopo = imiss
   endselect
 end function

! *******************************************************************************************************************
! function: get the index of the named variables for climate data
! *******************************************************************************************************************
 function get_ixDataClim(varName)
   USE var_lookup,only:ixVarClimData              ! indices of the named variables
   implicit none
   ! define dummy variables
   character(*), intent(in) :: varName            ! variable name
   integer(i4b)             :: get_ixDataClim     ! index of the named variable
   ! get the index of the named variables
   select case(trim(varName))
     case('polyid'); get_ixDataClim = ixVarClimData%polyid ! polygon ID
     case('prec');   get_ixDataClim = ixVarClimData%prec   ! Monthly total precipitation [mm]
     case('tavg');   get_ixDataClim = ixVarClimData%tavg   ! Monthly mean air temperature [degree-C]
     case('wind');   get_ixDataClim = ixVarClimData%wind   ! Monthly mean wind speed [m s-1]
     case('ai');     get_ixDataClim = ixVarClimData%ai     ! Annual mean aridity index [-]
     ! get to here if cannot find the variable
     case default;     get_ixDataClim = imiss
   endselect
 end function

! *******************************************************************************************************************
! function: get the index of the named variables for vegetation properties
! *******************************************************************************************************************
 function get_ixPrpVeg(varName)
  use var_lookup,only:ixPrpVeg                   ! indices of the named variables
  implicit none
  ! define dummy variables
  character(*), intent(in) :: varName            ! variable name
  integer(i4b)             :: get_ixPrpVeg       ! index of the named variable
  ! get the index of the named variables
  select case(trim(varName))
   case('lai');          get_ixPrpVeg = ixPrpVeg%lai
   case('vegtype');      get_ixPrpVeg = ixPrpVeg%vegtype
   case('nroot');        get_ixPrpVeg = ixPrpVeg%nroot
   case('snup');         get_ixPrpVeg = ixPrpVeg%snup
   case('rs');           get_ixPrpVeg = ixPrpVeg%rs
   case('mrs');          get_ixPrpVeg = ixPrpVeg%mrs
   case('leafDim');      get_ixPrpVeg = ixPrpVeg%leafDim
   case('can_top_h');    get_ixPrpVeg = ixPrpVeg%can_top_h
   case('can_bot_h');    get_ixPrpVeg = ixPrpVeg%can_bot_h
   case('c_veg');        get_ixPrpVeg = ixPrpVeg%c_veg
   case('maxMassVeg');   get_ixPrpVeg = ixPrpVeg%maxMassVeg
   ! get to here if cannot find the variable
   case default;         get_ixPrpVeg = imiss
  endselect
 end function

end module get_ixname
