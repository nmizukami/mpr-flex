MODULE var_lookup

 ! Define index arrays for named variables
 ! list of all the parameters including gamma and beta parameters

 USE nrtype
 USE public_var

 implicit none

 private

 ! local variables
 integer(i4b),parameter :: ixVal=1                     ! an example integer
 integer(i4b),parameter :: iLength=storage_size(ixVal) ! size of the example integer

 ! ***********************************************************************************************************
 ! ** define dimensions
 ! ***********************************************************************************************************
 type, public  ::  iLook_dims
  integer(i4b)     :: hru          = imiss   ! hru
  integer(i4b)     :: lyr          = imiss   ! soil layer
  integer(i4b)     :: mon          = imiss   ! month
 endtype iLook_dims

! ***********************************************************************************************************
! 1.Define indices for gamma parameters
! ***********************************************************************************************************
 type, public  ::  iLook_gamma
   integer(i4b)     :: ks1gamma1       = imiss  !
   integer(i4b)     :: ks1gamma2       = imiss  !
   integer(i4b)     :: ks1gamma3       = imiss  !
   integer(i4b)     :: ks2gamma1       = imiss  !
   integer(i4b)     :: ks2gamma2       = imiss  !
   integer(i4b)     :: ks2gamma3       = imiss  !
   integer(i4b)     :: phi1gamma1      = imiss  !
   integer(i4b)     :: phi1gamma2      = imiss  !
   integer(i4b)     :: phi1gamma3      = imiss  !
   integer(i4b)     :: phi2gamma1      = imiss  !
   integer(i4b)     :: phi2gamma2      = imiss  !
   integer(i4b)     :: phi2gamma3      = imiss  !
   integer(i4b)     :: phi2gamma4      = imiss  !
   integer(i4b)     :: phi2gamma5      = imiss  !
   integer(i4b)     :: phi2gamma6      = imiss  !
   integer(i4b)     :: fc1gamma1       = imiss  !
   integer(i4b)     :: wp1gamma1       = imiss  !
   integer(i4b)     :: resid1gamma1    = imiss  !
   integer(i4b)     :: transp1gamma1   = imiss  !
   integer(i4b)     :: b1gamma1        = imiss  !
   integer(i4b)     :: b1gamma2        = imiss  !
   integer(i4b)     :: b1gamma3        = imiss  !
   integer(i4b)     :: psis1gamma1     = imiss  !
   integer(i4b)     :: psis1gamma2     = imiss  !
   integer(i4b)     :: psis1gamma3     = imiss  !
   integer(i4b)     :: myu1gamma1      = imiss  !
   integer(i4b)     :: myu1gamma2      = imiss  !
   integer(i4b)     :: sof1gamma1      = imiss  ! soil organic fraction TF param
   integer(i4b)     :: z1gamma1        = imiss  ! total depth mulitplier
   integer(i4b)     :: h1gamma1        = imiss  ! fraction of top layer to total depth
   integer(i4b)     :: h2gamma1        = imiss  ! fraction of 2nd layer to total depth
   integer(i4b)     :: binfilt1gamma1  = imiss  !
   integer(i4b)     :: binfilt1gamma2  = imiss  !
   integer(i4b)     :: D11gamma1       = imiss  !
   integer(i4b)     :: D21gamma1       = imiss  !
   integer(i4b)     :: D31gamma1       = imiss  !
   integer(i4b)     :: D41gamma1       = imiss  !
   integer(i4b)     :: expt1gamma1     = imiss  !
   integer(i4b)     :: expt1gamma2     = imiss  !
   integer(i4b)     :: bbl1gamma1      = imiss  !
   integer(i4b)     :: bbl1gamma2      = imiss  !
   integer(i4b)     :: bd1gamma1       = imiss  !
   integer(i4b)     :: sd1gamma1       = imiss  !
   integer(i4b)     :: WcrFrac1gamma1  = imiss  !
   integer(i4b)     :: WpwpFrac1gamma1 = imiss  !
   integer(i4b)     :: fsm1gamma1      = imiss  !
   integer(i4b)     :: zk1gamma1       = imiss  !
   integer(i4b)     :: zsk1gamma1      = imiss  !
   integer(i4b)     :: zsk1gamma2      = imiss  !
   integer(i4b)     :: zpk1gamma1      = imiss  !
   integer(i4b)     :: pfree1gamma1    = imiss  !
   integer(i4b)     :: rexp1gamma1     = imiss  !
   integer(i4b)     :: lai1gamma1      = imiss  !
   integer(i4b)     :: cht1gamma1      = imiss  !
   integer(i4b)     :: chb1gamma1      = imiss  !
 endtype iLook_gamma
! ***********************************************************************************************************
! 2.Define indices for beta parameters
! ***********************************************************************************************************
 type, public  ::  iLook_beta
   integer(i4b)     :: uhshape         = imiss  ! uh gamma pdf shape parameter
   integer(i4b)     :: uhscale         = imiss  ! uh gamma pdf scale parameter
   integer(i4b)     :: ks              = imiss  ! Saturated conductivity
   integer(i4b)     :: bd              = imiss  ! bulk mineral density
   integer(i4b)     :: sd              = imiss  ! soil mineral density
   integer(i4b)     :: psis            = imiss  ! saturation matric potential
   integer(i4b)     :: b               = imiss  ! retension courve slope in log space
   integer(i4b)     :: phi             = imiss  ! porosity
   integer(i4b)     :: fc              = imiss  ! field capacity
   integer(i4b)     :: wp              = imiss  ! wilting point
   integer(i4b)     :: resid           = imiss  ! residual
   integer(i4b)     :: transp          = imiss  ! transpire
   integer(i4b)     :: myu             = imiss  !
   integer(i4b)     :: sof             = imiss  ! soil organic fraction [-]
   integer(i4b)     :: binfilt         = imiss  !
   integer(i4b)     :: D1              = imiss  !
   integer(i4b)     :: D2              = imiss  !
   integer(i4b)     :: D3              = imiss  !
   integer(i4b)     :: D4              = imiss  !
   integer(i4b)     :: Ds              = imiss  !
   integer(i4b)     :: Dsmax           = imiss  !
   integer(i4b)     :: Ws              = imiss  !
   integer(i4b)     :: c               = imiss  !
   integer(i4b)     :: expt            = imiss  !
   integer(i4b)     :: bbl             = imiss  !
   integer(i4b)     :: h1              = imiss  ! top layer thickness
   integer(i4b)     :: h2              = imiss  ! 2nd layer thickness
   integer(i4b)     :: h3              = imiss  !
   integer(i4b)     :: h4              = imiss  !
   integer(i4b)     :: h5              = imiss  !
   integer(i4b)     :: z               = imiss  !
   integer(i4b)     :: WcrFrac         = imiss  !
   integer(i4b)     :: WpwpFrac        = imiss  !
   integer(i4b)     :: twm             = imiss  !  tention water maximum
   integer(i4b)     :: fwm             = imiss  !
   integer(i4b)     :: fsm             = imiss  !
   integer(i4b)     :: fpm             = imiss  !
   integer(i4b)     :: zk              = imiss  !
   integer(i4b)     :: zsk             = imiss  !
   integer(i4b)     :: zpk             = imiss  !
   integer(i4b)     :: pfree           = imiss  !
   integer(i4b)     :: zperc           = imiss  !
   integer(i4b)     :: rexp            = imiss  !
   integer(i4b)     :: rmin            = imiss  ! minimum stomatal resistance
   integer(i4b)     :: lai             = imiss  ! Lai [m2/m2]
   integer(i4b)     :: cht             = imiss  ! top canopy height [m]
   integer(i4b)     :: chb             = imiss  ! bottom canopy height [m]
   integer(i4b)     :: scf             = imiss  !
   integer(i4b)     :: mfmax           = imiss  !
   integer(i4b)     :: mfmin           = imiss  !
   integer(i4b)     :: uadj            = imiss  !
   integer(i4b)     :: si              = imiss  !
   integer(i4b)     :: pxtemp          = imiss  !
   integer(i4b)     :: nmf             = imiss  !
   integer(i4b)     :: tipm            = imiss  !
   integer(i4b)     :: plwhc           = imiss  !
   integer(i4b)     :: daygm           = imiss  !
 endtype iLook_beta

! ***********************************************************************************************************
!  Define indices for variables for mapping data
! ***********************************************************************************************************
 type, public  ::  iLook_VarMapData
  integer(i4b)     :: hru_id         = imiss        ! hru id
  integer(i4b)     :: weight         = imiss        ! areal weight of intersecting geophysical data polygon for hru
  integer(i4b)     :: intersector    = imiss        ! id (=index of polygon in geophysical data) of intersecting geophysical data polygon for hru
  integer(i4b)     :: overlaps       = imiss        ! number of ntersecting geophysical data polygon for hru
 endtype iLook_varMapData

! ***********************************************************************************************************
!  Define index for variables in Veg data
! ***********************************************************************************************************
 type, public  ::  iLook_VarVegData
  integer(i4b)     :: polyid          = imiss       ! veg polygon id
  integer(i4b)     :: lai             = imiss       ! lai*10 [m2/m2]
  integer(i4b)     :: ch              = imiss       ! canopy height [m]
  integer(i4b)     :: vegclass        = imiss       ! veg class
 endtype iLook_VarVegData

! ***********************************************************************************************************
!  Define index for variables in soil data
! ***********************************************************************************************************
 type, public  ::  iLook_VarSoilData
  integer(i4b)     :: polyid       = imiss      ! polygon id
  integer(i4b)     :: hslyrs       = imiss      ! soil layer thickness
  integer(i4b)     :: sand_pct     = imiss      ! sand fraction in soil polygon and layer
  integer(i4b)     :: silt_pct     = imiss      ! silt fraction in soil polygon and layer
  integer(i4b)     :: clay_pct     = imiss      ! clay fraction in soil polygon and layer
  integer(i4b)     :: bulk_density = imiss      ! bulk density in soil polygon and layer
  integer(i4b)     :: soc          = imiss      ! soil carbon content in soil polygon and layer
 endtype iLook_VarSoilData

! ***********************************************************************************************************
!  Define index for variables in topographical data
! ***********************************************************************************************************
 type, public  ::  iLook_VarTopoData
  integer(i4b)     :: polyid       = imiss      ! polygon id
  integer(i4b)     :: ele_mean     = imiss      ! mean elevation over a soil polygon
  integer(i4b)     :: ele_std      = imiss      ! standard deviation of elevation over a soil polygon
  integer(i4b)     :: slp_mean     = imiss      ! mean slope over a soil polygon
 endtype iLook_VarTopoData

! ***********************************************************************************************************
!  Define index for variables in monthly climate data
! ***********************************************************************************************************
 type, public  ::  iLook_VarClimData
  integer(i4b)     :: polyid       = imiss      ! polygon id
  integer(i4b)     :: prec         = imiss      ! monthly total precipitation [mm]
  integer(i4b)     :: tavg         = imiss      ! monthly mean air temperature [degree-C]
  integer(i4b)     :: wind         = imiss      ! monthly mean wind speed  [m/s]
  integer(i4b)     :: ai           = imiss      ! annual aridity index
 endtype iLook_VarClimData

! ***********************************************************************************************************
!  Define indices for Veg properties
! ***********************************************************************************************************
 type, public  ::  iLook_PrpVeg
  integer(i4b)     :: lai            = imiss       ! monthly LAI  m^2/m^2
  integer(i4b)     :: vegtype        = imiss       ! vege type
  integer(i4b)     :: nroot          = imiss       ! rooitng depth m
  integer(i4b)     :: snup           = imiss       ! threshold SWE depth that implies 100% snow cover
  integer(i4b)     :: rs             = imiss       ! stomatal resistance
  integer(i4b)     :: mrs            = imiss       ! minimum stomatal resistance
  integer(i4b)     :: leafDim        = imiss       ! characteristic leaf dimension
  integer(i4b)     :: can_top_h      = imiss       ! height of top of vegetation canopy above ground
  integer(i4b)     :: can_bot_h      = imiss       ! height of bottom of vegetation canopy above ground
  integer(i4b)     :: c_veg          = imiss       ! specific heat of vegetation J kg-1 K-1
  integer(i4b)     :: maxMassVeg     = imiss       ! max. mass of vegetation with full foliage km m-2
 endtype iLook_PrpVeg

! ***********************************************************************************************************
! define data vectors
! ***********************************************************************************************************
 type(iLook_dims),        public,parameter  :: ixDim         = iLook_dims        (1,2,3)
 type(iLook_gamma),       public,parameter  :: ixGamma       = iLook_gamma       (1,2,3,4,5,6,7,8,9,10,&
                                                                                 11,12,13,14,15,16,17,18,19,20,&
                                                                                 21,22,23,24,25,26,27,28,29,30,&
                                                                                 31,32,33,34,35,36,37,38,39,40,&
                                                                                 41,42,43,44,45,46,47,48,49,50,&
                                                                                 51,52,53,54,55)
 type(iLook_beta),        public,parameter :: ixBeta         = iLook_beta        (1,2,3,4,5,6,7,8,9,10,&
                                                                                 11,12,13,14,15,16,17,18,19,20,&
                                                                                 21,22,23,24,25,26,27,28,29,30,&
                                                                                 31,32,33,34,35,36,37,38,39,40,&
                                                                                 41,42,43,44,45,46,47,48,49,50,&
                                                                                 51,52,53,54,55,56,57)
 type(iLook_VarMapData),  public,parameter :: ixVarMapData   = iLook_VarMapData  (1,2,3,4)
 type(iLook_VarSoilData), public,parameter :: ixVarSoilData  = iLook_VarSoilData (1,2,3,4,5,6,7)
 type(iLook_VarVegData),  public,parameter :: ixVarVegData   = iLook_VarVegData  (1,2,3,4)
 type(iLook_VarTopoData), public,parameter :: ixVarTopoData  = iLook_VarTopoData (1,2,3,4)
 type(iLook_VarClimData), public,parameter :: ixVarClimData  = iLook_VarClimData (1,2,3,4,5)
 type(iLook_PrpVeg),      public,parameter :: ixPrpVeg       = iLook_PrpVeg      (1,2,3,4,5,6,7,8,9,10,&
                                                                                  11)

! ***********************************************************************************************************
! define size of data vectors
! ***********************************************************************************************************
! Number of vairables defined
 integer(i4b),parameter,public :: nDim         = storage_size(ixDim)/iLength
 integer(i4b),parameter,public :: nGamma       = storage_size(ixGamma)/iLength
 integer(i4b),parameter,public :: nBeta        = storage_size(ixBeta)/iLength
 integer(i4b),parameter,public :: nVarMapData  = storage_size(ixVarMapData)/iLength
 integer(i4b),parameter,public :: nVarSoilData = storage_size(ixVarSoilData)/iLength
 integer(i4b),parameter,public :: nVarVegData  = storage_size(ixVarVegData)/iLength
 integer(i4b),parameter,public :: nVarTopoData = storage_size(ixVarTopoData)/iLength
 integer(i4b),parameter,public :: nVarClimData = storage_size(ixVarClimData)/iLength
 integer(i4b),parameter,public :: nPrpVeg      = storage_size(ixPrpVeg)/iLength

END MODULE var_lookup
