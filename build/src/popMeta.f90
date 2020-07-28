MODULE popMeta

use nrtype
use public_var, only:dmiss, imiss

implicit none

private

public::paramMaster
public::popMprMeta

CONTAINS

SUBROUTINE paramMaster(err,message)

  use public_var, only:nHru, nLyr
  use data_type,  only:gammaPar_meta, defDim
  use obj_type,   only:betaPar_meta
  use var_lookup, only:ixGamma,ixBeta,ixDim
  use globalData, only:gammaMeta,betaMeta, dimMeta

  implicit none
  !output variable
  integer(i4b),intent(out)      :: err     ! error code
  character(*),intent(out)      :: message ! error message

  ! initialize error control
  err=0; message='popMeta/'

  ! structure index             name   desc,          unit, dimension id,   dimension length
  dimMeta (ixDim%hru ) = defDim('hru', 'model HRU',   '-',  imiss,          nHru)  ! hru vector
  dimMeta (ixDim%lyr ) = defDim('lyr', 'model layer', '-',  imiss,          nLyr)  ! layer vector
  dimMeta (ixDim%mon ) = defDim('mon', 'month',       '-',  imiss,          12)     !

  ! -----
  !  Public subroutine: Master list of gamma parameters
  ! -----------------------
  !                                                        name,        default    ,unit,parent-beta,    TF,    mask
  !ks transfer function
  gammaMeta(ixGamma%ks1gamma1)       = gammaPar_meta('ks1gamma1'      ,   -0.6_dp  ,'-' ,"ks"       ,     1, .False.)
  gammaMeta(ixGamma%ks1gamma2)       = gammaPar_meta('ks1gamma2'      , 0.0126_dp  ,'-' ,"ks"       ,     1, .False.)
  gammaMeta(ixGamma%ks1gamma3)       = gammaPar_meta('ks1gamma3'      ,-0.0064_dp  ,'-' ,"ks"       ,     1, .False.)
  gammaMeta(ixGamma%ks2gamma1)       = gammaPar_meta('ks2gamma1'      ,   54.0_dp  ,'-' ,"ks"       ,     2, .False.)
  gammaMeta(ixGamma%ks2gamma2)       = gammaPar_meta('ks2gamma2'      ,  -0.07_dp  ,'-' ,"ks"       ,     2, .False.)
  gammaMeta(ixGamma%ks2gamma3)       = gammaPar_meta('ks2gamma3'      , -0.167_dp  ,'-' ,"ks"       ,     2, .False.)
  !pororsity transfer function
  gammaMeta(ixGamma%phi1gamma1)      = gammaPar_meta('phi1gamma1'     ,    50.5_dp ,'-' ,"phi"      ,     1, .False.)
  gammaMeta(ixGamma%phi1gamma2)      = gammaPar_meta('phi1gamma2'     ,  -0.142_dp ,'-' ,"phi"      ,     1, .False.)
  gammaMeta(ixGamma%phi1gamma3)      = gammaPar_meta('phi1gamma3'     , -0.0037_dp ,'-' ,"phi"      ,     1, .False.)
  gammaMeta(ixGamma%phi2gamma1)      = gammaPar_meta('phi2gamma1'     ,    0.76_dp ,'-' ,"phi"      ,     2, .False.)
  gammaMeta(ixGamma%phi2gamma2)      = gammaPar_meta('phi2gamma2'     ,  0.0009_dp ,'-' ,"phi"      ,     2, .False.)
  gammaMeta(ixGamma%phi2gamma3)      = gammaPar_meta('phi2gamma3'     ,  -0.264_dp ,'-' ,"phi"      ,     2, .False.)
  gammaMeta(ixGamma%phi2gamma4)      = gammaPar_meta('phi2gamma4'     ,    0.89_dp ,'-' ,"phi"      ,     2, .False.)
  gammaMeta(ixGamma%phi2gamma5)      = gammaPar_meta('phi2gamma5'     ,  -0.001_dp ,'-' ,"phi"      ,     2, .False.)
  gammaMeta(ixGamma%phi2gamma6)      = gammaPar_meta('phi2gamma6'     ,  -0.324_dp ,'-' ,"phi"      ,     2, .False.)
  !field capacity transfer function
  gammaMeta(ixGamma%fc1gamma1)       = gammaPar_meta('fc1gamma1'      ,     1.0_dp ,'-' ,"fc"       ,     1, .False.)
  !wilting point transfer function
  gammaMeta(ixGamma%wp1gamma1)       = gammaPar_meta('wp1gamma1'      ,     1.0_dp ,'-' ,"wp"       ,     1, .False.)
  !residual transfer function
  gammaMeta(ixGamma%resid1gamma1)    = gammaPar_meta('resid1gamma1'   ,     0.2_dp ,'-' ,"resid"    ,     1, .False.)
  !transpire transfer function
  gammaMeta(ixGamma%transp1gamma1)   = gammaPar_meta('transp1gamma1'  ,     0.5_dp ,'-' ,"transp"   ,     1, .False.)
  !b transfer function
  gammaMeta(ixGamma%b1gamma1)        = gammaPar_meta('b1gamma1'       ,   3.100_dp ,'-' ,"b"        ,     1, .False.)
  gammaMeta(ixGamma%b1gamma2)        = gammaPar_meta('b1gamma2'       ,   0.157_dp ,'-' ,"b"        ,     1, .False.)
  gammaMeta(ixGamma%b1gamma3)        = gammaPar_meta('b1gamma3'       ,  -0.003_dp ,'-' ,"b"        ,     1, .False.)
  !saturation matric potential transfer function
  gammaMeta(ixGamma%psis1gamma1)     = gammaPar_meta('psis1gamma1'    ,   1.540_dp ,'-' ,"psis"     ,     1, .False.)
  gammaMeta(ixGamma%psis1gamma2)     = gammaPar_meta('psis1gamma2'    , -0.0095_dp ,'-' ,"psis"     ,     1, .False.)
  gammaMeta(ixGamma%psis1gamma3)     = gammaPar_meta('psis1gamma3'    ,  0.0063_dp ,'-' ,"psis"     ,     1, .False.)
  !specific yield transfer function
  gammaMeta(ixGamma%myu1gamma1)      = gammaPar_meta('myu1gamma1'     ,     3.5_dp ,'-' ,"myu"      ,     1, .False.)
  gammaMeta(ixGamma%myu1gamma2)      = gammaPar_meta('myu1gamma2'     ,    1.66_dp ,'-' ,"myu"      ,     1, .False.)
  ! organic soil franction transfer function
  gammaMeta(ixGamma%sof1gamma1)      = gammaPar_meta('sof1gamma1'     ,     1.72_dp ,'-' ,"sof"     ,     1, .False.)
  ! transfer function
  gammaMeta(ixGamma%binfilt1gamma1)  = gammaPar_meta('binfilt1gamma1' ,     0.0_dp ,'-' ,"binfilt"  ,     1, .False.)
  gammaMeta(ixGamma%binfilt1gamma2)  = gammaPar_meta('binfilt1gamma2' ,     1.0_dp ,'-' ,"binfilt"  ,     1, .False.)
  gammaMeta(ixGamma%D11gamma1)       = gammaPar_meta('D11gamma1'      ,     2.0_dp ,'-' ,"D1"       ,     1, .False.)
  gammaMeta(ixGamma%D21gamma1)       = gammaPar_meta('D21gamma1'      ,     2.0_dp ,'-' ,"D2"       ,     1, .False.)
  gammaMeta(ixGamma%D31gamma1)       = gammaPar_meta('D31gamma1'      ,     1.0_dp ,'-' ,"D3"       ,     1, .False.)
  gammaMeta(ixGamma%D41gamma1)       = gammaPar_meta('D41gamma1'      ,     2.0_dp ,'-' ,"D4"       ,     1, .False.)
  gammaMeta(ixGamma%expt1gamma1)     = gammaPar_meta('expt1gamma1'    ,     3.0_dp ,'-' ,"expt"     ,     1, .False.)
  gammaMeta(ixGamma%expt1gamma2)     = gammaPar_meta('expt1gamma2'    ,     2.0_dp ,'-' ,"expt"     ,     1, .False.)
  gammaMeta(ixGamma%bbl1gamma1)      = gammaPar_meta('bbl1gamma1'     ,    0.32_dp ,'-' ,"bbl"      ,     1, .False.)
  gammaMeta(ixGamma%bbl1gamma2)      = gammaPar_meta('bbl1gamma2'     ,     4.2_dp ,'-' ,"bbl"      ,     1, .False.)
  gammaMeta(ixGamma%bd1gamma1)       = gammaPar_meta('bd1gamma1'      ,     1.0_dp ,'-' ,"bd"       ,     1, .False.)
  gammaMeta(ixGamma%sd1gamma1)       = gammaPar_meta('sd1gamma1'      ,     1.0_dp ,'-' ,"sd"       ,     1, .False.)
  gammaMeta(ixGamma%WcrFrac1gamma1)  = gammaPar_meta('WcrFrac1gamma1' ,     1.0_dp ,'-' ,"WcrFrac"  ,     1, .False.)
  gammaMeta(ixGamma%WpwpFrac1gamma1) = gammaPar_meta('WpwpFrac1gamma1',     1.0_dp ,'-' ,"WcrFrac"  ,     1, .False.)
  gammaMeta(ixGamma%bfr1gamma1)      = gammaPar_meta('bfr1gamma1'     ,    50.0_dp ,'-' ,"bfr"      ,     1, .False.)
  gammaMeta(ixGamma%fsm1gamma1)      = gammaPar_meta('fsm1gamma1',          1.0_dp ,'-' ,"fsm"      ,     1, .False.)
  gammaMeta(ixGamma%zk1gamma1)       = gammaPar_meta('zk1gamma1',           1.6_dp ,'-' ,"zk"       ,     1, .False.)
  gammaMeta(ixGamma%zsk1gamma1)      = gammaPar_meta('zsk1gamma1',          1.6_dp ,'-' ,"zsk"      ,     1, .False.)
  gammaMeta(ixGamma%zsk1gamma2)      = gammaPar_meta('zsk1gamma2',          2.0_dp ,'-' ,"zsk"      ,     1, .False.)
  gammaMeta(ixGamma%zpk1gamma1)      = gammaPar_meta('zpk1gamma1',          1.0_dp ,'-' ,"zpk"      ,     1, .False.)
  gammaMeta(ixGamma%pfree1gamma1)    = gammaPar_meta('pfree1gamma1',        1.6_dp ,'-' ,"pfree"    ,     1, .False.)
  gammaMeta(ixGamma%rexp1gamma1)     = gammaPar_meta('rexp1gamma1',        0.03_dp ,'-' ,"rexp"     ,     1, .False.)
  ! vegetation transfer function
  gammaMeta(ixGamma%lai1gamma1)      = gammaPar_meta('lai1gamma1',          1.0_dp ,'-' ,"lai"      ,     1, .False.)
  gammaMeta(ixGamma%cht1gamma1)      = gammaPar_meta('cht1gamma1',          1.0_dp ,'-' ,"cht"      ,     1, .False.)
  gammaMeta(ixGamma%chb1gamma1)      = gammaPar_meta('chb1gamma1',         0.10_dp ,'-' ,"chb"      ,     1, .False.)
  ! snow transfer function
  gammaMeta(ixGamma%scf1gamma1)      = gammaPar_meta('scf1gamma1',          0.0_dp ,'-' ,"scf"      ,     1, .False.)
  ! -----
  !  Master list of beta parameters
  ! -----------------------
  !                                   name         ,  desc                                                            , units   , type   , tftype, scaling          ,       scaleParam, dimension
  call betaMeta(ixBeta%uhshape) %init('uhshape'    , 'shape parameter in Gamma distribution used for overland routing', '-'     , "route", -999,   ["pnorm"],          [ 1.0_dp],         [ixDim%hru])
  call betaMeta(ixBeta%uhscale) %init('uhscale'    , 'scale parameter in Gamma distribution used for overland routing', 's'     , "route", -999,   ["pnorm"],          [ 1.0_dp],         [ixDim%hru])
  call betaMeta(ixBeta%ks)      %init('ks'         , 'saturated hydraulic conductivity'                               , 'm/s'   ,  "soil", -999,   ["pnorm", "pnorm"], [-1.0_dp,-1.0_dp], [ixDim%hru, ixDim%lyr])
  call betaMeta(ixBeta%bd)      %init('bd'         , 'bulk density'                                                   , ' '     ,  "soil", -999,   ["pnorm", "pnorm"], [ 1.0_dp, 1.0_dp], [ixDim%hru, ixDim%lyr])
  call betaMeta(ixBeta%sd)      %init('sd'         , 'soil mineral density'                                           , ' '     ,  "soil", -999,   ["pnorm", "pnorm"], [ 1.0_dp, 1.0_dp], [ixDim%hru, ixDim%lyr])
  call betaMeta(ixBeta%psis)    %init('psis'       , 'saturation matric potential'                                    , 'kPa'   ,  "soil", -999,   ["pnorm", "pnorm"], [ 1.0_dp, 1.0_dp], [ixDim%hru, ixDim%lyr])
  call betaMeta(ixBeta%b)       %init('b'          , 'slope of retention curve in log space'                          , '-'     ,  "soil", -999,   ["pnorm", "pnorm"], [ 1.0_dp, 1.0_dp], [ixDim%hru, ixDim%lyr])
  call betaMeta(ixBeta%phi)     %init('phi'        , 'porosity'                                                       , '-'     ,  "soil", -999,   ["pnorm", "pnorm"], [ 1.0_dp, 1.0_dp], [ixDim%hru, ixDim%lyr])
  call betaMeta(ixBeta%fc)      %init('fc'         , 'field capacity'                                                 , '-'     ,  "soil", -999,   ["pnorm", "pnorm"], [ 1.0_dp, 1.0_dp], [ixDim%hru, ixDim%lyr])
  call betaMeta(ixBeta%wp)      %init('wp'         , 'wilting point'                                                  , '-'     ,  "soil", -999,   ["pnorm", "pnorm"], [ 1.0_dp, 1.0_dp], [ixDim%hru, ixDim%lyr])
  call betaMeta(ixBeta%resid)   %init('resid'      , 'volumetric residual water content'                              , '-'     ,  "soil", -999,   ["pnorm", "pnorm"], [ 1.0_dp, 1.0_dp], [ixDim%hru, ixDim%lyr])
  call betaMeta(ixBeta%transp)  %init('transp'     , 'critical vol. liq. water content at limited transpiration'      , '-'     ,  "soil", -999,   ["pnorm", "pnorm"], [ 1.0_dp, 1.0_dp], [ixDim%hru, ixDim%lyr])
  call betaMeta(ixBeta%myu)     %init('myu'        , 'specific yield'                                                 , '-'     ,  "soil", -999,   ["pnorm", "pnorm"], [ 1.0_dp, 1.0_dp], [ixDim%hru, ixDim%lyr])
  call betaMeta(ixBeta%sof)     %init('sof'        , 'soil organic fraction '                                         , 'g/g'   ,  "soil", -999,   ["pnorm", "pnorm"], [ 1.0_dp, 1.0_dp], [ixDim%hru, ixDim%lyr])
  call betaMeta(ixBeta%binfilt) %init('binfilt'    , 'Variable infiltration curve parameter'                          , '-'     ,  "soil", -999,   ["pnorm"],          [ 1.0_dp],         [ixDim%hru])
  call betaMeta(ixBeta%D1)      %init('D1'         , 'linear reservoir coefficient in Nijssen baseflow eq.'           , '1/s'   ,  "soil", -999,   ["pnorm"],          [-1.0_dp],         [ixDim%hru])
  call betaMeta(ixBeta%D2)      %init('D2'         , 'non-linear reservoir coefficient in Nijssen baseflow eq.'       , 'wired' ,  "soil", -999,   ["pnorm"],          [-1.0_dp],         [ixDim%hru])
  call betaMeta(ixBeta%D3)      %init('D3'         , 'soil moisture level where nonliear reservoir effect begins'     , 'm'     ,  "soil", -999,   ["pnorm"],          [ 1.0_dp],         [ixDim%hru])
  call betaMeta(ixBeta%D4)      %init('D4'         , 'Exponent in nonliear part of Nijssen baseflow eq.'              , '-'     ,  "soil", -999,   ["pnorm"],          [ 1.0_dp],         [ixDim%hru])
  call betaMeta(ixBeta%Dsmax)   %init('Dsmax'      , 'Maximum velocity of baseflow'                                   , 'm/s'   ,  "soil", -999,   ["pnorm"],          [-1.0_dp],         [ixDim%hru])
  call betaMeta(ixBeta%Ds)      %init('Ds'         , 'Fraction of Dsmax where non-linear baseflow begins'             , '-'     ,  "soil", -999,   ["pnorm"],          [-1.0_dp],         [ixDim%hru])
  call betaMeta(ixBeta%Ws)      %init('Ws'         , 'Fraction of max. soil storage where non-linear baseflow occurs' , '-'     ,  "soil", -999,   ["pnorm"],          [-1.0_dp],         [ixDim%hru])
  call betaMeta(ixBeta%expt)    %init('expt'       , 'exponent in Campbell eqn for hydraulic conductivity'            , '-'     ,  "soil", -999,   ["pnorm", "pnorm"], [ 1.0_dp, 1.0_dp], [ixDim%hru, ixDim%lyr])
  call betaMeta(ixBeta%bbl)     %init('bbl'        , 'bubbling pressure'                                              , 'm'     ,  "soil", -999,   ["pnorm", "pnorm"], [ 1.0_dp, 1.0_dp], [ixDim%hru, ixDim%lyr])
  call betaMeta(ixBeta%WcrFrac) %init('WcrFrac'    , 'Fractional soil moisture content at the critical point'         , '-'     ,  "soil", -999,   ["pnorm", "pnorm"], [ 1.0_dp, 1.0_dp], [ixDim%hru, ixDim%lyr])
  call betaMeta(ixBeta%WpwpFrac)%init('WpwpFrac'   , 'Fractional soil moisture content at the wilting point'          , '-'     ,  "soil", -999,   ["pnorm", "pnorm"], [ 1.0_dp, 1.0_dp], [ixDim%hru, ixDim%lyr])
  call betaMeta(ixBeta%bfr)     %init('bfr'        , 'baseflow rate coefficient of normalized storage'                , 'm/s'   ,  "soil", -999,   ["pnorm"],          [-1.0_dp],         [ixDim%hru])
  call betaMeta(ixBeta%h)       %init('h'          , 'model layer thickness'                                          , 'm'     ,  "soil", -999,   ["pnorm", "na   "], [ 1.0_dp, dmiss],  [ixDim%hru, ixDim%lyr])
  call betaMeta(ixBeta%twm)     %init('twm'        , 'tension water maximum storage'                                  , 'mm'    ,  "soil", -999,   ["pnorm"],          [ 1.0_dp],         [ixDim%hru])
  call betaMeta(ixBeta%fwm)     %init('fwm'        , 'free water maximum storage'                                     , 'mm'    ,  "soil", -999,   ["pnorm"],          [ 1.0_dp],         [ixDim%hru])
  call betaMeta(ixBeta%fsm)     %init('fsm'        , 'free water supplementary maximum storage'                       , 'mm'    ,  "soil", -999,   ["pnorm"],          [ 1.0_dp],         [ixDim%hru])
  call betaMeta(ixBeta%fpm)     %init('fpm'        , 'free water primary maximum storage'                             , 'mm'    ,  "soil", -999,   ["pnorm"],          [ 1.0_dp],         [ixDim%hru])
  call betaMeta(ixBeta%zk)      %init('zk'         , 'free water flow rate'                                           , '1/d'   ,  "soil", -999,   ["pnorm"],          [ 1.0_dp],         [ixDim%hru])
  call betaMeta(ixBeta%zsk)     %init('zsk'        , 'supplementary flow rate'                                        , '1/d'   ,  "soil", -999,   ["pnorm"],          [-1.0_dp],         [ixDim%hru])
  call betaMeta(ixBeta%zpk)     %init('zpk'        , 'primary flow rate'                                              , '1/d'   ,  "soil", -999,   ["pnorm"],          [-1.0_dp],         [ixDim%hru])
  call betaMeta(ixBeta%pfree)   %init('pfree'      , 'percolation going to lower free water storage'                  , '-'     ,  "soil", -999,   ["pnorm"],          [ 1.0_dp],         [ixDim%hru])
  call betaMeta(ixBeta%zperc)   %init('zperc'      , 'maximum percolation rate'                                       , '1/d'   ,  "soil", -999,   ["pnorm"],          [ 1.0_dp],         [ixDim%hru])
  call betaMeta(ixBeta%rexp)    %init('rexp'       , 'percolation equation exponent'                                  , '-'     ,  "soil", -999,   ["pnorm"],          [ 1.0_dp],         [ixDim%hru])
  call betaMeta(ixBeta%lai)     %init('lai'        , 'monthly LAI'                                                    , 'm2/m2' ,   "veg", -999,   ["pnorm","na   "],  [ 1.0_dp, dmiss],  [ixDim%hru, ixDim%mon])
  call betaMeta(ixBeta%cht)     %init('cht'        , 'Height of canopy top'                                           , 'm'     ,   "veg", -999,   ["pnorm"],          [ 1.0_dp],         [ixDim%hru])
  call betaMeta(ixBeta%chb)     %init('chb'        , 'Height of canopy bottom'                                        , 'm'     ,   "veg", -999,   ["pnorm"],          [ 1.0_dp],         [ixDim%hru])
  call betaMeta(ixBeta%rmin)    %init('rmin'       , 'Minimum stomatal resistance'                                    , 's/m'   ,   "veg", -999,   ["pnorm"],          [ 1.0_dp],         [ixDim%hru])
  call betaMeta(ixBeta%scf)     %init('scf'        , 'frozen precipitation multiplier'                                , '-'     ,  "snow", -999,   ["pnorm"],          [ 1.0_dp],         [ixDim%hru])
  call betaMeta(ixBeta%mfmax)   %init('mfmax'      , 'max. melt factor during season'                                 , 'mm/C/d',  "snow", -999,   ["pnorm"],          [ 1.0_dp],         [ixDim%hru])
  call betaMeta(ixBeta%mfmin)   %init('mfmin'      , 'min. melt factor during season'                                 , 'mm/C/d',  "snow", -999,   ["pnorm"],          [ 1.0_dp],         [ixDim%hru])
  call betaMeta(ixBeta%uadj)    %init('uadj'       , 'mean wind function during rain-on-snow'                         , 'mm/mb ',  "snow", -999,   ["pnorm"],          [ 1.0_dp],         [ixDim%hru])
  call betaMeta(ixBeta%si)      %init('si'         , 'water equivalent above which 100% cover always exists'          , 'mm'    ,  "snow", -999,   ["pnorm"],          [ 1.0_dp],         [ixDim%hru])
  call betaMeta(ixBeta%pxtemp)  %init('pxtemp'     , 'snow-rain transition temperature'                               , 'K'     ,  "snow", -999,   ["pnorm"],          [ 1.0_dp],         [ixDim%hru])
  call betaMeta(ixBeta%nmf)     %init('nmf'        , 'maximum negetive melt factor'                                   , 'mm/C/d',  "snow", -999,   ["pnorm"],          [ 1.0_dp],         [ixDim%hru])
  call betaMeta(ixBeta%tipm)    %init('tipm'       , 'Antecedent snow temperature index parameter'                    , '-'     ,  "snow", -999,   ["pnorm"],          [ 1.0_dp],         [ixDim%hru])
  call betaMeta(ixBeta%plwhc)   %init('plwhc'      , 'Percent of liquid water capacity'                               , '-'     ,  "snow", -999,   ["pnorm"],          [ 1.0_dp],         [ixDim%hru])
  call betaMeta(ixBeta%daygm)   %init('daygm'      , 'Daily melt at snow‐soil interface'                              , 'mm/d'  ,  "snow", -999,   ["pnorm"],          [ 1.0_dp],         [ixDim%hru])

END SUBROUTINE

! -----
!  Public subroutine: Populate metadata for MPR infor
! -----------------------
SUBROUTINE popMprMeta(err,message)

  use data_type,  only:var_meta
  use var_lookup, only:ixVarMapData
  use var_lookup, only:ixVarSoilData
  use var_lookup, only:ixVarVegData
  use var_lookup, only:ixVarTopoData
  use var_lookup, only:ixVarClimData
  use var_lookup, only:ixPrpVeg
  use globalData, only:map_meta
  use globalData, only:sdata_meta
  use globalData, only:tdata_meta
  use globalData, only:vdata_meta
  use globalData, only:cdata_meta
  use globalData, only:vprp_meta

  implicit none
  !output variable
  integer(i4b),intent(out)      :: err     ! error code
  character(*),intent(out)      :: message ! error message

  ! initialize error control
  err=0; message='popMprMeta/'
  ! Mapping data meta
  map_meta(ixVarMapData%hru_id)          = var_meta('hru_id'       ,"hru id"                                             ,"-"           ,"1D", "integer")
  map_meta(ixVarMapData%weight)          = var_meta('weight'       ,"areal weight of intersecting polygon"               ,"-"           ,"1D", "integer")
  map_meta(ixVarMapData%intersector)     = var_meta('intersector'  ,"id of intersecting polygon"                         ,"-"           ,"1D", "integer")
  map_meta(ixVarMapData%overlaps)        = var_meta('overlaps'     ,"number of intersecting polygons"                    ,"-"           ,"1D", "integer")
  ! Soil data variables
  sdata_meta(ixVarSoilData%polyid)       = var_meta('polyid'       ,"soil polygon id"                                    ,"-"           ,"1D", "integer")
  sdata_meta(ixVarSoilData%hslyrs)       = var_meta('hslyrs'       ,"soil layer thickness"                               ,"m"           ,"2D", "double" )
  sdata_meta(ixVarSoilData%sand_pct)     = var_meta('sand_pct'     ,"sand percentage"                                    ,"%"           ,"2D", "double" )
  sdata_meta(ixVarSoilData%silt_pct)     = var_meta('silt_pct'     ,"silt percentage"                                    ,"%"           ,"2D", "double" )
  sdata_meta(ixVarSoilData%clay_pct)     = var_meta('clay_pct'     ,"clay percentage"                                    ,"%"           ,"2D", "double" )
  sdata_meta(ixVarSoilData%bulk_density) = var_meta('bulk_density' ,"bulk density"                                       ,"kg m-3"      ,"2D", "double" )
  sdata_meta(ixVarSoilData%soc)          = var_meta('soc'          ,"soil carbon content"                                ,"g/kg"        ,"2D", "double" )
  ! Vege data variables
  vdata_meta(ixVarVegData%polyid)        = var_meta('polyid'       ,"vege polygon id"                                    ,"-"           ,"1D", "integer")
  vdata_meta(ixVarVegData%vegclass)      = var_meta('vegclass'     ,"vegetation class"                                   ,"-"           ,"1D", "integer")
  vdata_meta(ixVarVegData%lai)           = var_meta('lai'          ,"monthly lai"                                        ,"m2 m-2"      ,"2D", "double" )
  vdata_meta(ixVarVegData%ch)            = var_meta('ch'           ,"canopy height"                                      ,"m"           ,"1D", "double" )
  ! topo data variables
  tdata_meta(ixVarTopoData%polyid)       = var_meta('polyid'       ,"vege polygon id"                                    ,"-"           ,"1D", "integer")
  tdata_meta(ixVarTopoData%ele_mean)     = var_meta('ele_mean'     ,"mean elevation"                                     ,"m"           ,"1D", "double" )
  tdata_meta(ixVarTopoData%ele_std)      = var_meta('ele_std'      ,"std elevation"                                      ,"m"           ,"1D", "double" )
  tdata_meta(ixVarTopoData%slp_mean)     = var_meta('slp_mean'     ,"mean slope"                                         ,"percent"     ,"1D", "double" )
  ! climate data variables
  cdata_meta(ixVarClimData%polyid)       = var_meta('polyid'       ,"climate polygon id"                                 ,"-"           ,"1D", "integer")
  cdata_meta(ixVarClimData%prec)         = var_meta('prec'         ,"monthly total precipitation"                        ,"mm"          ,"2D", "double" )
  cdata_meta(ixVarClimData%tavg)         = var_meta('tavg'         ,"monthly mean temperature"                           ,"degree-C"    ,"2D", "double" )
  cdata_meta(ixVarClimData%wind)         = var_meta('wind'         ,"monthly mean wind speed"                            ,"m s-1"       ,"2D", "double" )
  cdata_meta(ixVarClimData%ai)           = var_meta('ai'           ,"Annual aridity index"                               ,"-"           ,"1D", "double" )
  ! Vege property variables
  vprp_meta(ixPrpVeg%lai)                = var_meta('lai'          ,"Monthly lai"                                        ,"m2 m-2"      ,"2D", "double")
  vprp_meta(ixPrpVeg%vegtype)            = var_meta('vegtype'      ,'vegetation type'                                    ,"-"           ,"1D", "integer")
  vprp_meta(ixPrpVeg%nroot)              = var_meta('nroot'        ,'rooting depth'                                      ,"m"           ,"1D", "double" )
  vprp_meta(ixPrpVeg%snup)               = var_meta('snup'         ,'threshold SWE depth that implies 100% snow cover'   ,"m"           ,"1D", "double" )
  vprp_meta(ixPrpVeg%rs)                 = var_meta('rs'           ,'stomatal resistance'                                ,"s m-1"       ,"1D", "double" )
  vprp_meta(ixPrpVeg%mrs)                = var_meta('mrs'          ,'minimum stomatal resistance'                        ,"s m-1"       ,"1D", "double" )
  vprp_meta(ixPrpVeg%leafDim)            = var_meta('leafDim'      ,'characteristic leaf dimension'                      ,"m"           ,"1D", "double" )
  vprp_meta(ixPrpVeg%can_top_h)          = var_meta('can_top_h'    ,'height of top of vegetation canopy above ground'    ,"m"           ,"1D", "double" )
  vprp_meta(ixPrpVeg%can_bot_h)          = var_meta('can_bot_h'    ,'height of bottom of vegetation canopy above ground' ,"m"           ,"1D", "double" )
  vprp_meta(ixPrpVeg%c_veg)              = var_meta('c_veg'        ,'specific heat of vegetation'                        ,"J kg-1 K-1"  ,"1D", "double" )
  vprp_meta(ixPrpVeg%maxMassVeg)         = var_meta('maxMassVeg'   ,'maximum mass of vegetation'                         ,"kg m-2"      ,"1D", "double" )

END SUBROUTINE

END MODULE popMeta
