module popMeta

implicit none

private

public::paramMaster
public::popMprMeta

contains

subroutine paramMaster(err,message)
  use nrtype
  use data_type,  only:gammaPar_meta
  use data_type,  only:betaPar_meta
  use var_lookup, only:ixGamma,ixBeta
  use globalData, only:gammaMeta,betaMeta
  implicit none
  !output variable
  integer(i4b),intent(out)      :: err     ! error code
  character(*),intent(out)      :: message ! error message

  ! initialize error control
  err=0; message='popMeta/'

  ! -----
  !  Public subroutine: Master list of gamma parameters
  ! -----------------------
  !                                                        name,    default, lwr bound, upr bound,parent beta,   TF,    type,    mask
  !ks transfer function
  gammaMeta(ixGamma%ks1gamma1)       = gammaPar_meta('ks1gamma1'      ,   -0.6_dp ,  -0.90_dp,  -0.30_dp,"ks"       ,     1, "soil", .False.)
  gammaMeta(ixGamma%ks1gamma2)       = gammaPar_meta('ks1gamma2'      , 0.0126_dp , 0.0063_dp, 0.0189_dp,"ks"       ,     1, "soil", .False.)
  gammaMeta(ixGamma%ks1gamma3)       = gammaPar_meta('ks1gamma3'      ,-0.0064_dp ,-0.0096_dp,-0.0032_dp,"ks"       ,     1, "soil", .False.)
  gammaMeta(ixGamma%ks2gamma1)       = gammaPar_meta('ks2gamma1'      ,   54.0_dp ,   48.6_dp,   59.4_dp,"ks"       ,     2, "soil", .False.)
  gammaMeta(ixGamma%ks2gamma2)       = gammaPar_meta('ks2gamma2'      ,  -0.07_dp , -0.077_dp, -0.063_dp,"ks"       ,     2, "soil", .False.)
  gammaMeta(ixGamma%ks2gamma3)       = gammaPar_meta('ks2gamma3'      , -0.167_dp ,   -0.8_dp,   -0.4_dp,"ks"       ,     2, "soil", .False.)
  !pororsity transfer function
  gammaMeta(ixGamma%phi1gamma1)      = gammaPar_meta('phi1gamma1'     ,    50.5_dp,   45.5_dp,   55.5_dp,"phi"      ,     1, "soil", .False.)
  gammaMeta(ixGamma%phi1gamma2)      = gammaPar_meta('phi1gamma2'     ,  -0.142_dp, -0.170_dp, -0.110_dp,"phi"      ,     1, "soil", .False.)
  gammaMeta(ixGamma%phi1gamma3)      = gammaPar_meta('phi1gamma3'     ,  -0.037_dp, -0.044_dp, -0.029_dp,"phi"      ,     1, "soil", .False.)
  gammaMeta(ixGamma%phi2gamma1)      = gammaPar_meta('phi2gamma1'     ,    0.76_dp,   -0.8_dp,   -0.4_dp,"phi"      ,     2, "soil", .False.)
  gammaMeta(ixGamma%phi2gamma2)      = gammaPar_meta('phi2gamma2'     ,  0.0009_dp,   -0.8_dp,   -0.4_dp,"phi"      ,     2, "soil", .False.)
  gammaMeta(ixGamma%phi2gamma3)      = gammaPar_meta('phi2gamma3'     ,  -0.264_dp,   -0.8_dp,   -0.4_dp,"phi"      ,     2, "soil", .False.)
  gammaMeta(ixGamma%phi2gamma4)      = gammaPar_meta('phi2gamma4'     ,    0.89_dp,   -0.8_dp,   -0.4_dp,"phi"      ,     2, "soil", .False.)
  gammaMeta(ixGamma%phi2gamma5)      = gammaPar_meta('phi2gamma5'     ,  -0.001_dp,   -0.8_dp,   -0.4_dp,"phi"      ,     2, "soil", .False.)
  gammaMeta(ixGamma%phi2gamma6)      = gammaPar_meta('phi2gamma6'     ,  -0.324_dp,   -0.8_dp,   -0.4_dp,"phi"      ,     2, "soil", .False.)
  !field capacity transfer function
  gammaMeta(ixGamma%fc1gamma1)       = gammaPar_meta('fc1gamma1'      ,     1.0_dp,    0.8_dp,    1.2_dp,"fc"       ,     1, "soil", .False.)
  !wilting point transfer function
  gammaMeta(ixGamma%wp1gamma1)       = gammaPar_meta('wp1gamma1'      ,     1.0_dp,    0.8_dp,    1.2_dp,"wp"       ,     1, "soil", .False.)
  !b transfer function
  gammaMeta(ixGamma%b1gamma1)        = gammaPar_meta('b1gamma1'       ,   3.100_dp,  2.480_dp,  3.800_dp,"b"        ,     1, "soil", .False.)
  gammaMeta(ixGamma%b1gamma2)        = gammaPar_meta('b1gamma2'       ,   0.157_dp,  0.120_dp,  0.188_dp,"b"        ,     1, "soil", .False.)
  gammaMeta(ixGamma%b1gamma3)        = gammaPar_meta('b1gamma3'       ,  -0.003_dp, -0.005_dp, -0.001_dp,"b"        ,     1, "soil", .False.)
  !saturation matric potential transfer function
  gammaMeta(ixGamma%psis1gamma1)     = gammaPar_meta('psis1gamma1'    ,   1.540_dp,  1.232_dp,  1.848_dp,"psis"     ,     1, "soil", .False.)
  gammaMeta(ixGamma%psis1gamma2)     = gammaPar_meta('psis1gamma2'    , -0.0095_dp,-0.0114_dp,-0.0075_dp,"psis"     ,     1, "soil", .False.)
  gammaMeta(ixGamma%psis1gamma3)     = gammaPar_meta('psis1gamma3'    ,  0.0063_dp, 0.0050_dp, 0.0080_dp,"psis"     ,     1, "soil", .False.)
  !specific yield transfer function
  gammaMeta(ixGamma%myu1gamma1)      = gammaPar_meta('myu1gamma1'     ,     3.5_dp,   3.85_dp,   3.15_dp,"myu"      ,     1, "soil", .False.)
  gammaMeta(ixGamma%myu1gamma2)      = gammaPar_meta('myu1gamma2'     ,    1.66_dp,   1.83_dp,   1.50_dp,"myu"      ,     1, "soil", .False.)
  ! organic soil franction transfer function
  gammaMeta(ixGamma%sof1gamma1)      = gammaPar_meta('sof1gamma1'     ,     1.72_dp,  1.55_dp,   1.89_dp,"sof"      ,     1, "soil", .False.)
  ! total depth multiplier
  gammaMeta(ixGamma%z1gamma1)        = gammaPar_meta('z1gamma1'       ,     1.0_dp,    0.1_dp,    4.0_dp,"z"        ,     1, "soil", .False.)
  ! layer fractions
  gammaMeta(ixGamma%h1gamma1)        = gammaPar_meta('h1gamma1'       ,    0.05_dp,   0.01_dp,    0.1_dp,"h1"       ,     1, "soil", .False.)
  gammaMeta(ixGamma%h2gamma1)        = gammaPar_meta('h2gamma1'       ,     0.3_dp,   0.12_dp,    0.5_dp,"h2"       ,     1, "soil", .False.)
  ! transfer function
  gammaMeta(ixGamma%binfilt1gamma1)  = gammaPar_meta('binfilt1gamma1' ,     0.0_dp,   -2.0_dp,    1.0_dp,"binfilt"  ,     1, "soil", .False.)
  gammaMeta(ixGamma%binfilt1gamma2)  = gammaPar_meta('binfilt1gamma2' ,     1.0_dp,    0.8_dp,    1.2_dp,"binfilt"  ,     1, "soil", .False.)
  gammaMeta(ixGamma%D11gamma1)       = gammaPar_meta('D11gamma1'      ,     2.0_dp,   1.75_dp,    3.5_dp,"D1"       ,     1, "soil", .False.)
  gammaMeta(ixGamma%D21gamma1)       = gammaPar_meta('D21gamma1'      ,     2.0_dp,   1.75_dp,    3.5_dp,"D2"       ,     1, "soil", .False.)
  gammaMeta(ixGamma%D31gamma1)       = gammaPar_meta('D31gamma1'      ,     1.0_dp,  0.001_dp,    2.0_dp,"D3"       ,     1, "soil", .False.)
  gammaMeta(ixGamma%D41gamma1)       = gammaPar_meta('D41gamma1'      ,     2.0_dp,    1.2_dp,    2.5_dp,"D4"       ,     1, "soil", .False.)
  gammaMeta(ixGamma%expt1gamma1)     = gammaPar_meta('expt1gamma1'    ,     3.0_dp,    0.8_dp,    1.2_dp,"expt"     ,     1, "soil", .False.)
  gammaMeta(ixGamma%expt1gamma2)     = gammaPar_meta('expt1gamma2'    ,     2.0_dp,    0.8_dp,    1.2_dp,"expt"     ,     1, "soil", .False.)
  gammaMeta(ixGamma%bbl1gamma1)      = gammaPar_meta('bbl1gamma1'     ,    0.32_dp,    0.8_dp,    1.2_dp,"bbl"      ,     1, "soil", .False.)
  gammaMeta(ixGamma%bbl1gamma2)      = gammaPar_meta('bbl1gamma2'     ,     4.2_dp,    0.8_dp,    1.2_dp,"bbl"      ,     1, "soil", .False.)
  gammaMeta(ixGamma%bd1gamma1)       = gammaPar_meta('bd1gamma1'      ,     1.0_dp,    0.9_dp,    1.1_dp,"bd"       ,     1, "soil", .False.)
  gammaMeta(ixGamma%sd1gamma1)       = gammaPar_meta('sd1gamma1'      ,     1.0_dp,    0.9_dp,    1.1_dp,"sd"       ,     1, "soil", .False.)
  gammaMeta(ixGamma%WcrFrac1gamma1)  = gammaPar_meta('WcrFrac1gamma1' ,     1.0_dp,    0.8_dp,    1.2_dp,"WcrFrac"  ,     1, "soil", .False.)
  gammaMeta(ixGamma%WpwpFrac1gamma1) = gammaPar_meta('WpwpFrac1gamma1',     1.0_dp,    0.8_dp,    1.2_dp,"WcrFrac"  ,     1, "soil", .False.)
  gammaMeta(ixGamma%fsm1gamma1)      = gammaPar_meta('fsm1gamma1',          1.0_dp,    0.8_dp,    1.2_dp,"fsm"      ,     1, "soil", .False.)
  gammaMeta(ixGamma%zk1gamma1)       = gammaPar_meta('zk1gamma1',           1.6_dp,    1.4_dp,    1.8_dp,"zk"       ,     1, "soil", .False.)
  gammaMeta(ixGamma%zsk1gamma1)      = gammaPar_meta('zsk1gamma1',          1.6_dp,    1.4_dp,    1.8_dp,"zsk"      ,     1, "soil", .False.)
  gammaMeta(ixGamma%zsk1gamma2)      = gammaPar_meta('zsk1gamma2',          2.0_dp,    1.8_dp,    2.2_dp,"zsk"      ,     1, "soil", .False.)
  gammaMeta(ixGamma%zpk1gamma1)      = gammaPar_meta('zpk1gamma1',          1.0_dp,    0.8_dp,    1.2_dp,"zpk"      ,     1, "soil", .False.)
  gammaMeta(ixGamma%pfree1gamma1)    = gammaPar_meta('pfree1gamma1',        1.6_dp,    1.4_dp,    1.8_dp,"pfree"    ,     1, "soil", .False.)
  gammaMeta(ixGamma%rexp1gamma1)     = gammaPar_meta('rexp1gamma1',        0.03_dp,   0.02_dp,   0.04_dp,"rexp"     ,     1, "soil", .False.)
  gammaMeta(ixGamma%lai1gamma1)      = gammaPar_meta('lai1gamma1',          1.0_dp,    0.8_dp,    1.2_dp,"lai"      ,     1,  "veg", .False.)
  ! -----
  !  Master list of beta parameters
  ! -----------------------
  !                                                            name,    default, lwr bound, upr bound,parent beta,    TF,    type,    mask, h-scale,    p-norm,   v scale,    p-norm,  perLyr
  betaMeta(ixBeta%uhshape)         = betaPar_meta('uhshape'        ,     1.0_dp,    0.1_dp,    3.0_dp,     "beta",  -999, "route", .False.,     "na", -999.0_dp,      "na", -999.0_dp, .False.)
  betaMeta(ixBeta%uhscale)         = betaPar_meta('uhscale'        ,     1.0_dp,    0.5_dp,    3.0_dp,     "beta",  -999, "route", .False.,     "na", -999.0_dp,      "na", -999.0_dp, .False.)
  betaMeta(ixBeta%ks)              = betaPar_meta('ks'             ,     1.0_dp,    0.8_dp,    1.2_dp,     "beta",  -999,  "soil", .False.,  "pnorm",   -1.0_dp,   "pnorm",   -1.0_dp, .True.)
  betaMeta(ixBeta%bd)              = betaPar_meta('bd'             ,     1.0_dp,    0.9_dp,    1.1_dp,     "beta",  -999,  "soil", .False.,  "pnorm",    1.0_dp,   "pnorm",    1.0_dp, .True.)
  betaMeta(ixBeta%sd)              = betaPar_meta('sd'             ,     1.0_dp,    0.9_dp,    1.1_dp,     "beta",  -999,  "soil", .False.,  "pnorm",    1.0_dp,   "pnorm",    1.0_dp, .True.)
  betaMeta(ixBeta%psis)            = betaPar_meta('psis'           ,     1.0_dp,    0.8_dp,    1.2_dp,     "beta",  -999,  "soil", .False.,  "pnorm",    1.0_dp,   "pnorm",    1.0_dp, .True.)
  betaMeta(ixBeta%b)               = betaPar_meta('b'              ,     1.0_dp,    0.8_dp,    1.2_dp,     "beta",  -999,  "soil", .False.,  "pnorm",    1.0_dp,   "pnorm",    1.0_dp, .True.)
  betaMeta(ixBeta%phi)             = betaPar_meta('phi'            ,     1.0_dp,    0.8_dp,    1.2_dp,     "beta",  -999,  "soil", .False.,  "pnorm",    1.0_dp,   "pnorm",    1.0_dp, .True.)
  betaMeta(ixBeta%fc)              = betaPar_meta('fc'             ,     1.0_dp,    0.8_dp,    1.2_dp,     "beta",  -999,  "soil", .False.,  "pnorm",    1.0_dp,   "pnorm",    1.0_dp, .True.)
  betaMeta(ixBeta%wp)              = betaPar_meta('wp'             ,     1.0_dp,    0.8_dp,    1.2_dp,     "beta",  -999,  "soil", .False.,  "pnorm",    1.0_dp,   "pnorm",    1.0_dp, .True.)
  betaMeta(ixBeta%myu)             = betaPar_meta('myu'            ,     1.0_dp,    0.8_dp,    1.2_dp,     "beta",  -999,  "soil", .False.,  "pnorm",    1.0_dp,   "pnorm",    1.0_dp, .True.)
  betaMeta(ixBeta%sof)             = betaPar_meta('sof'            ,     0.0_dp,    0.0_dp,    1.0_dp,     "beta",  -999,  "soil", .False.,  "pnorm",    1.0_dp,   "pnorm",    1.0_dp, .True.)
  betaMeta(ixBeta%binfilt)         = betaPar_meta('binfilt'        ,     1.0_dp,    0.8_dp,    1.2_dp,     "beta",  -999,  "soil", .False.,  "pnorm",    1.0_dp,   "pnorm",    1.0_dp, .False.)
  betaMeta(ixBeta%D1)              = betaPar_meta('D1'             ,     1.0_dp,    0.8_dp,    1.2_dp,     "beta",  -999,  "soil", .False.,  "pnorm",   -1.0_dp,   "pnorm",   -1.0_dp, .False.)
  betaMeta(ixBeta%D2)              = betaPar_meta('D2'             ,     1.0_dp,    0.8_dp,    1.2_dp,     "beta",  -999,  "soil", .False.,  "pnorm",   -1.0_dp,   "pnorm",   -1.0_dp, .False.)
  betaMeta(ixBeta%D3)              = betaPar_meta('D3'             ,     1.0_dp,    0.8_dp,    1.2_dp,     "beta",  -999,  "soil", .False.,  "pnorm",    1.0_dp,   "pnorm",    1.0_dp, .False.)
  betaMeta(ixBeta%D4)              = betaPar_meta('D4'             ,     1.0_dp,    0.8_dp,    1.2_dp,     "beta",  -999,  "soil", .False.,  "pnorm",    1.0_dp,   "pnorm",    1.0_dp, .False.)
  betaMeta(ixBeta%c)               = betaPar_meta('c'              ,     1.0_dp,    0.8_dp,    1.2_dp,     "beta",  -999,  "soil", .False.,  "pnorm",    1.0_dp,   "pnorm",    1.0_dp, .False.)
  betaMeta(ixBeta%Dsmax)           = betaPar_meta('Dsmax'          ,     1.0_dp,    0.8_dp,    1.2_dp,     "beta",  -999,  "soil", .False.,  "pnorm",   -1.0_dp,   "pnorm",   -1.0_dp, .False.)
  betaMeta(ixBeta%Ds)              = betaPar_meta('Ds'             ,     1.0_dp,    0.8_dp,    1.2_dp,     "beta",  -999,  "soil", .False.,  "pnorm",   -1.0_dp,   "pnorm",   -1.0_dp, .False.)
  betaMeta(ixBeta%Ws)              = betaPar_meta('Ws'             ,     1.0_dp,    0.8_dp,    1.2_dp,     "beta",  -999,  "soil", .False.,  "pnorm",   -1.0_dp,   "pnorm",   -1.0_dp, .False.)
  betaMeta(ixBeta%expt)            = betaPar_meta('expt'           ,     1.0_dp,    0.8_dp,    1.2_dp,     "beta",  -999,  "soil", .False.,  "pnorm",    1.0_dp,   "pnorm",    1.0_dp, .True.)
  betaMeta(ixBeta%bbl)             = betaPar_meta('bbl'            ,     1.0_dp,    0.8_dp,    1.2_dp,     "beta",  -999,  "soil", .False.,  "pnorm",    1.0_dp,   "pnorm",    1.0_dp, .True.)
  betaMeta(ixBeta%h1)              = betaPar_meta('h1'             ,     1.0_dp,    0.8_dp,    1.2_dp,     "beta",  -999,  "soil", .False.,  "pnorm",    1.0_dp,      "na", -999.0_dp, .False.)
  betaMeta(ixBeta%h2)              = betaPar_meta('h2'             ,     1.0_dp,    0.8_dp,    1.2_dp,     "beta",  -999,  "soil", .False.,  "pnorm",    1.0_dp,      "na", -999.0_dp, .False.)
  betaMeta(ixBeta%h3)              = betaPar_meta('h3'             ,     1.0_dp,    0.8_dp,    1.2_dp,     "beta",  -999,  "soil", .False.,  "pnorm",    1.0_dp,      "na", -999.0_dp, .False.)
  betaMeta(ixBeta%h4)              = betaPar_meta('h4'             ,     1.0_dp,    0.8_dp,    1.2_dp,     "beta",  -999,  "soil", .False.,  "pnorm",    1.0_dp,      "na", -999.0_dp, .False.)
  betaMeta(ixBeta%h5)              = betaPar_meta('h5'             ,     1.0_dp,    0.8_dp,    1.2_dp,     "beta",  -999,  "soil", .False.,  "pnorm",    1.0_dp,      "na", -999.0_dp, .False.)
  betaMeta(ixBeta%z)               = betaPar_meta('z'              ,     1.0_dp,    0.8_dp,    1.2_dp,     "beta",  -999,  "soil", .False.,  "pnorm",    1.0_dp,      "na", -999.0_dp, .False.)
  betaMeta(ixBeta%WcrFrac)         = betaPar_meta('WcrFrac'        ,     1.0_dp,    0.8_dp,    1.2_dp,     "beta",  -999,  "soil", .False.,  "pnorm",    1.0_dp,   "pnorm",    1.0_dp, .True.)
  betaMeta(ixBeta%WpwpFrac)        = betaPar_meta('WpwpFrac'       ,     1.0_dp,    0.8_dp,    1.2_dp,     "beta",  -999,  "soil", .False.,  "pnorm",    1.0_dp,   "pnorm",    1.0_dp, .True.)
  betaMeta(ixBeta%twm)             = betaPar_meta('twm'            ,     1.0_dp,    0.8_dp,    1.2_dp,     "beta",  -999,  "soil", .False.,  "pnorm",    1.0_dp,   "asum",  -999.0_dp, .True.)
  betaMeta(ixBeta%fwm)             = betaPar_meta('fwm'            ,     1.0_dp,    0.8_dp,    1.2_dp,     "beta",  -999,  "soil", .False.,  "pnorm",    1.0_dp,   "asum",  -999.0_dp, .True.)
  betaMeta(ixBeta%fsm)             = betaPar_meta('fsm'            ,     1.0_dp,    0.8_dp,    1.2_dp,     "beta",  -999,  "soil", .False.,  "pnorm",    1.0_dp,   "asum",  -999.0_dp, .True.)
  betaMeta(ixBeta%fpm)             = betaPar_meta('fpm'            ,     1.0_dp,    0.8_dp,    1.2_dp,     "beta",  -999,  "soil", .False.,  "pnorm",    1.0_dp,   "asum",  -999.0_dp, .True.)
  betaMeta(ixBeta%zk)              = betaPar_meta('zk'             ,     1.0_dp,    0.8_dp,    1.2_dp,     "beta",  -999,  "soil", .False.,  "pnorm",    1.0_dp,   "pnorm",    1.0_dp, .True.)
  betaMeta(ixBeta%zsk)             = betaPar_meta('zsk'            ,     1.0_dp,    0.8_dp,    1.2_dp,     "beta",  -999,  "soil", .False.,  "pnorm",   -1.0_dp,   "pnorm",   -1.0_dp, .True.)
  betaMeta(ixBeta%zpk)             = betaPar_meta('zpk'            ,     1.0_dp,    0.8_dp,    1.2_dp,     "beta",  -999,  "soil", .False.,  "pnorm",   -1.0_dp,   "pnorm",   -1.0_dp, .True.)
  betaMeta(ixBeta%pfree)           = betaPar_meta('pfree'          ,     1.0_dp,    0.8_dp,    1.2_dp,     "beta",  -999,  "soil", .False.,  "pnorm",    1.0_dp,   "pnorm",    1.0_dp, .True.)
  betaMeta(ixBeta%zperc)           = betaPar_meta('zperc'          ,     1.0_dp,    0.8_dp,    1.2_dp,     "beta",  -999,  "soil", .False.,  "pnorm",    1.0_dp,   "pnorm",    1.0_dp, .True.)
  betaMeta(ixBeta%rexp)            = betaPar_meta('rexp'           ,     1.0_dp,    0.8_dp,    1.2_dp,     "beta",  -999,  "soil", .False.,  "pnorm",    1.0_dp,   "pnorm",    1.0_dp, .True.)
  betaMeta(ixBeta%rmin)            = betaPar_meta('rmin'           ,     1.0_dp,    0.8_dp,    1.2_dp,     "beta",  -999,   "veg", .False.,  "pnorm",    1.0_dp,      "na", -999.0_dp, .False.)
  betaMeta(ixBeta%lai)             = betaPar_meta('lai'            ,     1.0_dp,    0.8_dp,    1.2_dp,     "beta",  -999,   "veg", .False.,  "pnorm",    1.0_dp,      "na", -999.0_dp, .False.)
  betaMeta(ixBeta%scf)             = betaPar_meta('scf'            ,     1.0_dp,    0.8_dp,    1.2_dp,     "beta",  -999,  "snow", .False.,  "pnorm",    1.0_dp,      "na", -999.0_dp, .False.)
  betaMeta(ixBeta%mfmax)           = betaPar_meta('mfmax'          ,     1.0_dp,    0.8_dp,    1.2_dp,     "beta",  -999,  "snow", .False.,  "pnorm",    1.0_dp,      "na", -999.0_dp, .False.)
  betaMeta(ixBeta%mfmin)           = betaPar_meta('mfmin'          ,     1.0_dp,    0.8_dp,    1.2_dp,     "beta",  -999,  "snow", .False.,  "pnorm",    1.0_dp,      "na", -999.0_dp, .False.)
  betaMeta(ixBeta%uadj)            = betaPar_meta('uadj'           ,     1.0_dp,    0.8_dp,    1.2_dp,     "beta",  -999,  "snow", .False.,  "pnorm",    1.0_dp,      "na", -999.0_dp, .False.)
  betaMeta(ixBeta%si)              = betaPar_meta('si'             ,     1.0_dp,    0.8_dp,    1.2_dp,     "beta",  -999,  "snow", .False.,  "pnorm",    1.0_dp,      "na", -999.0_dp, .False.)
  betaMeta(ixBeta%pxtemp)          = betaPar_meta('pxtemp'         ,     1.0_dp,    0.8_dp,    1.2_dp,     "beta",  -999,  "snow", .False.,  "pnorm",    1.0_dp,      "na", -999.0_dp, .False.)
  betaMeta(ixBeta%nmf)             = betaPar_meta('nmf'            ,     1.0_dp,    0.8_dp,    1.2_dp,     "beta",  -999,  "snow", .False.,  "pnorm",    1.0_dp,      "na", -999.0_dp, .False.)
  betaMeta(ixBeta%tipm)            = betaPar_meta('tipm'           ,     1.0_dp,    0.8_dp,    1.2_dp,     "beta",  -999,  "snow", .False.,  "pnorm",    1.0_dp,      "na", -999.0_dp, .False.)
  betaMeta(ixBeta%plwhc)           = betaPar_meta('plwhc'          ,     1.0_dp,    0.8_dp,    1.2_dp,     "beta",  -999,  "snow", .False.,  "pnorm",    1.0_dp,      "na", -999.0_dp, .False.)
  betaMeta(ixBeta%daygm)           = betaPar_meta('daygm'          ,     1.0_dp,    0.8_dp,    1.2_dp,     "beta",  -999,  "snow", .False.,  "pnorm",    1.0_dp,      "na", -999.0_dp, .False.)

end subroutine

! -----
!  Public subroutine: Populate metadata for MPR infor
! -----------------------
subroutine popMprMeta(err,message)
  use nrtype
  use data_type,  only:var_meta
  use var_lookup, only:ixVarMapData
  use var_lookup, only:ixVarSoilData
  use var_lookup, only:ixVarVegData
  use var_lookup, only:ixVarTopoData
  use var_lookup, only:ixPrpVeg
  use globalData, only:map_meta
  use globalData, only:sdata_meta
  use globalData, only:tdata_meta
  use globalData, only:vdata_meta
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
  ! topo data variables
  tdata_meta(ixVarTopoData%polyid)       = var_meta('polyid'       ,"vege polygon id"                                    ,"-"           ,"1D", "integer")
  tdata_meta(ixVarTopoData%ele_mean)     = var_meta('ele_mean'     ,"mean elevation"                                     ,"m"           ,"1D", "double" )
  tdata_meta(ixVarTopoData%ele_std)      = var_meta('ele_std'      ,"std elevation"                                      ,"m"           ,"1D", "double" )
  tdata_meta(ixVarTopoData%slp_mean)     = var_meta('slp_mean'     ,"mean slope"                                         ,"-"           ,"1D", "double" )
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

end subroutine

end module popMeta
