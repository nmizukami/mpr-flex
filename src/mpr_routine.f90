module mpr_routine 
  USE nrtype                                           ! Including numerical type definition 
  USE data_type                                        ! Including custum data structure definition
  USE public_var                                       ! Including common constant (physical constant, other e.g., missingVal, etc.)

  implicit none

  PRIVATE
   
  public:: mpr

contains

subroutine mpr(idModel,           &     ! model ID
               gammaPar,          &     ! array of gamma parameter 
               gammaParMeta,      &     ! array of gamma parameter metadata
               err, message)
  use model_wrapper,        only:read_soil_param, read_hru_id
  use popMeta,              only:popMprMeta
  use globalData,           only:parMaster, betaInGamma
  use globalData,           only:sdata_meta
  use globalData,           only:map_meta
  use get_ixname,           only:get_ixPar
  use soiltf,               only:comp_soil_model_param         ! Including Soil model parameter transfer function
  use modelLayer,           only:comp_model_depth              ! Including model layr depth computation routine 
  use modelLayer,           only:map_slyr2mlyr                 ! Including model layr computation routine 
!   USE vegtf,                  only:comp_veg_model_param          ! Including Veg model parameter transfer function
   USE upscaling,           only:aggreg                        ! Including Upscaling operator 
  use read_mapdata,         only:getMapData                  ! routine to read mapping data into data structures
!   USE read_vegdata                                         ! routine to read veg data into data structures
  use read_soildata,        only:getData                     ! routine to read soil data into data structures
  use read_soildata,        only:mod_hslyrs                  ! routine to modify soil layer thickness and updata soil data structure
  use var_lookup,           only:ixPar,nPar                  ! 
  USE var_lookup,           only:ixVarSoilData,nVarSoilData  ! index of soil data variables and number of variables 
  USE var_lookup,           only:ixVarVegData,nVarVegData    ! index of vege data variables and number of variables
  USE var_lookup,           only:ixVarMapData,nVarMapData    ! index of map data variables and number of variables
  USE var_lookup,           only:ixVarHru,nVarHru            ! index of model HRU variables and number of variables
!  USE var_lookup,             only:ixPrpSoil,nPrpSoil           ! index of soil properties and number of properties
!  USE var_lookup,             only:ixPrpVeg,nPrpVeg             ! index of veg properties and number of properties

   implicit none

  ! input
  integer(i4b),         intent(in)   :: idModel 
  real(dp),             intent(in)   :: gammaPar(:)              ! array of parameter value adjusted with calibration
  type(cpar_meta),      intent(in)   :: gammaParMeta(:)          ! array of calibrating meta data
  ! output
  integer(i4b),         intent(out)  :: err                      ! error code
  character(len=strLen),intent(out)  :: message                  ! error message 
  !! local
  character(len=strLen)              :: cmessage                 ! error message from downward subroutine
  integer,     parameter             :: iHruPrint = 1            ! model hru id for which everything is printed for checking
  integer(i4b),parameter             :: nSub=11                  ! max. number of Soil layer within Model layer
  integer(i4b)                       :: iLocal                   ! index of hru array in mapping file that match hru id of interest 
  integer(i4b)                       :: iDummy(1)                ! 1D integer array for temporal storage 
  integer(i4b)                       :: iGamma, iGammaMaster     ! index loop
  integer(i4b)                       :: iPoly                    ! Loop index of soil polygon
  integer(i4b)                       :: iSLyr                    ! Loop index of soil layer 
  integer(i4b)                       :: iMLyr                    ! loop index of model soil layer (1,...,n from top to bottom) 
  integer(i4b)                       :: iParm                    ! Loop index of model parameters (e.g., VIC)
  integer(i4b)                       :: iVar                     ! Loop index of miscleneous variables 
  integer(i4b)                       :: iHru                     ! loop index of hrus 
  integer(i4b)                       :: iSub                     ! Loop index of multiple soi layers in model layer
  logical(lgc),allocatable           :: mask(:)                  ! mask for 1D array 
  integer(i4b)                       :: hruID(nHru)
  real(dp),      allocatable         :: paramtemplate(:,:)       ! 
  type(par_meta),allocatable         :: gammaParMasterMeta(:)
  integer(i4b)                       :: nSpoly                   ! number of soil polygon in entire soil data domain 
  integer(i4b)                       :: nSlyrs                   ! number of soil layers
  real(dp)                           :: hmult                    ! mulitplier of soil layer  
  real(dp)                           :: hfrac(nLyr-1)            ! fraction of soil depth for each model layer 
  type(namevar)                      :: sdata(nVarSoilData)      ! soil data container for all the soil polygons
  type(namevar)                      :: sdataLocal(nVarSoilData) ! soil data container for local soil polygon 
  ! vege data stuff
  integer(i4b)                      :: nVpoly                   ! number of vege polygon (grid box) in entire vege data domain 
  integer(i4b),    allocatable      :: vegClass(:)              ! veg class array (e.g., IGBP)
  integer(i4b),    allocatable      :: vclsLocal(:)             ! veg class id for each poly for subset of veg data
  type(var_d),     allocatable      :: vcls2prp(:)              ! storage of property value for each veg class
  type(namedvar),  allocatable      :: vprp(:)                  ! storage of veg property values for 1D(poly) 
  type(namedvar),  allocatable      :: vprpLocal(:)             ! storage of veg property values for 1D(poly) 
  type(namevar)                     :: vdata(nVarVegdata)       ! veg data container for all the veg polygons
  integer(i4b)                      :: iVclass                  ! ID (= index) of vege class 
  integer(i4b)                      :: iPrpVeg                  ! Loop index of veg properties 
  integer(i4b)                      :: nVegParModel             ! Number of model vege parameters
  integer(i4b)                      :: nSoilParModel            ! Number of model soil parameters
  real(dp),        allocatable      :: hModel(:,:)              ! 
  real(dp),        allocatable      :: zModel(:,:)              ! 
  type(dat_d2d),   allocatable      :: parSxySz(:)              ! storage of model soil parameter for 2D field (2nd variable)-soil poy x soil layer
  type(dat_d2d),   allocatable      :: parSxyMz(:)              ! storage of model soil parameter for 2D field (2nd variable)-soil poy x model layer
  type(dat_d2d),   allocatable      :: parMxyMz(:)              ! current soil parameter values for each HRU 
  type(namedvar),  allocatable      :: vegParMxy(:)             ! storage of model soil parameter for 2D field (2nd variable)-model hru x model layer
  type(namedvar),  allocatable      :: ParVxy(:)                ! storage of model vege parameter for 1D or 2D field - vege poly (x month)
  ! mapping stuff  
  integer(i4b),    allocatable      :: polySub(:)               ! list of ID (=index) of soil polygons contributing model hru
  integer(i4b),    allocatable      :: vPolySub(:)              ! list of ID (=index) of veg polygons contributing model hru 
  logical(lgc),    allocatable      :: vmask(:)                 ! mask for vpolIdSub array 
  integer(i4b)                      :: nShru                    ! number of hrus in soil mapping file) 
  integer(i4b)                      :: nVhru                    ! number of hrus in vege mapping file (should be equal to nVhru)
  integer(i4b)                      :: nOverSpoly               ! number of overlapped soil polygon for each model HRU 
  integer(i4b)                      :: nOverVpoly               ! number of overlapped vege polygon for each model HRU 
  integer(i4b)                      :: nSpolyLocal              ! number of subset overlapped soil polygon for each model HRU 
  integer(i4b)                      :: nVpolyLocal              ! number of subset overlapped vege polygon for each model HRU 
  type(mapvar)                      :: mapdata(2)               ! map data container for all the soil polygons
  real(dp),        allocatable      :: swgtSub(:)               ! adjusted Areal weight of soil polygon for all model hrus
  real(dp),        allocatable      :: vwgtSub(:)               ! adjusted Areal weight of veg polygon for one model hrus
  type(poly),      allocatable      :: soil2model_map(:)        ! data structure to hold weight and index of contributing soil layer per model layer and per soil polygon
  type(lyr_d),     allocatable      :: paramvec(:)              !
  
  ! initialize error control
  err=0; message='mpr/'
  ! print out list of gamma/beta parameters
  print*,"--list of gamma parameters"
  print*,gammaParMeta(:)%pname
  print*,"--beta in gamma"
  print*,betaInGamma
  !(0) Preparation
  allocate(gammaParMasterMeta, source=parMaster) ! copy master parameter metadata
  ! separate beta parameter into veg and soil 
  nSoilParModel=size(betaInGamma)  !number of soil and vege parameters to be computed
  ! Swap gammaParMasterMeta%val with gammaPar value
  do iGamma=1,size(gammaPar)
    gammaParMasterMeta(gammaParMeta(iGamma)%ixMaster)%val=gammaPar(iGamma)
  enddo
  call popMprMeta( err, cmessage)   !for sdata_meta, vdata_meta, map_meta
  if(err/=0)then; message=trim(message)//cmessage; return; endif
  hmult=gammaParMasterMeta(ixPar%z1gamma1)%val
  call pop_hfrac(gammaPar, gammaParMeta, hfrac, err, message) ! to get hfrac 
  if(err/=0)then; message=trim(message)//cmessage; return; endif
  ! (0.6) read model parameter file as template
  allocate(paramTemplate(nHru,TotNpar))
  call read_soil_param(idModel, paramTemplate, err, cmessage) 
  if(err/=0)then; message=trim(message)//cmessage; return; endif
  ! (0.6) Memory allocation
  allocate(parSxySz(nSoilParModel),stat=err);if(err/=0)then; message=trim(message)//'error allocating parSxySz'; return; endif
  allocate(parSxyMz(nSoilParModel),stat=err);if(err/=0)then; message=trim(message)//'error allocating parSxyMz'; return; endif
  allocate(parMxyMz(nSoilParModel),stat=err);if(err/=0)then; message=trim(message)//'error allocating parMxyMz'; return; endif
  allocate(vegParMxy(nVegParModel),stat=err);if(err/=0)then; message=trim(message)//'error allocating vegParMxy';return; endif
  allocate(paramvec(nSoilParModel),stat=err);if(err/=0)then; message=trim(message)//'error allocating paramvec'; return; endif 
  ! (1) Get Geophysical data 
  ! *****
  ! (1.1) soil data  
  ! *********************************************
  call getData(trim(mpr_input_dir)//trim(fname_soil),& ! input: soil data (netCDF)
               sdata_meta,                           & ! input: soil data meta
               dname_spoly,                          & ! input: spatial dimension (polygon ID)
               dname_slyrs,                          & ! input: spatial dimension (polygon ID)
               sdata,                                & ! input-output: soil data structure
               nSpoly,                               & ! output: number of dimension (i.e. number of soil polygon)
               nSlyrs,                               & ! output: number of dimension (i.e. number of soil layer)
               err, cmessage)
  if(err/=0)then; message=trim(message)//cmessage; return; endif 
  call mod_hslyrs(sdata,hmult,err,cmessage) ! modify soil layer thickness in sdata data structure
  if(err/=0)then; message=trim(message)//cmessage; return; endif 
  ! *****
  ! (1.2) Read in veg class - properties lookup table ...
  ! *********************************************
!  ! (1.2.1) Read in veg data netCDF...
!   allocate(vegClass(nVclass),stat=err); if(err/=0) call handle_err(err,'error allocating for vegClass')
!   allocate(vcls2prp(nVclass),stat=err); if(err/=0) call handle_err(err,'error allocating for vcls2prp')
!   do iVclass=1,nVclass
!     allocate(vcls2prp(iVclass)%var(nPrpVeg), stat=err); if(err/=0) call handle_err(err,'error allocating for vcls2prp%var') 
!   enddo
!   call getVegData(trim(mpr_input_dir)//trim(fname_veg), & ! input: file name
!                   vdata_meta,                       & ! input: veg data meta
!                   dname_vpoly,                      & ! input: dimension name for veg polygon
!                   vdata,                            & ! input-output: veg data structure
!                   nVpoly,                           & ! output: number of veg polygon
!                   err, cmessage)                     ! output: error control
!   if(err/=0)then; message=message//cmessage; return; endif
!  ! (1.2.2) Read in veg class-property lookup table 
!   call getvegClassLookup(trim(mpr_input_dir)//trim(vclass_table), &
!                          nVclass,                             &
!                          vegClass,                            &
!                          vcls2prp,                            &
!                          err, cmessage)
!   if(err/=0)then; message=message//cmessage; return; endif
!  ! (1.2.3) populate veg properties for each polygon 
!   allocate(vprp(nPrpVeg),stat=err); if(err/=0) call handle_err(err,'error allocating for sprp')
!   do iPrpVeg=1,nPrpVeg
!     allocate(vprp(iPrpVeg)%varData(nVpoly), stat=err); if(err/=0) call handle_err(err,'error allocating for sprp%varData')
!   enddo
!   call map_vcls2prp(vdata, vcls2prp, vegClass, vprp, err, cmessage)
!   if(err/=0)then; message=message//cmessage; return; endif
  ! *****
  ! (2.) Read in mapping netcdf 
  ! *********************************************
  call getMapData(trim(mpr_input_dir)//trim(fname_smapping), &   ! input: file name
                  'soil',                                    &   ! input: geophysical data type 
                  map_meta,                                  &   ! input: map data meta
                  dname_hru,                                 &   ! input: dimension name for hru 
                  dname_overSpoly,                           &   ! input: dimension name for overlap polygon 
                  mapdata,                                   &   ! input-output: map data structure
                  nShru,                                     &   ! output: number of hru 
                  nOverSpoly,                                &   ! output: max number of overlap polygons
                  err,cmessage)                                  ! output: error control
  if (err/=0)then; message=trim(message)//cmessage; return; endif
  call read_hru_id(idModel, hruID, err, cmessage)  ! get hruID
  if (err/=0)then; message=trim(message)//cmessage; return; endif
!    ! (2.2) mapping vege polygon to model hru 
!    call getMapData(trim(mpr_input_dir)//trim(fname_vmapping), &   ! input: file name
!                    'veg',                                 &   ! input: geophysical data type 
!                    map_meta,                              &   ! input: map data meta
!                    dname_hru,                             &   ! input: dimension name for hru 
!                    dname_overVpoly,                       &   ! input: dimension name for overlap Polygon 
!                    mapdata,                               &   ! input-output: map data structure
!                    nVhru,                                 &   ! output: number of hru 
!                    nOverVpoly,                            &   ! output: max number of overlap polygon
!                    err,cmessage)                             ! output: error control
!    if (err/=0)then; message=message//cmessage; return; endif
!    ! (2.3) Check if "hru" variables in vege and soil mappling netcdf are identifal
!    if ( nShru /= nVhru ) then
!      call handle_err(10,'Different number of hru in vege and soil mapping file')  
!    end if
!    if ( minval(abs(mapdata(1)%var(ixVarMapData%hru_id)%ivar1-mapdata(2)%var(ixVarMapData%hru_id)%ivar1)) /= 0 ) then
!      call handle_err(11,'different hru id in vege and soil mapping file')
!    end if
    associate( hruMap      => mapdata(1)%var(ixVarMapData%hru_id)%ivar1,  &
               swgt        => mapdata(1)%var(ixVarMapData%weight)%dvar2,  &
               overSpolyID => mapdata(1)%var(ixVarMapData%overlapPolyId)%ivar2 )
!  ! *****
!  ! (3.) Computing model parameters from geophysical properties (soil, vege) and scale them to model HRU 
!  ! *********************************************************************
    do iParm=1,nSoilParModel
      allocate(parMxyMz(iParm)%dat(nLyr,nHru),stat=err)
    enddo
!    do iParm=1,nVegParModel
!      allocate(vegParMxy(iParm)%varData(nHru),stat=err)
!    enddo
    !!! ---------------------------------------------
    !!! Start of model hru loop (from mapping file) !!!
    !!! ---------------------------------------------
    hru: do iHru=1,nHru
      ! Get index (iLocal) of hru id that matches with current hru from hru id array in mapping file
      if (minval( abs(hruMap-hruID(iHru)) ) /= 0 )then; err=10; message=trim(message)//'hru id does not exist in mapping file';return; endif
      idummy = minloc( abs(hruMap-hruID(iHru)) )
      iLocal=idummy(1)
      ! Select list of soil polygons contributing a current hru
      allocate(mask(nOverSpoly))
      mask = ( overSpolyID(:,iLocal) /= imiss )
      allocate(polySub(count(mask)))
      allocate(swgtSub(count(mask)))
      nSpolyLocal = size(polySub)                ! number of contributing soil polygons to current hru
      polySub = pack(overSpolyID(:,iLocal),mask) ! id of soil polygons contributing to current hru
      swgtSub = pack(swgt(:,iLocal),mask)        ! weight of soil polygons contributing to current hru
      ! allocate memmory
      do iParm=1,nSoilParModel
        allocate(parSxySz(iParm)%dat(nSlyrs,nSpolyLocal),stat=err); if(err/=0)then; message=message//'error allocating parSxySz%dat'; return; endif 
        allocate(parSxyMz(iparm)%dat(nLyr,nSpolyLocal),stat=err);   if(err/=0)then; message=message//'error allocating parSxyMz%dat'; return; endif
      enddo
      allocate(hModel(nLyr,nSpolyLocal),stat=err);    if(err/=0)then; message=message//'error allocating hModel'; return; endif
      allocate(zModel(nLyr,nSpolyLocal),stat=err);    if(err/=0)then; message=message//'error allocating zModel'; return; endif
      allocate(soil2model_map(nSpolyLocal),stat=err); if(err/=0)then; message=trim(message)//'error allocating soil2model_map';return;endif
    ! *****
    ! (3.1) Extract soil poly ID, weight polygon , and soil properties for current model hru 
    ! *********************************************************************
      call subSoilData(sdata, polySub, sdataLocal, err, message)
      if ( iHru == iHruPrint ) then
        print*,' '
        print*,'****************************************************'
        print*,'HRU, hruID = ',iHru,hruID(iHru)
        print*,'****************************************************'
        print*,'(1.1) Print list of soil polygon ID and weigth'
        write(*,"(' polyID = ',100I9)") (polySub(iPoly), iPoly=1,nSpolyLocal)
        write(*,"(' weight = ',100f9.3)") (swgtSub(iPoly), iPoly=1,nSpolyLocal)
        do iVar=1,nVarSoilData
          write(*,"(A12,'= ')") adjustl(sdataLocal(iVar)%varName)
          do iPoly=1,nSpolyLocal
          select case(trim(sdata_meta(iVar)%vartype))
            case('integer')
              select case(trim(sdata_meta(iVar)%vardims))
                case('2D'); write(*,"(20I9)") (sdataLocal(iVar)%ivar2(iSLyr,iPoly), iSlyr=1,nSlyrs)
                case('1D'); write(*,"(I9)") (sdataLocal(iVar)%ivar1(iPoly))
              end select
            case('double')
              select case(trim(sdata_meta(iVar)%vardims))
                case('2D')
                  write(*,"(20f9.3)") (sdataLocal(iVar)%dvar2(iSLyr,iPoly), iSlyr=1,nSlyrs)
                case('1D')
                  write(*,"(f9.3)") (sdataLocal(iVar)%dvar1(iPoly))
              end select
            end select
          end do
        enddo
      endif
!    ! *****
!    ! (3.2) Extract veg poly ID, weight polygon, and veg properties for current model hru 
!    ! *********************************************************************
!      ! Select list of veg polygon ID contributing a current hru 
!      allocate(vmask(nOverVpoly),stat=err);        if(err/=0) call handle_err(err,'error allocating for mask')
!      vmask        = ( overVpolyID(:,iLocal) /= imiss )
!      allocate(vPolySub(count(vmask)),stat=err); if(err/=0) call handle_err(err,'error allocating for vPolySub')
!      allocate(vwgtSub(count(vmask)),stat=err); if(err/=0) call handle_err(err,'error allocating for swgtSub')
!      vPolySub    = pack(overVpolyID(:,iLocal),vmask)
!      vwgtSub     = pack(overVpolyID(:,iLocal),vmask)
!      nVpolyLocal = size(vPolySub)
!  
!      allocate(vprpLocal(nPrpVeg),stat=err); if(err/=0) call handle_err(err,'error allocating for vprp')
!      do iPrpVeg=1,nPrpVeg 
!        allocate(vprpLocal(iPrpVeg)%varData(nVpolyLocal))
!      enddo
!      allocate(vclsLocal(nVpolyLocal))
!  
!      do iPoly = 1,nVpolyLocal
!        do iPrpVeg = 1,nPrpVeg
!          vprpLocal(iPrpVeg)%varData(iPoly) =vprp(iPrpVeg)%varData(vPolySub(iPoly)) 
!        enddo
!        vclsLocal(iPoly) = vdata(ixVarVegData%vegclass)%ivar1(vPolySub(iPoly))
!      end do
!      if ( iHru == iHruPrint ) then
!        print*,' '
!        print*,'(1.3) Print list of vege polygon ID and weigth'
!        write(*,"(' polyID = ',100I7)") (vPolySub(iPoly), iPoly=1,nVpolyLocal)
!        write(*,"(' weight = ',100f7.3)") (vwgtSub(iPoly), iPoly=1,nVpolyLocal)
!        write(*,"(' vegclass = ',100I5)") (vclsLocal(iPoly), iPoly=1,nVpolyLocal)
!        print*,'(1.3) Print veg property for polygon'
!        do iPrpVeg=1,nPrpVeg
!          write(*,"(1X,A12,'= ',100f7.3)") vprp_meta(iPrpVeg)%varname, (vprpLocal(iPrpVeg)%varData(iPoly), iPoly=1,nVpolyLocal)
!        enddo
!      endif
    ! *****
    ! (3.4) Compute model soil parameters using transfer function
    ! *********************************************************
      ! compute model soil parameters
      call comp_soil_model_param(parSxySz, sdataLocal, gammaParMasterMeta, nSlyrs, nSpolyLocal)
      if ( iHru == iHruPrint ) then
        print*,'(2) Print Model parameter for polygon and layer'
        write(*,"(' Layer       =',20I9)") (iSLyr, iSlyr=1,nSlyrs)
        do iParm=1,nSoilParModel
          do iPoly = 1,nSpolyLocal
            write(*,"(1X,A12,'=',20f9.3)") betaInGamma(iParm), (parSxySz(iparm)%dat(iSLyr,iPoly), iSlyr=1,nSlyrs)
          enddo
        enddo
      endif
!    ! *****
!    ! (3.5) Compute Model vege parameters using transfer function
!    ! *********************************************************
!      ! compute model veg parameters
!      allocate(ParVxy(nVegParModel),stat=err); if(err/=0) call handle_err(err,'error allocating for ParVxy')
!      do iparm=1,nVegParModel
!        select case ( vpar_meta(iparm)%dims )
!          case( '1D' , 'ST' )
!            allocate(ParVxy(iparm)%varData(nVpolyLocal),stat=err); if(err/=0) call handle_err(err,'error allocating for ParVxy%varData')
!        end select
!      enddo
!      call comp_veg_model_param(ParVxy, vprpLocal, vpar_meta, model_name)
!      if ( iHru == iHruPrint ) then
!        print*,'(2) Print Model parameter for Vege polygon'
!        do iparm=1,nVegParModel
!          write(*,"(1X,A10,'= ',100f5.2)") vpar_meta(iparm)%parname, (ParVxy(iparm)%varData(iPoly), iPoly=1,nVpolyLocal)
!        enddo
!      endif
  ! ***********
  ! (4) Spatial aggregation of Model parameter 
  ! *********************************************************************
    ! **********
    ! (4.1) Aggregate model parameveter vertical direction - soil data layers to model soil layers
    ! *********************************************************************
    ! (4.1.1) Compute Model layer depth 
      call comp_model_depth(hModel, zModel, hfrac, sdataLocal) 
      if ( iHru == iHruPrint ) then
        print*, '(3.1.1) Print model depth ---' 
        write(*,"(' Layer       =',20I9)") (iMLyr, iMlyr=1,nlyr)
        do iPoly = 1,nSpolyLocal
          write(*,"('z            =',20f9.3)") (zModel(iMLyr,iPoly), iMlyr=1,nLyr)
        enddo 
      endif
    ! (4.1.2) Computing vertical weight of soil layer for each model layer
      second: associate( hSoil =>sdataLocal(ixVarSoilData%hslyrs)%dvar2 ) 
      call map_slyr2mlyr(hSoil, zModel, soil2model_map, err, cmessage)
      end associate second
      if ( iHru == iHruPrint ) then
        print*, '(3.1.1) Print info on mapping from soil to model layer --' 
        do iPoly = 1,nSpolyLocal
          do iMLyr = 1,nLyr
            print*, 'Poly, Layer=',iPoly,iMLyr
            write(*,"('weightSubLyr= ', 11f9.3)") (soil2model_map(iPoly)%layer(iMLyr)%weight(iSub), iSub=1,nSub)
            write(*,"('indexSubLyr = ', 11I7)")   (soil2model_map(iPoly)%layer(iMLyr)%ixSubLyr(iSub), iSub=1,nSub)
          enddo 
        enddo
      endif
    ! (4.1.3) Computing vertical weight for each model layer
      do iPoly=1,nSpolyLocal
        do iMLyr = 1,nLyr
          do iParm = 1,nSoilParModel
            allocate( paramvec(iParm)%layer(nSub),stat=err); if(err/=0)then;message=trim(message)//'error allocating paramvec';return;endif 
            paramvec(iParm)%layer = 0._dp
          enddo
          do iSub=1,nSub
            third: associate(iSelect => soil2model_map(iPoly)%layer(iMLyr)%ixSubLyr(iSub) )
            if (iSelect > 0 ) then 
              do iparm = 1,nSoilParModel
                paramvec(iParm)%layer(iSub) = parSxySz(iparm)%dat(iSelect,iPoly)
              enddo
            endif
            end associate third
          enddo
          do iParm = 1,nSoilParModel
            forth: associate( ix=>get_ixPar(trim(betaInGamma(iParm))) )
            if ( trim(parMaster(ix)%vups)/='na' )then
              call aggreg(parSxyMz(iParm)%dat(iMLyr,iPoly),             &
                          soil2model_map(iPoly)%layer(iMLyr)%weight(:), &
                          paramvec(iParm)%layer(:),                     &
                          parMaster(ix)%vups,                           &
                          err, cmessage)
               if(err/=0)then;message=trim(message)//trim(cmessage);return;endif
            endif
            end associate forth
          enddo
          do iparm = 1,nSoilParModel
            deallocate(paramvec(iParm)%layer,stat=err);if(err/=0)then;message=trim(message)//'error deallocating paramvec%layer';return;endif
          enddo
        enddo ! end of iMLyr (model layer) loop
      enddo ! end of iPoly
    ! ************
    ! (4.2) Aggregate model parameveter horizontally - use spatial weight netCDF 
    ! *********************************************************************
      do iMLyr=1,nLyr
        do iParm = 1,nSoilParModel
          allocate( paramvec(iParm)%layer(nSpolyLocal),stat=err); if(err/=0)then; message=trim(message)//'error allocating paramvec'; return; endif
          paramvec(iParm)%layer = 0._dp
        enddo
        do iPoly=1,nSpolyLocal
          do iParm = 1,nSoilParModel
              paramvec(iParm)%layer(iPoly) = parSxyMz(iparm)%dat(iMLyr,iPoly)
          enddo
        enddo
        do iParm = 1,nSoilParModel
          fifth: associate( ix=>get_ixPar(trim(betaInGamma(iParm))) )
          if ( trim(parMaster(ix)%hups)/='na' )then
            call aggreg(parMxyMz(iParm)%dat(iMLyr,iHru), &
                        swgtsub(:),                      &
                        paramvec(iparm)%layer(:),        &
                        parMaster(ix)%hups,              &
                        err, cmessage)
            if ( iHru == iHruPrint ) then
              print*,'-----------------------------------'
              print*,'Aggregated soil parameter '
              write(*,"(1X,A17,'(layer ',I2,') = ',100f9.3)") parMaster(ix)%pname,iMLyr ,parMxyMz(iParm)%dat(iMLyr,iHru)
            endif
          endif
          end associate fifth
        enddo
        do iParm = 1,nSoilParModel
          deallocate(paramvec(iParm)%layer, stat=err); if(err/=0)then; message=trim(message)//'error deallocating paramvec%layer'; return; endif 
        enddo
      enddo
!      ! Aggregation veg parameter per model hru 
!      do iparm = 1,nVegParModel
!        if (vpar_meta(iparm)%h_agg) then
!          call aggreg(vegParMxy(iparm)%varData(iHru), &
!                      vwgtSub(:),                     &
!                      ParVxy(iparm)%varData,          &
!                      dmiss,                          &
!                      vpar_meta(iparm)%haggmethod)
!          if ( iHru == iHruPrint ) then
!            print*,'-----------------------------------'
!            print*,'Aggregated vege parameter '
!            write(*,"(1X,A17,'= ',100f9.3)") vpar_meta(iparm)%parname, (vegParMxy(iparm)%varData(iHru))
!          endif
!        endif
!      enddo
      ! deallocate memmory
      deallocate(mask,stat=err);           if(err/=0)then; message=trim(message)//'error deallocating mask'; return; endif
      deallocate(polySub,stat=err);        if(err/=0)then; message=trim(message)//'error deallocating polyIdSub'; return; endif
      deallocate(swgtSub,stat=err);        if(err/=0)then; message=trim(message)//'error deallocating swgtSub'; return; endif
!      deallocate(vmask,stat=err);            if(err/=0) call handle_err(err,'error deallocating for vmask')
!      deallocate(vpolySub,stat=err);         if(err/=0) call handle_err(err,'error deallocating for vpolyIdSub')
!      deallocate(vwgtSub,stat=err);          if(err/=0) call handle_err(err,'error deallocating for vwgtSub')
!      deallocate(vclsLocal,stat=err);        if(err/=0) call handle_err(err,'error deallocating for vclsLocal')
!      deallocate(vprpLocal,stat=err);        if(err/=0) call handle_err(err,'error deallocating for sprpLocal')
      deallocate(soil2model_map,stat=err); if(err/=0)then; message=trim(message)//'error deallocating soil2model_map';return;endif
      deallocate(hModel,stat=err);         if(err/=0)then; message=trim(message)//'error deallocating hModel';return;endif
      deallocate(zModel,stat=err);         if(err/=0)then; message=trim(message)//'error deallocating zModel';return;endif
      do iParm=1,nSoilParModel
        deallocate(parSxySz(iParm)%dat,stat=err);if(err/=0)then; message=message//'error deallocating parSxySz%dat'; return; endif 
        deallocate(parSxyMz(iparm)%dat,stat=err);if(err/=0)then; message=message//'error deallocating parSxyMz%dat'; return; endif
      enddo
    enddo hru
    end associate
    print*, parMxyMz
    return
end subroutine
  
  subroutine subSoilData(soilData, subPolyID, soilDataLocal, err, message)
    use var_lookup,           only:ixVarSoilData,nVarSoilData  ! index of soil data variables and number of variables 
    use globalData,           only:sdata_meta
    implicit none
    !input variables
    type(namevar),        intent(in)    :: soilData(:)      ! soil data container for all the soil polygons
    integer(i4b),         intent(in)    :: subPolyID(:)  ! subset of soil polygon id  
    !output variables
    type(namevar),        intent(inout) :: soilDataLocal(:) ! soil data container for local soil polygon 
    integer(i4b),         intent(out)   :: err           ! error code
    character(*),         intent(out)   :: message       ! error message
    !local variables
    integer(i4b),allocatable            :: polyID(:)
    integer(i4b)                        :: iPoly         ! index of soil polygon loop 
    integer(i4b)                        :: iVar          ! index of named variable loop
    integer(i4b)                        :: iLocal        ! index of hru array in mapping file that match hru id of interest 
    integer(i4b)                        :: nPoly         ! number of polygons in subset 
    integer(i4b)                        :: nSlyrs        ! number of soil layers 
    integer(i4b)                        :: iDummy(1)     ! 1D integer array for temporal storage 

    err=0; message='subSoilData/'
    !associate( polyID => soilData(ixVarSoilData%polyid)%ivar1 )
    allocate(polyID(size(soilData(ixVarSoilData%polyid)%ivar1)))
    polyID = soilData(ixVarSoilData%polyid)%ivar1 
    nPoly=size(subPolyID)
    do iVar=1,nVarSoilData
      soilDataLocal(ivar)%varName=trim(soilData(ivar)%varName)
      select case(trim(sdata_meta(iVar)%vartype))
        case('integer')
          select case(trim(sdata_meta(iVar)%vardims))
            case('2D')
              nSlyrs=size(soilData(ivar)%ivar2,1) 
              allocate(soilDataLocal(ivar)%ivar2(nSlyrs,nPoly),stat=err)
              if(err/=0)then; message=trim(message)//'problem allocating 2D int space for soilDataLocal data structure'; return; endif
            case('1D')
!              print*,soilData(ixVarSoilData%polyid)%ivar1
!              print*, '-----------'
              allocate(soilDataLocal(ivar)%ivar1(nPoly),stat=err)
              if(err/=0)then; message=trim(message)//'problem allocating 1D int space for soilDataLocal data structure'; return; endif
!              print*,soilData(ixVarSoilData%polyid)%ivar1
            end select
        case('double')
          select case(trim(sdata_meta(iVar)%vardims))
            case('2D')
              nSlyrs=size(soilData(ivar)%dvar2,1) 
              allocate(soilDataLocal(ivar)%dvar2(nSlyrs,nPoly),stat=err)
              if(err/=0)then; message=trim(message)//'problem allocating 2D real space for soilDataLocal data structure'; return; endif
            case('1D')
              allocate(soilDataLocal(ivar)%dvar1(nPoly),stat=err)
              if(err/=0)then; message=trim(message)//'problem allocating 1D real space for soilDataLocal data structure'; return; endif
          end select
      end select 
      do iPoly=1,nPoly
        if (minval( abs(polyID-subPolyID(iPoly)) ) /= 0 )then; err=10; message=trim(message)//'hru id does not exist in mapping file';return; endif
        iDummy = minloc( abs(polyID-subPolyID(iPoly)) )
        iLocal=iDummy(1)
        select case(trim(sdata_meta(iVar)%vartype))
          case('integer')
            select case(trim(sdata_meta(iVar)%vardims))
              case('2D'); soilDataLocal(ivar)%ivar2(:,iPoly) = soilData(ivar)%ivar2(:,iLocal)
              case('1D'); soilDataLocal(ivar)%ivar1(iPoly)   = soilData(ivar)%ivar1(iLocal)
              end select
          case('double')
            select case(trim(sdata_meta(iVar)%vardims))
              case('2D'); soilDataLocal(ivar)%dvar2(:,iPoly) = soilData(ivar)%dvar2(:,iLocal)
              case('1D'); soilDataLocal(ivar)%dvar1(iPoly)   = soilData(ivar)%dvar1(iLocal)
            end select
        end select 
      end do
    end do 
!    end associate
    return 
  end subroutine 

  subroutine pop_hfrac(gammaPar, gammaParMeta,hfrac, err, message)
    use globalData,   only: gammaSubset
    implicit none
    !input variables
    real(dp),             intent(in)  :: gammaPar(:)
    type(cpar_meta),      intent(in)  :: gammaParMeta(:)
    !output variables
    real(dp),             intent(out) :: hfrac(:)
    integer(i4b),         intent(out) :: err         ! error code
    character(*),         intent(out) :: message     ! error message
    !local variables
    real(dp)                          :: dummy(20) 
    logical(lgc)                      :: mask(20) 
    logical(lgc),allocatable          :: checkH(:) 
    character(len=strLen)             :: cmessage    ! error message from downward subroutine
    integer(i4b)                      :: unt         ! DK: need to either define units globally, or use getSpareUnit
    integer(i4b)                      :: i,j 
  
    ! initialize error control
    err=0; message='pop_hfrac/'
    dummy=-999
    !check h parameters - now can chcek up to 5 layers
    do i=1,size(gammaSubset)
      if (gammaParMeta(i)%pname=="h1gamma1")then;dummy(1)=gammaPar(i);cycle;endif 
      if (gammaParMeta(i)%pname=="h1gamma2")then;dummy(2)=gammaPar(i);cycle;endif
      if (gammaParMeta(i)%pname=="h1gamma3")then;dummy(3)=gammaPar(i);cycle;endif
      if (gammaParMeta(i)%pname=="h1gamma4")then;dummy(4)=gammaPar(i);cycle;endif
    enddo
    mask=(dummy>0)
    if ( count(mask)/=nLyr-1 ) stop 'number of h1gamma prameters mismatch with nLyr'
    hfrac=pack(dummy,mask)
    return
  end subroutine 

end module mpr_routine