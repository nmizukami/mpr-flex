module mpr_routine

  use nrtype
  use data_type
  use obj_type
  use public_var, only: nHru, nLyr, nMonth
  use nr_utility_module, only: findIndex
  use nr_utility_module, only: int2str

  implicit none

  private

  public:: run_mpr
  public:: mpr

contains

! ************************************************************************************************
! Public subroutine: run MPR and save estimated parameters in netCDF
! ************************************************************************************************
subroutine run_mpr( calParam, err, message )

  use globalData,    only: calScaleMeta, calGammaMeta, nCalGamma, nSoilBetaModel, nVegBetaModel
  use globalData,    only: soilBetaCalName, vegBetaCalName
  use globalData,    only: betaMeta
  use get_ixname,    only: get_ixBeta
  use read_mapdata,  only: read_hru_id
  use write_param_nc,only: write_nc_beta

  implicit none
  ! input variables
  real(dp),             intent(in)  :: calParam(:)                   ! parameter in namelist, not necessarily all parameters are calibrated
  ! output variables
  integer(i4b),         intent(out) :: err                           ! error id
  character(len=strLen),intent(out) :: message                       ! error message
  ! local
  type(var_d)                       :: pnormCoef(size(calScaleMeta)) ! parameter storage converted from parameter array
  type(var_d),          allocatable :: paramGammaStr(:)              ! calibratin gamma parameter storage extracted from calParStr
  integer(i4b)                      :: idx                           ! counter
  integer(i4b)                      :: idxBeta                        ! beta parameter index
  integer(i4b)                      :: iPar                          ! loop index for parameter
  integer(i4b)                      :: nScale                        ! number of scaling operotors associated with beta parameters
  integer(i4b)                      :: hruID(nHru)                   ! Hru ID
  real(dp)                          :: hModel(nLyr,nHru)             ! storage of model layer thickness at model layer x model hru
  type(namevar)                     :: soilParMxyMz(nSoilBetaModel)  ! storage of model soil parameter at model layer x model hru
  type(namevar)                     :: vegParMxy(nVegBetaModel)      ! storage of model vege parameter at month x model hru
  character(len=strLen)             :: cmessage                      ! error message from subroutine

  err=0; message='run_mpr/' ! to initialize error control

  if ( nCalGamma == 0) then
    err=10; message=trim(message)//'there is no gamma pamameters in parameter input file to perform MPR';return
  end if

  ! -----------------------------------------
  ! transform parameter vector to own data types - calParStr and pnormCoef
  idx=1
  allocate(paramGammaStr(nCalGamma) ,stat=err)
  do iPar=1,nCalGamma !beta and gamma parameter values
    allocate(paramGammaStr(iPar)%var(1))
    paramGammaStr(iPar)%var=calParam(idx)
    idx=idx+1
  end do

  do iPar=1,size(calScaleMeta) ! scaling parameter for beta parameter estimated with MPR
    idxBeta = get_ixBeta(calScaleMeta(iPar)%betaname)
    nScale = size(betaMeta(idxBeta)%parScale)
    allocate(pnormCoef(iPar)%var(nScale))
    pnormCoef(iPar)%var(1:nScale)=calParam(idx:idx+nScale-1)
    idx=idx+nScale
  end do

  ! -----------------------------------------
  ! Get hruID from mapping file
  call read_hru_id( hruID, err, cmessage)
  if (err/=0)then;message=trim(message)//trim(cmessage);return;endif

  ! -----------------------------------------
  ! allocate model parameter data structures
  ! soil
  call allocateModelData( soilBetaCalName, soilParMxyMz, err, cmessage )
  if(err/=0)then;message=trim(message)//trim(cmessage);return;endif

  ! vegetation
  call allocateModelData( vegBetaCalName, vegParMxy, err, cmessage )
  if(err/=0)then;message=trim(message)//trim(cmessage);return;endif

  ! -----------------------------------------
  !perform MPR
  call mpr(hruID, pnormCoef, paramGammaStr, calGammaMeta, hModel, soilParMxyMz, vegParMxy, err, cmessage) ! to output model layer thickness and model parameter via MPR
  if(err/=0)then;message=trim(message)//trim(cmessage);return;endif

  !! Write parameter derived from MPR in netCDF
  call write_nc_beta(trim(mpr_output_dir)//trim(model_param_nc), hruID, hModel, soilParMxyMz, vegParMxy, err, cmessage)
  if(err/=0)then;message=trim(message)//trim(cmessage);return;endif

END SUBROUTINE


! ************************************************************************************************
! private subroutine: allcate model data structures
! ************************************************************************************************
SUBROUTINE allocateModelData( modelParName, modelParStruct, err, message )
  use globalData, only: betaMeta, dimMeta
  use get_ixname, only: get_ixBeta
  implicit none
  ! input variables
  character(*),               intent(in)  :: modelParName(:)      ! model parameter names
  ! output variables
  type(namevar),              intent(out) :: modelParStruct(:)    ! storage of model vege parameter at month x model hru
  integer(i4b),               intent(out) :: err                  ! error id
  character(len=strLen),      intent(out) :: message              ! error message
  ! local variable
  integer(i4b)                            :: idxBeta              ! beta parameter index
  integer(i4b)                            :: ix                   ! loop index
  integer(i4b)                            :: iPar                 ! loop index
  integer(i4b)                            :: nBetaModel           ! number of model parameters
  integer(i4b)                            :: nDims                ! number of dimensions
  integer(i4b)                            :: dimIdx               ! dimension index array
  integer(i4b),  allocatable              :: dimSize(:)           ! dimension size array

  err=0; message='allocateModelData/' ! to initialize error control

  nBetaModel = size(modelParName)
  do iPar=1,nBetaModel
    idxBeta = get_ixBeta(modelParName(iPar))

    nDims = size(betaMeta(idxBeta)%parDim)
    allocate(dimSize(nDims))
    if(err/=0)then;message=trim(message)//'error allocating dimSize';return;endif

    do ix = 1,nDims
      dimIdx = betaMeta(idxBeta)%parDim(ix)
      dimSize(ix) = dimMeta(dimIdx)%dimLength
    end do

    if (nDims == 1) then
      allocate(modelParStruct(iPar)%dvar1(dimSize(1)),stat=err)
      if(err/=0)then;message=trim(message)//'error allocating modelParStruct(iPar)%dvar1';return;endif
    elseif (nDims== 2) then
      allocate(modelParStruct(iPar)%dvar2(dimSize(1),dimSize(2)),stat=err)
      if(err/=0)then;message=trim(message)//'error allocating modelParStruct(iPar)%dvar2';return;endif
    end if

    deallocate(dimSize)
  enddo

END SUBROUTINE

! ************************************************************************************************
! public subroutine: mpr
! ************************************************************************************************
subroutine mpr(hruID,             &     ! input: hruID
               pnormCoefStr,      &     ! input: list of pnorm coefficients
               gammaParStr,       &     ! input: array of gamma parameter (including h and z)
               gammaParMeta,      &     ! input: array of gamma parameter metadata (including h and z)
               hModel,            &     ! output: Model layer thickness
               parMxyMz,          &     ! output: MPR derived soil parameter
               vegParMxy,         &     ! output: MPR derived veg parameter
               err, message)            ! output: error id and message

  use popMeta,              only:popMprMeta
  use globalData,           only:dimMeta, betaMeta, gammaMeta, soilBetaCalName, vegBetaCalName, calScaleMeta, &
                                 sdata_meta, vdata_meta, tdata_meta, cdata_meta, map_meta, nSoilBetaModel, nVegBetaModel
  use get_ixname,           only:get_ixGamma, get_ixBeta
  use tf,                   only:comp_model_param              ! Including Soil model parameter transfer function
  use modelLayer,           only:comp_model_depth              ! Including model layr depth computation routine
  use modelLayer,           only:map_slyr2mlyr                 ! Including model layr computation routine
  use upscaling,            only:aggreg                        ! Including Upscaling operator
  use read_soildata,        only:getSoilData                   ! routine to read soil data into data structures
  use read_soildata,        only:mod_hslyrs                    ! routine to modify soil layer thickness and updata soil data structure
  use read_vegdata,         only:getVegData                    ! routine to read veg data into data structures
  use read_vegdata,         only:getVegClassLookup             ! routine to read veg calss-property lookupu table
  use read_topodata,        only:getTopoData                   ! routine to read topography data into data structures
  use read_climdata,        only:getClimData                   ! routine to read climate data into data structures
  use read_mapdata,         only:getMapData                    ! routine to read mapping data into data structures
  use var_lookup,           only:ixBeta, ixGamma               !
  use var_lookup,           only:ixVarSoilData,nVarSoilData    ! index of soil data variables and number of variables
  use var_lookup,           only:ixVarVegData,nVarVegData      ! index of vege data variables and number of variables
  use var_lookup,           only:ixVarTopoData,nVarTopoData    ! index of topography data variables and number of variables
  use var_lookup,           only:ixVarClimData,nVarClimData    ! index of climate data variables and number of variables
  use var_lookup,           only:ixVarMapData,nVarMapData      ! index of map data variables and number of variables
  use var_lookup,           only:ixPrpVeg,nPrpVeg              ! index of veg properties and number of properties

  implicit none

  ! input
  integer(i4b),         intent(in)   :: hruID(:)                    ! hruID list
  type(var_d),          intent(in)   :: gammaParStr(:)              ! data structure of gamma parameter value adjusted with calibration
  type(var_d),          intent(in)   :: pnormCoefStr(:)             ! data structure of pnorm coefficient value adjusted with calibration
  type(cpar_meta),      intent(in)   :: gammaParMeta(:)             ! array of calibrating gamma parameter meta data
  ! output
  real(dp),             intent(out)  :: hModel(:,:)                 ! Model layer thickness at model layer x model hru
  type(namevar),        intent(inout):: parMxyMz(:)                 ! storage of model soil parameter at model layer x model hru
  type(namevar),        intent(inout):: vegParMxy(:)                ! storage of model vege parameter at model hru
  integer(i4b),         intent(out)  :: err                         ! error code
  character(len=strLen),intent(out)  :: message                     ! error message
  ! local
  character(len=strLen)              :: cmessage                    ! error message from downward subroutine
  integer(i2b),parameter             :: iHruPrint = -999            ! model hru id (index) for which everything is printed for checking
  integer(i4b),parameter             :: nSub=11                     ! max. number of Soil layer within Model layer
  integer(i4b)                       :: iLocal                      ! index of hru array in mapping file that match hru id of interest
  integer(i4b)                       :: idxDim                      ! dimension index
  integer(i4b)                       :: idxBeta                     ! beta index
  integer(i4b)                       :: iGamma                      ! index loop
  integer(i4b)                       :: ixDims                      ! loop index for dimension
  integer(i4b)                       :: iPoly                       ! Loop index of soil polygon
  integer(i4b)                       :: iSLyr                       ! Loop index of soil layer
  integer(i4b)                       :: iMLyr                       ! loop index of model soil layer (1,...,n from top to bottom)
  integer(i4b)                       :: iParm                       ! Loop index of model parameters (e.g., VIC)
  integer(i4b)                       :: iVar                        ! Loop index of miscleneous variables
  integer(i4b)                       :: iMon                        ! Loop index of month
  integer(i4b)                       :: iHru                        ! loop index of hrus
  integer(i4b)                       :: iSub                        ! Loop index of multiple soi layers in model layer
  integer(i4b)                       :: ixStart                     ! starting index of subset geophysical polygons in the entire dataset
  integer(i4b)                       :: ixEnd                       ! ending index of subset geophysical polygons in the entire dataset
  integer(i4b),       allocatable    :: dimSize(:)           ! dimension size array
  type(gammaPar_meta),allocatable    :: gammaUpdateMeta(:)
  type(betaPar_meta), allocatable    :: betaUpdateMeta(:)
  integer(i4b)                       :: nDims                       ! dimension size for beta parameter
  integer(i4b)                       :: nGpoly,nDummy               ! number of geophyical propertiy polygon in entire data domain (nGpoly=nVpoly=nTpoly)
  integer(i4b)                       :: nSlyrs                      ! number of soil layers
  integer(i4b)                       :: nGhru                       ! number of hrus in soil mapping file)
  integer(i4b)                       :: nGpolyLocal                 ! number of subset overlapped soil polygon for each model HRU
  type(namevar)                      :: sdata(nVarSoilData)         ! soil data container for all the soil polygons
  type(namevar)                      :: sdataLocal(nVarSoilData)    ! soil data container for local soil polygon
  type(namevar)                      :: vdata(nVarVegData)          ! veg data container for all the polygons
  type(namevar)                      :: vdataLocal(nVarVegData)     ! veg data container for local polygon
  type(namevar)                      :: tdata(nVarTopoData)         ! veg data container for all the polygons
  type(namevar)                      :: tdataLocal(nVarTopoData)    ! veg data container for local polygon
  type(namevar)                      :: cdata(nVarClimData)         ! climate data container for all the polygons
  type(namevar)                      :: cdataLocal(nVarClimData)    ! climate data container for local polygon
  integer(i4b)                       :: vegClass(nVclass)           ! veg class array (e.g., IGBP)
  type(var_d)                        :: vcls2prp(nVclass)           ! storage of property value for each veg class
  integer(i4b)                       :: iVclass                     ! ID (= index) of vege class
  real(dp),        allocatable       :: hModelLocal(:,:)            ! Model layer thickness for soil polygon within one hru
  real(dp),        allocatable       :: zModelLocal(:,:)            ! Model layer depth for soil polygon within one hru
  type(namevar)                      :: parSxySz(nSoilBetaModel)    ! storage of model soil parameter for 2D field -soil layer x soil poy
  type(namevar)                      :: parSxyMz(nSoilBetaModel)    ! storage of model soil parameter for 2D field -model layer x soil poy
  type(namevar)                      :: parVxy(nVegBetaModel)       ! storage of model vege parameter for 1D or 2D field - vege poly (x month)
  integer(i4b),    allocatable       :: polySub(:)                  ! list of ID (=index) of soil polygons contributing model hru
  type(mapvar)                       :: mapdata(1)                  ! map data container for the geophysical polygons
  real(dp),        allocatable       :: wgtSub(:)                   ! adjusted Areal weight of soil polygon for all model hrus
  type(poly),      allocatable       :: soil2model_map(:)           ! data structure to hold weight and index of contributing soil layer per model layer and per soil polygon
  type(var_d)                        :: soilParVec                  ! data structure to hold soil parameter vector e.g., all layers for one polygon
  type(var_d)                        :: vegParVec                   ! data structure to hold veg parameter vector e.g., all months for one polygon

  ! initialize error control
  err=0; message='mpr/'

  !(0) Preparation
  allocate(gammaUpdateMeta, source=gammaMeta) ! copy master gamma parameter metadata
  allocate(betaUpdateMeta,  source=betaMeta)  ! copy master beta parameter metadata

  ! update gammaUpdateMeta%val with adjusted gammaPar value
  do iGamma=1,size(gammaParStr)
    gammaUpdateMeta(gammaParMeta(iGamma)%ixMaster)%val=gammaParStr(iGamma)%var(1)
  enddo

  ! update betaUpdateMeta%hpnorm and vpnorm with adjusted pnorm coefficient value
  do iParm=1,size(pnormCoefStr)
    associate(ix=>get_ixBeta(calScaleMeta(iParm)%betaname))
    nDims = size(betaUpdateMeta(ix)%parDim)
    do ixDims = 1, nDims
      betaUpdateMeta(ix)%parPnorm(ixDims) = pnormCoefStr(iParm)%var(ixDims)
    end do
    end associate
  enddo

  call popMprMeta( err, cmessage)   !for sdata_meta, vdata_meta, map_meta
  if(err/=0)then; message=trim(message)//trim(cmessage); return; endif

  ! *****
  ! (1) Get Geophysical data - THIS SHOULD BE OUTSIDE MPR SOUBROUTINE
  ! *********************************************
  ! (1.1) Get soil data
  call getSoilData(trim(mpr_input_dir)//trim(fname_soil),& ! input: soil data input name (netCDF)
                   sdata_meta,                           & ! input: soil data meta
                   dname_spoly,                          & ! input: spatial dimension (polygon ID)
                   dname_slyrs,                          & ! input: spatial dimension (polygon ID)
                   sdata,                                & ! input-output: soil data structure
                   nGpoly,                               & ! output: number of dimension (i.e. number of soil polygon)
                   nSlyrs,                               & ! output: number of dimension (i.e. number of soil layer)
                   err, cmessage)
  if(err/=0)then; message=trim(message)//trim(cmessage); return; endif

  call mod_hslyrs(sdata, gammaUpdateMeta(ixGamma%z1gamma1)%val, err,cmessage) ! modify soil layer thickness in sdata data structure
  if(err/=0)then; message=trim(message)//trim(cmessage); return; endif

  ! (1.2)  veg class - properties lookup table ...
  ! (1.2.1) Read in veg data netCDF...
   call getVegData(trim(mpr_input_dir)//trim(fname_veg), & ! input: veg data input name (netCDF)
                   vdata_meta,                           & ! input: veg data meta
                   dname_vpoly,                          & ! input: dimension name for veg polygon
                   vdata,                                & ! input-output: veg data structure
                   nDummy,                               & ! output: number of veg polygon
                   err, cmessage)                          ! output: error control
   if(err/=0)then; message=trim(message)//trim(cmessage); return; endif
   if(nGpoly/=nDummy)then; err=10;message=trim(message)//'nGpoly='//trim(int2str(nGpoly))//'NotEqualTOnVpoly='//trim(int2str(nDummy));return;endif

  ! (1.2.2) Read in veg class-property lookup table
   do iVclass=1,nVclass
     allocate(vcls2prp(iVclass)%var(nPrpVeg),stat=err);if(err/=0)then;message=trim(message)//'error allocating vcls2prp%var';return;endif
   enddo
   call getvegClassLookup(trim(mpr_input_dir)//trim(vclass_table), &
                          nVclass,                                 &
                          vegClass,                                &
                          vcls2prp,                                &
                          err, cmessage)
   if(err/=0)then; message=trim(message)//trim(cmessage); return; endif

  ! (1.3) Get topo data
  call getTopoData(trim(mpr_input_dir)//trim(fname_topo),& ! input: topographical data input name (netCDF)
                   tdata_meta,                           & ! input: topographical data meta
                   dname_tpoly,                          & ! input: spatial dimension (polygon ID)
                   tdata,                                & ! input-output: topographical data structure
                   nDummy,                               & ! output: number of dimension (i.e. number of geophysical data polygon)
                   err, cmessage)
  if(err/=0)then; message=trim(message)//trim(cmessage); return; endif
  if(nGpoly/=nDummy)then;err=10;message=trim(message)//'nGpoly='//trim(int2str(nGpoly))//'NotEqualTOnTpoly='//trim(int2str(nDummy));return;endif

  ! (1.3) Get climate data
  call getClimData(trim(mpr_input_dir)//trim(fname_clim),& ! input: climate data input name (netCDF)
                   cdata_meta,                           & ! input: climate data meta
                   dname_cpoly,                          & ! input: spatial dimension (polygon ID)
                   cdata,                                & ! input-output: climate data structure
                   nDummy,                               & ! output: number of dimension (i.e. number of geophysical data polygon)
                   err, cmessage)
  if(err/=0)then; message=trim(message)//trim(cmessage); return; endif
  if(nGpoly/=nDummy)then;err=10;message=trim(message)//'nGpoly='//trim(int2str(nGpoly))//'NotEqualTOnCpoly='//trim(int2str(nDummy));return;endif

  ! *****
  ! (2.) Read in mapping netcdf - THIS SHOULD BE OUTSIDE MPR SOUBROUTINE
  ! *********************************************
  call getMapData(trim(mpr_input_dir)//trim(fname_mapping), &    ! input: file name
                  map_meta,                                  &   ! input: map data meta
                  dname_hru,                                 &   ! input: dimension name for hru
                  dname_overPoly,                            &   ! input: dimension name for overlap polygon
                  mapdata,                                   &   ! input-output: map data structure
                  nGhru,                                     &   ! output: number of hru
                  err,cmessage)                                  ! output: error control
  if(err/=0)then; message=trim(message)//trim(cmessage); return; endif
  if ( nHru /= nGhru )then;err=10;message=trim(message)//'nHru= '//trim(int2str(nGhru))//' NOT '//trim(int2str(nHru));return;endif

  !!! ---------------------------------------------
  !!! Start of model hru loop (from mapping file) !!!
  !!! ---------------------------------------------
  associate( hruMap        => mapdata(1)%var(ixVarMapData%hru_id)%ivar1(1:nGhru),  &
             wgt           => mapdata(1)%var(ixVarMapData%weight)%dvar1,           &
             nOverPoly     => mapdata(1)%var(ixVarMapData%overlaps)%ivar1,         &
             overSpolyID   => mapdata(1)%var(ixVarMapData%intersector)%ivar1 )

  hru_loop: do iHru=1,nHru

    ! Get index (iLocal) of hru id that matches with current hru from hru id array in mapping file
    iLocal = findIndex(hruMap, hruID(iHru))

    ! Select list of soil polygons contributing a current hru
    ixEnd       = sum(nOverPoly(1:iLocal));
    ixStart     = ixEnd-nOverPoly(iLocal)+1
    nGpolyLocal = nOverPoly(iLocal)
    allocate(polySub(nGpolyLocal), wgtSub(nGpolyLocal), stat=err);if(err/=0)then;message=trim(message)//'error allocating polySub/wgtSub';return;endif
    polySub = overSpolyId(ixStart:ixEnd) ! id of soil polygons contributing to current hru
    wgtSub  = wgt(ixStart:ixEnd)         ! weight of soil polygons contributing to current hru

    ! Memory allocation
    allocate(hModelLocal(nLyr,nGpolyLocal),stat=err); if(err/=0)then;message=trim(message)//'error allocating hModelLocal'; return;endif
    allocate(zModelLocal(nLyr,nGpolyLocal),stat=err); if(err/=0)then;message=trim(message)//'error allocating zModelLocal'; return;endif
    allocate(soil2model_map(nGpolyLocal),stat=err);   if(err/=0)then;message=trim(message)//'error allocating soil2model_map';return;endif
    do iPoly=1,nGpolyLocal
      allocate(soil2model_map(iPoly)%layer(nLyr),stat=err); if(err/=0)then;message=trim(message)//'error allocating soil2model_map%layer';return;endif
      do iMLyr=1,nLyr
        allocate(soil2model_map(iPoly)%layer(iMLyr)%weight(nSub),stat=err);  if(err/=0)then;message=trim(message)//'error allocating lyrmap%layer%weight';return;endif
        allocate(soil2model_map(iPoly)%layer(iMLyr)%ixSubLyr(nSub),stat=err);if(err/=0)then;message=trim(message)//'error allocating lyrmap%layer%ixSubLyr';return;endif
      enddo
    enddo

    if (nSoilBetaModel>0) then
      do iParm=1,nSoilBetaModel
        idxBeta=get_ixBeta(trim(soilBetaCalName(iParm)))
        nDims = size(betaMeta(idxBeta)%parDim)
        allocate(dimSize(nDims))

        do ixDims = 1,nDims
          idxDim = betaMeta(idxBeta)%parDim(ixDims)
          dimSize(ixDims) = dimMeta(idxDim)%dimLength
        end do

        if (nDims == 1) then
          allocate(parSxySz(iParm)%dvar1(nGpolyLocal),stat=err); if(err/=0)then;message=trim(message)//'error allocating parSxySz%varData';return;endif
          allocate(parSxyMz(iParm)%dvar1(nGpolyLocal),stat=err); if(err/=0)then;message=trim(message)//'error allocating parSxyMz%varData';return;endif
        else if (nDims== 2) then
          allocate(parSxySz(iParm)%dvar2(nSlyrs,    nGpolyLocal),stat=err); if(err/=0)then;message=trim(message)//'error allocating parSxySz%varData';return;endif
          allocate(parSxyMz(iParm)%dvar2(dimSize(2),nGpolyLocal),stat=err); if(err/=0)then;message=trim(message)//'error allocating parSxyMz%varData';return;endif
        end if

        deallocate(dimSize)

      enddo
    endif

    if (nVegBetaModel>0)then
      do iParm=1,nVegBetaModel

        idxBeta=get_ixBeta(trim(vegBetaCalName(iParm)))

        nDims = size(betaMeta(idxBeta)%parDim)
        allocate(dimSize(nDims))

        do ixDims = 1,nDims
          idxDim = betaMeta(idxBeta)%parDim(ixDims)
          dimSize(ixDims) = dimMeta(idxDim)%dimLength
        end do

        if (nDims == 1) then
          allocate(parVxy(iParm)%dvar1(nGpolyLocal),stat=err)
        else if (nDims== 2) then
          allocate(parVxy(iParm)%dvar2(dimSize(2), nGpolyLocal),stat=err)
        end if

        deallocate(dimSize)

      enddo
    endif

  ! (3.1) Extract soil data for current model hru
    call subsetData(sdata, polySub, sdataLocal, 'soil' ,err, cmessage)
    if(err/=0)then; message=trim(message)//cmessage; return; endif

    if ( iHru == iHruPrint ) then
      print*,' '
      print*,'****************************************************'
      print*,'HRU, hruID = ',iHru,hruID(iHru)
      print*,'****************************************************'
      print*,'(1.1) Print list of geophysical gridbox ID and weight (first 100)'
      write(*,"(' polyID = ',100I9)")   (polySub(iPoly), iPoly=1,nGpolyLocal)
      write(*,"(' weight = ',100f10.5)") (wgtSub(iPoly), iPoly=1,nGpolyLocal)
      do iVar=1,nVarSoilData
        write(*,"(A12,'= ')") adjustl(sdata_meta(iVar)%varname)
        do iPoly=1,nGpolyLocal
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

  ! (3.2) Extract vege data for current model hru
    call subsetData(vdata, polySub, vdataLocal, 'veg' ,err, cmessage)
    if(err/=0)then; message=trim(message)//trim(cmessage); return; endif

  ! (3.3) Extract topo data for current model hru
    call subsetData(tdata, polySub, tdataLocal, 'topo' ,err, cmessage)
    if(err/=0)then; message=trim(message)//trim(cmessage); return; endif

  ! (3.3) Extract topo data for current model hru
    call subsetData(cdata, polySub, cdataLocal, 'clim' ,err, cmessage)
    if(err/=0)then; message=trim(message)//trim(cmessage); return; endif

  ! (3.4) Compute model parameters using transfer function
    ! compute model soil parameters
    if (nSoilBetaModel>0)then
      call comp_model_param(parSxySz, parVxy, sdataLocal, tdataLocal, vdataLocal, gammaUpdateMeta, nSlyrs, nGpolyLocal, err, cmessage)
      if(err/=0)then;message=trim(message)//trim(cmessage);return;endif

      if ( iHru == iHruPrint ) then
        print*,'(2) Print Model parameter at native resolution'
        if (nSoilBetaModel>0)then
          write(*,"(' Layer       =',20I9)") (iSLyr, iSlyr=1,nSlyrs)
          do iParm=1,nSoilBetaModel
            do iPoly = 1,nGpolyLocal
              write(*,"(1X,A12,'=',20f9.3)") soilBetaCalName(iParm), (parSxySz(iParm)%dvar2(iSLyr,iPoly), iSlyr=1,nSlyrs)
            enddo
          enddo
        endif
        if (nVegBetaModel>0)then
          do iParm=1,nVegBetaModel
            do iPoly = 1,nGpolyLocal
              write(*,"(1X,A10,'= ',100f9.3)") vegBetaCalName(iParm), (parVxy(iParm)%dvar2(iMon, iPoly), iMon=1,nMonth)
            enddo
          enddo
        endif
      endif
    end if

    ! (4.1.1) Compute Model layer depth
    call comp_model_depth(hModelLocal, zModelLocal, hfrac, sdataLocal, err, cmessage)
    if(err/=0)then;message=trim(message)//trim(cmessage);return;endif

    if ( iHru == iHruPrint ) then
      print*, '(3.1.1) Print model depth ---'
      write(*,"(' Layer       =',20I9)") (iMLyr, iMlyr=1,nlyr)
      do iPoly = 1,nGpolyLocal
        write(*,"('z            =',20f9.3)") (zModelLocal(iMLyr,iPoly), iMlyr=1,nLyr)
      enddo
    endif

  ! ***********
  ! (4) Spatial aggregation of Model parameter
  ! *********************************************************************
    if (nSoilBetaModel>0)then
      ! **********
      ! (4.1) Aggregate model parameveter vertical direction - soil data layers to model soil layers
      ! *********************************************************************
      ! (4.1.2) Computing vertical weight of soil layer for each model layer
      call map_slyr2mlyr(sdataLocal(ixVarSoilData%hslyrs)%dvar2, zModelLocal, soil2model_map, err, cmessage)

      if ( iHru == iHruPrint ) then
        print*, '(3.1.1) Print info on mapping from soil to model layer --'
        do iPoly = 1,nGpolyLocal
          do iMLyr = 1,nLyr
            print*, 'Poly, Layer=',iPoly,iMLyr
            write(*,"('weightSubLyr= ', 11f9.3)") (soil2model_map(iPoly)%layer(iMLyr)%weight(iSub), iSub=1,nSub)
            write(*,"('indexSubLyr = ', 11I7)")   (soil2model_map(iPoly)%layer(iMLyr)%ixSubLyr(iSub), iSub=1,nSub)
          enddo
        enddo
      endif

      ! (4.1.3) Computing vertical weight for each model layer
      do iParm = 1,nSoilBetaModel
        idxBeta=get_ixBeta(trim(soilBetaCalName(iParm)))
        nDims = size(betaMeta(idxBeta)%parDim)

        if (nDims==1) then
          parSxyMz(iParm)%dvar1 = parSxySz(iParm)%dvar1
          deallocate(parSxySz(iParm)%dvar1)
          cycle
        end if

        do iPoly=1,nGpolyLocal
          do iMLyr = 1,nLyr

            allocate( soilParVec%var(nSub),stat=err)
            if(err/=0)then;message=trim(message)//'error allocating soilParVec';return;endif

            do iSub=1,nSub
              third: associate(iSelect => soil2model_map(iPoly)%layer(iMLyr)%ixSubLyr(iSub) )
              if (iSelect > 0 ) then
                soilParVec%var(iSub) = parSxySz(iParm)%dvar2(iSelect,iPoly)
              endif
              end associate third
            enddo

            if ( trim(betaUpdateMeta(idxBeta)%parScale(2))/='na' )then
              call aggreg(parSxyMz(iParm)%dvar2(iMLyr,iPoly),           &
                          soil2model_map(iPoly)%layer(iMLyr)%weight(:), &
                          soilParVec%var(:),                            &
                          betaUpdateMeta(idxBeta)%parScale(2),          &
                          betaUpdateMeta(idxBeta)%parPnorm(2),          &
                          err, cmessage)
               if(err/=0)then;message=trim(message)//trim(cmessage);return;endif
            endif

            deallocate(soilParVec%var)

          enddo ! end of iMLyr (model layer) loop
        enddo ! end of iPoly
        deallocate(parSxySz(iParm)%dvar2)
      enddo
    endif

    ! ************
    ! (4.2) Aggregate model parameveter horizontally - use spatial weight netCDF
    ! *********************************************************************
    do iMLyr=1,nLyr
      call aggreg(hModel(iMLyr,iHru), wgtsub(:), hModelLocal(iMLyr,:),'pnorm', 1.0_dp, err, cmessage)
    end do

    if (nSoilBetaModel>0) then

      allocate( soilParVec%var(nGpolyLocal),stat=err)
      if(err/=0)then; message=trim(message)//'error allocating soilParVec'; return; endif

      do iParm = 1,nSoilBetaModel

        idxBeta=get_ixBeta(trim(soilBetaCalName(iParm)))
        nDims = size(betaMeta(idxBeta)%parDim)

        if (nDims==1) then
          do iPoly=1,nGpolyLocal
            soilParVec%var(iPoly) = parSxyMz(iParm)%dvar1(iPoly)
          enddo
          if ( trim(betaUpdateMeta(idxBeta)%parScale(1))/='na' )then
            call aggreg(parMxyMz(iParm)%dvar1(iHru),         &
                        wgtSub(:),                           &
                        soilParVec%var(:),                   &
                        betaUpdateMeta(idxBeta)%parScale(1), &
                        betaUpdateMeta(idxBeta)%parPnorm(1), &
                        err, cmessage)
            if(err/=0)then;message=trim(message)//trim(cmessage);return;endif
          endif
          deallocate(parSxyMz(iParm)%dvar1)
        else if (nDims==2) then
          do iMLyr=1,nLyr
            do iPoly=1,nGpolyLocal
              soilParVec%var(iPoly) = parSxyMz(iParm)%dvar2(iMLyr,iPoly)
            enddo

            if ( trim(betaUpdateMeta(idxBeta)%parScale(1))/='na' )then
              call aggreg(parMxyMz(iParm)%dvar2(iHru, iMLyr),  &
                          wgtSub(:),                           &
                          soilParVec%var(:),                   &
                          betaUpdateMeta(idxBeta)%parScale(1), &
                          betaUpdateMeta(idxBeta)%parPnorm(1), &
                          err, cmessage)
              if(err/=0)then;message=trim(message)//trim(cmessage);return;endif

              if ( iHru == iHruPrint ) then
                write(*,"(a)")'-----------------------------------'
                write(*,"(a)")'Aggregated soil parameter '
                write(*,"(1X,A17,'(layer ',I2,') = ',100f9.3)") betaUpdateMeta(idxBeta)%parName,iMLyr ,parMxyMz(iParm)%dvar2(iMLyr,iHru)
              endif

            endif

          enddo
          deallocate(parSxyMz(iParm)%dvar2)
        end if
      enddo
      deallocate(soilParVec%var)
    endif

    deallocate(soil2model_map, hModelLocal, zModelLocal)

    if (nVegBetaModel>0)then

      allocate( vegParVec%var(nGpolyLocal),stat=err)
      if(err/=0)then; message=trim(message)//'error allocating vegParVec%var'; return; endif

      do iParm = 1,nVegBetaModel

        idxBeta=get_ixBeta(trim(vegBetaCalName(iParm)))
        nDims = size(betaMeta(idxBeta)%parDim)

        if (nDims==1) then
          do iPoly=1,nGpolyLocal
            vegParVec%var(iPoly) = parVxy(iParm)%dvar1(iPoly)
          end do
          if ( trim(betaUpdateMeta(idxBeta)%parScale(1))/='na' )then
            call aggreg(vegParMxy(iParm)%dvar1(iHru),        &
                        wgtSub(:),                           &
                        vegParVec%var,                       &
                        betaUpdateMeta(idxBeta)%parScale(1), &
                        betaUpdateMeta(idxBeta)%parPnorm(1), &
                        err,cmessage)
            if(err/=0)then;message=trim(message)//trim(cmessage);return;endif
          end if
          deallocate(parVxy(iParm)%dvar1)
        else if (nDims==2) then
          do iMon=1,nMonth
            do iPoly=1,nGpolyLocal
              vegParVec%var(iPoly) = parVxy(iParm)%dvar2(iMon, iPoly)
            enddo

            if ( trim(betaUpdateMeta(idxBeta)%parScale(1))/='na' )then
              call aggreg(vegParMxy(iParm)%dvar2(iHru, iMon),  &
                          wgtSub(:),                           &
                          vegParVec%var,                       &
                          betaUpdateMeta(idxBeta)%parScale(1), &
                          betaUpdateMeta(idxBeta)%parPnorm(1), &
                          err,cmessage)
              if(err/=0)then;message=trim(message)//trim(cmessage);return;endif

              if ( iHru == iHruPrint ) then
                print*,'-----------------------------------'
                print*,'Aggregated vege parameter '
                write(*,"(1X,A17,'(Month ',I2,') = ',100f9.3)") betaUpdateMeta(idxBeta)%parName, iMon ,vegParMxy(iParm)%dvar2(iHru,iMon)
              endif

            endif

          end do
          deallocate(parVxy(iParm)%dvar2)
        end if
      enddo
      deallocate(vegParVec%var)
    endif

    deallocate(polySub, wgtSub)

  enddo hru_loop
  end associate

end subroutine

! private subroutine:
subroutine subsetData(entireData, subPolyID, localData, data_name, err, message)
  use var_lookup,           only:ixVarSoilData,nVarSoilData  ! index of soil data variables and number of variables
  use var_lookup,           only:ixVarVegData,nVarVegData    ! index of vege data variables and number of variables
  use var_lookup,           only:ixVarTopoData,nVarTopoData  ! index of topograpy data variables and number of variables
  use var_lookup,           only:ixVarClimData,nVarClimData  ! index of climate data variables and number of variables
  use globalData,           only:sdata_meta, vdata_meta, tdata_meta, cdata_meta
  implicit none
  !input variables
  type(namevar),        intent(in)    :: entireData(:) ! soil data container for all the soil polygons
  integer(i4b),         intent(in)    :: subPolyID(:)  ! subset of polygon id
  character(*),         intent(in)    :: data_name     ! data_name e.g., soil, vege
  !output variables
  type(namevar),        intent(inout) :: localData(:) ! soil data container for local soil polygon
  integer(i4b),         intent(out)   :: err           ! error code
  character(*),         intent(out)   :: message       ! error message
  !local variables
  type(var_meta), allocatable         :: data_meta(:)
  integer(i4b)                        :: polyIdx       ! index of polyid variable in dataset
  integer(i4b)                        :: nVarData      ! number of data
  integer(i4b)                        :: iPoly         ! index of soil polygon loop
  integer(i4b)                        :: iVar          ! index of named variable loop
  integer(i4b)                        :: iLocal        ! index of hru array in mapping file that match hru id of interest
  integer(i4b)                        :: nPoly         ! number of polygons in subset
  integer(i4b)                        :: nDim1         ! number of 1st dimension

  err=0; message='subsetData/'
  select case(trim(data_name))
    case('soil');nVarData=nVarSoilData; allocate(data_meta,source=sdata_meta); polyIdx=ixVarSoilData%polyid
    case('veg'); nVarData=nVarVegData;  allocate(data_meta,source=vdata_meta); polyIdx=ixVarVegData%polyid
    case('topo');nVarData=nVarTopoData; allocate(data_meta,source=tdata_meta); polyIdx=ixVarTopoData%polyid
    case('clim');nVarData=nVarClimData; allocate(data_meta,source=cdata_meta); polyIdx=ixVarClimData%polyid
  end select

  nPoly=size(subPolyID)

  do iVar=1,nVarData

    localData(ivar)%varName=trim(entireData(ivar)%varName)

    select case(trim(data_meta(iVar)%vartype))
      case('integer')
        select case(trim(data_meta(iVar)%vardims))
          case('2D')
            nDim1=size(entireData(ivar)%ivar2,1)
            if ( allocated(localData(ivar)%ivar2) ) deallocate(localData(ivar)%ivar2)
            allocate(localData(ivar)%ivar2(nDim1,nPoly),stat=err)
            if(err/=0)then; message=trim(message)//'problem allocating 2D int space for localData data structure'; return; endif
          case('1D')
            if ( allocated(localData(ivar)%ivar1) ) deallocate(localData(ivar)%ivar1)
            allocate(localData(ivar)%ivar1(nPoly),stat=err)
            if(err/=0)then; message=trim(message)//'problem allocating 1D int space for localData data structure'; return; endif
          end select
      case('double')
        select case(trim(data_meta(iVar)%vardims))
          case('2D')
            nDim1=size(entireData(ivar)%dvar2,1)
            if ( allocated(localData(ivar)%dvar2) ) deallocate(localData(ivar)%dvar2)
            allocate(localData(ivar)%dvar2(nDim1,nPoly),stat=err)
            if(err/=0)then; message=trim(message)//'problem allocating 2D real space for localData data structure'; return; endif
          case('1D')
            if ( allocated(localData(ivar)%dvar1) ) deallocate(localData(ivar)%dvar1)
            allocate(localData(ivar)%dvar1(nPoly),stat=err)
            if(err/=0)then; message=trim(message)//'problem allocating 1D real space for localData data structure'; return; endif
        end select
    end select

    do iPoly=1,nPoly
      iLocal=subPolyID(iPoly)
      select case(trim(data_meta(iVar)%vartype))
        case('integer')
          select case(trim(data_meta(iVar)%vardims))
            case('2D'); localData(ivar)%ivar2(:,iPoly) = entireData(ivar)%ivar2(:,iLocal)
            case('1D'); localData(ivar)%ivar1(iPoly)   = entireData(ivar)%ivar1(iLocal)
            end select
        case('double')
          select case(trim(data_meta(iVar)%vardims))
            case('2D'); localData(ivar)%dvar2(:,iPoly) = entireData(ivar)%dvar2(:,iLocal)
            case('1D'); localData(ivar)%dvar1(iPoly)   = entireData(ivar)%dvar1(iLocal)
          end select
      end select
    end do

  end do

end subroutine

end module mpr_routine
