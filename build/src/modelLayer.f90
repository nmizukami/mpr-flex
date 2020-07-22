MODULE modelLayer

USE nrtype                                        ! variable types, etc.
USE data_type                                     ! Including custum data structure definition
USE public_var                                    ! Including common constant (physical constant, other e.g., missingVal, etc.)
USE var_lookup, ONLY:ixVarSoilData,nVarSoilData   ! index of soil data variables and number of variables
USE globalData, ONLY:sdata_meta

implicit none

private

public::extend_soil_layer
public::comp_model_depth
public::map_slyr2mlyr

contains

! ***********
! public subroutine to compute model layer thickness and bottom depth
! *********************************************************************
  subroutine extend_soil_layer(sdata, &          ! input : soil data
                               err, message)     ! output: error id and message
  implicit none
  ! inout
  type(namevar),intent(inout) :: sdata(:)               ! soil data container for local soil polygon
  ! output
  integer(i4b), intent(out)   :: err                    ! error code
  character(*), intent(out)   :: message                ! error message
  ! local
  integer(i4b)                :: ivar                   ! loop index of polygon
  real(dp)                    :: extra_thickness=10._dp ! extra_thickness underneath the bottom soil layer
  real(dp),     allocatable   :: dvar_temp(:,:)         ! temporal real array
  integer(i4b), allocatable   :: ivar_temp(:,:)         ! temporal integer array
  integer(i4b)                :: nSLyrs                 ! number of Soil layer
  integer(i4b)                :: nPolys                 ! number of soil polygon

  ! Initialize error control
  err=0; message=trim(message)//'extend_soil_layer/'

  nSlyrs=size(sdata(ixVarSoilData%hslyrs)%dvar2,1)
  nPolys=size(sdata(ixVarSoilData%hslyrs)%dvar2,2)

  do iVar=1,size(sdata)
    if (trim(sdata_meta(iVar)%vardims)=='2D') then
      if (trim(sdata_meta(iVar)%vartype) == 'integer') then
        allocate(ivar_temp(nSLyrs,nPolys))
        ivar_temp(1:nSLyrs,1:nPolys) = sdata(ivar)%ivar2(1:nSLyrs,1:nPolys)

        deallocate(sdata(ivar)%ivar2)
        allocate(sdata(ivar)%ivar2(nSlyrs+1,nPolys))

        sdata(ivar)%ivar2(1:nSlyrs, 1:nPolys) = ivar_temp(1:nSlyrs, 1:nPolys)
        sdata(ivar)%ivar2(nSlyrs+1, 1:nPolys) = ivar_temp(nSlyrs, 1:nPolys)

        deallocate(ivar_temp)

      elseif (trim(sdata_meta(iVar)%vartype) == 'double') then

        allocate(dvar_temp(nSLyrs,nPolys))
        dvar_temp(1:nSLyrs,1:nPolys) = sdata(ivar)%dvar2(1:nSLyrs,1:nPolys)

        deallocate(sdata(ivar)%dvar2)
        allocate(sdata(ivar)%dvar2(nSlyrs+1,nPolys))

        sdata(ivar)%dvar2(1:nSlyrs, 1:nPolys) = dvar_temp(1:nSlyrs, 1:nPolys)
        if (trim(sdata_meta(ivar)%varName) == 'hslyrs') then
          sdata(ivar)%dvar2(nSlyrs+1, 1:nPolys) = extra_thickness
        else
          sdata(ivar)%dvar2(nSlyrs+1, 1:nPolys) = dvar_temp(nSlyrs, 1:nPolys)
        end if
        deallocate(dvar_temp)
      end if
    end if
  end do

  end subroutine

! ***********
! public subroutine to compute model layer thickness and bottom depth
! *********************************************************************
  subroutine comp_model_depth(hModel,   &          ! output: final model layer thickness [m]
                              zModel,   &          ! output: final model layer bottom depth [m]
                              thickness,&          ! input : default thickness for each model layer [m]
                              coef,     &          ! input : multiplier to adjust default thickness
                              soilData, &          ! input : soil data
                              err, message)        ! output: error id and message

  implicit none
  ! input
  real(dp),     intent(in)    :: thickness(:)      ! default thickness for each model layer [m]
  real(dp),     intent(in)    :: coef(:,:)         ! multiplier to adjust default thickness
  type(namevar),intent(in)    :: soilData(:)       ! soil data container for local soil polygon
  ! output
  real(dp),     intent(out)   :: hModel(:,:)       ! final model layer thickness [m]
  real(dp),     intent(out)   :: zModel(:,:)       ! fnial depth of model layer bottom [m]
  integer(i4b), intent(out)   :: err               ! error code
  character(*), intent(out)   :: message           ! error message
  ! local
  integer(i4b)                :: iPoly             ! loop index of polygon
  integer(i4b)                :: iLyr              ! loop index of model layer
  integer(i4b)                :: nSLyr             ! number of Soil layer
  integer(i4b)                :: nPoly             ! number of soil polygon
  real(dp)                    :: depth_soil        ! total depth of soil layers
  logical(lgc),allocatable    :: mask(:,:)
  real(dp),allocatable        :: lyr_packed(:)
  integer(i4b)                :: nElm

  ! Initialize error control
  err=0; message=trim(message)//'comp_model_depth/'

  ! Get number of soil layer
  associate( hslyrs => soilData(ixVarSoilData%hslyrs)%dvar2 )
  nSLyr=size(hslyrs,1)
  nPoly=size(hslyrs,2)

  ! Make mask to exclude layers with missing soil texture data assuming
  allocate(mask(nSLyr,nPoly),stat=err)
  if(err/=0)then;message=trim(message)//'error allocating mask';return;endif

  mask = ( soilData(ixVarSoilData%sand_pct)%dvar2 >= 0 .and. &
           soilData(ixVarSoilData%clay_pct)%dvar2 >= 0 .and. &
           soilData(ixVarSoilData%silt_pct)%dvar2 >= 0)

  do iPoly=1,nPoly
    allocate(lyr_packed(count(mask(:,iPoly))),stat=err)
    if(err/=0)then;message=trim(message)//'error allocating lyr_packed';return;endif

    lyr_packed = pack(hslyrs(:,iPoly), mask(:,iPoly))
    nElm = size(lyr_packed)      ! number of valid soil layer

    if (nElm > 0) then           ! if actually soil layers exist
      depth_soil = sum(lyr_packed)  ! Total depth for soil layer
      zModel(:,iPoly) = 0._dp
      do iLyr=1,nLyr
        hModel(iLyr,iPoly)=thickness(iLyr)*coef(iLyr,iPoly)
        zModel(iLyr,iPoly)=zModel(iLyr,iPoly)+hModel(iLyr,iPoly)
      enddo

      if (zModel(nLyr,iPoly)>=depth_soil) then
        zModel(:,iPoly) = zModel(:,iPoly)*depth_soil/zModel(nLyr,iPoly)
        hModel(:,iPoly) = hModel(:,iPoly)*depth_soil/zModel(nLyr,iPoly)
      endif

    else ! if there are no real soil layers
      hModel(:,iPoly) = dmiss
      zModel(:,iPoly) = dmiss
    endif
    deallocate(lyr_packed)
  end do

  end associate

end subroutine

! *****************************************************************************
! Public subroutine: weight of soil layers within each mode layer
! ******************************************************************************
 subroutine map_slyr2mlyr( hSoil, zModel, lyrmap, err, message)
  ! Define variables
  implicit none
  ! input
  real(dp), intent(in)        :: hSoil(:,:)      ! thickness of soil layer [m]
  real(dp), intent(in)        :: zModel(:,:)     ! depth of model layer bottom [m]
  ! output
  type(poly),intent(inout)    :: lyrmap(:)       ! data type storing weight and intersecting soil layer index for each model layer
  integer(i4b),intent(out)    :: err             ! error code
  character(*),intent(out)    :: message         ! error message
  ! local
  real(dp),    allocatable    :: zSoil(:,:)      ! thickness of soil layer [m]
  integer(i4b),parameter      :: nSub=11         ! max. number of Soil layer within Model layer
  integer(i4b)                :: ctr             ! counter
  integer(i4b)                :: iPoly           ! loop index of polygon
  integer(i4b)                :: iSlyr           ! loop index of soil layer
  integer(i4b)                :: iMlyr           ! loop index of model layer
  integer(i4b)                :: nPoly           ! number of polygon
  integer(i4b)                :: nSLyr           ! number of Soil layer (to be computed based on input soil data)
  real(dp),allocatable        :: Zs_top(:)       ! depth to top of ith soil layer (i=1..nSlyr)
  real(dp),allocatable        :: Zs_bot(:)       ! depth to bottom of ith soil layer (i=1..nSlyr)
  real(dp),allocatable        :: Zm_top(:)       ! depth to top of ith model layer (i=1..nLyr)
  real(dp),allocatable        :: Zm_bot(:)       ! depth to bottom of ith model layer (i=1..nLyr)
  integer(i4b),allocatable    :: idxTop(:)       ! index of soil layer of which top is within ith model layer (i=1..nLyr)
  integer(i4b),allocatable    :: idxBot(:)       ! index of the lowest soil layer of which bottom is within ith model layer (i=1..nLyr)

  ! initialize error control
  err=0; message='map_slyr2mlyr/'

  nSLyr=size(hSoil,1)  !get soil layer number
  nPoly=size(hSoil,2)  !get polygon
  if (nPoly /= size(zModel,2))then;err=30;message=trim(message)//'number of polygon mismatch'; return; endif

  allocate(zSoil,source=hSoil)
  do iSLyr=2,nSlyr
    zSoil(iSlyr,:)=hSoil(iSlyr,:)+zSoil(iSlyr-1,:)
  enddo

  do iPoly=1,nPoly

    do iMLyr=1,nLyr
      lyrmap(iPoly)%layer(iMLyr)%weight = dmiss
      lyrmap(iPoly)%layer(iMLyr)%ixSubLyr = imiss
    enddo

    if ( zModel(1,iPoly) >= dmiss .and. zModel(1,iPoly) <= dmiss) then !if model layer depth has missing value
      do iMLyr=1,nLyr
        lyrmap(iPoly)%layer(iMLyr)%weight  = dmiss
        lyrmap(iPoly)%layer(iMLyr)%ixSubLyr= imiss
      enddo
    else
      allocate(Zs_bot(nSlyr),stat=err); if(err/=0)then; message=trim(message)//'error allocating Zs_bot'; return; endif
      allocate(Zs_top(nSlyr),stat=err); if(err/=0)then; message=trim(message)//'error allocating Zs_top'; return; endif
      allocate(Zm_top(nLyr),stat=err);  if(err/=0)then; message=trim(message)//'error allocating Zm_top'; return; endif
      allocate(Zm_bot(nLyr),stat=err);  if(err/=0)then; message=trim(message)//'error allocating Zm_bot'; return; endif
      allocate(idxTop(nLyr),stat=err);  if(err/=0)then; message=trim(message)//'error allocating idxTop'; return; endif
      allocate(idxBot(nLyr),stat=err);  if(err/=0)then; message=trim(message)//'error allocating idxBot'; return; endif
      !-- Compute for depths to 1)top and 2) bottom of soil and model layer
      Zm_top(1)=0.0_dp
      Zs_top(1)=0.0_dp
      do iMLyr=2,nLyr
        Zm_top(iMLyr) = zModel(iMLyr-1,iPoly)
      end do
      do iSLyr=2,nSlyr
        Zs_top(iSLyr) = zSoil(iSLyr-1,iPoly)
      end do
      Zm_bot = zModel(:,iPoly)
      Zs_bot = zSoil(:,iPoly)
      !-- Find index of upper-most soil layer which gets within model layer (for each model layer)
      ! condition: from top to bottom of soil layer, 1st soil layer whose bottom gets below top of i-th model layer
      do iMLyr=1,nLyr
        do iSLyr = 1,nSlyr
          if ( Zm_top(iMlyr)-Zs_bot(iSLyr)<0.0_dp ) then
            idxTop(iMlyr) = iSLyr; exit
          endif
        enddo
      enddo
      !-- Find index of lowest soil layer which get within model layer (for each layer)
      ! condition: from top to bottom of soil layer 1st soil layer whose top get deeper than bottom of i-th model layer
      do iMLyr=1,nLyr
        do iSLyr = 1,nSlyr
          if ( Zm_bot(iMlyr)-Zs_bot(iSLyr)<=valMin ) then
              idxBot(iMlyr) = iSLyr; exit
          endif
        enddo
      enddo

      ! Error check
      do iMLyr=1,nLyr
        if (idxTop(iMlyr)>11)then;             err=30;message=trim(message)//'index of idxTop not assinged'; return;endif
        if (idxBot(iMlyr)>11)then;             err=30;message=trim(message)//'index of idxBot not assinged'; return;endif
        if (idxTop(iMlyr)-idxBot(iMlyr)>0)then;err=30;message=trim(message)//'index of idxTop lower than idxBot';return;endif
      enddo

      !-- Compute weight of soil layer contributing to each model layer and populate lyrmap variable
      do iMLyr=1,nLyr
        ctr = 1
        ! loop frm the upper most soil layer to the lowest soil layer, but only soil layers that intersect current model layer
        do iSLyr=idxTop(iMlyr),idxBot(iMLyr)
          if ( idxTop(iMlyr) == idxBot(iMlyr) )then ! if model layer is completely within soil layer
              lyrmap(iPoly)%layer(iMLyr)%weight(ctr)   = 1.0
              lyrmap(iPoly)%layer(iMLyr)%ixSubLyr(ctr) = iSLyr
          else                                      ! if model layer contains multiple soil layers
            if ( iSLyr == idxTop(iMLyr) )then      ! for the upper most soil layer that intersect model layer
              lyrmap(iPoly)%layer(iMLyr)%weight(ctr)   = (Zs_bot(iSLyr)-Zm_top(iMlyr))/zModel(iMlyr,iPoly)
              lyrmap(iPoly)%layer(iMLyr)%ixSubLyr(ctr) = iSLyr
            elseif ( iSLyr == idxBot(iMLyr) ) then  ! for the lowest soil layer that intersect model layer
              lyrmap(iPoly)%layer(iMLyr)%weight(ctr)   = (Zm_bot(iMlyr)-Zs_top(iSLyr))/zModel(iMLyr,iPoly)
              lyrmap(iPoly)%layer(iMLyr)%ixSubLyr(ctr) = iSLyr
            else                                    ! for soil layers that completely in model layer
              lyrmap(iPoly)%layer(iMLyr)%weight(ctr)   = hSoil(iSLyr,iPoly)/zModel(iMlyr,iPoly)
              lyrmap(iPoly)%layer(iMLyr)%ixSubLyr(ctr) = iSLyr
            endif
          endif
          ctr = ctr+1
        enddo
      enddo

      deallocate(Zs_bot,stat=err); if(err/=0)then; message=trim(message)//'error deallocating Zs_bot'; return; endif
      deallocate(Zs_top,stat=err); if(err/=0)then; message=trim(message)//'error deallocating Zs_top'; return; endif
      deallocate(Zm_top,stat=err); if(err/=0)then; message=trim(message)//'error deallocating Zm_top'; return; endif
      deallocate(Zm_bot,stat=err); if(err/=0)then; message=trim(message)//'error deallocating Zm_bot'; return; endif
      deallocate(idxTop,stat=err); if(err/=0)then; message=trim(message)//'error deallocating idxTop'; return; endif
      deallocate(idxBot,stat=err); if(err/=0)then; message=trim(message)//'error deallocating idxBot'; return; endif

    endif

  enddo

 end subroutine

END MODULE modelLayer
