module read_soildata

USE nrtype
USE netcdf
USE data_type
USE public_var                                     ! Including common constant (physical constant, other e.g., missingVal, etc.)
USE var_lookup,only:ixVarSoilData,nVarSoilData    ! index of soil polygon variables and number of variables

implicit none

private

public::check_polyID
public::getSoilData
public::mod_hslyrs

contains

 ! *********************************************************************
 ! Subroutine: check polygon ID (to Make sure that ID = index
 ! *********************************************************************
 subroutine check_polyID(fname, dname_poly, err, message)
  implicit none
  ! input variables
  character(*),  intent(in)    :: fname        ! filename
  character(*),  intent(in)    :: dname_poly   ! dimension name for polygon the same as variable name
  ! output variables
  integer(i4b),  intent(out)   :: err          ! error code
  character(*),  intent(out)   :: message      ! error message
  ! local variables
  integer(i4b),allocatable     :: polyID(:)    !
  integer(i4b)                 :: ncid         ! NetCDF file ID
  integer(i4b)                 :: idimID_poly  ! dimension ID for HRUs
  integer(i4b)                 :: iPoly        ! loop index
  integer(i4b)                 :: nPoly        ! loop index
  integer(i4b)                 :: iVarID       ! variable ID

  ! initialize error control
  err=0; message='check_polyID/'
  ! open file for reading
  err = nf90_open(fname, nf90_nowrite, ncid)
  ! get the ID of the poly dimension
  err = nf90_inq_dimid(ncid, dname_poly, idimID_poly)
  if(err/=0)then; message=trim(message)//trim(nf90_strerror(err))//'; name='//trim(dname_poly); return; endif
  ! get the length of the poly dimension
  err = nf90_inquire_dimension(ncid, idimID_poly, len=nPoly)
  if(err/=0)then; message=trim(message)//trim(nf90_strerror(err))//'; name='//trim(dname_poly); return; endif
  ! get the ID for polyID
  err = nf90_inq_varid(ncid, dname_poly, iVarID)
  if(err/=0)then; message=trim(message)//trim(nf90_strerror(err))//'; name='//trim(dname_poly); return; endif
  ! get the variable ID
  allocate(polyID(nPoly),stat=err)
  err = nf90_get_var(ncid, iVarID, polyID)
  if(err/=0)then; message=trim(message)//trim(nf90_strerror(err))//'; name='//trim(dname_poly); return; endif
  ! check polygon ID == index
  do iPoly=1,nPoly
    !if ( iPoly/=polyID(iPoly) )then;err=10; message=trim(message)//'Polygon_ID='//polyID(iPoly)//'Not_Identical_To_Index';return;endif
    if ( iPoly/=polyID(iPoly) )then; err=10; write(message,'(A,I8,A)') trim(message)//'Polygon_ID=',polyID(iPoly),'Not_Identical_To_Index' ;return;endif
  enddo
  print*, 'All Polygon ID Is Identical To Index'
  return
 end subroutine


 ! *********************************************************************
 ! Subroutine: get soil data for polygons and layers
 ! *********************************************************************
  subroutine getSoilData(fname,       &   ! input: file name
                         sdata_meta,  &   ! input: soil data meta
                         dname_spoly, &   ! input: dimension name for soil polygon
                         dname_slyrs, &   ! input: dimension name for soil layer
                         sdata,       &   ! input-output: soil data structure
                         nSpoly,      &   ! output: number of soil polygon
                         nSLyrs,      &   ! output: number of soil layer
                         ierr, message)   ! output: error control
  implicit none
  ! input variables
  character(*),  intent(in)       :: fname          ! filename
  type(var_meta),intent(in)       :: sdata_meta(:)  ! soil data meta
  character(*),  intent(in)       :: dname_spoly    ! dimension name for polygon
  character(*),  intent(in)       :: dname_slyrs    ! dimension name for layer
  ! input-output
  type(namevar), intent(inout)    :: sdata(:)       ! soil data container
  ! output variables
  integer(i4b),  intent(out)      :: nSpoly         ! number of soil polygons
  integer(i4b),  intent(out)      :: nSlyrs         ! number of soil layers
  integer(i4b),  intent(out)      :: ierr           ! error code
  character(*),  intent(out)      :: message        ! error message
  ! local variables
  integer(i4b)                    :: iVar           ! variable index
  integer(i4b)                    :: ncid           ! NetCDF file ID
  integer(i4b)                    :: idimID_poly    ! dimension ID for HRUs
  integer(i4b)                    :: idimID_slyr    ! dimension ID for stream segments
  integer(i4b)                    :: iVarID         ! variable ID
  ! initialize error control
  ierr=0; message='getSoilData/'

  ! open file for reading
  ierr = nf90_open(fname, nf90_nowrite, ncid)
  if(ierr/=0)then; message=trim(message)//trim(nf90_strerror(ierr)); return; endif
  ! get the ID of the poly dimension
  ierr = nf90_inq_dimid(ncid, dname_spoly, idimID_poly)
  if(ierr/=0)then; message=trim(message)//trim(nf90_strerror(ierr))//'; name='//trim(dname_spoly); return; endif
  ! get the length of the poly dimension
  ierr = nf90_inquire_dimension(ncid, idimID_poly, len=nSpoly)
  if(ierr/=0)then; message=trim(message)//trim(nf90_strerror(ierr))//'; name='//trim(dname_spoly); return; endif
  ! get the ID of the soil layer dimension
  ierr = nf90_inq_dimid(ncid, dname_slyrs, idimID_slyr)
  if(ierr/=0)then; message=trim(message)//trim(nf90_strerror(ierr))//'; name='//trim(dname_slyrs); return; endif
  ! get the length of the soil layer dimension
  ierr = nf90_inquire_dimension(ncid, idimID_slyr, len=nSlyrs)
  if(ierr/=0)then; message=trim(message)//trim(nf90_strerror(ierr))//'; name='//trim(dname_slyrs); return; endif
  ! ** read in soil poly variables
  do iVar=1,size(sdata)
    ! get the variable ID
    ierr = nf90_inq_varid(ncid, trim(sdata_meta(ivar)%varName), iVarID)
    if(ierr/=0)then; message=trim(message)//trim(nf90_strerror(ierr))//'; name='//trim(sdata_meta(ivar)%varName); return; endif
    !sdata(ivar)%varName=trim(sdata_meta(ivar)%varName)
    select case(sdata_meta(iVar)%vartype)
      case('integer')
       select case(sdata_meta(iVar)%vardims)
         case('2D')
           ! allocate space for the 2D integer array
           allocate(sdata(ivar)%ivar2(nSlyrs,nSpoly),stat=ierr)
           if(ierr/=0)then; message=trim(message)//'problem allocating 2D int space for sdata data structure'; return; endif
           ! get the data
           ierr = nf90_get_var(ncid, iVarID, sdata(ivar)%ivar2)
         case('1D')
           ! allocate space for the 1D integer array
           allocate(sdata(ivar)%ivar1(nSpoly),stat=ierr)
           if(ierr/=0)then; message=trim(message)//'problem allocating 1D int space for sdata data structure'; return; endif
           ! get the data
           ierr = nf90_get_var(ncid, iVarID, sdata(ivar)%ivar1)
         end select
     case('double')
       select case(sdata_meta(iVar)%vardims)
         case('2D')
           ! allocate space for the 2D integer array
           allocate(sdata(ivar)%dvar2(nSlyrs,nSpoly),stat=ierr)
           if(ierr/=0)then; message=trim(message)//'problem allocating 2D real space for sdata data structure'; return; endif
           ! get the data
           ierr = nf90_get_var(ncid, iVarID, sdata(ivar)%dvar2)
         case('1D')
           ! allocate space for the 1D integer array
           allocate(sdata(ivar)%dvar1(nSpoly),stat=ierr)
           if(ierr/=0)then; message=trim(message)//'problem allocating 1D real space for sdata data structure'; return; endif
           ! get the data
           ierr = nf90_get_var(ncid, iVarID, sdata(ivar)%dvar1)
       end select
   end select
   if(ierr/=0)then; message=trim(message)//trim(nf90_strerror(ierr)); return; endif

  end do  ! (looping through variables)

  ! close the NetCDF file
  ierr = nf90_close(ncid)
  if(ierr/=0)then; message=trim(message)//trim(nf90_strerror(ierr)); return; endif

  end subroutine

 ! *****
 ! Subroutine: soil thickness mod
 ! *********************************************
  subroutine mod_hslyrs(sdata,        &  ! input/output: data structure of soil data including soil layer thickness [m]
                        hmult,        &  ! input : scalar multiplier of soil thickness
                        ierr, message)   ! output: error control
   implicit none
   ! input
   real(dp),intent(in)             :: hmult
   ! Input/output
   type(namevar), intent(inout)    :: sdata(:)       ! soil data container
   ! local
   real(dp),allocatable            :: soil_h_mod(:)  ! temp holder of modified soil layer thickness
   integer(i4b)                    :: nSpoly         ! number of soil polygon
   integer(i4b)                    :: nSlyrs         ! number of soil layer
   integer(i4b)                    :: iSpoly         ! Loop index of soil polygon
   integer(i4b), intent(out)       :: ierr           ! error code
   character(*), intent(out)       :: message        ! error message

   ! initialize error control
   ierr=0; message='mod_hslyrs/'

   nSpoly=size(sdata(ixVarSoilData%hslyrs)%dvar2,2)
   nSlyrs=size(sdata(ixVarSoilData%hslyrs)%dvar2,1)
   allocate(soil_h_mod(nSlyrs),stat=ierr);
   if(ierr/=0)then; message=trim(message)//'problem with allocating soil_h_mod'; return; endif
   do iSpoly =1,nSpoly
     soil_h_mod = hmult*sdata(ixVarSoilData%hslyrs)%dvar2(:,iSpoly)*0.01  ! modified soil layer thickness and convert cm to m
     sdata(ixVarSoilData%hslyrs)%dvar2(:,iSpoly) = soil_h_mod             ! reassign modified layer thickness in data structure
   end do

  end subroutine

end module read_soildata
