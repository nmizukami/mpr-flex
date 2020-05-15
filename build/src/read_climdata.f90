module read_climdata

USE netcdf
USE nrtype                                   ! Including numerical type definition
USE data_type                                ! Including custum data structure definition
USE public_var                               ! Including common constant (physical constant, other e.g., missingVal, etc.)

private

public::getClimData

contains

! *********************************************************************
! Public subroutine: get veg data for polygons (and months)
! *********************************************************************
 subroutine getClimData(fname,       &   ! input: file name
                       cdata_meta,  &   ! input: veg data meta
                       dname_cpoly, &   ! input: dimension name for veg polygon
                       cdata,       &   ! input-output: veg data structure
                       nCpoly,      &   ! output: number of veg polygon
                       ierr, message)   ! output: error control

   implicit none

   character(*), intent(in)           :: fname          ! filename
   type(var_meta),intent(in)          :: cdata_meta(:)  ! climate data meta
   character(*), intent(in)           :: dname_cpoly    ! dimension name for polygon
   ! input-output
   type(namevar), intent(inout)       :: cdata(:)       ! climate data container
   ! output variables
   integer(i4b), intent(out)          :: nCpoly         ! number of climate polygons
   integer(i4b), intent(out)          :: ierr           ! error code
   character(len=strLen), intent(out) :: message        ! error message
   ! local variables
   integer(i4b)                       :: iVar           ! variable index
   integer(i4b)                       :: ncid           ! NetCDF file ID
   integer(i4b)                       :: idimID_poly    ! dimension ID for HRUs
   integer(i4b)                       :: iVarID         ! variable ID
   integer(i4b)                       :: nMonth=12      ! number of month

   ! initialize error control
   ierr=0; message='getClimData/'

   ! open file for reading
   ierr = nf90_open(fname, nf90_nowrite, ncid)
   if(ierr/=0)then; message=trim(message)//trim(nf90_strerror(ierr)); return; endif

   ! get the ID of the poly dimension
   ierr = nf90_inq_dimid(ncid, dname_cpoly, idimID_poly)
   if(ierr/=0)then; message=trim(message)//trim(nf90_strerror(ierr))//'; name='//trim(dname_cpoly); return; endif

   ! get the length of the poly dimension
   ierr = nf90_inquire_dimension(ncid, idimID_poly, len=nCpoly)
   if(ierr/=0)then; message=trim(message)//trim(nf90_strerror(ierr))//'; name='//trim(dname_cpoly); return; endif

   ! ** read in veg poly variables
   do iVar=1,size(cdata)

     ! get the variable ID
     ierr = nf90_inq_varid(ncid, trim(cdata_meta(ivar)%varName), ivarID)
     if(ierr/=0)then; message=trim(message)//trim(nf90_strerror(ierr))//'; name='//trim(cdata_meta(ivar)%varName); return; endif

     select case(cdata_meta(iVar)%vartype)
     case('integer')
       select case(cdata_meta(iVar)%vardims)
       case('2D')
         ! allocate space for the 2D integer array
         allocate(cdata(ivar)%ivar2(nMonth,nCpoly),stat=ierr)
         if(ierr/=0)then; message=trim(message)//'err allocating 2D int space for cdata data structure'; return; endif
         ! get the data
         ierr = nf90_get_var(ncid, ivarID, cdata(ivar)%ivar2)
       case('1D')
         ! allocate space for the 1D integer array
         allocate(cdata(ivar)%ivar1(nCpoly),stat=ierr)
         if(ierr/=0)then; message=trim(message)//'err allocating 2D dbl space for cdata data structure'; return; endif
         ! get the data
         ierr = nf90_get_var(ncid, ivarID, cdata(ivar)%ivar1)
       end select
     case('double')
       select case(cdata_meta(iVar)%vardims)
       case('2D')
         ! allocate space for the 2D integer array
         allocate(cdata(ivar)%dvar2(nMonth,nCpoly),stat=ierr)
         if(ierr/=0)then; message=trim(message)//'err allocating 2D int space for cdata data structure'; return; endif
         ! get the data
         ierr = nf90_get_var(ncid, ivarID, cdata(ivar)%dvar2)
       case('1D')
         ! allocate space for the 1D integer array
         allocate(cdata(ivar)%dvar1(nCpoly),stat=ierr)
         if(ierr/=0)then; message=trim(message)//'err allocating 2D int space for cdata data structure'; return; endif
         ! get the data
         ierr = nf90_get_var(ncid, ivarID, cdata(ivar)%dvar1)
       end select
     end select
     if(ierr/=0)then; message=trim(message)//trim(nf90_strerror(ierr)); return; endif
   end do ! (looping through variables)

   ! close the NetCDF file
   ierr = nf90_close(ncid)
   if(ierr/=0)then; message=trim(message)//trim(nf90_strerror(ierr)); return; endif

 end subroutine getClimData

end module read_climdata
