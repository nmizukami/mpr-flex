MODULE data_type

! Here define custum data type

  use nrtype
  use public_var

  implicit none

! ***********************************************************************************************************
! Define data structure of master parameter (both gamma and beta) metadata
! ***********************************************************************************************************
type,public  :: gammaPar_meta
  character(len=strLen)        :: pname  =''        ! parameter name
  real(dp)                     :: val    =-999.0_dp ! default bound
  character(len=strLen)        :: unt    =''        ! unit
  character(len=strLen)        :: beta   =''        ! name of parent beta parameter - if parameter is beta parameter, use "beta"
  integer(i4b)                 :: tftype =-999_i4b  ! id of transfer function type
  logical(lgc)                 :: flag   =.False.   ! flag to calibrate or not
endtype gammaPar_meta

! extended parameter meta data for selected set
type,extends(gammaPar_meta), public  :: cpar_meta
  integer(i4b)        :: ixMaster=-999   ! idex of master parameter list
endtype cpar_meta

! ***********************************************************************************************************
! Define data structure of beta parameters
! ***********************************************************************************************************
type, public :: beta_meta
  integer(i4b),allocatable     :: depend(:)         ! list of idex of dependent beta parameter
  integer(i4b)                 :: order=-999_i4b    ! computing order
endtype beta_meta

! ***********************************************************************************************************
! Define data structure of input parameter metadata
! ***********************************************************************************************************
type,public :: scale_meta
  character(len=strLen)            :: betaname=''        ! Beta name
  real(dp),            allocatable :: pdefault(:)        ! default P exponent values
  logical(lgc),        allocatable :: mask(:)            ! logical to tell scaling parameter is not necessary (e.g., majority)
endtype scale_meta

! ***********************************************************************************************************
! Define data structure of input parameter metadata
! ***********************************************************************************************************
type,public :: input_meta
  character(len=strLen)            :: betaname=''              ! Beta name
  integer(i4b)                     :: TF=1                     ! which Transfer function type?
endtype input_meta

! ***********************************************************************************************************
! Define data structure of variable metadata - soil propeties, topography, vege propeties, model hru propeties
! ***********************************************************************************************************
type,public :: var_meta
  character(len=strLen)            :: varname=''               ! name
  character(len=strLen)            :: vardesc=''               ! description
  character(len=strLen)            :: varunit=''               ! units
  character(len=strLen)            :: vardims=''               ! dimension (1D, 2D, 3D)
  character(len=strLen)            :: vartype=''               ! type (integer, double)
endtype var_meta

! ***********************************************************************************************************
! Define data structure of netCDF dimension metadata
! ***********************************************************************************************************
  type, public :: defDim
    character(len=32)         :: dname=''
    character(len=32)         :: desc=''
    character(len=32)         :: unt=''
    integer(i4b)              :: dimId     = imiss  ! dimension id
    integer(i4b)              :: dimLength = imiss  ! dim length
  endtype defDim

! *****
! data structure to hold integer type data (both vector and 2D)
! ********************************************
! poly(:)%layer(:)%weight(:)
!                 %ixSubLyr(:)
! 1st level - horizontal
! 2nd level - model layer
! 3rd level - weight and index of sublayer
type,public :: mapping
  real(dp),    allocatable          :: weight(:)
  integer(i4b),allocatable          :: ixSubLyr(:)
endtype mapping
type,public :: poly
  type(mapping),allocatable    :: layer(:)
endtype poly

! *****
! Other data type (need to clean up)
! ********************************************
! Define derived types to hold data values in vector (soil properties or model parameters) given their indices

! ** double precision type
 type, public :: var_d
  real(dp),allocatable               :: var(:)
 endtype var_d
 ! ** integer type
 type, public :: var_i
  integer(i4b),allocatable           :: var(:)
 endtype var_i

! data type containing a name and vector and 2D array variable for both integer and double precision
 type,public :: namevar
   character(len=strLen)       :: varName
   real(dp),    allocatable    :: dvar1(:)
   integer(i4b),allocatable    :: ivar1(:)
   real(dp),    allocatable    :: dvar2(:,:)
   integer(i4b),allocatable    :: ivar2(:,:)
 endtype namevar

 type mapvar
   type(namevar),allocatable         :: var(:)
 endtype mapvar

END MODULE data_type

MODULE obj_type

  use nrtype
  use public_var

  type,public  :: betaPar_meta
    character(len=strLen)               :: parName     = ''        ! parameter name
    character(len=strLen)               :: parDesc     = ''        ! description
    character(len=strLen)               :: parUnit     = ''        ! unit
    character(len=strLen)               :: parType     = ''        ! Parameter type, soil, veg, climate, routing
    integer(i4b)                        :: tftype      = -999_i4b  ! id of transfer function type
    character(len=strLen), allocatable  :: parScale(:)             ! scaling operator for each dimension
    real(dp),              allocatable  :: parPnorm(:)             ! pnorm parameter
    integer(i4b),          allocatable  :: parDim(:)               ! ID of dimension associated with variable
   CONTAINS
     procedure, pass :: init
  end type betaPar_meta

 CONTAINS

  SUBROUTINE init(this, pName, pDesc, pUnit, pType, ptftype, pScale, pPnorm, pDim)
    implicit none
    class(betaPar_meta)           :: this
    character(*), intent(in)      :: pName        ! parameter name
    character(*), intent(in)      :: pDesc        ! description
    character(*), intent(in)      :: pUnit        ! unit
    character(*), intent(in)      :: pType        !
    integer(i4b), intent(in)      :: ptftype      ! id of transfer function type
    character(*), intent(in)      :: pScale(:)    ! scaling operator for horizontal direction
    real(dp),     intent(in)      :: pPnorm(:)    ! pnorm parameter
    integer(i4b), intent(in)      :: pDim(:)      ! dimension names
    integer(i4b)                  :: n            ! size of dimension

    n = size(pDim)
    allocate(this%parScale(n))
    allocate(this%parPnorm(n))
    allocate(this%parDim(n))
    this%parName      = pName
    this%parDesc      = pDesc
    this%parUnit      = pUnit
    this%parType      = pType
    this%tftype       = ptftype
    this%parScale(1:n)= pScale(1:n)
    this%parPnorm(1:n)= pPnorm(1:n)
    this%parDim(1:n)  = pDim(1:n)
  END SUBROUTINE init

END MODULE obj_type


MODULE model_parameter

  use nrtype
  use public_var

  implicit none

  type, abstract :: elem
   CONTAINS
     procedure(init_elem), deferred :: init
  end type elem

  ABSTRACT INTERFACE

    SUBROUTINE init_elem(this, nsize)
      use nrtype
      import elem
      class(elem)               :: this
      integer(i4b), intent(in)  :: nsize(:)
    END SUBROUTINE init_elem

  END INTERFACE

  type, extends(elem) :: elem_int1d
     integer(i4b), allocatable :: array(:)
   CONTAINS
     procedure, pass :: init => init_int1d
  end type elem_int1d

  type, extends(elem) :: elem_real1d
     real(dp), allocatable :: array(:)
   CONTAINS
     procedure, pass :: init => init_real1d
  end type elem_real1d

  type, extends(elem) :: elem_int2d
     integer(i4b), allocatable :: array(:,:)
   CONTAINS
     procedure, pass :: init => init_int2d
  end type elem_int2d

  type, extends(elem) :: elem_real2d
     real(dp), allocatable :: array(:,:)
   CONTAINS
     procedure, pass :: init => init_real2d
  end type elem_real2d

  CONTAINS

    SUBROUTINE init_int1d(this,nsize)
      implicit none
      class(elem_int1d)        :: this
      integer(i4b),intent(in)  :: nsize(:)

      allocate(this%array(nsize(1)))
      this%array(:) = imiss

    END SUBROUTINE init_int1d

    SUBROUTINE init_real1d(this,nsize)
      implicit none
      class(elem_real1d)       :: this
      integer(i4b), intent(in) :: nsize(:)

      allocate(this%array(nsize(1)))
      this%array(:) = dmiss

    END SUBROUTINE init_real1d

    SUBROUTINE init_int2d(this,nsize)
      implicit none
      class(elem_int2d)         :: this
      integer(i4b), intent(in)  :: nsize(:)

      allocate(this%array(nsize(1),nsize(2)))
      this%array(:,:) = imiss

    END SUBROUTINE init_int2d

    SUBROUTINE init_real2d(this,nsize)
      implicit none
      class(elem_real2d)        :: this
      integer(i4b), intent(in)  :: nsize(:)

      allocate(this%array(nsize(1),nsize(2)))
      this%array(:,:) = dmiss

    END SUBROUTINE init_real2d

END MODULE model_parameter
