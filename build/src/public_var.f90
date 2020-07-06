module public_var

  use nrtype

  implicit none

  save

  ! some common constant variables
  integer(i4b),parameter,public   :: nMonth=12              ! number of months
  integer(i4b),parameter,public   :: imiss=-999             ! missing value for integer value
  real(dp),    parameter,public   :: dmiss=-999.0_dp        ! missing value for floating value
  real(dp),    parameter,public   :: verySmall=tiny(1.0_dp) ! a very small number
  real(dp),    parameter,public   :: valMin=1.e-10_dp       ! minimum value for positive value

  ! Namelist variables
  ! &runconfig
  integer(i4b),         public    :: idModel
  character(len=strLen),public    :: mpr_param_file
  character(len=strLen),public    :: inParList
  ! mprconfig
  character(len=strLen),public    :: mpr_input_dir
  character(len=strLen),public    :: mpr_output_dir
  character(len=strLen),public    :: model_param_nc
  character(len=strLen),public    :: fname_soil
  character(len=strLen),public    :: fname_topo
  character(len=strLen),public    :: fname_veg
  character(len=strLen),public    :: fname_clim
  character(len=strLen),public    :: fname_mapping
  character(len=strLen),public    :: dname_overPoly
  character(len=strLen),public    :: dname_hru
  character(len=strLen),public    :: sclass_table
  integer(i4b)                    :: nSclass
  character(len=strLen),public    :: vclass_table
  integer(i4b)                    :: nVclass
  character(len=strLen),public    :: dname_spoly
  character(len=strLen),public    :: dname_slyrs
  character(len=strLen),public    :: dname_tpoly
  character(len=strLen),public    :: dname_vpoly
  character(len=strLen),public    :: dname_cpoly
  integer(i4b),         public    :: nHru

  integer(i4b),         public    :: nLyr
  real(dp),allocatable, public    :: hfrac(:)

end module public_var
