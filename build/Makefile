#========================================================================
# Makefile to compile MPR-flex
#========================================================================
#
# To troubleshoot your paths and setup, type 'make check'
# Need to edit FC, FC_EXE, EXE, isOpenMP (if OMP is used), F_MASTER, NCDF_PATH
# Feel free to modify compiler flag (FLAGS)
# Do not leave space after your edits
#
#========================================================================
# User configure part
#========================================================================
# Define fortran compiler - gnu, intel or pgi
FC  =

# Define the compiler exe
# For MPI use, mpifort, mpif77, mpif90, mpif08
FC_EXE =

# Define the executable
EXE =

# Define optional setting
# fast:      Enables optimizations
# debug:     Minimum debug options, still
# profile:   Enables profiling
MODE = debug

# Define core directory.
# This is the parent directory of 'build' where all the source codes reside
# 'mpr-flex" under home directory is default.
F_MASTER =

# Define the NetCDF libraries and path to include files
ifeq "$(FC)" "gnu"
 NCDF_PATH =
endif
ifeq "$(FC)" "intel"
 NCDF_PATH =
endif
ifeq "$(FC)" "pgi"
 NCDF_PATH=
endif

LIBNETCDF = -Wl,-rpath,$(NCDF_PATH)/lib \
            -L$(NCDF_PATH)/lib -lnetcdff -lnetcdf
INCNETCDF = -I$(NCDF_PATH)/include

#========================================================================
# Define flags
#========================================================================
ifeq "$(FC)" "gnu"
  ifeq "$(MODE)" "fast"
    FLAGS = -O3 -fmax-errors=0 -ffree-line-length-none
  endif
  ifeq "$(MODE)" "debug"
    FLAGS = -g -Wall -fmax-errors=0 -fbacktrace -fcheck=all -ffpe-trap=zero -ffree-line-length-none
  endif
  ifeq "$(MODE)" "profile"
    FLAGS = -p -g -Wall -fmax-errors=0 -fbacktrace -fbounds-check -ffpe-trap=zero -ffree-line-length-none
  endif
endif

ifeq "$(FC)" "intel"
  ifeq "$(MODE)" "fast"
    FLAGS = -O3 -FR -auto -fltconsistency -fpe0
  endif
  ifeq "$(MODE)" "debug"
    FLAGS = -g -debug all -warn all -check all -FR -O0 -auto -WB -fpe0 -traceback -fltconsistency
  endif
  ifeq "$(MODE)" "profile"
    FLAGS = -p -g -debug -warn all -check all -noerror_limit -FR -O0 -auto -WB -fpe0 -traceback -fltconsistency
  endif
endif

ifeq "$(FC)" "pgi"
  ifeq "$(MODE)" "fast"
    FLAGS = -fast -O3 -Mdclchk
  endif
  ifeq "$(MODE)" "debug"
    FLAGS = -g -traceback -Mbounds -Mlist -Minfo -Mdclchk
  endif
endif

#========================================================================
# Define directory paths
#========================================================================
# Core directory that contains source code
F_KORE_DIR = $(F_MASTER)build/src/

# Location of the compiled modules
MOD_PATH = $(F_MASTER)build/

# Define the directory for the executables
EXE_PATH = $(F_MASTER)bin

#========================================================================
# Assemble all of the sub-routines
#========================================================================
# Define subroutines
DATATYPES = \
		nrtype.f90 \
    public_var.f90 \
		data_type.f90 \
		var_lookup.f90

UTILS = \
    ascii_util.f90 \
    stringmod.f90

IO = \
    globalData.f90 \
    popMeta.f90 \
    read_config.f90 \
    read_ncdata.f90 \
    read_soildata.f90 \
    read_topodata.f90 \
    read_vegdata.f90 \
    read_mapdata.f90 \
    write_param_nc.f90

CORE = \
    get_ixname.f90 \
    process_meta.f90 \
    tf.f90 \
    modelLayer.f90 \
    upscaling.f90 \
    mpr_routine.f90

# concatanate model subroutines
TEMP_MODSUB = $(DATATYPES) $(UTILS) $(IO) $(CORE)

# insert appropriate directory name
MODSUB = $(patsubst %, $(F_KORE_DIR)%, $(TEMP_MODSUB))

#========================================================================
# Define driver programs
#========================================================================
# Define the driver routine
DRIVER = $(F_KORE_DIR)main_calibration.f90

#========================================================================
# Compile the puppy
#========================================================================

# tasks
all: compile clean install

# compile
compile:
	$(FC_EXE) $(FLAGS) $(MODSUB) $(DRIVER) \
	$(LIBNETCDF) $(INCNETCDF) -o $(EXE)

# Remove object files
clean:
						rm -f *.o
						rm -f *.mod
#
# Copy the executable to the bin directory
install:
	@mkdir -p $(EXE_PATH)
	@mv $(EXE) $(EXE_PATH)

check:
  $(info)
  $(info Displaying make variables:)
  $(info   F_MASTER   : $(F_MASTER))
  $(info   EXE_PATH   : $(EXE_PATH))
  $(info   FC         : $(FC))
  $(info   FLAGS      : $(FLAGS))
  $(info   NCDF_PATH  : $(NCDF_PATH))
  $(info   LIBNETCDF  : $(LIBNETCDF))
  $(info   INCNETCDF  : $(INCNETCDF))
  $(info Add $(NCDF_PATH)/lib to your LD_LIBRARY_PATH)
  $(info)
