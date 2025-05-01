#!/usr/bin/make
#----------------------------------------------------------------------------------------------------------------------------------
# shell
SHELL = /bin/zsh
# no verbose
$(VERBOSE).SILENT:

COMPILER = intel
OPTIMIZE = yes
OPENMP   = yes
MKL      = yes
MPI      = no
DEBUG    = no

MAIN_DIR := app
SRC_DIR  := src
MOD_DIR  := MakeBuild/.mod
OBJ_DIR  := MakeBuild/.obj
BIN_DIR  := bin

# Intel MKL options
MKL_OPTIONS := ${MKLROOT}/lib/libmkl_blas95_lp64.a \
               ${MKLROOT}/lib/libmkl_lapack95_lp64.a \
               -Wl,--start-group \
               ${MKLROOT}/lib/libmkl_intel_lp64.a \
               ${MKLROOT}/lib/libmkl_intel_thread.a \
               ${MKLROOT}/lib/libmkl_core.a \
               -Wl,--end-group \
               -liomp5 \
               -lpthread \
               -lm \
               -ldl

MKL_INC_DIRS := -I${MKLROOT}/include/mkl/intel64/lp64 \
                -I${MKLROOT}/include

# Json-Fortran Include and Libraries
INC_DIRS_JSONFORTRAN := -I/workspaces/FTDSS/include/Json-Fortran/
LIBS_JSONFORTRAN := /workspaces/FTDSS/lib/libjsonfortran-static.a

# stdlib Include and Libraries
INC_DIRS_STD := -I/workspaces/FTDSS/EXTERNAL/fortran_stdlib/include/fortran_stdlib/IntelLLVM-2025.0.4/
LIBS_STD := /workspaces/FTDSS/EXTERNAL/fortran_stdlib/lib/libfortran_stdlib.a

# Compilation and Linking options
OPTIONS_COMPILE := -fpp -c -module $(MOD_DIR) -traceback -standard-semantics -qopt-report-file=docs/report.optrpt -qopt-report=3
OPTIONS_COMPILE += $(INC_DIRS_JSONFORTRAN) $(INC_DIRS_STD) $(MKL_INC_DIRS)

OPTIONS_LINK := $(LIBS_JSONFORTRAN) $(LIBS_STD) $(MKL_OPTIONS)
OPTIONS_LINK += -fpp -module $(MOD_DIR) -traceback -standard-semantics -qopt-report-file=docs/report.optrpt -qopt-report=3

OMP_INTEL := -fiopenmp
OPT_INTEL := -O3 -flto -xHost

CHK_INTEL := -check arg_temp_created -check format -check assume -check format -check output_conversion -check pointers -check stack -check uninit
DEB_INTEL := -g -debug all -fpe-all=0 -fp-stack-check -fstack-protector-all -ftrapuv -no-ftz -gen-interfaces

# Compiler
ifeq "$(COMPILER)" "intel"
    FC := ifx
    OPTIONS_COMPILE += $(OPT_INTEL) $(OMP_INTEL)
    OPTIONS_LINK += $(OPT_INTEL) $(OMP_INTEL)
    WRN := $(WRN_INTEL)
    CHK := $(CHK_INTEL)
    DEB := $(DEB_INTEL)
endif

ifeq "$(DEBUG)" "yes"
    OPTIONS_COMPILE := $(OPTIONS_COMPILE) -O0 -C -g $(WRN) $(CHK) $(DEB)
    OPTIONS_LINK    := $(OPTIONS_LINK) -O0 -C -g $(WRN) $(CHK) $(DEB)
endif

ifeq "$(MPI)" "yes"
    FC := mpiifx
    OPTIONS_COMPILE += -D_MPI
    OPTIONS_LINK    += -D_MPI
endif

# Source files and object files
SRC_FILES = $(SRC_DIR)/Type/Types.f90 \
            $(SRC_DIR)/Solver/error.f90 \
            $(SRC_DIR)/Allocate/Allocate.f90 \
            $(SRC_DIR)/Allocate/Allocate_Structure.f90 \
            $(SRC_DIR)/Calculate/LatentHeat.f90 \
            $(SRC_DIR)/Calculate/Points.f90 \
            $(SRC_DIR)/Calculate/Area.f90 \
            $(SRC_DIR)/Calculate/Count.f90 \
            $(SRC_DIR)/Calculate/BLAS.f90 \
            $(SRC_DIR)/Calculate/Unique.f90 \
            $(SRC_DIR)/Calculate/Shape.f90 \
            $(SRC_DIR)/Calculate/Observation.f90 \
            $(SRC_DIR)/RootFinding/SecantMethod.f90 \
            $(SRC_DIR)/RootFinding/BinaryFinding.f90 \
            $(SRC_DIR)/Calculate/Update.f90 \
            $(SRC_DIR)/Calculate/Product.f90 \
            $(SRC_DIR)/Calculate/WRF.f90 \
            $(SRC_DIR)/Calculate/HCF.f90 \
            $(SRC_DIR)/Calculate/GCC.f90 \
            $(SRC_DIR)/Matrix/ConvertCRS.f90 \
            $(SRC_DIR)/Matrix/FindInd.f90 \
            $(SRC_DIR)/Matrix/Assemble.f90 \
            $(SRC_DIR)/Calculate/TRM.f90 \
            $(SRC_DIR)/Conditions/FixBoundaryCondition.f90 \
            $(SRC_DIR)/Inout/SetProjectPath.f90 \
            $(SRC_DIR)/Inout/VTK.f90 \
            $(SRC_DIR)/Inout/Input.f90 \
            $(SRC_DIR)/Inout/Output.f90 \
            $(SRC_DIR)/Inout/Stdout.f90 \
            $(SRC_DIR)/Main/Solver.f90 \
            $(SRC_DIR)/Solver/Initialize.f90 \
            $(SRC_DIR)/Solver/InitCopy.f90 \
            $(SRC_DIR)/Solver/Precon_jacobi.f90 \
            $(SRC_DIR)/Solver/Solve.f90 \
            $(SRC_DIR)/Conditions/FixInitialCondition.f90

OBJ_FILES = $(patsubst $(SRC_DIR)/%, $(OBJ_DIR)/%, $(SRC_FILES:.f90=.obj))
MAIN_FILE = $(MAIN_DIR)/ysy_fc.f90
TEST_FILE = test/test.f90

# Compile the program
ysy: $(MAIN_FILE) $(OBJ_FILES)
	@echo "  Assemble"
	@$(FC) -o $(BIN_DIR)/MAKE_$@ $(OBJ_FILES) $(MAIN_FILE) $(OPTIONS_LINK) || (echo "  Compilation failed!" && exit 1)
	@echo "  Compilation completed successfully! Executable: $(BIN_DIR)/MAKE_$@"

# Compile and link the test program
test: $(TEST_FILE) $(OBJ_FILES)
	@echo "  Assemble"
	$(FC) -o $(BIN_DIR)/MAKE_$@ $(OBJ_FILES) $(TEST_FILE) $(OPTIONS_LINK)  || (echo "  Compilation failed!" && exit 1)
	@echo "  Compilation completed successfully!"

# Create directories if they do not exist
setup_directories:
	@mkdir -p $(MOD_DIR) $(OBJ_DIR) $(BIN_DIR)

# Compile source files into object files
$(OBJ_DIR)/%.obj: $(SRC_DIR)/%.f90
	@mkdir -p $(dir $@)
	@echo "  Compile  $<"
	$(FC) $(OPTIONS_COMPILE) -c $< -o $@ || (echo "  Compilation failed!" && exit 1)

.PHONY: info
info:
	@echo "Compiler: $(FC)"
	@echo "Options: $(OPTIONS_COMPILE)"
	@echo "Link options: $(OPTIONS_LINK)"

# Clean object and module files
.PHONY: clean
clean:
	@echo "  Deleting objects"
	@rm -rf $(OBJ_DIR)/*
	@echo "  Deleting mods"
	@rm -rf $(MOD_DIR)/*
	@find ./ -name "*.pdb" -delete
	@find ./ -name "*.optrpt" -delete
	@find ./ -name "*.yaml" -delete
	@rm -f $(BIN_DIR)/ysy_fc $(BIN_DIR)/tests
	@echo "  Deleting binaries"

.PHONY: allclean
allclean:
	@echo "  Deleting objects directory"
	@rm -rf MakeBuild
	@echo "  Deleting binaries"
	@rm -f $(BIN_DIR)/ysy_fc $(BIN_DIR)/tests
