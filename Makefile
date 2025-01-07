#!/usr/bin/make
#----------------------------------------------------------------------------------------------------------------------------------
# shell
SHELL = /bin/bash
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
MOD_DIR  := build/mod
OBJ_DIR  := build/obj
BIN_DIR  := bin



# Compilation flags for Intel Fortran Compiler in windows
MKL_OPTIONS_INTEL := ${MKLROOT}/lib/libmkl_blas95_lp64.a \
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

MKL_OPTIONS_INTEL_MPI := ${MKLROOT}/lib/libmkl_blas95_lp64.a \
                         ${MKLROOT}/lib/libmkl_lapack95_lp64.a \
                         ${MKLROOT}/lib/libmkl_scalapack_lp64.a \
                         -Wl,--start-group \
                         ${MKLROOT}/lib/libmkl_intel_lp64.a \
                         ${MKLROOT}/lib/libmkl_intel_thread.a \
                         ${MKLROOT}/lib/libmkl_core.a \
                         ${MKLROOT}/lib/libmkl_blacs_intelmpi_lp64.a \
                         -Wl,--end-group \
                         -liomp5 \
                         -lpthread \
                         -lm \
                         -ldl

MKL_INC_DIRS_INTEL := -I${MKLROOT}/include/mkl/intel64/lp64 \
                      -I"${MKLROOT}/include"

# Include path and Libraries
INC_DIRS =
LIBS = 

OMP_INTEL := -fiopenmp
OPT_INTEL := -O3 -flto -xHost
CHK_INTEL := -check arg_temp_created -check format -check assume -check format -check output_conversion -check pointers -check stack -check uninit
DEB_IMTEL := -g -debug all -fpe-all=0 -fp-stack-check -fstack-protector-all -ftrapuv -no-ftz -gen-interfaces

# Compiler
ifeq "$(COMPILER)" "intel"
    FC              := ifx
    OPTIONS_COMPILE := -fpp -c -module $(MOD_DIR) -traceback -standard-semantics -qopt-report-file=docs/report.optrpt -qopt-report=3
    OPTIONS_LINK    := -fpp -module $(MOD_DIR) -traceback -standard-semantics -qopt-report-file=docs/report.optrpt -qopt-report=3
    WRN             := $(WRN_INTEL)
    CHK             := $(CHK_INTEL)
    DEB             := $(DEB_INTEL)
    OMP             := $(OMP_INTEL)
    OPT             := $(OPT_INTEL)
endif
ifeq "$(DEBUG)" "yes"
    OPTIONS_COMPILE := $(OPTIONS_COMPILE) -O0 -C -g $(WRN) $(CHK) $(DEB)
    OPTIONS_LINK    := $(OPTIONS_LINK) -O0 -C -g $(WRN) $(CHK) $(DEB)
endif
ifeq "$(OPTIMIZE)" "yes"
    OPTIONS_COMPILE := $(OPTIONS_COMPILE) $(OPT)
    OPTIONS_LINK    := $(OPTIONS_LINK) $(OPT)
endif
ifeq "$(OPENMP)" "yes"
    OPTIONS_COMPILE := $(OPTIONS_COMPILE) $(OMP)
    OPTIONS_LINK    := $(OPTIONS_LINK) $(OMP)
endif
ifeq "$(MKL)" "yes"
    OPTIONS_COMPILE  := $(OPTIONS_COMPILE) $(MKL_INC_DIRS_INTEL)
    ifeq "$(MPI)" "yes"
        OPTIONS_LINK := $(OPTIONS_LINK) $(MKL_OPTIONS_INTEL_MPI)
    else
        OPTIONS_LINK := $(OPTIONS_LINK) $(MKL_OPTIONS_INTEL)
    endif
endif
ifeq "$(MPI)" "yes"
    FC = mpiifx
    OPTIONS_COMPILE := $(OPTIONS_COMPILE) -D_MPI
    OPTIONS_LINK    := $(OPTIONS_LINK) -D_MPI
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
            $(SRC_DIR)/Calculate/Shape.f90 \
            $(SRC_DIR)/Calculate/Observation.f90 \
            $(SRC_DIR)/RootFinding/SecantMethod.f90 \
            $(SRC_DIR)/RootFinding/BinaryFinding.f90 \
            $(SRC_DIR)/Calculate/Update.f90 \
            $(SRC_DIR)/Calculate/Product.f90 \
            $(SRC_DIR)/Calculate/WRF.f90 \
            $(SRC_DIR)/Matrix/ConvertCRS.f90 \
            $(SRC_DIR)/Matrix/FindInd.f90 \
            $(SRC_DIR)/Matrix/Assemble.f90 \
            $(SRC_DIR)/Calculate/TRM.f90 \
            $(SRC_DIR)/Conditions/FixBoundaryCondition.f90 \
            $(SRC_DIR)/Inout/SetProjectPath.f90 \
            $(SRC_DIR)/Inout/Input.f90 \
            $(SRC_DIR)/Inout/Output.f90 \
            $(SRC_DIR)/Inout/Inout.f90 \
            $(SRC_DIR)/Inout/Stdout.f90 \
            $(SRC_DIR)/Main/Heat.f90 \
            $(SRC_DIR)/Solver/Initialize.f90 \
            $(SRC_DIR)/Solver/InitCopy.f90 \
            $(SRC_DIR)/Solver/Precon_jacobi.f90 \
            $(SRC_DIR)/Solver/Solve.f90 \
            $(SRC_DIR)/Conditions/FixInitialCondition.f90

OBJ_FILES = $(patsubst $(SRC_DIR)/%, $(OBJ_DIR)/%, $(SRC_FILES:.f90=.obj))
MAIN_FILE = $(MAIN_DIR)/ysy_fc.f90
TEST_FILE = $(MAIN_DIR)/test.f90

WHICHFC = $(shell which $(FC))

COMPILE_TEXT  = -e "\033[1;36m  Compile  \033[0m\033[1m $(patsubst $(SRC_DIR)/%,%,$<)\033[0m"
ASSEMBLE_TEXT = -e "\033[1;31m  Assemble \033[0m\033[1m $<\033[0m"
FINISHED_TEXT = -e "\033[1;34m  Compilation completed successfully! Executable: $(BIN_DIR)/$@\033[0m"
ERROR_TEXT    = -e "\033[1;31m  Compilation failed! \033[0m"

COMPIELE_OPTIONS_TEXT = "\
                        \\033[1;31m Compile options \\033[0m \n\
                        \\033[1m [$(OPTIONS_COMPILE)] \\033[0m"

LINK_OPTIONS_TEXT = "\
                    \\033[1;31m Link options \\033[0m \n\
                    \\033[1m [$(OPTIONS_LINK)] \\033[0m"

PRINTCHK =  "\
            \\033[1;31m Compiler \\033[0m\\033[1m $(FC) => $(WHICHFC) \\033[0m \n\
            \\033[1;31m Debug    \\033[0m\\033[1m $(DEBUG)            \\033[0m \n\
            \\033[1;31m Optimize \\033[0m\\033[1m $(OPTIMIZE)         \\033[0m \n\
            \\033[1;31m OpenMP   \\033[0m\\033[1m $(OPENMP)           \\033[0m \n\
            \\033[1;31m MPI      \\033[0m\\033[1m $(MPI)              \\033[0m "

# Default target
all: setup_directories ysy

# Compile and link the program
ysy: $(MAIN_FILE) $(OBJ_FILES)
	@echo $(ASSEMBLE_TEXT)
	@$(FC) -o $(BIN_DIR)/$@ $(OBJ_FILES) $(MAIN_FILE) $(OPTIONS_LINK) || (echo $(ERROR_TEXT) && exit 1)
	@echo $(FINISHED_TEXT)

# Compile and link the test program
test: $(TEST_FILE) $(OBJ_FILES)
	@echo $(ASSEMBLE_TEXT)
	@$(FC) -o $(BIN_DIR)/$@ $(OBJ_FILES) $(TEST_FILE) $(OPTIONS_LINK) || (echo $(ERROR_TEXT) && exit 1)
	@echo $(FINISHED_TEXT)

setup_directories:
	@mkdir -p $(MOD_DIR) $(OBJ_DIR) $(BIN_DIR)

$(OBJ_DIR)/%.obj: $(SRC_DIR)/%.f90
	@mkdir -p $(dir $@)
	@echo $(COMPILE_TEXT)
	@$(FC) $(OPTIONS_COMPILE) -c $< -o $@


.PHONY : info
info:
	@echo
	@echo -e $(PRINTCHK)
	@echo
	@echo -e $(COMPIELE_OPTIONS_TEXT)
	@echo
	@echo -e $(LINK_OPTIONS_TEXT)
	@echo

# Clean target to remove object and module files
.PHONY: clean
clean:
	@echo
	@rm -rf $(OBJ_DIR)/*
	@echo -e "\033[1;32m  Deleting objects \033[0m"
	@rm -rf $(MOD_DIR)/*
	@echo -e "\033[1;32m  Deleting mods    \033[0m"
	@find ./ -name "*.pdb" -delete
	@find ./ -name "*.optrpt" -delete
	@find ./ -name "*.yaml" -delete
	@rm -f $(BIN_DIR)/ysy_fc $(BIN_DIR)/tests
	@echo -e "\033[1;32m  Deleting binaries \033[0m"
	@echo

.PHONY: allclean
allclean:
	@echo
	@rm -rf build
	@echo -e "\033[1;32m  Deleting objects directory \033[0m"
	@echo -e "\033[1;32m  Deleting mods directory    \033[0m"
	@echo -e "\033[1;32m  Deleting build directory    \033[0m"
	@find ./ -name "*.pdb" -delete
	@find ./ -name "*.optrpt" -delete
	@find ./ -name "*.yaml" -delete
	@rm -f $(BIN_DIR)/ysy_fc $(BIN_DIR)/tests
	@echo -e "\033[1;32m  Deleting binaries directory \033[0m"
	@echo


.PHONY: run
run:
	@export OMP_NUM_THREADS=4 && ./$(BIN_DIR)/ysy

.PHONY: testrun1
testrun1:
	@export OMP_NUM_THREADS=1 && ./$(BIN_DIR)/test

.PHONY: testrun2
testrun2:
	@export OMP_NUM_THREADS=8 && ./$(BIN_DIR)/test

.PHONY: testrun3
testrun3:
	@export OMP_NUM_THREADS=16 && ./$(BIN_DIR)/test
