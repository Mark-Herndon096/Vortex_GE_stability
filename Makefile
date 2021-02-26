# [[file:PROJECT.org::*Makefile][Makefile:1]]
# Head Makefile for vortex_solver.exe
# INSTALL DIRECTORY
#install_dir = EXECUTABLES
# Comment on Makefile
# Commond source directories
SRC_DIR = src
GSL_DIR = GSL_INTERFACE

# Compiler (ifort, gfortran)
FC = ifort
CC = icc

# Libraries
COMPILER = $(shell $(FC) --version | head -n1 | cut -d' ' -f1)

# These flags needed for GNU GSL library -- path dependent on your system
INCLUDE   = -I/custom_builds/GSL/include
LDFLAGS   = -L/custom_builds/GSL/lib -lgsl -lgslcblas -lm
LIBRARIES = $(INCLDUE) $(LDFLAGS)


# ifort and gfortran take different compiler flags
ifeq ($(COMPILER),ifort)
   # Intel
   COMMONFLAGS =
   PRODFLAGS = -O3
endif

ifeq ($(COMPILER),GNU)
   # gfortran
   COMMONFLAGS = -ffree-line-length-0
   PRODFLAGS = -O2
endif

# Set flags for debug or release
ifeq ($(MAKECMDGOALS),debug)
   COMPFLAGS = ${COMMONFLAGS} ${DEBUGFLAGS}
else
   COMPFLAGS = ${COMMONFLAGS} ${PRODFLAGS}
endif

# Executable name
EXEC_NAME = vortex_solver.exe

# Object list
OBJECTS = $(GSL_DIR)/special_function_wrapper.o   \
	  $(GSL_DIR)/special_function_interface.o \
	  $(SRC_DIR)/mod_global.o                 \
	  $(SRC_DIR)/mod_numerical_routines.o     \
	  $(SRC_DIR)/mod_file_io.o                \
	  $(SRC_DIR)/main.o
solver:
	$(MAKE) -C $(GSL_DIR) gsl_objs
	$(MAKE) -C $(SRC_DIR) src_objs
	$(FC) -o $(EXEC_NAME) $(COMPFLAGS) $(OBJECTS) $(LIBRARIES)

clean:
	$(MAKE) -C $(GSL_DIR) clean
	$(MAKE) -C $(SRC_DIR) clean
	rm $(EXEC_NAME)
# Makefile:1 ends here
