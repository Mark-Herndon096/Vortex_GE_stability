
SRC_DIR = src
GSL_DIR = GSL_INTERFACE

# Compiler (ifort, gfortran)
FC = ifort
CC = icc

# Libraries 
COMPILER = $(shell $(FC) --version | head -n1 | cut -d' ' -f1)
INCLUDE   = -I/custom_builds/GSL/include
LDFLAGS   = -L/custom_builds/GSL/lib -lgsl -lgslcblas -lm
LIBRARIES = $(INCLDUE) $(LDFLAGS)
VPATH=GSL_INTERFACE:src
# ifort and gfortran take different compiler flags
ifeq ($(COMPILER),ifort)
   # Intel
   COMMONFLAGS =
   PRODFLAGS = -O3  
   DEBUGFLAGS = -O0 -C -g -debug all -warn all -check all -traceback -ftrapuv -stand f08
endif

ifeq ($(COMPILER),GNU)
   # gfortran
   COMMONFLAGS = -ffree-line-length-0
   PRODFLAGS = -O2
   DEBUGFLAGS = -O0 -fbacktrace -ffpe-trap=zero -Wall -fcheck=all -fbounds-check -fcheck-array-temporaries
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

# Object dependencies
special_function_wrapper.o: special_function_wrapper.c
special_function_interface.o: special_function_interface.f90 special_function_wrapper.o
mod_global.o: mod_global.f90
mod_numerical_routines.o: mod_numerical_routines.f90
mod_file_io.o: mod_file_io.f90 mod_global.o
main.o: main.f90 mod_file_io.o mod_global.o mod_numerical_routines.o
