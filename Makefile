.SUFFIXES:
.SUFFIXES: .o .f90

# Compiler (ifort, gfortran)
FC = ifort

# Libraries 
COMPILER = $(shell $(FC) --version | head -n1 | cut -d' ' -f1)

# ifort and gfortran take different compiler flags
ifeq ($(COMPILER),ifort)
   # Intel
   COMMONFLAGS =
   PRODFLAGS = -O2  
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
OBJECTS = mod_global.o             \
	  mod_numerical_routines.o \
	  mod_file_io.o            \
	  main.o

.f.o:; $(FC) $(COMPFLAGS) -c -o $@ $< 
.f90.o:; $(FC) $(COMPFLAGS) -c -o $@ $< 

solver: $(OBJECTS)
	$(FC) -o $(EXEC_NAME) $(COMPFLAGS) $(OBJECTS) $(LIBRARIES)

debug: $(OBJECTS)
	$(FC) -o $(EXEC_NAME) $(COMPFLAGS) $(OBJECTS) $(LIBRARIES)

clean:
	rm -rf *.o *.mod $(EXEC_NAME)

# Object dependencies
mod_global.o: mod_global.f90
mod_file_io.o: mod_file_io.f90 mod_global.o
main.o: main.f90 mod_file_io.o mod_global.o
