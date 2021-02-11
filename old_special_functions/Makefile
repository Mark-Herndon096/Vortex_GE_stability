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
EXEC_NAME = function_test.exe

# Object list
OBJECTS = mod_function_interface.o  \
	  special_functions.o       \
	  main.o

.f.o:; $(FC) $(COMPFLAGS) -c -o $@ $< 
.f90.o:; $(FC) $(COMPFLAGS) -c -o $@ $< 

funcs: $(OBJECTS)
	$(FC) -o $(EXEC_NAME) $(COMPFLAGS) $(OBJECTS) $(LIBRARIES)

debug: $(OBJECTS)
	$(FC) -o $(EXEC_NAME) $(COMPFLAGS) $(OBJECTS) $(LIBRARIES)

clean:
	rm -rf *.o *.mod $(EXEC_NAME)

# Object dependencies
mod_function_interface.o: mod_function_interface.f90
special_functions.o: special_functions.f90 mod_function_interface.o
main.o: main.f90 special_functions.o mod_function_interface.o
