.SUFFIXES:
.SUFFIXES: .o .f90 .c

# Compiler (ifort, gfortran)
FC = ifort
CC = icc

COMPFLAGS = -O2

# Libraries 
LIBRARIES = -I/custom_builds/GSL/include -L/custom_builds/GSL/lib -lgsl -lgslcblas -lm
# Executable name
EXEC_NAME = vortex_solver.exe

# Object list
OBJECTS = special_function_wrapper.o \
	  special_function_interface.o  \

.c.o:; $(CC) $(COMPFLAGS) -c -o $@ $< 
.f.o:; $(FC) $(COMPFLAGS) -c -o $@ $< 
.f90.o:; $(FC) $(COMPFLAGS) -c -o $@ $< 

gsl_objs: $(OBJECTS)

clean:
	rm -rf *.o *.mod $(EXEC_NAME)

# Object Dependencies
special_function_wrapper.o: special_function_wrapper.c
special_function_interface.o: special_function_interface.f90 special_function_wrapper.o

