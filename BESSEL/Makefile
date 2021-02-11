.SUFFIXES:
.SUFFIXES: .o .f90 .c

# Compiler (ifort, gfortran)
FC = ifort
CC = icc

# Libraries 
LIBRARIES = -I/custom_builds/GSL/include -L/custom_builds/GSL/lib -lgsl -lgslcblas -lm
# Executable name
EXEC_NAME = vortex_solver.exe

# Object list
OBJECTS = special_function_wrapper.o \
	  mod_special_function_interface.o           \
	  main.o 

.c.o:; $(CC) $(COMPFLAGS) -c -o $@ $< 
.f.o:; $(FC) $(COMPFLAGS) -c -o $@ $< 
.f90.o:; $(FC) $(COMPFLAGS) -c -o $@ $< 

solver: $(OBJECTS)
	$(FC) -o $(EXEC_NAME) $(COMPFLAGS) $(OBJECTS) $(LIBRARIES)

debug: $(OBJECTS)
	$(FC) -o $(EXEC_NAME) $(COMPFLAGS) $(OBJECTS) $(LIBRARIES)

clean:
	rm -rf *.o *.mod $(EXEC_NAME)

# Object dependencies
special_function_wrapper.o: special_function_wrapper.c
mod_special_function_interface.o: mod_special_function_interface.f90 special_function_wrapper.o
main.o: mod_special_function_interface.o special_function_wrapper.o
