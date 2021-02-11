.SUFFIXES:
.SUFFIXES: .o .f90 .c

# Compiler (ifort, gfortran)
FC = ifort
CC = icc

# Libraries 
COMPILER = $(shell $(FC) --version | head -n1 | cut -d' ' -f1)

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


GSL_INC = -I/home/markherndon/Vortex_Codes/FORTRAN/GSL_INTERFACE/
# Object list
OBJECTS = mod_global.o                 \
	  mod_numerical_routines.o     \
	  mod_file_io.o                \
	  main.o

.c.o:; $(CC) $(CFLAGS) -c -o $@ $< 
.f.o:; $(FC) $(COMPFLAGS) -c -o $@ $< 
.f90.o:; $(FC) $(COMPFLAGS) -c -o $@ $< $(GSL_INC) 

src_objs: $(OBJECTS)

clean:
	rm -rf *.o *.mod $(EXEC_NAME)

## Object dependencies
mod_global.o: mod_global.f90
mod_numerical_routines.o: mod_numerical_routines.f90
mod_file_io.o: mod_file_io.f90 mod_global.o
main.o: main.f90 mod_file_io.o mod_global.o mod_numerical_routines.o
