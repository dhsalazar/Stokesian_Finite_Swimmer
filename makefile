main: HTYPE.o parameters.o subs.o main.o
	gfortran -o main HTYPE.o parameters.o subs.o main.o -lm -lc -lfftw3 -llapack  

HTYPE.mod: HTYPE.o HTYPE.f90
	gfortran -c HTYPE.f90

HTYPE.o: HTYPE.f90
	gfortran -c HTYPE.f90

parameters.mod: HTYPE.mod parameters.o parameters.f90
	gfortran -c parameters.f90

parameters.o: HTYPE.mod parameters.f90
	gfortran -c parameters.f90

subs.mod: HTYPE.mod parameters.mod  subs.o subs.f90
	gfortran -c subs.f90

subs.o: HTYPE.mod parameters.mod  subs.f90
	gfortran -c subs.f90

main.o: HTYPE.mod parameters.mod subs.mod main.f90
	gfortran -c main.f90

clean:
	rm  *.o *.mod
