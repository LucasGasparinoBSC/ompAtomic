FC = nvfortran
FCFLAGS = -fast -gpu=cc61,lineinfo,managed -mp=gpu -acc -cuda -Minfo=all

all: main.x clean

main.x: elemOps.o meshOps.o main.f90
	$(FC) $(FCFLAGS) -o main.x *.o main.f90

meshOps.o: meshOps.f90
	$(FC) $(FCFLAGS) -c meshOps.f90

elemOps.o: elemOps.f90
	$(FC) $(FCFLAGS) -c elemOps.f90

clean:
	rm -f *.o *.mod