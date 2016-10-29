.PHONY: all clean run force

GFORTRAN_DBG_OPTS := -std=f2008 -Wall -ggdb -fcheck=all -Wl,-rpath /usr/local/lib64
IFORT_DBG_OPTS := -assume realloc_lhs -check all -debug -O0 -ftrapuv -init=snan -init=arrays
GFORTRAN_OPTS := -std=f2008 -Ofast -Wl,-rpath /usr/local/lib64
IFORT_OPTS := -assume realloc_lhs -O3 -ipo -ip -fp-model fast=2 -parallel

all: decathlon decathlon_gf

decathlon: Decathlon.f90 Makefile
	ifort -o decathlon $(IFORT_OPTS) Decathlon.f90

decathlon_gf: Decathlon.f90 Makefile
	gfortran -o decathlon_gf $(GFORTRAN_OPTS) Decathlon.f90
	
    
clean:
	rm decathlon *.mod decathlon_gf

run: all
	./decathlon
	@cat Decathlon.out


force: clean all	