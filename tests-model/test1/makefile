
# Macros
# the compiler
FC = gfortran

# flags for debugging or for maximum performance
FCFLAGS = -g -fbounds-check
FCFLAGS = -O2

# flags forall (e.g. look for system .mod files, required in gfortran)
FCFLAGS += -I/usr/include
FCFLAGS += -I$(LIBINCLUDES)

# libraries needed for linking
LDFLAGS += -L/usr/lib
LDFLAGS += -static -lnetcdf

LIBB = /home/faust/Desktop/KrillModel/objs
LIBINCLUDES = /home/faust/Desktop/KrillModel/objs

OBJECTS = class_krill.o listKrill_mod.o abstList_mod.o link_mod.o ran_mod.o utility_krill_mod.o

VPATH = ../../src : ../../objs

test_prog1: test_prog1.o $(OBJECTS)
	$(FC) $(FCFLAGS) -o $@ $^ $(LDFLAGS)

test_prog1.o: test_prog1.f90
	$(FC) $(FCFLAGS) -c $<

# General rules for building prog.o from prog.f90 or prog.F90; $< is
# used in order to list only the first prerequisite (the source file)
# and not the additional prerequisites such as module or include files
%: %.o
	$(FC) $(FCFLAGS) -o $@ $^ $(LDFLAGS)

%.o: %.f90
	$(FC) $(FCFLAGS) -c $<

# utility targets
.phony: clean veryclean test1

clean:
	rm -f *.o *.mod *.MOD

veryclean: clean
	rm -f *~ test_prog1

test1:
	make
	make clean
	./test_prog1
