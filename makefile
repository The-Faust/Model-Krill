
# Macros
# the compiler
FC = gfortran

# flags for debugging or for maximum performance
FCFLAGS = -g -fbounds-check
FCFLAGS = -O2
# flags forall (e.g. look for system .mod files, required in gfortran)
FCFLAGS += -I/usr/include

# List of executables to be build within package
PROGRAMS = test_prog1
OBJECTS = class_krill.o listKrill_mod.o abstList_mod.o link_mod.o

# 'make' builds all
all: $(PROGRAMS)

# establishing dependencies and linking files
# growth_trunk.o: krill_mod.o

test_prog1: $(OBJECTS)

test_prog1.o: class_krill.o listKrill_mod.o

listKrill_mod.o: abstList_mod.o class_krill.o

abstList_mod.o: link_mod.o

# General rules for building prog.o from prog.f90 or prog.F90; $< is
# used in order to list only the first prerequisite (the source file)
# and not the additional prerequisites such as module or include files
%: %.o
	$(FC) $(FCFLAGS) -o $@ $^ $(LDFLAGS)

%.o: %.f90
	$(FC) $(FCFLAGS) -c $<

# utility targets
.PHONY: clean veryclean

clean:
	rm -f *.o *.mod *.MOD

veryclean: clean
	rm -f *~ $(PROGRAMS)


