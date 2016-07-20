
# Macros
# the compiler
FC = gfortran

# GNU ar tool to build static library 
LIBB = ar rc

# flags for debugging or for maximum performance
FCFLAGS = -g -fbounds-check
FCFLAGS = -O2

# flags forall (e.g. look for system .mod files, required in gfortran)
FCFLAGS += -I/usr/include

# libraries needed for linking
LDFLAGS += -L/usr/lib
LDFLAGS += -lnetcdff -lnetcdf
# variables
OBJECTS = class_krill.o listKrill_mod.o abstList_mod.o link_mod.o ran_mod.o utility_krill_mod.o
UTILITY_OBJ = class_krill.o listKrill_mod.o
LIST_KRILL_OBJ = abstList_mod.o class_krill.o

vpath %.f90 ../src





# establishing dependencies and linking files
# growth_trunk.o: krill_mod.o
test_prog1: test_prog1.o $(OBJECTS)

test_prog1.o: $(OBJECTS)

utility_krill_mod.o: $(UTILITY_OBJ)

listKrill_mod.o: $(LIST_KRILL_OBJ)

abstList_mod.o: link_mod.o

# General rules for building prog.o from prog.f90 or prog.F90; $< is
# used in order to list only the first prerequisite (the source file)
# and not the additional prerequisites such as module or include files
%: %.o
	$(FC) $(FCFLAGS) -o $@ $^ $(LDFLAGS)

%.o: %.f90
	$(FC) $(FCFLAGS) -c $<



# utility targets
.PHONY: clean veryclean test1

clean:
	rm -f *.o

veryclean: clean
	rm -f *~ *.a *.mod *.MOD

test1: 
	rm -f *.o *.mod
	make
	./test_prog1
