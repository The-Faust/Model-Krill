
# Macros
# the compiler
FC = gfortran

# flags for debugging or for maximum performance
FCFLAGS = -g -fbounds-check
FCFLAGS = -O2

# flags forall (e.g. look for system .mod files, required in gfortran)
FCFLAGS += -I/usr/include
FCFLAGS += -I$(LIBINCLUDES)

# libraries needed for linking, unused in the examples
LDFLAGS += -L$(LIBB)
LDFLAGS += -static -lKrillModel

LIBB = /home/faust/Desktop/KrillModel/objs
LIBINCLUDES = /home/faust/Desktop/KrillModel/objs

VPATH = ../../src : ../../objs

test_prog1: test_prog1.o
	$(FC) $(FCFLAGS) -o $@ $^ $(LDFLAGS)

test_prog1.o: test_prog1.f90
	$(FC) $(FCFLAGS) -c $<

.phony: clean veryclean test1

clean:
	rm -f *.o *.mod *.MOD

veryclean: clean
	rm -f *~ test_prog1

test1:
	make
	make clean
	./test_prog1
