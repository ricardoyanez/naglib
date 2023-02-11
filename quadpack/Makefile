# Written by Ricardo Yanez <ricardo.yanez@calel.org>

# QUADPACK routines DQAGI and DQAWFE and dependencies

F77=gfortran
FFLAGS=-g -std=legacy -fno-automatic

SRC=dqagi.f dqagie.f dqelg.f dqk15i.f dqpsrt.f dqawfe.f dqawoe.f dqc25f.f \
    dqcheb.f dqk15w.f dqwgtf.f i1mach.f r1mach.f xerror.f xerrwv.f xerprt.f \
    xersav.f xerabt.f xerctl.f xgetua.f j4save.f s88fmt.f fdump.f dgtsl.f

OBJ=$(SRC:%.f=%.o)

quadpack:
ifeq (,$(wildcard dqagi.f))
	wget https://netlib.org/quadpack/dqagi.f
endif
ifeq (,$(wildcard dqagie.f))
	wget https://netlib.org/quadpack/dqagie.f
endif
ifeq (,$(wildcard dqelg.f))
	wget https://netlib.org/quadpack/dqelg.f
endif
ifeq (,$(wildcard dqk15i.f))
	wget https://netlib.org/quadpack/dqk15i.f
endif
ifeq (,$(wildcard dqpsrt.f))
	wget https://netlib.org/quadpack/dqpsrt.f
endif
ifeq (,$(wildcard dqawfe.f))
	wget https://netlib.org/quadpack/dqawfe.f
endif
ifeq (,$(wildcard dqawoe.f))
	wget https://netlib.org/quadpack/dqawoe.f
endif
ifeq (,$(wildcard dqc25f.f))
	wget https://netlib.org/quadpack/dqc25f.f
endif
ifeq (,$(wildcard dqcheb.f))
	wget https://netlib.org/quadpack/dqcheb.f
endif
ifeq (,$(wildcard dqk15w.f))
	wget https://netlib.org/quadpack/dqk15w.f
endif
ifeq (,$(wildcard dqwgtf.f))
	wget https://netlib.org/quadpack/dqwgtf.f
endif
ifeq (,$(wildcard i1mach.f))
	wget https://netlib.org/port/i1mach.f
endif
ifeq (,$(wildcard r1mach.f))
	wget https://netlib.org/port/r1mach.f
endif
ifeq (,$(wildcard xerror.f))
	wget https://netlib.org/alliant/quad/xerror.f
endif
ifeq (,$(wildcard xerrwv.f))
	wget https://netlib.org/alliant/quad/xerrwv.f
endif
ifeq (,$(wildcard xerprt.f))
	wget https://netlib.org/alliant/quad/xerprt.f
endif
ifeq (,$(wildcard xersav.f))
	wget https://netlib.org/alliant/quad/xersav.f
endif
ifeq (,$(wildcard xerabt.f))
	wget https://netlib.org/alliant/quad/xerabt.f
endif
ifeq (,$(wildcard xerctl.f))
	wget https://netlib.org/alliant/quad/xerctl.f
endif
ifeq (,$(wildcard xgetua.f))
	wget https://netlib.org/alliant/quad/xgetua.f
endif
ifeq (,$(wildcard j4save.f))
	wget https://netlib.org/alliant/quad/j4save.f
endif
ifeq (,$(wildcard s88fmt.f))
	wget https://netlib.org/alliant/quad/s88fmt.f
endif
ifeq (,$(wildcard fdump.f))
	wget https://netlib.org/alliant/quad/fdump.f
endif
ifeq (,$(wildcard dgtsl.f))
	wget https://netlib.org/linpack/dgtsl.f
endif
	$(F77) $(FFLAGS) -fPIC -c $(SRC)
	install -m 644 $(OBJ) ../

clean:
	-rm -f *.o *.so *~
