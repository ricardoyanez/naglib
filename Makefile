F77=gfortran
FFLAGS=-g -std=legacy -fno-automatic

CC=gcc
CFLAGS=-g

OBJ=bessi0.o dpchim.o dqk15i.o r1mach.o xerprt.o zasyi.o zlog.o zunhj.o \
    bessi1.o dpchst.o dqk15w.o rksuite.o xerror.o zbesi.o zmlri.o zuni1.o \
    cnag.o dqagie.o dqpsrt.o s88fmt.o xerrwv.o zbinu.o zmlt.o zuni2.o \
    dchfdv.o dqagi.o dqwgtf.o xerabt.o xersav.o zbknu.o zrati.o zunik.o \
    dchfev.o dqawfe.o fdump.o xercnt.o xersve.o zbrent.o zs1s2.o zuoik.o \
    dgamln.o dqawoe.o fnag.o xerctl.o xgetua.o zbuni.o zseri.o zwrsk.o \
    dgtsl.o dqc25f.o fourpt.o xerhlt.o zabs.o zdiv.o zshch.o \
    dpchfd.o dqcheb.o i1mach.o xermsg.o zacai.o zexp.o zsqrt.o \
    dpchfe.o dqelg.o j4save.o xerprn.o zairy.o zkscl.o zuchk.o

install: libnag.so
	install libnag.so /usr/local/lib
	ldconfig
	$(MAKE) clean
	@echo
	@echo libnag.o installed in /usr/local/lib
	@echo

libnag.so: fnag.o cnag.o fourpt.o rksuite.o nrf77.o quadpack.o amos.o pchip.o
	$(F77) -shared -o libnag.so $(OBJ)

fnag.o:
	$(F77) $(FFLAGS) -fPIC -c fnag.f

cnag.o:
	$(CC) $(CFLAGS) -fPIC -c cnag.c

fourpt.o:
	$(F77) $(FFLAGS) -fPIC -c fourpt.f

rksuite.o:
	$(MAKE) -C rksuite

nrf77.o:
	$(MAKE) -C nrf77

quadpack.o:
	$(MAKE) -C quadpack

amos.o:
	$(MAKE) -C amos

pchip.o:
	$(MAKE) -C pchip

clean:
	rm -f *.o *.so fort.* *~
	$(MAKE) -C nrf77 clean
	$(MAKE) -C rksuite clean
	$(MAKE) -C quadpack clean
	$(MAKE) -C amos clean
	$(MAKE) -C pchip clean
