.PHONY: default build run utop install uninstall reinstall clean

default: run

# Launch utop with a suitable value for OCAMLPATH
utop:
	OCAMLPATH=$$(dirname `pwd`):$$OCAMLPATH utop

PACKAGES = unix moving-percentile

LIBSOURCES = \
  u_log.mli u_log.ml \
  u_perf.ml \
  u_set.mli u_set.ml \
  u_permanent_id.ml \
  u_controlid.ml \
  u_actionid.ml \
  u_control.ml \
  u_action.ml \
  u_recent.ml \
  u_recent_acts.ml \
  u_loop.ml \
  u_system.ml \
  u_test.ml \
  u_tests.ml \
  unitron.ml

build: META
	ocamlfind ocamlc -a -o unitron.cma -annot -package "$(PACKAGES)" \
		$(LIBSOURCES)
	ocamlfind ocamlopt -a -o unitron.cmxa -annot -package "$(PACKAGES)" \
		$(LIBSOURCES)
	ocamlfind ocamlopt -o demo -annot -linkpkg -package "$(PACKAGES)" \
		unitron.cmxa demo_main.ml

run: build
	./demo

META: META.in
	echo 'requires = "$(PACKAGES)"' > META
	cat META.in >> META

install: META
	ocamlfind install unitron META \
		`ls *.cm[ioxa] *.cmx[as] *.o *.a *.mli | grep -F -v '_main.'`

uninstall:
	ocamlfind remove unitron

reinstall:
	$(MAKE) uninstall; $(MAKE) install

clean:
	rm -f *~ *.cm[ioxa] *.cmx[as] *.o *.a *.annot demo META
