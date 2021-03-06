.PHONY: default build test utop install uninstall reinstall clean

default: build

# Launch utop with a suitable value for OCAMLPATH
utop:
	OCAMLPATH=$$(dirname `pwd`):$$OCAMLPATH utop

PACKAGES = unix moving-percentile

LIBSOURCES = \
  u_time.ml \
  u_log.mli u_log.ml \
  u_test.ml \
  u_perf.ml \
  u_set.mli u_set.ml \
  u_float.ml \
  u_normal.ml \
  u_stat.ml \
  u_random.ml \
  u_lazy.ml \
\
  u_permanent_id.ml \
  u_controlid.ml \
  u_actionid.ml \
  u_control.ml \
  u_action.ml \
  u_recent.ml \
  u_recent_acts.ml \
  u_loop.ml \
  u_info.ml \
  u_obs.mli u_obs.ml \
  u_system.ml \
  u_learn.ml \
  u_cycle.mli u_cycle.ml \
\
  u_exp.ml \
  u_eval.ml \
\
  u_tests.ml

build: META
	ocamlfind ocamlc -a -o unitron.cma -bin-annot -package "$(PACKAGES)" \
		$(LIBSOURCES)
	ocamlfind ocamlopt -a -o unitron.cmxa -bin-annot -package "$(PACKAGES)" \
		$(LIBSOURCES)
	ocamlfind ocamlopt -o u_test -bin-annot -linkpkg -package "$(PACKAGES)" \
		unitron.cmxa u_test_main.ml

test: build
	time -p \
    ./u_test 2>&1 | tee test.log | stdbuf -o 0 grep '^>' | tee summary.log

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
	rm -f *~ *.cm[ioxat] *.cmti *.cmx[as] *.o *.a *.annot META \
    u_test test.log summary.log
