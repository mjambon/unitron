.PHONY: default build run install uninstall reinstall clean

default: run

PACKAGES = moving-percentile

LIBSOURCES = \
  u_log.ml \
  u_permanent_id.ml \
  u_test.ml \
  unitron.ml

build:
	ocamlfind ocamlc -a -o unitron.cma -annot -package $(PACKAGES) \
		$(LIBSOURCES)
	ocamlfind ocamlopt -a -o unitron.cmxa -annot -package $(PACKAGES) \
		$(LIBSOURCES)
	ocamlfind ocamlopt -o demo -annot -linkpkg -package $(PACKAGES) \
		unitron.cmxa demo_main.ml

run: build
	./demo

META: META.in
	cp META.in META

install: META
	ocamlfind install unitron META \
		`ls *.cm[ioxa] *.cmx[as] *.o *.a *.mli | grep -F -v '_main.'`

uninstall:
	ocamlfind remove unitron

reinstall:
	$(MAKE) uninstall; $(MAKE) install

clean:
	rm -f *~ *.cm[ioxa] *.cmx[as] *.o *.a *.annot demo
