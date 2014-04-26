ECL=/usr/local/ecl/ecl.exe

all: turtl-pkg

turtl-pkg:

	$(ECL) \
		-norc \
		-eval '(require :asdf)' \
		-eval '(ext:install-c-compiler)' \
		-eval '(setf asdf:*central-registry* (list "./" "./asdf/registry/"))' \
		-eval '(asdf:make-build :turtl-pkg :type :fasl :monolithic t :move-here "./")' \
		-eval '(quit)'

	mv turtl-pkg--all-systems.fasb turtl-pkg.fas
	strip turtl-pkg.fas

clean:

	rm -f turtl-pkg.fas





