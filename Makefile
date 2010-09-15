PROXYBIN=ProxyBot
# Don't change this, the server will try to run this binary.
BIN=MyBot
DIRNAME=`basename $$PWD`
STARTER_PACKAGE_ZIP="common-lisp-starter-package-v0.1.zip"

$BIN:   src/*.lisp .asdf-dirs
	sh bin/run-sbcl.sh --eval "(require :planet-wars)" \
	        --eval "(save-lisp-and-die \"$(BIN)\" :executable t :toplevel #'planet-wars:play)"

$(PROXYBIN): src/proxy-bot/*.lisp .asdf-dirs
	sh bin/run-sbcl.sh --eval "(require :proxy-bot)" \
	        --eval "(save-lisp-and-die \"$(PROXYBIN)\" :executable t :toplevel #'pw-proxy-bot:proxy)"

# To be compatible with ASDF v1 we set ASDF:*CENTRAL-REGISTRY* (see
# bin/run-sbcl.sh). Find the directories that hold asd files.
.asdf-dirs:
	find . -name '*.asd' | xargs -n1 dirname | sort | uniq | \
		sed -e 's/\(.*\)/\"\1\/\"/' > .asdf-dirs

clean:
	rm -f src/*.fasl src/*~ .asdf-dirs

distclean: clean
	rm -f "$(BIN)" "$(PROXYBIN)" "$(STARTER_PACKAGE_ZIP)"

starter-package:
	git archive --format=zip --prefix "common-lisp-starter-package/" HEAD > \
		"$(STARTER_PACKAGE_ZIP)"

submission:
	./bin/make-submission.sh "$(DIRNAME)"
