PROXYBIN=ProxyBot
# Don't change this, the server will try to run this binary.
BIN=MyBot
DIRNAME=`basename $$PWD`
STARTER_PACKAGE_ZIP="common-lisp-starter-package-v0.1.zip"

$BIN:   src/*.lisp
	sh bin/run-sbcl.sh --load setup.lisp \
		--eval "(require :planet-wars)" \
	        --eval "(save-lisp-and-die \"$(BIN)\" :executable t :toplevel #'planet-wars:play)"

$(PROXYBIN): src/proxy-bot/*.lisp
	sh bin/run-sbcl.sh --load setup.lisp \
		--eval "(require :proxy-bot)" \
	        --eval "(save-lisp-and-die \"$(PROXYBIN)\" :executable t :toplevel #'pw-proxy-bot:proxy)"

clean:
	rm -f src/*.fasl src/*~

distclean: clean
	rm -f "$(BIN)" "$(PROXYBIN)" "$(STARTER_PACKAGE_ZIP)"

starter-package:
	git archive --format=zip --prefix "common-lisp-starter-package/" HEAD > \
		"$(STARTER_PACKAGE_ZIP)"

submission:
	./bin/make-submission.sh "$(DIRNAME)"
