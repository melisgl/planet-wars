PROXYBIN=ProxyBot
DIRNAME=`basename $$PWD`
STARTER_PACKAGE="common-lisp-starter-package-v0.8"

$BIN:   src/*.lisp
	sh bin/run-sbcl.sh --load MyBot.lisp \
		--eval "(save-lisp-and-die \"MyBot\" :executable t :toplevel #'pwbot::main)"

$(PROXYBIN): src/proxy-bot/*.lisp
	sh bin/run-sbcl.sh --load ProxyBot.lisp \
	        --eval "(save-lisp-and-die \"$(PROXYBIN)\" :executable t :toplevel #'pw-proxy-bot:proxy)"

clean:
	rm -f src/*.fasl src/*~

distclean: clean
	rm -f "$(BIN)" "$(PROXYBIN)" "$(STARTER_PACKAGE_ZIP)"

starter-package:
	git archive --format=zip --prefix "$(STARTER_PACKAGE)/" HEAD > \
		"$(STARTER_PACKAGE).zip"

submission:
	./bin/make-submission.sh "$(DIRNAME)"
