PROXYBIN=ProxyBot
DIRNAME=`basename $$PWD`
STARTER_PACKAGE="common-lisp-starter-package-v0.4"

$BIN:   src/*.lisp
	bin/compile-bot.sh

$(PROXYBIN): src/proxy-bot/*.lisp
	sh bin/run-sbcl.sh --load setup.lisp \
		--eval "(require :proxy-bot)" \
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
