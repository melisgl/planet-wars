GIT_VERSION=`git describe --tags --dirty`
BIN="bocsimacko-${GIT_VERSION}"
PROXYBIN=ProxyBot
DIRNAME=`basename $$PWD`
STARTER_PACKAGE="common-lisp-starter-package-v0.8"

$BIN:   src/*.lisp
	sh bin/run-lisp.sh --load MyBot.lisp --eval "(cl-user::dump \"${BIN}\")"
	chmod +x "${BIN}"

$(PROXYBIN): src/proxy-bot/*.lisp
	sh bin/run-lisp.sh --load ProxyBot.lisp --eval "(cl-user::dump \"${PROXYBIN}\")"
	chmod +x "${PROXYBIN}"

clean:
	rm -f src/*.fasl src/*~

distclean: clean
	rm -f "$(BIN)" "$(PROXYBIN)" "$(STARTER_PACKAGE_ZIP)" *.dxl

archclean: distclean
	rm -rf config

starter-package:
	git archive --format=zip --prefix "$(STARTER_PACKAGE)/" HEAD > \
		"$(STARTER_PACKAGE).zip"

submission:
	./bin/make-submission.sh "$(DIRNAME)"
