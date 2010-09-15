SUBMISSION=bocsimacko.zip
DIST=bocsimacko-dist.zip
PROXYBIN=ProxyBot
# Don't change this, the server will try to run this binary.
BIN=MyBot

$BIN:   src/*.lisp
	sh bin/run-sbcl.sh --eval "(require :planet-wars)" \
	        --eval "(save-lisp-and-die \"$(BIN)\" :executable t :toplevel #'planet-wars:main)"

$(PROXYBIN): src/proxy-bot/*.lisp
	sh bin/run-sbcl.sh --eval "(require :proxy-bot)" \
	        --eval "(save-lisp-and-die \"$(PROXYBIN)\" :executable t :toplevel #'pw-proxy-bot:proxy)"

clean:
	rm -f src/*.fasl src/*~

distclean: clean
	rm -f "$(BIN)" "$(PROXYBIN)" "$(DIST)" "$(SUBMISSION)"

dist:
	git archive --format=zip --prefix "bocsimacko/" HEAD > $(DIST)

submission:
	git archive --format=zip --prefix "bocsimacko/" HEAD \
		src/*.lisp MyBot.lisp > $(SUBMISSION)
