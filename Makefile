SUBMISSION=bocsimacko.zip
DIST=bocsimacko-dist.zip
# Don't change this, the server will try to run this binary.
BIN=MyBot

$BIN: src/*.lisp
	sh run-this.sh --eval \
        "(save-lisp-and-die \"$(BIN)\" :executable t :toplevel #'planet-wars:main)"

clean:
	rm -f src/*.fasl src/*~

distclean: clean
	rm -f "$(BIN)" "$(DIST)" "$(SUBMISSION)"

dist:
	git archive --format=zip --prefix "bocsimacko/" HEAD > $(DIST)

submission:
	git archive --format=zip --prefix "bocsimacko/" HEAD \
		src/*.lisp MyBot.lisp > $(SUBMISSION)
