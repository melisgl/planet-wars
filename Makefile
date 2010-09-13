SUBMISSION=bocsimacko.zip
DIST=bocsimacko-dist.zip
BIN=my-bot

$BIN: src/*.lisp
	sh run-this.sh --eval \
        "(save-lisp-and-die \"$BIN\" :executable t :toplevel #'planet-wars:main)"

clean:
	rm -f src/*.fasl src/*~

distclean: clean
	rm -f "$BIN" "$DIST" "$SUBMISSION"

dist:
	git archive --format=zip --prefix "bocsimacko/" HEAD > $(DIST)

submission:
	git archive --format=zip --prefix "bocsimacko/" HEAD \
		src/*.lisp MyBot.lisp > $(SUBMISSION)
