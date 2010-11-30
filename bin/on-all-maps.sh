#!/bin/sh
PLAYER1="$1"
PLAYER2="$2"
DIR=${3:-games/`date "+%F-%H-%M-%S"`}
TIMEOUT=100
ONEWAY=1

echo "In $DIR"

mkdir -p "$DIR"
echo "Player1:" > "$DIR"/summary
echo "$PLAYER1" >> "$DIR"/summary
echo "Player2:" >> "$DIR"/summary
echo "$PLAYER2" >> "$DIR"/summary

statistics ()
{
    echo -n "Number of player 1 wins: "
    (grep "Player 1 Wins" "$DIR"/*-1-vs-2-play.log;
        if [ "$ONEWAY" != 1 ]; then grep "Player 2 Wins" "$DIR"/*-2-vs-1-play.log; fi) | wc -l
    echo -n "Number of player 2 wins: "
    (grep "Player 2 Wins" "$DIR"/*-1-vs-2-play.log;
        if [ "$ONEWAY" != 1 ]; then grep "Player 1 Wins" "$DIR"/*-2-vs-1-play.log; fi) | wc -l
    echo -n "Number of draws: "
    (grep "Draw" "$DIR"/*-1-vs-2-play.log;
        if [ "$ONEWAY" != 1 ]; then grep "Draw" "$DIR"/*-2-vs-1-play.log; fi) | wc -l
}

rm -f suspend-on-all

for map in maps/*.txt; do
    if [ -f suspend-on-all ]; then
        echo "Suspended"
        while [ -f suspend-on-all ]; do
            sleep 5
        done
    fi
    echo "On map $map"
    X="$DIR"/`basename "$map"`
    echo "1 vs 2"
    if [ ! -f "$X-1-vs-2-play.log" ]; then
        time ./planet_wars-cpp/playgame -t "$TIMEOUT" -ft "$TIMEOUT" \
            -m "$map" "$PLAYER1" "$PLAYER2" > \
            "$X-1-vs-2.log" 2>> "$X-1-vs-2-play.log"
    fi
    tail -n 2 "$X-1-vs-2-play.log"
    if [ "$ONEWAY" != 1 ]; then
        echo "2 vs 1"
        if [ ! -f "$X-2-vs-1-play.log" ]; then
            time ./planet_wars-cpp/playgame -t "$TIMEOUT" -ft "$TIMEOUT" \
                -m "$map" "$PLAYER2" "$PLAYER1" > \
                "$X-2-vs-1.log" 2>> "$X-2-vs-1-play.log"
        fi
        tail -n 2 "$X-2-vs-1-play.log"
    fi
    statistics
    echo
done

statistics >> "$DIR"/summary
