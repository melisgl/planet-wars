echo -n "Number of player 1 wins: "
ls -1 *1-vs-2-play.log | sort | head -n "$1" | xargs -n 1 grep '1 Wins' | wc -l
echo -n "Number of player 2 wins: "
ls -1 *1-vs-2-play.log | sort | head -n "$1" | xargs -n 1 grep '2 Wins' | wc -l
echo -n "Number of draws: "
ls -1 *1-vs-2-play.log | sort | head -n "$1" | xargs -n 1 grep 'Draw' | wc -l
