# Wordle in scheme

`wordle-multiplayer address your-name opponent-name`

Made using chicken and SDL2. I made it multiplayer using Jabril's design: https://www.youtube.com/watch?v=kxCPmSB2OgA


This program gets assets (font & dictionary) from the root of the repo folder, or from `WORDLE_ETC`.

Dict from: https://gist.github.com/scholtes/94f3c0303ba6a7768b47583aff36654d (dict-la are possible answers, dict-ta are valid words that will never be the answer)

Fonts from: https://github.com/justrajdeep/fonts/blob/master/Times%20New%20Roman.ttf

- [X] client/server multiplayer
- [ ] clean stuff up
