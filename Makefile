all: multiplayer server wordle

wordle:
	csc main.scm -o wordle

multiplayer:
	csc multiplayer.scm -o wordle-multiplayer

server:
	csc server.scm -o wordle-server


