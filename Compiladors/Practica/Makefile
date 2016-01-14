all:
	antlr -gt fonts.g
	dlg -ci parser.dlg scan.c
	g++ -o fonts fonts.c scan.c err.c
