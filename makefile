test: src/lex.hs
	cd src
	ghc src/lex.hs
	./src/lex "test/test1/t.vm"
