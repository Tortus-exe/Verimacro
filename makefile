build: src/vmac.hs
	# ghc -o build/verimacro src/lex.hs
	cabal build
	cp ./dist-newstyle/build/x86_64-linux/ghc-9.6.2/vmac-0.1.0.0/x/vmac/build/vmac/vmac .
