all:
	ghc -O2 --make -isrc -o nerf -outputdir tmp -rtsopts -threaded Main
