default : run

SpellChecker :
	[ -d build ] || mkdir build
	ghc -isrc -outputdir build src/Main.hs -o build/SpellChecker

run : SpellChecker
	build/SpellChecker

clean :
	rm -rf build