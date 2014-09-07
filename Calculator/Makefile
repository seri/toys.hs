default : run

Calculator :
	[ -d build ] || mkdir build
	ghc -isrc -outputdir build src/Main.hs -o build/Calculator

run : Calculator
	build/Calculator

clean :
	rm -rf build