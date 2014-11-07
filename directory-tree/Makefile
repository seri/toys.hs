longPath = "build/java/src/org/fizz/buzz/enterprise/abstract/factory"

default : run

DirectoryTree :
	[ -d build ] || (mkdir -p $(longPath) && cp -R src $(longPath))
	ghc -isrc -outputdir build src/Main.hs -o build/DirectoryTree

run : DirectoryTree
	build/DirectoryTree ".." --exclude ".git,SpellChecker" --collapse

clean :
	rm -rf build