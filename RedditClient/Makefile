default : run

RedditClient :
	[ -d build ] || mkdir build
	ghc -isrc -outputdir build src/Main.hs -o build/RedditClient

run : RedditClient
	build/RedditClient

clean :
	rm -rf build