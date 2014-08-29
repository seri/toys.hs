import SpellChecker (SpellChecker)
import CheckWithMap (checkWithMap)
import CheckWithSTTrie (checkWithSTTrie)
import CheckWithBTrie (checkWithBTrie)

import Data.Char (isAlpha, toUpper)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import System.IO (readFile)
import Control.DeepSeq (deepseq)
import Control.Monad (mapM_)


sourceFile = "wordsEn.txt"
targetFile = "fiction.txt"

solvers :: [ (String, SpellChecker) ]
solvers = [ ("Data.Map SpellChecker", checkWithMap)
          , ("Data.Trie SpellChecker", checkWithBTrie)
          , ("Our mutable trie SpellChecker", checkWithSTTrie) ]

convert :: String -> [String]
convert = map (map toUpper . filter isAlpha) . words


eachSolver :: [String] -> [String] -> (String, SpellChecker) -> IO ()
eachSolver source target (name, spellChecker) = do
    before <- getCurrentTime
    let result = spellChecker source target
    after <- result `deepseq` getCurrentTime
    let elapsed = diffUTCTime after before
    putStrLn . concat $ [ name, " found ", show (length result)
                        , " invalid words in ", show elapsed,  " seconds" ]

main :: IO ()
main = do
    sourceContent <- readFile sourceFile
    targetContent <- readFile targetFile
    let source = convert sourceContent
        target = convert targetContent
        putLen name xs = putStrLn . concat $ 
            [ "The ", name, " contains ", show (length xs), " words" ]
    putLen "source" source
    putLen "target" target
    mapM_ (eachSolver source target) solvers
