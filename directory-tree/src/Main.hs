import Config
import Directories

import qualified Options.Applicative as O
import Data.Monoid ((<>))
import Control.Applicative ((<*>), (<$>))

config :: O.Parser RawConfig
config = RawConfig 
    <$> O.optional (O.argument O.str (O.metavar "ROOT" <>
                    O.help "Root path (default to current dir)"))
    <*> O.optional (O.strOption (O.long "exclude" <>
                    O.metavar "NAMES" <> 
                    O.help "Comma separated list of names to ignore"))
    <*> O.switch (O.long "collapse" <>
                  O.help "Whether to collapse long empty paths (like Github)")

description :: O.InfoMod RawConfig
description = O.progDesc "Display a tree view of a part of the file system"

dispatch :: RawConfig -> IO ()
dispatch rawConfig = convertConfig rawConfig >>= showTree >>= putStr

main :: IO ()
main = O.execParser (O.info (O.helper <*> config) description) >>= dispatch