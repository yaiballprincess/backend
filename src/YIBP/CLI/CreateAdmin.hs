{-# LANGUAGE OverloadedStrings #-}

module YIBP.CLI.CreateAdmin (createAdmin) where

import System.IO
import System.Random

import YIBP.Config
import YIBP.Db

import YIBP.Db.User (insertUser)
import YIBP.Db.User.Types (InsertUser (..))
import YIBP.Service.User (makePassword)

import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Vector qualified as V
import Data.Word (Word32)

import Fmt

getPassword :: IO T.Text
getPassword = do
  putStr "Enter password (leave empty for automatically generated): "
  hFlush stdout
  getLine >>= \case
    "" -> generatePassword
    x -> pure (T.pack x)

generatePassword :: IO T.Text
generatePassword = do
  rawIndices :: [Word32] <- take 5 . randoms <$> newStdGen
  let indices = map ((`mod` V.length someWords) . fromIntegral) rawIndices
  pure $ T.intercalate "-" $ map (someWords V.!) indices
  where
    someWords :: V.Vector T.Text
    someWords = V.fromList ["imperfect", "clap", "zipper", "accidental", "spy", "hospital", "bottle", "laughable", "concern", "dam", "moan", "crib", "voyage", "screw", "bad", "sour", "enormous", "rush", "absorbed", "houses", "lamp", "madly", "deeply", "scandalous", "heady", "stew", "rustic", "ancient", "explode", "glistening", "secret", "doubtful", "grieving", "nauseating", "permit", "babies", "answer", "illustrious", "fly", "offer", "temper", "belief", "daily", "ignore", "steam", "stingy", "cable", "share", "alluring", "wink", "mate", "pause", "underwear", "innocent", "plug", "brash", "shocking", "ugly", "brief", "yard", "prefer", "glib", "taste", "handsomely", "nutritious", "embarrassed", "shade", "fragile", "handy", "committee", "pigs", "knit", "breakable", "humor", "title", "uttermost", "nappy", "ahead", "juice", "fit", "route", "cattle", "cherry", "private", "disgusting", "rake", "slap", "sparkle", "real", "ceaseless", "mitten", "spotted", "cows", "vessel", "reflective", "plot", "passenger", "sedate", "internal", "scribble"]

createAdmin :: IO ()
createAdmin = do
  cfg <- parseConfig
  conn <- makeConnection cfg.dbSettings
  plainPassword <- getPassword
  hashedPassword <- makePassword plainPassword
  let user = InsertUser {username = "admin", hashedPassword = hashedPassword, isAdmin = True}
  runWithDb conn $
    insertUser user >>= \case
      Nothing -> putStrLn "an error occured while creating user 'admin'"
      Just _ -> T.putStrLn $ "successfully created user 'admin' with password '" +| plainPassword |+ ""
