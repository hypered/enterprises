{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Main
-- Description : Download Open Data files from the Belgian CBE
--
-- This program assists in downloading CSV files from the CBE Open Data pages.
module Main
  ( main

    -- * Data types
  , Command (..)
  , DownloadOptions (..)

    -- * Parser definitions
  , parserInfo

    -- * Development
    -- $development
  , help
  , parse
  ) where

import Configuration.Dotenv qualified as Dotenv
import Control.Lens
import Data.ByteString.Lazy qualified as BL
import Data.String (String)
import Data.Text qualified as T
import Network.Wreq
import Network.Wreq.Session qualified as S
import Options.Applicative qualified as A
import Protolude
import System.Environment (lookupEnv)
import System.FilePath.Glob qualified as Glob
import Text.HTML.TagSoup
import Text.StringLike (toString) -- From Tagsoup.

--------------------------------------------------------------------------------
main :: IO ()
main = A.execParser parserInfo >>= run

--------------------------------------------------------------------------------
-- Represents all the commands understood by @enterprises@.
data Command
  = -- | Download (missing) archives from a specified URL.
    DownloadArchives DownloadOptions
  | -- | List remoate archives or archives that we already downloaded.
    ListArchives ListOptions
  | -- | Download an index from a specified URL.
    DownloadIndex DownloadOptions
  | -- | Parse a local (alread downloaded index).
    ParseIndex
  | -- | Check the provided username and password.
    Login
  deriving (Eq, Show)

-- Options for the download command.
data DownloadOptions = DownloadOptions
  { baseUrl :: Text
  -- ^ Base URL.
  }
  deriving (Eq, Show)

-- Options for the list command.
data ListOptions = ListOptions
  { location :: LocalOrRemote
  }
  deriving (Eq, Show)

data LocalOrRemote = Local | Remote | Missing
  deriving (Eq, Show)

--------------------------------------------------------------------------------
parserInfo :: A.ParserInfo Command
parserInfo =
  A.info (parser <**> A.helper) $
    A.fullDesc
      <> A.header "enterprises - Download Open Data files from the CBE."
      <> A.progDesc "This program assists in downloading CSV files from the CBE Open Data pages."

--------------------------------------------------------------------------------
parser :: A.Parser Command
parser =
  A.subparser
    ( A.command
        "download-archives"
        ( A.info (parserDownloadArchives <**> A.helper) $
            A.progDesc "Download missing archives"
        )
        <> A.command
          "list-archives"
          ( A.info (parserListArchives  <**> A.helper) $
              A.progDesc "List downloaded and/or remote archives"
          )
        <> A.command
          "download-index"
          ( A.info (parserDownloadIndex <**> A.helper) $
              A.progDesc "Download an index of available remote archives"
          )
        <> A.command
          "parse-index"
          ( A.info (parserParseIndex <**> A.helper) $
              A.progDesc "Parse a downloaded index of available remote archives"
          )
        <> A.command
          "login"
          ( A.info (parserLogin <**> A.helper) $
              A.progDesc "Verify the provided username and password"
          )
    )

--------------------------------------------------------------------------------
parserDownloadArchives :: A.Parser Command
parserDownloadArchives = do
  baseUrl <- parserBaseUrl
  pure $ DownloadArchives DownloadOptions {..}

--------------------------------------------------------------------------------
parserListArchives :: A.Parser Command
parserListArchives = do
  location <- parserLocalOrRemote
  pure $ ListArchives ListOptions {..}

--------------------------------------------------------------------------------
parserDownloadIndex :: A.Parser Command
parserDownloadIndex = do
  baseUrl <- parserBaseUrl
  pure $ DownloadIndex DownloadOptions {..}

--------------------------------------------------------------------------------
parserParseIndex :: A.Parser Command
parserParseIndex = pure ParseIndex

--------------------------------------------------------------------------------
parserLogin :: A.Parser Command
parserLogin = pure Login

--------------------------------------------------------------------------------
parserBaseUrl :: A.Parser Text
parserBaseUrl =
  A.argument
    A.str
    ( A.metavar "URL"
        <> A.value "https://kbopub.economie.fgov.be/kbo-open-data/"
        <> A.help "The base URL from which to download archives."
    )

parserLocalOrRemote :: A.Parser LocalOrRemote
parserLocalOrRemote =
  A.flag'
    Local
    ( A.long "local"
        <> A.help "Local archives (default)."
    )
    <|> A.flag'
      Remote
      ( A.long "remote"
          <> A.help "Remote archive."
      )
    <|> A.flag'
      Missing
      ( A.long "missing"
          <> A.help "Missing archive (i.e present remotely but not locally)."
      )
    <|> pure Local

--------------------------------------------------------------------------------

-- | A Text with the same content as `enterprises --help`.
help :: Text
help =
  let result =
        A.execParserPure A.defaultPrefs parserInfo ["--help"]
   in case result of
        A.Failure (A.ParserFailure f) -> let (err, _, _) = f "" in show err
        _ -> "Can't happen."

-- | Try the command parser to inspect what `enterprises` gets.
-- E.g.:
--
-- @
--     Enterprises.Command.parse "login"
-- @
parse :: Text -> A.ParserResult Command
parse s =
  A.execParserPure A.defaultPrefs parserInfo $ map T.unpack $ T.words s

--------------------------------------------------------------------------------
run :: Command -> IO ()
run (DownloadArchives _) = do
  credentials <- readCredentials
  session <- uncurry login credentials
  missings <- listMissingArchives "."
  let n = length missings
  case n of
    0 -> putStrLn @Text "Nothing to download."
    1 -> putStrLn @Text "Downloading 1 archive..."
    _ -> putStrLn @Text $ "Downloading " <> show n <> " archives..."
  mapM_ (downloadArchive session) missings
run (ListArchives ListOptions {..}) = case location of
  Local -> do
    fns <- listLocalArchives "."
    mapM_ putStrLn fns
  Remote -> do
    fns <- listRemoteArchives
    mapM_ putStrLn fns
  Missing -> do
    fns <- listMissingArchives "."
    mapM_ putStrLn fns
run (DownloadIndex _) = do
  credentials <- readCredentials
  session <- uncurry login credentials
  downloadIndex session
run ParseIndex = do
  mentries <- parseIndex "index.html"
  case mentries of
    Nothing -> putStrLn @Text "Can't extract entries from index page."
    Just entries -> mapM_ print entries
run Login = do
  credentials <- readCredentials
  _ <- uncurry login credentials
  pure ()

--------------------------------------------------------------------------------

-- | Just a wrapper over a Wreq session, to better hint that `login` should be
-- used before using `download`.
data Session = Session S.Session

readCredentials :: IO (String, String)
readCredentials = do
  -- Obtain the username and password from a .env file.
  Dotenv.loadFile Dotenv.defaultConfig
  -- TODO Fail properly.
  Just username <- lookupEnv "CBE_USERNAME"
  Just password <- lookupEnv "CBE_PASSWORD"
  pure (username, password)

-- | POST using a session, to obtain a cookie.
login :: String -> String -> IO Session
login username password = do
  putStrLn @Text "Logging in to kbopub.economie.fgov.be..."
  let loginUrl =
        "https://kbopub.economie.fgov.be/kbo-open-data/static/j_spring_security_check"
      credentials =
        [ "j_username" := username
        , "j_password" := password
        ]
  sess <- S.newSession
  response <- S.post sess loginUrl credentials

  let code = response ^. responseStatus . statusCode
  when (code /= 200) $ do
    putStrLn @Text "Login to kbopub.economie.fgov.be failed. Exiting."
    exitFailure

  putStrLn @Text "Credentials are valid."
  pure $ Session sess

downloadIndex :: Session -> IO ()
downloadIndex (Session sess) = do
  putStrLn @Text "Downloading index..."
  let indexUrl = "https://kbopub.economie.fgov.be/kbo-open-data/affiliation/xml/?files"
  response <- S.get sess indexUrl

  let code = response ^. responseStatus . statusCode
  when (code /= 200) $ do
    putStrLn @Text "Requesting the index failed. Exiting."
    exitFailure

  let fn = "index.html"
  BL.writeFile fn (response ^. responseBody)

  putStrLn $ "Index written to " <> fn <> "."

-- | GET using a session (a cookie is necessary) to obtain a .zip file.
downloadArchive :: Session -> FilePath -> IO ()
downloadArchive (Session sess) archiveName = do
  putStrLn $ "Downloading archive " <> T.pack archiveName <> "..."
  when (not $ isValidArchiveName archiveName) $ do
    putStrLn @Text "Invalid archive name. Exiting."
    exitFailure
  let archiveUrl =
        "https://kbopub.economie.fgov.be/kbo-open-data/affiliation/xml/files/"
          <> archiveName
  response <- S.get sess archiveUrl

  let code = response ^. responseStatus . statusCode
  when (code /= 200) $ do
    putStrLn @Text "Requesting the archive failed. Exiting."
    exitFailure

  BL.writeFile archiveName (response ^. responseBody)

  putStrLn $ "Archive \"" <> archiveName <> "\" downloaded."

isValidArchiveName :: FilePath -> Bool
isValidArchiveName fn = all valid fn
 where
  valid c = isAlphaNum c || c `elem` ("_." :: String)

--------------------------------------------------------------------------------

parseIndex :: FilePath -> IO (Maybe [IndexEntry])
parseIndex fn = do
  putStrLn @Text "Parsing index..."
  content <- readFile fn
  pure $ extractIndex content

-- | Try to turn an HTML text to a list of index entries. Return Nothing when
-- it can't extract the entries.
extractIndex :: Text -> Maybe [IndexEntry]
extractIndex content =
  let tags = canonicalizeTags $ parseTags content
      tbody =
        filter (not . isTagTextWhitespace)
        . takeWhile (not . isTagCloseName @Text "tbody")
        $ dropWhile (not . isTagOpenName @Text "tbody") tags
      trs = partitions (~== (TagOpen @Text "tr") []) tbody
      rows = map (map fromTagText . filter isTagText) trs
      mentries = mapM mkIndexEntry rows
  in mentries

isTagTextWhitespace :: Tag Text -> Bool
isTagTextWhitespace tag =
  case tag of
    TagText x | all isSpace (toString x) -> True
    _ -> False

-- | Month, filename of the "full" archive, filename of the "update" archive.
data IndexEntry = IndexEntry Text FilePath FilePath
  deriving (Eq, Show)

mkIndexEntry :: [Text] -> Maybe IndexEntry
mkIndexEntry [a, b, c] = Just $ IndexEntry a (T.unpack b) (T.unpack c)
mkIndexEntry _ = Nothing

--------------------------------------------------------------------------------
listLocalArchives :: FilePath -> IO [FilePath]
listLocalArchives archivesDir = do
  fns <- sort <$> Glob.globDir1 pat archivesDir
  pure $ map dropDotSlash fns
 where
  pat = Glob.compile "*.zip"
  dropDotSlash fn | "./" `isPrefixOf` fn = drop 2 fn
                  | otherwise = fn

listRemoteArchives :: IO [FilePath]
listRemoteArchives = do
  credentials <- readCredentials
  session <- uncurry login credentials
  downloadIndex session
  mentries <- parseIndex "index.html"
  case mentries of
    Nothing -> do
      putStrLn @Text "Can't extract entries from index page."
      pure []
    Just entries -> do
      let takeFilenames (IndexEntry _ full update) = [full, update]
      pure . sort $ concatMap takeFilenames entries

listMissingArchives :: FilePath -> IO [FilePath]
listMissingArchives archivesDir = do
  locals <- listLocalArchives archivesDir
  remotes <- listRemoteArchives
  let missings = filter (not . (`elem` locals)) remotes
  pure missings
