module Main (main) where

import Lib as L
import Utils (toUri, putStrLn')

import Data.Maybe (fromMaybe)
import Data.Version (showVersion)
import Options.Applicative
import Options.Applicative.Builder (helpIndent)
import Paths_hmig (version)
import System.Console.Terminal.Size
import Network.URI
import Data.String (IsString, fromString)
import Control.Exception (catch)
import System.Exit (ExitCode(..), exitSuccess, exitWith)
import System.Environment (lookupEnv)

defaultBitbucketURLBase :: URI
defaultBitbucketURLBase = toUri "https://stash.billing.ru"

defaultGitlabURLBase :: URI
defaultGitlabURLBase = toUri "https://gitlab.nexign.com"

-- | Command line options
newtype Options = Options
  { optCommand :: Command
  }

data CommonOptions = CommonOptions
  { optBitbucketAccessToken :: String
  , optGitlabAccessToken :: String
  , optBitbucketUrl :: URI
  , optGitlabUrl :: URI
  , optBitbucketProjectName :: String
  , optGitlabNamespace :: String
  , optRepoNames :: [String]
  , optNoRepoNames :: [String]
  , optRepoNamesFile :: Maybe FilePath
  , optNoRepoNamesFile :: Maybe FilePath
  }

data Command
  = Import ImportOptions
  | Clean CleanOptions
  | List ListOptions
  | PostProcessing PostProcessingOptions

programVersion :: String
programVersion =
  showVersion version

bitbucketAccessTokenLong :: String
bitbucketAccessTokenLong = "bitbucket-access-token"

commonOptions :: Maybe String -> Maybe String -> Parser CommonOptions
commonOptions bitbucketAccessToken gitlabAccessToken =
  let
    repoLong = "repo"
    noRepoLong = "no-repo"
  in
    CommonOptions
    <$> option (eitherReader nonEmpty)
        ( long bitbucketAccessTokenLong
       <> short 'a'
       <> metavar "TOKEN"
       <> help ("BitBucket access token (default: `" <> bitbucketAccessTokenEnvName <> "` environment variable value)")
       <> maybe mempty value bitbucketAccessToken
        )
    <*> option (eitherReader nonEmpty)
        ( long "gitlab-access-token"
       <> short 'A'
       <> metavar "TOKEN"
       <> help ("BitBucket access token (default: `" <> gitlabAccessTokenEnvName <> "` environment variable value)")
       <> maybe mempty value gitlabAccessToken
        )
    <*> option uriReader
        ( long "bitbucket-url"
       <> short 'u'
       <> metavar "URL"
       <> help "BitBuckey URL base"
       <> showDefault
       <> value defaultBitbucketURLBase
        )
    <*> option uriReader
        ( long "gitlab-url"
       <> short 'U'
       <> metavar "URL"
       <> help "GitLab URL base"
       <> showDefault
       <> value defaultGitlabURLBase
        )
    <*> option (eitherReader nonEmpty)
        ( long "bitbucket-project"
       <> short 'p'
       <> metavar "PROJECT"
       <> help "BitBucket project name. Only repositories from this project will be taken inot account"
        )
    <*> option (eitherReader nonEmpty)
        ( long "gitlab-namespace"
       <> short 'P'
       <> metavar "NAMESPACE"
       <> help "GitLab namespace name. Only repositories from this namespace will be taken into account"
        )
    <*> many (option (eitherReader nonEmpty)
              
        ( long repoLong
       <> short 'r'
       <> metavar "REPO"
       <> help "Repository name. Use it if you want to restrict actions to a specified repository only.\
               \ This option can be specified several times to select several repositories"
        ))
    <*> many (option (eitherReader nonEmpty)
        ( long noRepoLong
       <> short 'R'
       <> metavar "REPO"
       <> help "Repository name to skip. Use it if you want to ignore this repository.\
               \ This option can be specified several times to ignore several repositories"
        ))
    <*> option (eitherReader ((Just <$>) . nonEmpty))
        ( long "file-repo"
       <> short 'f'
       <> metavar "FILE"
       <> help ("File with repository names, one per line. This way you can restrict actions to specified repositories only. Use as addition or replaicement for `--" <> repoLong <> "` option")
       <> value Nothing
        )
    <*> option (eitherReader ((Just <$>) . nonEmpty))
        ( long "file-no-repo"
       <> short 'F'
       <> metavar "FILE"
       <> help ("File with repository names to skip, one per line. This way you can ignore some repositories.  Use as addition or replaicement for `--" <> noRepoLong <> "` option")
       <> value Nothing
        )

options :: Maybe String -> Maybe String -> Parser Options
options bitbucketAccessToken gitlabAccessToken = Options
  <$> hsubparser
      ( command "import" (info
                            (importParser bitbucketAccessToken gitlabAccessToken)
                            (fullDesc
                             <> progDesc "Interactively imports repositories from the selected BitBucket project into the selected GitLab namespace.\
                                         \ Repositories already present in the selected GitLab namespace are ignored and are not offered for import.\
                                         \ In this way, you can import only part of the repositories, and then the remaining ones.\
                                         \ Alternatively, you can make sure that all repositories have already been\
                                         \ imported if no repositories are offered for import.\
                                         \ If you need to reimport an existing repository in GitLub, then you first need to delete it.\
                                         \ This can be done, for example, with the `clean` command of this utility."))
        <>
        command "clean" (info
                           (cleanParser bitbucketAccessToken gitlabAccessToken)
                           (fullDesc
                            <> progDesc "Allows you to interactively select to delete GitLab\
                                        \ repositories from the specified namespace that have\
                                        \ corresponding repositories in the specified BitBucket project.\
                                        \ This will allow you to re-import these repositories from BitBucket to GitLab."))
        <>
        command "list" (info
                          (listParser bitbucketAccessToken gitlabAccessToken)
                          (fullDesc
                           <> progDesc "Prints a list of imported (or ready-to-import) repositories."))
        <>
        command "post-processing" (info
                          (postProcessingParser bitbucketAccessToken gitlabAccessToken)
                          (fullDesc
                           <> progDesc "Post processing of imported repositories. \
                                       \ This command will be useful if you have already imported some \
                                       \ repositories from BitBucket to GitLab with another utility. \
                                       \ Or if during the initial import using this utility there were not enough permissions \
                                       \ to read the corresponding settings in the BitBucket repository and the import \
                                       \ of these settings was skipped."))
       )

data ImportOptions = ImportOptions
  { importOptionsCommon :: CommonOptions
  , importOptionsBitbucketUserName :: BitbucketUserName
  , importOptionsSkipDocpart :: Bool
  , importOptionsOnlyDocpart :: Bool
  , importOptionsSkipSubtreeDocpart :: Bool
  , importOptionsOnlySubtreeDocpart :: Bool
  }
importParser :: Maybe String -> Maybe String -> Parser Command
importParser bitbucketAccessToken gitlabAccessToken =
  Import <$> importOptionsParser
  where
    importOptionsParser :: Parser ImportOptions
    importOptionsParser = ImportOptions
      <$> commonOptions bitbucketAccessToken gitlabAccessToken
      <*> option (eitherReader nonEmpty)
          ( long "bitbucket-user"
         <> short 'p'
         <> metavar "USER"
         <> help ("BitBucket user name. Must correspond to BitBucket access token (see option `--" <> bitbucketAccessTokenLong <> "`)")
          )
      <*> skipDocPartJustParser
      <*> onlyDocPartJustParser
      <*> skipDocPartSubtreeParser
      <*> onlyDocPartSubtreeParser


skipDocPartJustParser :: Parser Bool
skipDocPartJustParser =
  switch
  ( long "skip-docpart-just"
    <> help "Skip BitBucket repositories whose name ends with `_docpart`, and there is a corresponding BitBucket repository with the same name, but without this suffix"
  )

onlyDocPartJustParser :: Parser Bool
onlyDocPartJustParser =
  switch
  ( long "only-docpart-just"
    <> help "Only BitBucket repositories whose name ends with `_docpart`, and there is a corresponding BitBucket repository with the same name, but without this suffix"
  )

skipDocPartSubtreeParser :: Parser Bool
skipDocPartSubtreeParser =
  switch
  ( long "skip-docpart-merged-as-subtree"
    <> help "Skip BitBucket repositories whose name ends with `_docpart`, and there is a corresponding BitBucket repository with the same name, but without this suffix, that contains commits from `_docpart` repository (probably added there by `git subtree`)"
  )

onlyDocPartSubtreeParser :: Parser Bool
onlyDocPartSubtreeParser =
  switch
  ( long "only-docpart-merged-as-subtree"
    <> help "Only BitBucket repositories whose name ends with `_docpart`, and there is a corresponding BitBucket repository with the same name, but without this suffix, that contains commits from `_docpart` repository (probably added there by `git subtree`)"
  )

data CleanOptions = CleanOptions
  { cleanOptionsCommon :: CommonOptions
  , cleanOptionsAllowChanged :: Bool
  }
cleanParser :: Maybe String -> Maybe String -> Parser Command
cleanParser bitbucketAccessToken gitlabAccessToken =
  Clean <$> cleanOptionsParser
  where
    cleanOptionsParser :: Parser CleanOptions
    cleanOptionsParser = CleanOptions
      <$> commonOptions bitbucketAccessToken gitlabAccessToken
      <*> switch
          ( long "allow-changed-repos"
         <> short 'c'
         <> help "By default, GitLab repositories with commits that are missing in the corresponding BitBucket repositories are excluded from processing. Use this option so that such repositories are also processed."
          )

data ListOptions = ListOptions
  { listOptionsCommon :: CommonOptions
  , listOptionsMode :: ListMode
  , listOptionsOnlyChanged :: Bool
  , listOptionsSkipChanged :: Bool
  , listOptionsSkipDocpart :: Bool
  , listOptionsOnlyDocpart :: Bool
  , listOptionsSkipSubtreeDocpart :: Bool
  , listOptionsOnlySubtreeDocpart :: Bool
  }
listParser :: Maybe String -> Maybe String -> Parser Command
listParser bitbucketAccessToken gitlabAccessToken =
  List <$> listOptionsParser
  where
    listOptionsParser :: Parser ListOptions
    listOptionsParser =
      let
        notLong = "not"
        availableOnlyInImportedMode = "Incompaible with `--" <> notLong <> "` option"
      in
        ListOptions
        <$> commonOptions bitbucketAccessToken gitlabAccessToken
        <*> flag ListImported ListNotImpoted
            ( short 'n'
           <> long notLong
           <> help "By default, imported GitLab repositories will be listed. Use this option if you want to list not imported BitBucket repositories"
            )
        <*> switch
            ( short 'c'
           <> long "only-changed"
           <> help ("Only list GitLab repositoies with commits that missing in corresponding BitBucket repositories. " <> availableOnlyInImportedMode)
            )
        <*> switch
            ( short 'C'
           <> long "skip-changed"
           <> help ("Do not list GitLab repositoies with commits that missing in corresponding BitBucket repositories. " <> availableOnlyInImportedMode)
            )
        <*> skipDocPartJustParser
        <*> onlyDocPartJustParser
        <*> skipDocPartSubtreeParser
        <*> onlyDocPartSubtreeParser

data PostProcessingOptions = PostProcessingOptions
  { postProcessingOptionsCommon :: CommonOptions
  , postProcessingOptionsAll :: Bool
  , postProcessingOptionsImportBranchPermissions :: Bool
  , postProcessingOptionsCorrecRepoPath :: Bool
  }
postProcessingParser :: Maybe String -> Maybe String -> Parser Command
postProcessingParser bitbucketAccessToken gitlabAccessToken =
  PostProcessing <$> postProcessingOptionsParser
  where
    postProcessingOptionsParser :: Parser PostProcessingOptions
    postProcessingOptionsParser =
      PostProcessingOptions
      <$> commonOptions bitbucketAccessToken gitlabAccessToken
      <*> switch
          ( long "all"
         <> help "Do all applicable post-processing actions"
          )
      <*> switch
          ( long "branch-permissions"
         <> help "Import branch permissions that are not in GitLab repository and present in the corresponding BitBucket repository"
          )
      <*> switch
          ( long "correct-repo-path"
         <> help "Normalization of repositories paths so that the repository URL does not contain uppercase characters."
          )

uriReader :: ReadM URI
uriReader = maybeReader parseAbsoluteURI

nonEmpty :: IsString b => String -> Either String b
nonEmpty [] = Left "Option value cannot be an empty string"
nonEmpty s  = return $ fromString s

bitbucketAccessTokenEnvName, gitlabAccessTokenEnvName :: String
bitbucketAccessTokenEnvName = "BITBUCKET_ACCESS_TOKEN"
gitlabAccessTokenEnvName = "GITLAB_ACCESS_TOKEN"

main :: IO ()
main = do
  w <- (width <$>) <$> size
  bitbucketAccessTokenDef <- lookupEnv bitbucketAccessTokenEnvName
  gitlabAccessTokenDef <- lookupEnv gitlabAccessTokenEnvName
  migrate' . optCommand =<< customExecParser (p w) (opts bitbucketAccessTokenDef gitlabAccessTokenDef)
  where
    opts bb gl = info
      (    options bb gl
      <**> infoOption programVersion (long "version" <> help "Print version info")
      <**> helper
      )
      ( fullDesc
     <> header ("Migration utility from BitBucket to GitLab. Version " <> programVersion)
     <> progDesc "An interactive utility designed to help migrate `git` repositories from BitBucket to GitLab."
      )
    p width_ = prefs $ disambiguate <> showHelpOnEmpty <> columns (fromMaybe 80 width_) <> helpIndent 33

catchErrorAndExit :: ApiException -> IO a
catchErrorAndExit (NothingToDo s) = do
  putStrLn' s
  exitSuccess
catchErrorAndExit (ActionCanceledByUser s) = do
  putStrLn' s
  exitWith $ ExitFailure 2
catchErrorAndExit (OtherError s) = do
  putStrLn' s
  exitWith $ ExitFailure 1


makeRepoNames :: [String] -> Maybe FilePath  -> IO (Maybe [String])
makeRepoNames repoNames repoNamesFile = do
  (\x -> if null x then Nothing else Just x)
  . (repoNames <>)
  <$> case repoNamesFile of
        Nothing -> return []
        Just fn -> lines <$> readFile fn 

migrate' :: Command -> IO ()
migrate' (Import opt) = do
  let common = importOptionsCommon opt
  repos <- makeRepoNames (optRepoNames common) (optRepoNamesFile common)
  noRepos <- makeRepoNames (optNoRepoNames common) (optNoRepoNamesFile common)
  let
    cfg =
        ImportConfig
        { importConfigBitbucketAccessToken = optBitbucketAccessToken common
        , importConfigBitbucketUrlBase = optBitbucketUrl common
        , importConfigGitlabAccessToken = optGitlabAccessToken common
        , importConfigGitlabUrlBase = optGitlabUrl common
        , importConfigBitbucketProjectName = optBitbucketProjectName common
        , importConfigGitlabNamespaceName = optGitlabNamespace common
        , importConfigRepoNames = repos
        , importConfigNoRepoNames = noRepos
        , importConfigBitbucketUserName = importOptionsBitbucketUserName opt
        , importConfigBitbucketSkipDocpart = importOptionsSkipDocpart opt
        , importConfigBitbucketOnlyDocpart = importOptionsOnlyDocpart opt
        , importConfigBitbucketSkipSubtreeDocpart = importOptionsSkipSubtreeDocpart opt
        , importConfigBitbucketOnlySubtreeDocpart = importOptionsOnlySubtreeDocpart opt
        }
  import' cfg
    `catch` catchErrorAndExit

migrate' (Clean opt) = do
  let common = cleanOptionsCommon opt
  repos <- makeRepoNames (optRepoNames common) (optRepoNamesFile common)
  noRepos <- makeRepoNames (optNoRepoNames common) (optNoRepoNamesFile common)
  let
    allowChanged = cleanOptionsAllowChanged opt
    cfg =
        CleanConfig
        { cleanConfigBitbucketAccessToken = optBitbucketAccessToken common
        , cleanConfigBitbucketUrlBase = optBitbucketUrl common
        , cleanConfigGitlabAccessToken = optGitlabAccessToken common
        , cleanConfigGitlabUrlBase = optGitlabUrl common
        , cleanConfigBitbucketProjectName = optBitbucketProjectName common
        , cleanConfigGitlabNamespaceName = optGitlabNamespace common
        , cleanConfigRepoNames = repos
        , cleanConfigNoRepoNames = noRepos
        , cleanConfigAllowChanged = allowChanged
        }
  clean cfg
    `catch` catchErrorAndExit

migrate' (List opt) = do
  let common = listOptionsCommon opt
  repos <- makeRepoNames (optRepoNames common) (optRepoNamesFile common)
  noRepos <- makeRepoNames (optNoRepoNames common) (optNoRepoNamesFile common)
  let
    cfg =
        ListConfig
        { listConfigBitbucketAccessToken = optBitbucketAccessToken common
        , listConfigBitbucketUrlBase = optBitbucketUrl common
        , listConfigGitlabAccessToken = optGitlabAccessToken common
        , listConfigGitlabUrlBase = optGitlabUrl common
        , listConfigBitbucketProjectName = optBitbucketProjectName common
        , listConfigGitlabNamespaceName = optGitlabNamespace common
        , listConfigRepoNames = repos
        , listConfigNoRepoNames = noRepos
        , listConfigListMode = listOptionsMode opt
        , listConfigOnlyChanged = listOptionsOnlyChanged opt
        , listConfigSkipChanged = listOptionsSkipChanged opt
        , listConfigBitbucketSkipDocpart = listOptionsSkipDocpart opt
        , listConfigBitbucketOnlyDocpart = listOptionsOnlyDocpart opt
        , listConfigBitbucketSkipSubtreeDocpart = listOptionsSkipSubtreeDocpart opt
        , listConfigBitbucketOnlySubtreeDocpart = listOptionsOnlySubtreeDocpart opt
        }
  list' cfg
    `catch` catchErrorAndExit

migrate' (PostProcessing opt) = do
  let common = postProcessingOptionsCommon opt
  repos <- makeRepoNames (optRepoNames common) (optRepoNamesFile common)
  noRepos <- makeRepoNames (optNoRepoNames common) (optNoRepoNamesFile common)
  let
    cfg =
        PostProcessingConfig
        { postProcessingConfigBitbucketAccessToken = optBitbucketAccessToken common
        , postProcessingConfigBitbucketUrlBase = optBitbucketUrl common
        , postProcessingConfigGitlabAccessToken = optGitlabAccessToken common
        , postProcessingConfigGitlabUrlBase = optGitlabUrl common
        , postProcessingConfigBitbucketProjectName = optBitbucketProjectName common
        , postProcessingConfigGitlabNamespaceName = optGitlabNamespace common
        , postProcessingConfigRepoNames = repos
        , postProcessingConfigNoRepoNames = noRepos
        , postProcessingConfigAll = postProcessingOptionsAll opt
        , postProcessingConfigImportBranchPermissions = postProcessingOptionsImportBranchPermissions opt
        , postProcessingConfigCorrectRepoPath = postProcessingOptionsCorrecRepoPath opt
        }
  postProcessing' cfg
    `catch` catchErrorAndExit
