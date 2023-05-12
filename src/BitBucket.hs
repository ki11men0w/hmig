{-# LANGUAGE OverloadedStrings #-}
module BitBucket
    ( findProject
    , findRepo
    , listProjectRepos
    , newBitbucketApiConfig
    , listBranchPermisions
    , BitbucketApi
    , BitbucketApiConfig
    , BitbucketProject(..)
    , BitbucketRepo(..)
    , BitbucketBranchPermission(..)
    , BitbucketBranchPermissionType(..)
    , BitbucketBranchPermissionMatcher(..)
    , BitbucketBranchPermissionMatcherType(..)
    , BitbucketCommit(..)
    , findCommit
    , findFirstCommit
    ) where

import Utils (toPath)
import Control.Monad.Reader
import Network.Wreq as W
import Control.Lens
import qualified Data.ByteString.UTF8 as BU
import Data.Aeson hiding (Options)
import Control.Applicative
import Network.Wreq.Session as S
import Network.URI
import Data.Maybe (fromJust, listToMaybe)
import Control.Exception as E
import qualified Network.HTTP.Client as HC
import Data.List (isSuffixOf)
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.String (fromString)


restApiPath :: URI
restApiPath = fromJust $ parseRelativeReference "rest/api/latest/"
restBranchPermissionsPath :: URI
restBranchPermissionsPath = fromJust $ parseRelativeReference "rest/branch-permissions/latest/"

data BitbucketApiConfig = BitbucketApiConfig
  { bitbucketSession :: S.Session
  , bitbucketUrlBase :: URI
  , bitbucketDefaults :: Options
  }
  deriving (Show)

newBitbucketApiConfig :: URI -> String -> IO BitbucketApiConfig
newBitbucketApiConfig urlBase accessToken = do
  sess <- S.newSession
  return BitbucketApiConfig
    { bitbucketSession = sess
    , bitbucketUrlBase = urlBase
    , bitbucketDefaults = defaults_
    }
  where
    defaults_ = defaults
      & auth ?~ oauth2Bearer (BU.fromString accessToken)
      & header "Accept" .~ ["application/json"]


data BitbucketError = BitbucketError
  { bitbucketErrorMessage :: String
  , bitbucketErrorExceptionName :: String
  }
  deriving (Show)

instance FromJSON BitbucketError where
  parseJSON (Object v) =
    BitbucketError
    <$> v .: "message"
    <*> v .: "exceptionName"
  parseJSON _ = empty

newtype BitbucketErrors = BitbucketErrors
  { bitbucketErrors :: [BitbucketError]
  }

instance FromJSON BitbucketErrors where
  parseJSON (Object v) = BitbucketErrors <$> v .: "errors"
  parseJSON _ = empty

notFound :: ([BitbucketError] -> Bool) -> (HC.HttpException -> IO (Maybe a))
notFound predicat =
  handler
  where
    handler e@(HC.HttpExceptionRequest _ (HC.StatusCodeException response msg))
      | response ^. responseStatus . statusCode == 404 =
          case decode . Char8.pack . BU.toString $ msg of
            Just errors ->
              if predicat (bitbucketErrors errors)
              then return Nothing
              else throwIO e
            Nothing -> throwIO e
      | otherwise = throwIO e
    handler e = throwIO e

notFoundAndExceptionNameMatch :: (String -> Bool) -> (HC.HttpException -> IO (Maybe a))
notFoundAndExceptionNameMatch predicat =
  notFound $ any $ predicat . bitbucketErrorExceptionName


data BitbucketProject = BitbucketProject
  { bitbucketProjectKey :: String
  , bitbucketProjectId :: Integer
  , bitbucketProjectName :: String
  }
  deriving (Show)

instance FromJSON BitbucketProject where
  parseJSON (Object v) =
    BitbucketProject
    <$> v .: "key"
    <*> v .: "id"
    <*> v .: "name"
  parseJSON _ = empty


data BitbucketLink = BitbucketLink
  { bitbucketLinkRef :: String
  , bitbucketLinkName :: Maybe String
  } deriving (Show)
instance FromJSON BitbucketLink where
  parseJSON (Object v) =
    BitbucketLink
    <$> v .:  "href"
    <*> v .:? "name"
  parseJSON _ = empty

newtype BitbucketLinks = BitbucketLinks
  { bitbucketLinksSelf :: [BitbucketLink]
  } deriving (Show)
instance FromJSON BitbucketLinks where
  parseJSON (Object v) =
    BitbucketLinks
    <$> v .: "self"
  parseJSON _ = empty

data BitbucketRepo = BitbucketRepo
  { bitbucketRepoSlug :: String
  , bitbucketRepoId :: Integer
  , bitbucketRepoName :: String
  , bitbucketRepoLinks :: BitbucketLinks
  }
  deriving (Show)

instance FromJSON BitbucketRepo where
  parseJSON (Object v) =
    BitbucketRepo
    <$> v .: "slug"
    <*> v .: "id"
    <*> v .: "name"
    <*> v .: "links"
  parseJSON _ = empty

type BitbucketApi a = ReaderT BitbucketApiConfig IO a

bitbucketRestApiUrlBase :: BitbucketApi URI
bitbucketRestApiUrlBase = do
  urlBase <- asks bitbucketUrlBase
  return $ restApiPath `relativeTo` urlBase

findProject :: String -> BitbucketApi (Maybe BitbucketProject)
findProject "" = error "Project name cannot be empty"
findProject projectName = do
  cfg <- ask
  urlBase_ <- bitbucketRestApiUrlBase
  let query_ =
        S.getWith
        (bitbucketDefaults cfg)
        (bitbucketSession cfg)
        (show $ toPath ["projects", projectName] `relativeTo` urlBase_)
      doIt = (^. responseBody) <$> (asJSON =<< query_)
  liftIO (doIt `E.catch` notFoundAndExceptionNameMatch (".NoSuchProjectException" `isSuffixOf`))

findRepo :: BitbucketProject -> String -> BitbucketApi (Maybe BitbucketRepo)
findRepo _ "" = error "Repo name cannot be empty"
findRepo project repoName = do
  cfg <- ask
  urlBase_ <- bitbucketRestApiUrlBase
  let query_ =
        S.getWith
        (bitbucketDefaults cfg)
        (bitbucketSession cfg)
        (show $ toPath ["projects", bitbucketProjectKey project, "repos", repoName] `relativeTo` urlBase_)
      doIt = (^. responseBody) <$> (asJSON =<< query_)
  liftIO (doIt `E.catch` notFoundAndExceptionNameMatch (".NoSuchRepositoryException" `isSuffixOf`))


data BitbucketReposList = BitbucketReposList
  { bitbucketReposListNextPageStart :: Maybe Int
  , bitbucketReposListValues :: [BitbucketRepo]
  }
  deriving (Show)
instance FromJSON BitbucketReposList where
  parseJSON (Object v) =
    BitbucketReposList
    <$> v .:? "nextPageStart"
    <*> v .:  "values"
  parseJSON _ = empty


listProjectRepos :: BitbucketProject -> BitbucketApi [BitbucketRepo]
listProjectRepos project = do
  cfg <- ask
  urlBase_ <- bitbucketRestApiUrlBase
  let
      doIt accum start = do
        r <- (^. responseBody) <$> (asJSON =<< query_)
        let result = accum <> bitbucketReposListValues r
        case bitbucketReposListNextPageStart r of
          Just i -> doIt result i
          Nothing -> return result
        where
          query_ =
            S.getWith
            (bitbucketDefaults cfg & param "start" .~ [fromString $ show start])
            (bitbucketSession cfg)
            (show $ toPath ["projects", bitbucketProjectKey project, "repos"] `relativeTo` urlBase_)

  liftIO $ doIt [] 0

data BitbucketBranchPermissionType = BitbucketBranchPermissionTypeReadOnly
                                   | BitbucketBranchPermissionTypeFastForwardOnly
                                   | BitbucketBranchPermissionTypePullRequestOnly
                                   | BitbucketBranchPermissionTypeNoDeletes
                                   deriving (Show, Eq)
instance FromJSON BitbucketBranchPermissionType where
  parseJSON (String "read-only") = pure BitbucketBranchPermissionTypeReadOnly
  parseJSON (String "fast-forward-only") = pure BitbucketBranchPermissionTypeFastForwardOnly
  parseJSON (String "pull-request-only") = pure BitbucketBranchPermissionTypePullRequestOnly
  parseJSON (String "no-deletes") = pure BitbucketBranchPermissionTypeNoDeletes
  parseJSON x = error $ "Unknown branch permission type: " <> show x

data BitbucketBranchPermissionMatcherType = BitbucketBranchPermissionMatcherTypeBranch
                                          | BitbucketBranchPermissionMatcherTypePattern
                                          | BitbucketBranchPermissionMatcherTypeModelCategoy
                                          | BitbucketBranchPermissionMatcherTypeModelBranch
                                          deriving (Show, Eq)
instance FromJSON BitbucketBranchPermissionMatcherType where
  parseJSON (String "BRANCH") = pure BitbucketBranchPermissionMatcherTypeBranch
  parseJSON (String "PATTERN") = pure BitbucketBranchPermissionMatcherTypePattern
  parseJSON (String "MODEL_CATEGORY") = pure BitbucketBranchPermissionMatcherTypeModelCategoy
  parseJSON (String "MODEL_BRANCH") = pure BitbucketBranchPermissionMatcherTypeModelBranch
  parseJSON x = error $ "Unknown branch permission matcher type: " <> show x

data BitbucketBranchPermissionMatcher = BitbucketBranchPermissionMatcher
  { bitbucketBranchPermissionMatcherId :: String
  , bitbucketBranchPermissionMatcherType :: BitbucketBranchPermissionMatcherType
  , bitbucketBranchPermissionMatcherActive :: Bool
  } deriving (Show, Eq)
instance FromJSON BitbucketBranchPermissionMatcher where
  parseJSON (Object v) =
    BitbucketBranchPermissionMatcher
    <$> v .: "id"
    <*> ((v .: "type") >>= (.: "id"))
    <*> v .: "active"
  parseJSON _ = empty

data BitbucketBranchPermission = BitbucketBranchPermission
  { bitbucketBranchPermissionId :: Int
  , bitbucketBranchPermissionType :: BitbucketBranchPermissionType
  , bitbucketBranchPermissionMatcher :: BitbucketBranchPermissionMatcher
  } deriving (Show, Eq)
instance FromJSON BitbucketBranchPermission where
  parseJSON (Object v) =
    BitbucketBranchPermission
    <$> v .: "id"
    <*> v .: "type"
    <*> v .: "matcher"
  parseJSON _ = empty

bitbucketRestBranchPermissionsUrlBase :: BitbucketApi URI
bitbucketRestBranchPermissionsUrlBase = do
  urlBase <- asks bitbucketUrlBase
  return $ restBranchPermissionsPath `relativeTo` urlBase

data BitbucketBranchPermittionsList = BitbucketBranchPermittionsList
  { bitbucketBranchPermittionsListNextPageStart :: Maybe Int
  , bitbucketBranchPermittionsListValues :: [BitbucketBranchPermission]
  }
instance FromJSON BitbucketBranchPermittionsList where
  parseJSON (Object v) =
    BitbucketBranchPermittionsList
    <$> v .:? "nextPageStart"
    <*> v .:  "values"
  parseJSON _ = empty

listBranchPermisions :: BitbucketProject -> BitbucketRepo -> BitbucketApi [BitbucketBranchPermission]
listBranchPermisions project repo = do
  cfg <- ask
  urlBase_ <- bitbucketRestBranchPermissionsUrlBase
  let
    doIt accum start = do
      r <- (^. responseBody) <$> (asJSON =<< query_)
      let result = accum <> bitbucketBranchPermittionsListValues r
      case bitbucketBranchPermittionsListNextPageStart r of
        Just i -> doIt result i
        Nothing -> return result
      where
        query_ =
          S.getWith
          (bitbucketDefaults cfg & param "start" .~ [fromString $ show start])
          (bitbucketSession cfg)
          (show $ toPath
             [ "projects", bitbucketProjectKey project
             , "repos", bitbucketRepoSlug repo
             , "restrictions"
             ] `relativeTo` urlBase_)

  liftIO $ doIt [] 0

newtype BitbucketCommit = BitbucketCommit
  { bitbucketCommitId :: String
  } deriving Show
instance FromJSON BitbucketCommit where
  parseJSON (Object v) =
    BitbucketCommit
    <$> v .: "id"
  parseJSON _ = empty

findCommit :: BitbucketProject -> BitbucketRepo -> String -> BitbucketApi (Maybe BitbucketCommit)
findCommit project repo commitHash = do
  cfg <- ask
  urlBase_ <- bitbucketRestApiUrlBase
  let query_ =
        S.getWith
        (bitbucketDefaults cfg)
        (bitbucketSession cfg)
        (show $ toPath
           [ "projects", bitbucketProjectKey project
           , "repos", fromString (bitbucketRepoSlug repo)
           , "commits", fromString commitHash
           ] `relativeTo` urlBase_)
      doIt = (^. responseBody) <$> (asJSON =<< query_)
  liftIO (doIt `E.catch` notFoundAndExceptionNameMatch (".NoSuchCommitException" `isSuffixOf`))

newtype BitbucketCommits = BitbucketCommits
  { bitbucketCommitsValues :: [BitbucketCommit]
  } deriving Show
instance FromJSON BitbucketCommits where
  parseJSON (Object v) =
    BitbucketCommits
    <$> v .: "values"
  parseJSON _ = empty

findFirstCommit :: BitbucketProject -> BitbucketRepo -> BitbucketApi (Maybe BitbucketCommit)
findFirstCommit project repo = do
  cfg <- ask
  urlBase_ <- bitbucketRestApiUrlBase
  let query_ =
        S.getWith
        (bitbucketDefaults cfg & param "limit" .~ ["1"])
        (bitbucketSession cfg)
        (show $ toPath
           [ "projects", bitbucketProjectKey project
           , "repos", fromString (bitbucketRepoSlug repo)
           , "commits"
           ] `relativeTo` urlBase_)
      doIt = listToMaybe . bitbucketCommitsValues . (^. responseBody) <$> (asJSON =<< query_)
  liftIO (doIt `E.catch` notFoundAndExceptionNameMatch (".NoSuchCommitException" `isSuffixOf`))
