{-# LANGUAGE OverloadedStrings #-}
module GitLab
  ( GitlabApi
  , GitlabApiConfig
  , newGitlabApiConfig
  , ImportFromBitbucketParams(..)
  , GitlabNamespace(..)
  , GitlabRepo(..)
  , GitlabRepoImportStatus(..)
  , importBitbucketProject
  , findNamespace
  , findRepo
  , findRepos
  , deleteRepo
  , GitlabProtectedBranch(..)
  , GitlabProtectedBranchAccessLevel(..)
  , listProtectedBanches
  , deleteProtectedBranch
  , createProtectedBranch
  , GitlabCommit(..)
  , wasCommitsAfterCreation
  , anyLastCommit
  , setRepoPath
  ) where

import Utils (toPath)
import Control.Monad.Reader
import Network.Wreq as W
import Network.Wreq.Types (renderFormValue)
import Control.Lens hiding ((.=))
import qualified Data.ByteString.UTF8 as BU
import Data.Aeson hiding (Options)
import Network.Wreq.Session as S
import Network.URI
import Data.Maybe (fromJust)
import qualified Network.HTTP.Client as HC
import Control.Exception as E
import qualified Data.ByteString.Lazy.Char8 as Char8
import Control.Applicative
import Data.List (isSuffixOf)
import Data.String (fromString)
import Data.Char (toLower)
import Data.Text (Text, pack)
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Control.Monad.Extra (anyM)

apiPath :: URI
apiPath = fromJust $ parseRelativeReference "api/v4/"

data GitlabApiConfig = GitlabApiConfig
  { gitlabSession :: S.Session
  , gitlabUrlBase :: URI
  , gitlabDefaults :: Options
  }
  deriving (Show)

type GitlabApi a = ReaderT GitlabApiConfig IO a

newGitlabApiConfig :: URI -> String -> IO GitlabApiConfig
newGitlabApiConfig urlBase accessToken = do
  sess <- S.newSession
  return GitlabApiConfig
    { gitlabSession = sess
    , gitlabUrlBase = urlBase
    , gitlabDefaults = defaults_
    }
  where
    defaults_ = defaults
      & auth ?~ oauth2Bearer (BU.fromString accessToken)
      & header "Accept" .~ ["application/json"]

gitlabRestApiUrlBase :: GitlabApi URI
gitlabRestApiUrlBase = do
  urlBase <- asks gitlabUrlBase
  return $ apiPath `relativeTo` urlBase

data ImportFromBitbucketParams = ImportFromBitbucketParams
  { ibbpBitbucketServerName :: String
  , ibbpBitbucketServerUserName :: String
  , ibbpBitbucketPersonalAccesToken :: String
  , ibbpBitbucketProject :: String
  , ibbpBitbucketRepo :: String
  , ibbpGitlabRepo :: Maybe String
  , ibbpGitlabNamespace :: Maybe String
  }
  deriving (Show)
instance ToJSON ImportFromBitbucketParams where
  toJSON o = object
    [ "bitbucket_server_url"      .= ibbpBitbucketServerName o
    , "bitbucket_server_username" .= ibbpBitbucketServerUserName o
    , "personal_access_token"     .= ibbpBitbucketPersonalAccesToken o
    , "bitbucket_server_project"  .= ibbpBitbucketProject o
    , "bitbucket_server_repo"     .= ibbpBitbucketRepo o
    , "new_name"                  .= ibbpGitlabRepo o
    , "target_namespace"          .= ibbpGitlabNamespace o
    ]

contentTypeApplicaionJson :: Options -> Options
contentTypeApplicaionJson = header "Content-Type" .~ ["application/json"]

importBitbucketProject :: ImportFromBitbucketParams -> GitlabApi ()
importBitbucketProject importParams = do
  cfg <- ask
  urlBase_ <- gitlabRestApiUrlBase
  let query_ =
        S.postWith
        (gitlabDefaults cfg & contentTypeApplicaionJson)
        (gitlabSession cfg)
        (show $ toPath ["import", "bitbucket_server"] `relativeTo` urlBase_)
        (encode importParams)
  _ <- liftIO query_
  return ()


newtype GitlabError = GitlabError
  { gitlabErrorMessage :: Maybe String
  }
  deriving (Show)

instance FromJSON GitlabError where
  parseJSON (Object v) = GitlabError <$> v .:? "message"
  parseJSON _ = empty

notFound :: (GitlabError -> Bool) -> (HC.HttpException -> IO (Maybe a))
notFound predicat =
  handler
  where
    handler e@(HC.HttpExceptionRequest _ (HC.StatusCodeException response msg))
      | response ^. responseStatus . statusCode == 404 =
          case decode . Char8.pack . BU.toString $ msg of
            Just err ->
              if predicat err
              then return Nothing
              else throwIO e
            Nothing -> throwIO e
      | otherwise = throwIO e
    handler e = throwIO e

notFoundAndExceptionMessageMatch :: (String -> Bool) -> (HC.HttpException -> IO (Maybe a))
notFoundAndExceptionMessageMatch predicate =
  notFound predicate'
  where
    predicate' err = maybe False predicate (gitlabErrorMessage err)

data GitlabNamespace = GitlabNamespace
  { gitlabNamespaceId :: Int
  , gitlabNamespaceName :: String
  , gitlabNamespacePath :: String
  , gitlabNamespaceFullPath :: String
  , gitlabNamespaceMembersCountWithDescendants :: Int
  }
  deriving (Show)
instance FromJSON GitlabNamespace where
  parseJSON (Object v) =
    GitlabNamespace
    <$> v .: "id"
    <*> v .: "name"
    <*> v .: "path"
    <*> v .: "full_path"
    <*> v .: "members_count_with_descendants"
  parseJSON _ = empty

findNamespace :: String -> GitlabApi (Maybe GitlabNamespace)
findNamespace namespace = do
  cfg <- ask
  urlBase_ <- gitlabRestApiUrlBase
  let query_ =
        S.getWith
        (gitlabDefaults cfg)
        (gitlabSession cfg)
        (show $ toPath ["namespaces", namespace] `relativeTo` urlBase_)
      doIt = (^. responseBody) <$> (asJSON =<< query_)
  liftIO (doIt `E.catch` notFoundAndExceptionMessageMatch ("Namespace Not Found" `isSuffixOf`))

data GitlabRepo = GitlabRepo
  { gitlabRepoId :: Int
  , gitlabRepoName :: String
  , gitlabRepoPath :: String
  , gitlabRepoPathWithNamespace :: String
  , gitlabRepoImportUrl :: Maybe String
  , gitlabRepoImportStatus :: GitlabRepoImportStatus
  , gitlabRepoCreatedAt :: UTCTime
  , gitlabRepoDefaultBranch :: String
  }
  deriving (Show)
instance FromJSON GitlabRepo where
  parseJSON (Object v) =
    GitlabRepo
    <$> v .:  "id"
    <*> v .:  "name"
    <*> v .:  "path"
    <*> v .:  "path_with_namespace"
    <*> v .:? "import_url"
    <*> v .:  "import_status"
    <*> v .:  "created_at"
    <*> v .:  "default_branch"
  parseJSON _ = empty

data GitlabRepoImportStatus = GitlabRepoImportStatusNone
                            | GitlabRepoImportStatusScheduled
                            | GitlabRepoImportStatusStarted
                            | GitlabRepoImportStatusFinished
                            deriving (Show, Eq)
instance FromJSON GitlabRepoImportStatus where
  parseJSON (String "none") = pure GitlabRepoImportStatusNone
  parseJSON (String "scheduled") = pure GitlabRepoImportStatusScheduled
  parseJSON (String "started") = pure GitlabRepoImportStatusStarted
  parseJSON (String "finished") = pure GitlabRepoImportStatusFinished
  parseJSON x = error $ "Unknown import_status: " <> show x

findRepo :: GitlabNamespace -> String -> GitlabApi (Maybe GitlabRepo)
findRepo _ "" = error "Repo name cannot be empty"
findRepo namespace repoName = do
  cfg <- ask
  urlBase_ <- gitlabRestApiUrlBase
  let query_ =
        S.getWith
        (gitlabDefaults cfg)
        (gitlabSession cfg)
        (show $ toPath ["projects", gitlabNamespaceFullPath namespace <> "/" <> repoName] `relativeTo` urlBase_)
      doIt = (^. responseBody) <$> (asJSON =<< query_)
  liftIO (doIt `E.catch` notFoundAndExceptionMessageMatch ("Project Not Found" `isSuffixOf`))

findRepos :: GitlabNamespace -> Maybe [String] -> GitlabApi [GitlabRepo]
findRepos namespace repoNames = do
  cfg <- ask
  urlBase_ <- gitlabRestApiUrlBase
  let perPage = 20 :: Int
      doIt accum page = do
        rs <- (^. responseBody) <$> (asJSON =<< query_)
        let result = accum <> rs
        if length rs == perPage
          then doIt result (succ page)
          else pure result
        where
          query_ =
            S.getWith
            ( gitlabDefaults cfg
              & param "search_namespaces" .~ ["true"]
              & param "search" .~ [fromString (gitlabNamespaceFullPath namespace) <> "/"]
              & param "per_page" .~ [(fromString . show) perPage]
              & param "page" .~ [(fromString . show) (page :: Int)]
            )
            (gitlabSession cfg)
            (show $ toPath ["projects"] `relativeTo` urlBase_)
  repos <- liftIO $ doIt [] 1
  return $
    case repoNames of
      Just rns -> filter (equals rns) repos
      Nothing -> repos
  where
    equals patterns r = any (\s ->
                     (toLower <$> s) == (toLower <$> gitlabRepoName r)
                     ||
                     (toLower <$> s) == (toLower <$> gitlabRepoPath r)
                   ) patterns

deleteRepo :: GitlabRepo -> GitlabApi ()
deleteRepo repo = do
  cfg <- ask
  urlBase_ <- gitlabRestApiUrlBase
  let query_ =
        S.deleteWith
        (gitlabDefaults cfg)
        (gitlabSession cfg)
        (show $ toPath ["projects", show (gitlabRepoId repo)] `relativeTo` urlBase_)
      doIt = query_
  void $ liftIO doIt

data GitlabProtectedBranchAccessLevel
  = GitlabProtectedBranchAccessLevelNoOne
  | GitlabProtectedBranchAccessLevelDevelopersAndMaintainers
  | GitlabProtectedBranchAccessLevelMaintainers
  | GitlabProtectedBranchAccessLevelAdmin
  deriving (Show, Eq)
instance FromJSON GitlabProtectedBranchAccessLevel where
  parseJSON (Number 0)  = pure GitlabProtectedBranchAccessLevelNoOne
  parseJSON (Number 30) = pure GitlabProtectedBranchAccessLevelDevelopersAndMaintainers
  parseJSON (Number 40) = pure GitlabProtectedBranchAccessLevelMaintainers
  parseJSON (Number 60) = pure GitlabProtectedBranchAccessLevelAdmin
  parseJSON x           = error $ "Unknown branch access level: " <> show x
instance ToJSON GitlabProtectedBranchAccessLevel where
  toJSON GitlabProtectedBranchAccessLevelNoOne                    = Number 0
  toJSON GitlabProtectedBranchAccessLevelDevelopersAndMaintainers = Number 30
  toJSON GitlabProtectedBranchAccessLevelMaintainers              = Number 40
  toJSON GitlabProtectedBranchAccessLevelAdmin                    = Number 60

newtype GitlabProtectedBranchAccessLevel' = GitlabProtectedBranchAccessLevel'
  { gitlabProtectedBranchAccessLevel' :: GitlabProtectedBranchAccessLevel
  }
instance FromJSON GitlabProtectedBranchAccessLevel' where
  parseJSON (Object v) =
    GitlabProtectedBranchAccessLevel' <$> v .: "access_level"
  parseJSON _ = empty

type GitlabBranchName = String
type GitlabAllowForcePush = Bool
data GitlabProtectedBranch = GitlabProtectedBranch
  { gitlabProtectedBranchName :: GitlabBranchName
  , gitlabProtectedBranchPushAccessLevels :: [GitlabProtectedBranchAccessLevel]
  , gitlabProtectedBranchMergeAccessLevels :: [GitlabProtectedBranchAccessLevel]
  , gitlabProtectedBranchAllowForcePush :: GitlabAllowForcePush
  } deriving (Show, Eq)
instance FromJSON GitlabProtectedBranch where
  parseJSON (Object v) =
    GitlabProtectedBranch
    <$> v .: "name"
    <*> ((v .: "push_access_levels") <&> (gitlabProtectedBranchAccessLevel' <$>))
    <*> ((v .: "merge_access_levels") <&> (gitlabProtectedBranchAccessLevel' <$>))
    <*> v .: "allow_force_push"
  parseJSON _ = empty

listProtectedBanches :: GitlabRepo -> GitlabApi [GitlabProtectedBranch]
listProtectedBanches repo = do
  cfg <- ask
  urlBase_ <- gitlabRestApiUrlBase
  let perPage = 20 :: Int
      doIt accum page = do
        rs <- (^. responseBody) <$> (asJSON =<< query_)
        let result = accum <> rs
        if length rs == perPage
          then doIt result (succ page)
          else pure result
        where
          query_ =
            S.getWith
            ( gitlabDefaults cfg
              & param "per_page" .~ [(fromString . show) perPage]
              & param "page" .~ [(fromString . show) (page :: Int)]
            )
            (gitlabSession cfg)
            (show $ toPath ["projects", show (gitlabRepoId repo), "protected_branches"] `relativeTo` urlBase_)
  liftIO $ doIt [] 1

deleteProtectedBranch :: GitlabRepo -> GitlabBranchName -> GitlabApi ()
deleteProtectedBranch repo branchName = do
  cfg <- ask
  urlBase_ <- gitlabRestApiUrlBase
  let query_ =
        S.deleteWith
        (gitlabDefaults cfg)
        (gitlabSession cfg)
        (show $ toPath
           [ "projects", show (gitlabRepoId repo)
           , "protected_branches", branchName
           ] `relativeTo` urlBase_)
      doIt = query_
  void $ liftIO doIt

encodeToJSON :: ToJSON a => a  -> Text
encodeToJSON = pack . Char8.unpack . encode . toJSON

createProtectedBranch :: GitlabRepo -> GitlabBranchName -> GitlabProtectedBranchAccessLevel -> GitlabProtectedBranchAccessLevel -> GitlabAllowForcePush -> GitlabApi ()
createProtectedBranch repo branchName pushLevel mergeLevel allowForcePush = do
  cfg <- ask
  urlBase_ <- gitlabRestApiUrlBase
  let query_ =
        S.postWith
          (gitlabDefaults cfg
           & param "name" .~ [fromString branchName]
           & param "push_access_level" .~ [encodeToJSON  pushLevel]
           & param "merge_access_level" .~ [encodeToJSON mergeLevel]
           & param "allow_force_push" .~ [encodeToJSON allowForcePush]
          )
          (gitlabSession cfg)
          (show $ toPath
             [ "projects", show (gitlabRepoId repo)
             , "protected_branches"
             ] `relativeTo` urlBase_)
          Null
      doIt = query_
  void $ liftIO doIt

data GitlabCommit = GitlabCommit
  { gitlabCommitId :: String
  , gitlabCommitCreatedAt :: UTCTime
  , gitlabCommitCommittedDate :: UTCTime
  , gitlabCommitAuthoredDate :: UTCTime
  } deriving (Show)
instance FromJSON GitlabCommit where
  parseJSON (Object v) =
    GitlabCommit
    <$> v .: "id"
    <*> v .: "created_at"
    <*> v .: "committed_date"
    <*> v .: "authored_date"
  parseJSON _ = empty

wasCommitsAfterCreation :: GitlabRepo -> GitlabApi Bool
wasCommitsAfterCreation repo = do
  cfg <- ask
  urlBase_ <- gitlabRestApiUrlBase
  let query_ =
        S.getWith
        (gitlabDefaults cfg
         & param "per_page" .~ [fromString $ show (1 :: Int)]
         & param "all" .~ ["true"]
         & param "since" .~ [fromString . iso8601Show $ gitlabRepoCreatedAt repo]
        )
        (gitlabSession cfg)
        (show $ toPath
           [ "projects", show (gitlabRepoId repo)
           , "repository"
           , "commits"
           ] `relativeTo` urlBase_)
      doIt = not . null <$> ((^. responseBody) <$> (asJSON =<< query_) :: IO [GitlabCommit])
  liftIO doIt

listLastCommits :: GitlabRepo -> Int -> GitlabApi [GitlabCommit]
listLastCommits repo limit = do
  cfg <- ask
  urlBase_ <- gitlabRestApiUrlBase
  let perPage = 20 :: Int
      doIt accum page = do
        let perPage' = if length accum + perPage > limit
                         then limit - length accum
                         else perPage
        rs <- (^. responseBody) <$> (asJSON =<< query_ perPage')
        let result = accum <> reverse rs
        if (length rs == perPage') && (length result < limit)
          then doIt result (succ page)
          else pure result
        where
          query_ perPage' =
            S.getWith
              ( gitlabDefaults cfg
                & param "per_page" .~ [(fromString . show) perPage']
                & param "page" .~ [(fromString . show) (page :: Int)]
                & param "all" .~ ["true"]
              )
              (gitlabSession cfg)
              (show $ toPath
                 [ "projects", show (gitlabRepoId repo)
                 , "repository"
                 , "commits"
                 ] `relativeTo` urlBase_)
  liftIO $ take limit <$> doIt [] 1


anyLastCommit :: GitlabRepo -> Int -> (GitlabCommit -> IO Bool) -> GitlabApi Bool
anyLastCommit repo limit predicat = do
  checkLastCommits repo limit (anyM predicat) True False

checkLastCommits :: GitlabRepo -> Int -> ([GitlabCommit] -> IO Bool) -> Bool -> Bool -> GitlabApi Bool
checkLastCommits repo limit predicat successResult listEndedResult = do
  cfg <- ask
  urlBase_ <- gitlabRestApiUrlBase
  let perPage = 20 :: Int
      doIt page = do
        let perPage' = if (page * perPage) > limit
                         then limit - ((page -1) * perPage)
                         else perPage
        if perPage' == 0
          then return listEndedResult
          else do
            rs <- reverse <$> ((^. responseBody) <$> (asJSON =<< query_ perPage'))
            succeed <- liftIO $ predicat rs
            if succeed
              then return successResult
              else if length rs < perPage'
                     then return listEndedResult
                     else doIt (succ page)
        where
          query_ perPage' = do
            S.getWith
              ( gitlabDefaults cfg
                & param "per_page" .~ [(fromString . show) perPage']
                & param "page" .~ [(fromString . show) (page :: Int)]
                & param "all" .~ ["true"]
              )
              (gitlabSession cfg)
              (show $ toPath
                 [ "projects", show (gitlabRepoId repo)
                 , "repository"
                 , "commits"
                 ] `relativeTo` urlBase_)
  liftIO $ doIt 1


setRepoPath :: GitlabRepo -> String -> GitlabApi ()
setRepoPath repo newPath = do
  cfg <- ask
  urlBase_ <- gitlabRestApiUrlBase
  let query_ =
        S.putWith
          (gitlabDefaults cfg)
          (gitlabSession cfg)
          (show $ toPath
             [ "projects", show (gitlabRepoId repo)
             ] `relativeTo` urlBase_)
          ("path" := renderFormValue newPath)
      doIt = query_
  void $ liftIO doIt
