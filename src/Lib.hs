{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Lib
    ( ApiException(..)
    , import'
    , ImportConfig(..)
    , BitbucketUserName
    , clean
    , CleanConfig(..)
    , list'
    , ListConfig(..)
    , ListMode(..)
    , postProcessing'
    , PostProcessingConfig(..)
    ) where

import GitLab as GL
import BitBucket as BB
import Network.URI
import Control.Monad.Reader
import Control.Monad.State
import Prompt (PromptVariant(..), PromptVariantConfig(..), promptChar', promptYorN)
import Data.Maybe (catMaybes, mapMaybe, fromMaybe, isNothing, isJust, fromJust)
import Utils (showProgess, putStr', putStrLn', ensureEndDot, error')
import Data.Char (toLower, isUpper)
import Data.Map (fromListWith, toList)
import Data.List (stripPrefix, sortOn, isSuffixOf)
import Data.List.Extra (stripSuffix, trim)
import Data.Foldable (find)
import Control.Concurrent (threadDelay)
import Control.Exception (Exception, throwIO)
import Data.Typeable (Typeable)
import Data.Bifunctor (second)

type CommonApi a = ReaderT BitbucketApiConfig (ReaderT GitlabApiConfig IO) a

runBitbucketApi :: BitbucketApi a -> CommonApi a
runBitbucketApi apiAction = do
  config <- ask
  liftIO $ runReaderT apiAction config

runGitlabApi :: GitlabApi a -> CommonApi a
runGitlabApi apiAction = do
  config <- lift ask
  liftIO $ runReaderT apiAction config

data ApiException = NothingToDo String
                  | ActionCanceledByUser String
                  | OtherError String
                  deriving (Show, Typeable)
instance Exception ApiException


data ImportPromptItem = ImportPromptItemYes -- ^ Включить этот репозиторий в обработку
                       | ImportPromptItemNo -- ^ Не включать этот репозиторий в обработку
                       | ImportPromptItemNoAllTheRest -- ^ Включить все оставшиеся в списке репозитории в обработку
                       | ImportPromptItemYesAllTheRest -- ^ Не включать все оставшиеся в списке репозитории в обработку
                       | ImportPromptItemCancel -- ^ Прекратить обработку
                       deriving Eq
instance PromptVariant ImportPromptItem where
  promptVariantChar ImportPromptItemYes           = 'y'
  promptVariantChar ImportPromptItemNo            = 'n'
  promptVariantChar ImportPromptItemNoAllTheRest  = 's'
  promptVariantChar ImportPromptItemYesAllTheRest = 'a'
  promptVariantChar ImportPromptItemCancel        = 'q'

  promptVariantHelp ImportPromptItemYes           = "import this repository"
  promptVariantHelp ImportPromptItemNo            = "don't import this repository"
  promptVariantHelp ImportPromptItemYesAllTheRest = "import all the remaining repositories"
  promptVariantHelp ImportPromptItemNoAllTheRest  = "import selected repositories, skipping all the remaining repositories"
  promptVariantHelp ImportPromptItemCancel        = "cancel migration"

  promptVariantIsAdvanced _                        = False

data ProcessingState = ProcessingStateCommon
                     | ProcessingStateAllRemaining
                     | ProcessingStateSkipRemaining

data ProcessingContext = ProcessingContext
  { processingContextItemNo :: Int
  , processingContextItemsCount :: Int
  , processingContextState :: ProcessingState
  }

filterBitbucketReposByCommandLineOption :: Bool -> Maybe [String] -> BitbucketRepo -> Bool
filterBitbucketReposByCommandLineOption def bitbucketRepoNames repo =
  case bitbucketRepoNames of
    Nothing -> def
    Just rs ->
      let
        check f = elem (toLower <$> f repo) ((toLower <$>) <$> rs)
      in
        check bitbucketRepoName || check bitbucketRepoSlug


filterGitlabReposByCommandLineOption :: Bool -> Maybe [String] -> GitlabRepo -> Bool
filterGitlabReposByCommandLineOption def bitbucketRepoNames repo =
  case bitbucketRepoNames of
    Nothing -> def
    Just rs ->
      let
        check f = elem (toLower <$> f repo) ((toLower <$>) <$> rs)
      in
        check gitlabRepoName || check gitlabRepoPath


withNamespaces :: Maybe String -> String -> String -> (BitbucketProject -> GitlabNamespace -> CommonApi ()) -> CommonApi ()
withNamespaces actionLabel bitbucketProjectName_ gitlabNamespaceName_ action = do
   maybeGitlabNamespace <- runGitlabApi $ GL.findNamespace gitlabNamespaceName_
   case maybeGitlabNamespace of
     Nothing -> liftIO . throwIO . OtherError $ "GitLab namespace `" <> gitlabNamespaceName_ <> "` not found"
     Just gitlabNamespace -> do
       maybeBitbucketProject <- runBitbucketApi $ BB.findProject bitbucketProjectName_
       case maybeBitbucketProject of
         Nothing -> liftIO . throwIO . OtherError $ "BitBucket poject `" <> bitbucketProjectName_ <> "` not found"
         Just bitbucketProject -> do
           forM_ actionLabel (liftIO . putStrLn')
           action bitbucketProject gitlabNamespace

docPartSuffix :: String
docPartSuffix = "_docpart"

isDocPartRepoName :: BitbucketRepo -> Bool
isDocPartRepoName repo =
  docPartSuffix `isSuffixOf` ((toLower <$>) . bitbucketRepoSlug $ repo)

isDocPartRepo :: [BitbucketRepo] -> BitbucketRepo -> Bool
isDocPartRepo allBibucketRepos repo =
  isJust $ findMainRepoForDocPartRepo allBibucketRepos repo

findMainRepoForDocPartRepo :: [BitbucketRepo] -> BitbucketRepo -> Maybe BitbucketRepo
findMainRepoForDocPartRepo allBibucketRepos repo =
  stripSuffix docPartSuffix ((toLower <$>) . bitbucketRepoSlug $ repo)
  >>= \rn -> find (\e -> rn == (toLower <$> bitbucketRepoSlug e)) allBibucketRepos

type BitbucketUserName = String

data ImportConfig = ImportConfig
  { importConfigBitbucketAccessToken :: String
  , importConfigBitbucketUrlBase :: URI
  , importConfigGitlabAccessToken :: String
  , importConfigGitlabUrlBase :: URI
  , importConfigBitbucketProjectName :: String
  , importConfigGitlabNamespaceName :: String
  , importConfigRepoNames :: Maybe [String]
  , importConfigNoRepoNames :: Maybe [String]
  , importConfigBitbucketUserName :: BitbucketUserName
  , importConfigBitbucketSkipDocpart :: Bool
  , importConfigBitbucketOnlyDocpart :: Bool
  , importConfigBitbucketSkipSubtreeDocpart :: Bool
  , importConfigBitbucketOnlySubtreeDocpart :: Bool
  } deriving Show

import' :: ImportConfig -> IO ()
import' cfg = do
  bitbucketCfg <- newBitbucketApiConfig (importConfigBitbucketUrlBase cfg) (importConfigBitbucketAccessToken cfg)
  gitlabCfg <- newGitlabApiConfig (importConfigGitlabUrlBase cfg) (importConfigGitlabAccessToken cfg)
  runReaderT (runReaderT doIt bitbucketCfg) gitlabCfg
  allDoneReport
  where
    doIt :: CommonApi ()
    doIt =
      withNamespaces
        (Just ("Migration of repositories from BitBucket group `" <> importConfigBitbucketProjectName cfg <> "` to GitLab namespace `" <> importConfigGitlabNamespaceName cfg <> "`"))
        (importConfigBitbucketProjectName cfg)
        (importConfigGitlabNamespaceName cfg)
        $ \bitbucketProject gitlabNamespace -> do
          gitlabRepos' <- sortOn gitlabRepoPath <$> runGitlabApi (GL.findRepos gitlabNamespace Nothing)
          bitbucketRepos' <- runBitbucketApi (BB.listProjectRepos bitbucketProject)
          bitbucketReposToImport <-
            filterM
              (\bbr -> do
                  if importConfigBitbucketSkipSubtreeDocpart cfg
                    then isBitbucketSkipSubtreeDocpart bitbucketProject bitbucketRepos' bbr
                    else return True
              )
              (sortOn bitbucketRepoSlug
               . filter (\r -> not (importConfigBitbucketSkipDocpart cfg && isDocPartRepo bitbucketRepos' r))
               . filter (\r -> not (importConfigBitbucketOnlyDocpart cfg) || isDocPartRepo bitbucketRepos' r)
               . filter (not . filterBBSameName gitlabRepos')
               . filter (filterBitbucketReposByCommandLineOption True (importConfigRepoNames cfg))
               . filter (not . filterBitbucketReposByCommandLineOption False (importConfigNoRepoNames cfg))
               $ bitbucketRepos')
            >>=
            filterM
              (\bbr -> do
                  if importConfigBitbucketOnlySubtreeDocpart cfg
                    then isBitbucketOnlySubtreeDocpart bitbucketProject bitbucketRepos' bbr
                    else return True
              )

          case bitbucketReposToImport of
            [] -> liftIO . throwIO . NothingToDo $ "No BitBucket repository was found that meets the selection criteria"
            _ -> do
              reposToImport <-
                fmap catMaybes $
                  flip evalStateT (ProcessingContext 0 (length bitbucketReposToImport) ProcessingStateCommon) $
                    prompt `mapM` bitbucketReposToImport

              when (null reposToImport) $ liftIO . throwIO . NothingToDo $ "Nothing selected"

              do
                t <- liftIO $ case reposToImport of
                  [_] -> promptYorN "Shall I import ONE selected repository"
                  _ -> promptYorN $ "Shall I import " <> show (length reposToImport) <> " selected repositories"
                unless t $ liftIO . throwIO . ActionCanceledByUser $ "Migration canceled by user"

              forM_ reposToImport $ \repo -> do
                let importParams = ImportFromBitbucketParams
                      { ibbpBitbucketServerName = show $ importConfigBitbucketUrlBase cfg
                      , ibbpBitbucketServerUserName = importConfigBitbucketUserName cfg
                      , ibbpBitbucketPersonalAccesToken = importConfigBitbucketAccessToken cfg
                      , ibbpBitbucketProject = bitbucketProjectKey bitbucketProject
                      , ibbpBitbucketRepo = bitbucketRepoName repo
                      , ibbpGitlabNamespace = Just $ gitlabNamespaceFullPath gitlabNamespace
                      , ibbpGitlabRepo = Nothing
                      }
                runGitlabApi $ GL.importBitbucketProject importParams
                liftIO $ putStrLn' $ "Import of repository `" <> bitbucketProjectName bitbucketProject <> "/" <> bitbucketRepoName repo <> "` was started"

              waitForImportComplete bitbucketProject gitlabNamespace reposToImport

      where
        prompt repo = do
          incrementNo
          ProcessingContext n c s <- get
          case s of
            ProcessingStateAllRemaining -> return (Just repo)
            ProcessingStateSkipRemaining -> return Nothing
            ProcessingStateCommon -> do
              let promptConfig = PromptVariantConfig
                    { promptVariantConfigPrompt = "Shall I import repo `" <> bitbucketRepoName repo <> "`? (" <> show n <> "/" <> show c <> ")"
                    , promptVariantConfigItems =
                        [ ImportPromptItemYes
                        , ImportPromptItemNo
                        , ImportPromptItemYesAllTheRest
                        , ImportPromptItemNoAllTheRest
                        , ImportPromptItemCancel]
                    , promptVariantConfigDefault = Just ImportPromptItemNo
                    }
              promptResult <- liftIO $ promptChar' promptConfig
              case promptResult of
                ImportPromptItemYes           -> return $ Just repo
                ImportPromptItemNo            -> return Nothing
                ImportPromptItemYesAllTheRest -> putNewState ProcessingStateAllRemaining  >> return (Just repo)
                ImportPromptItemNoAllTheRest  -> putNewState ProcessingStateSkipRemaining >> return Nothing
                ImportPromptItemCancel        -> liftIO . throwIO $ ActionCanceledByUser "Migration canceled by user"
          where
            incrementNo = modify $ \p -> p { processingContextItemNo = 1 + processingContextItemNo p }
            putNewState s = modify $ \p -> p { processingContextState = s }


waitForImportComplete :: BitbucketProject -> GitlabNamespace -> [BitbucketRepo] -> CommonApi ()
waitForImportComplete bitbucketProject gitlabNamespace bitbucketProjects = do
  wait bitbucketProjects True
  where
    wait [] _ = return ()
    wait bbProjects firstCall = do
      importedGitlabRepos <-
        filter (\r -> any (`isTheSameRepos` r) bbProjects)
        . filter (\r -> gitlabRepoImportStatus r == GitlabRepoImportStatusFinished)
        <$> runGitlabApi (GL.findRepos gitlabNamespace Nothing)
      case importedGitlabRepos of
        [] -> do
          when firstCall $ liftIO $ putStrLn' $ "Waiting for the completion of importing repositories " <> showProgess (length bbProjects) (length bitbucketProjects) <> "..."
          liftIO $ threadDelay 5000000
          wait bbProjects False
        _ -> do
          mapM_ postImportAction importedGitlabRepos
          let rest = filter (\p -> (not . any (isTheSameRepos p)) importedGitlabRepos) bbProjects
          wait rest True
      where
        postImportAction glRepo = do
          let bbRepo = head $ filter (`isTheSameRepos` glRepo) bbProjects
          liftIO $ putStrLn' $ "Additional import actions for `" <> gitlabRepoName glRepo <> "` repository..."
          importBranchPermissions bitbucketProject bbRepo glRepo
          correctRepoPath glRepo
          liftIO $ putStrLn' $ "Additional import actions for `" <> gitlabRepoName glRepo <> "` repository. Done."


importBranchPermissions :: BitbucketProject -> BitbucketRepo -> GitlabRepo -> CommonApi ()
importBranchPermissions bitbucketProject bitbucketRepo gitlabRepo = do
  maybeBitbucketBranchPermissions <- runBitbucketApi (BB.listBranchPermisions bitbucketProject bitbucketRepo)
  liftIO $ putStr' $ "Import branch permissions for `" <> gitlabRepoName gitlabRepo <> "`..."
  case maybeBitbucketBranchPermissions of
    Right branchPermissions -> do
      delCnt <- deleteAutoCreatedBranches =<< runGitlabApi (GL.listProtectedBanches gitlabRepo)
      gitlabBranches <- runGitlabApi $ GL.listProtectedBanches gitlabRepo
      let
        bitbucketBranchesParts = filter isMigratableBranchPermission branchPermissions
        onlyAbsent =
          let
            bitbucketBranches :: [(String, [BitbucketBranchPermission])]
            bitbucketBranches = (toList . fromListWith (<>)) $ (\p -> (branchName' p, [p])) <$> bitbucketBranchesParts
          in (\bb -> not (any (\gl -> gitlabProtectedBranchName gl == fst bb) gitlabBranches)) `filter` bitbucketBranches
      
      cnt <- length <$> mapM migrateBranch onlyAbsent
      liftIO $ putStrLn' $
        case (cnt, delCnt) of
          (0, 0) -> " Nothing to do."
          _ -> " Done:"
               <> if cnt > 0 then " " <> show cnt <> " branch permission(s) was created." else ""
               <> if delCnt > 0 then " " <> show delCnt <> " auto created branch permission(s) was deleted." else ""
    Left msg -> do
      let msg' = trim msg
      liftIO $ putStrLn' $ " Skipped: Cannot get branch permissions for BitBucket repository `" <> bitbucketRepoName bitbucketRepo <> "`: "
                           <> ensureEndDot msg'
  where
    branchPrefix = "refs/heads/"
    branchName' bp =
      let
        matcherId = (bitbucketBranchPermissionMatcherId . bitbucketBranchPermissionMatcher) bp
      in
        case (bitbucketBranchPermissionMatcherType . bitbucketBranchPermissionMatcher) bp of
          BitbucketBranchPermissionMatcherTypeBranch -> fromMaybe matcherId (stripPrefix branchPrefix matcherId)
          _ -> matcherId

    isMigratableBranchPermission bp =
      isActive && isMatchTypeFit && not (isIgnored bp)
      where
        isActive = bitbucketBranchPermissionMatcherActive . bitbucketBranchPermissionMatcher $ bp
        isMatchTypeFit =
          case (bitbucketBranchPermissionMatcherType . bitbucketBranchPermissionMatcher) bp of
            BitbucketBranchPermissionMatcherTypeBranch -> True
            BitbucketBranchPermissionMatcherTypePattern -> True
            _ -> False

        -- Игнорируем полный запрет на изменение любых веток. Это может быть признаком того,
        -- что репозиторий переведён в режим Read Only после импорта.
        isIgnored (BitbucketBranchPermission
                    _
                    BitbucketBranchPermissionTypeReadOnly
                    (BitbucketBranchPermissionMatcher
                      "*"
                      BitbucketBranchPermissionMatcherTypePattern
                      _)) = True
        isIgnored _ = False

    deleteAutoCreatedBranches :: [GitlabProtectedBranch] -> CommonApi Int
    deleteAutoCreatedBranches gitlabBranches = do
      let autoCreatedProtectedBranch =
            GitlabProtectedBranch
              (gitlabRepoDefaultBranch gitlabRepo)
              [GitlabProtectedBranchAccessLevelMaintainers]
              [GitlabProtectedBranchAccessLevelMaintainers]
              False
      length <$> forM
        (filter (== autoCreatedProtectedBranch) gitlabBranches)
        (runGitlabApi . GL.deleteProtectedBranch gitlabRepo . gitlabProtectedBranchName)

    migrateBranch :: (String, [BitbucketBranchPermission]) -> CommonApi ()
    migrateBranch (branchName, permissions) = do
      let (pushLevel, mergeLevel, allowForcePush) = convertPermissions
      runGitlabApi $ GL.createProtectedBranch gitlabRepo branchName pushLevel mergeLevel allowForcePush
      where
        convertPermissions
          | any' BitbucketBranchPermissionTypeReadOnly =
              (GitlabProtectedBranchAccessLevelNoOne, GitlabProtectedBranchAccessLevelNoOne, False)
          | any' BitbucketBranchPermissionTypePullRequestOnly =
              (GitlabProtectedBranchAccessLevelNoOne, GitlabProtectedBranchAccessLevelDevelopersAndMaintainers, False)
          | any' BitbucketBranchPermissionTypeFastForwardOnly =
              (GitlabProtectedBranchAccessLevelDevelopersAndMaintainers, GitlabProtectedBranchAccessLevelDevelopersAndMaintainers, False)
          | otherwise =
              (GitlabProtectedBranchAccessLevelDevelopersAndMaintainers, GitlabProtectedBranchAccessLevelDevelopersAndMaintainers, True)
          where
            any' permType = any (\p -> permType == bitbucketBranchPermissionType p) permissions

allDoneReport :: IO ()
allDoneReport = putStrLn' "All done"

correctRepoPath :: GitlabRepo -> CommonApi ()
correctRepoPath repo =
  let
    path' = gitlabRepoPath repo
    newPath = toLower <$> path'
  in
    when (any isUpper path')
    $ do
        liftIO $ putStr' $ "Normalize `" <> gitlabRepoName repo <> "` repository path: `" <> path' <> "`->`" <> newPath <> "`..."
        runGitlabApi (GL.setRepoPath repo newPath)
        liftIO $ putStrLn' " Done."

findTheSameByName :: [BitbucketRepo] -> GitlabRepo -> Maybe BitbucketRepo
findTheSameByName bitbucketRepos gitlabRepo =
  find (`isTheSameRepos` gitlabRepo) bitbucketRepos

findTheSameByName' :: [GitlabRepo] -> BitbucketRepo -> Maybe GitlabRepo
findTheSameByName' gitlabRepos bitbucketRepo =
  find (bitbucketRepo `isTheSameRepos`) gitlabRepos

filterBBSameName :: [GitlabRepo] -> BitbucketRepo -> Bool
filterBBSameName gitlabRepos br =
  any (isTheSameRepos br) gitlabRepos

isTheSameRepos :: BitbucketRepo -> GitlabRepo -> Bool
isTheSameRepos bitbucketRepo gitlabRepo =
  (toLower <$> bitbucketRepoSlug bitbucketRepo) == (toLower <$> gitlabRepoPath gitlabRepo)


data DeletePromptItem = DeletePromptItemYes -- ^ Удалить этот репозиторий
                      | DeletePromptItemNo -- ^ Не удалять этот репозиторий
                      | DeletePromptItemNoAllTheRest -- ^ Удалить все оставшиеся в списке репозитории
                      | DeletePromptItemYesAllTheRest -- ^ Не удалять все оставшиеся в списке репозитории
                      | DeletePromptItemCancel -- ^ Прекратить обработку
                       deriving Eq
instance PromptVariant DeletePromptItem where
  promptVariantChar DeletePromptItemYes           = 'y'
  promptVariantChar DeletePromptItemNo            = 'n'
  promptVariantChar DeletePromptItemNoAllTheRest  = 's'
  promptVariantChar DeletePromptItemYesAllTheRest = 'a'
  promptVariantChar DeletePromptItemCancel        = 'q'

  promptVariantHelp DeletePromptItemYes           = "delete this repository"
  promptVariantHelp DeletePromptItemNo            = "don't delete this repository"
  promptVariantHelp DeletePromptItemYesAllTheRest = "delete all the remaining repositories"
  promptVariantHelp DeletePromptItemNoAllTheRest  = "delete selected repositories, skipping all the remaining repositories"
  promptVariantHelp DeletePromptItemCancel        = "cancel deletion"

  promptVariantIsAdvanced _                        = False

data CleanConfig = CleanConfig
  { cleanConfigBitbucketAccessToken :: String
  , cleanConfigBitbucketUrlBase :: URI
  , cleanConfigGitlabAccessToken :: String
  , cleanConfigGitlabUrlBase :: URI
  , cleanConfigBitbucketProjectName :: String
  , cleanConfigGitlabNamespaceName :: String
  , cleanConfigRepoNames :: Maybe [String]
  , cleanConfigNoRepoNames :: Maybe [String]
  , cleanConfigAllowChanged :: Bool
  } deriving Show

clean :: CleanConfig -> IO ()
clean o = do
  bitbucketCfg <- newBitbucketApiConfig (cleanConfigBitbucketUrlBase o) (cleanConfigBitbucketAccessToken o)
  gitlabCfg <- newGitlabApiConfig (cleanConfigGitlabUrlBase o) (cleanConfigGitlabAccessToken o)
  runReaderT (runReaderT doIt bitbucketCfg) gitlabCfg
  allDoneReport
  where
    doIt :: CommonApi ()
    doIt =
      withNamespaces
        (Just ("Deletion of GitLab namespace `" <> cleanConfigGitlabNamespaceName o <> "` repositories that can be (or were) imported from BitBucket project `" <> cleanConfigBitbucketProjectName o <> "`"))
        (cleanConfigBitbucketProjectName o) (cleanConfigGitlabNamespaceName o) $
        \bitbucketProject gitlabNamespace -> do
          bitbucketRepos <- runBitbucketApi (BB.listProjectRepos bitbucketProject)
          case bitbucketRepos of
            [] -> liftIO . throwIO . NothingToDo $ "No BitBucket repository was found that meets the selection criteria"
            _ -> do
              reposToDelete <- do
                gitlabRepos <- do
                  gitlabRepos' <-
                    sortOn (gitlabRepoPath . fst)
                    . filter (filterGitlabReposByCommandLineOption True (cleanConfigRepoNames o) . fst)
                    . filter (not . filterGitlabReposByCommandLineOption False (cleanConfigNoRepoNames o) . fst)
                    . mapMaybe (\glr -> findTheSameByName bitbucketRepos glr >>= \bbr -> Just (glr, bbr))
                    <$> runGitlabApi (GL.findRepos gitlabNamespace Nothing)
                  if cleanConfigAllowChanged o
                    then return gitlabRepos'
                    else do
                         liftIO $ putStr' "Check whether the GitLab repositories have been modified after import. It may take some time..."
                         rs <- filterM (\(glr,bbr) -> not <$> wasGitlabRepoChangedSinceImported glr bitbucketProject bbr) gitlabRepos'
                         liftIO $ putStrLn' " Done."
                         return rs
                when (null gitlabRepos) $ liftIO . throwIO . NothingToDo $ "No GitLab repository was found that meets the selection criteria"

                fmap catMaybes
                  $ flip evalStateT
                      (ProcessingContext 0 (length gitlabRepos) ProcessingStateCommon)
                      $ (prompt . fst) `mapM` gitlabRepos

              when (null reposToDelete) $ liftIO . throwIO . NothingToDo $ "Nothing selected"
              do
                t <- liftIO $ case reposToDelete of
                  [_] -> promptYorN "Shall I delete one selected repository"
                  _ -> promptYorN $ "Shall I delete " <> show (length reposToDelete) <> " selected repositories"
                unless t $ liftIO . throwIO . ActionCanceledByUser $ "Clearing canceled by user"

              forM_ reposToDelete $ \repo -> do
                runGitlabApi $ GL.deleteRepo repo
                liftIO $ putStrLn' $ "Repository `" <> gitlabNamespaceName gitlabNamespace <> "/" <> gitlabRepoName repo <> "` was deleted"
      where
        prompt gitlabRepo = do
          incrementNo
          ProcessingContext n c s <- get
          case s of
            ProcessingStateAllRemaining -> return (Just gitlabRepo)
            ProcessingStateSkipRemaining -> return Nothing
            ProcessingStateCommon -> do
              let promptConfig = PromptVariantConfig
                    { promptVariantConfigPrompt = "Shall I delete repo `" <> gitlabRepoName gitlabRepo <> "`? (" <> show n <> "/" <> show c <> ")"
                    , promptVariantConfigItems =
                        [ DeletePromptItemYes
                        , DeletePromptItemNo
                        , DeletePromptItemYesAllTheRest
                        , DeletePromptItemNoAllTheRest
                        , DeletePromptItemCancel
                        ]
                    , promptVariantConfigDefault = Just DeletePromptItemNo
                    }
              promptResult <- liftIO $ promptChar' promptConfig
              case promptResult of
                DeletePromptItemYes           -> return $ Just gitlabRepo
                DeletePromptItemNo            -> return Nothing
                DeletePromptItemYesAllTheRest -> putNewState ProcessingStateAllRemaining  >> return (Just gitlabRepo)
                DeletePromptItemNoAllTheRest  -> putNewState ProcessingStateSkipRemaining >> return Nothing
                DeletePromptItemCancel        -> liftIO . throwIO $ ActionCanceledByUser "Clearing canceled by user"
          where
            incrementNo = modify $ \p -> p { processingContextItemNo = 1 + processingContextItemNo p }
            putNewState s = modify $ \p -> p { processingContextState = s }


-- | Определяет были ли в указанном GitLab репозитории изменения после импорта этого репозитория из BitBucket.
wasGitlabRepoChangedSinceImported :: GitlabRepo -> BitbucketProject -> BitbucketRepo -> CommonApi Bool
wasGitlabRepoChangedSinceImported gitlabRepo@GitlabRepo{gitlabRepoImportStatus = GitlabRepoImportStatusNone} _ _ =
  liftIO . throwIO . OtherError $ "Repository `" <> gitlabRepoName gitlabRepo <> "` has never been imported"
wasGitlabRepoChangedSinceImported gitlabRepo@GitlabRepo{gitlabRepoImportStatus = GitlabRepoImportStatusScheduled} _ _ =
  liftIO . throwIO . OtherError $ "The import of repository `" <> gitlabRepoName gitlabRepo <> "` is not finished yet"
wasGitlabRepoChangedSinceImported gitlabRepo@GitlabRepo{gitlabRepoImportStatus = GitlabRepoImportStatusStarted} _ _ =
  liftIO . throwIO . OtherError $ "The import of repository `" <> gitlabRepoName gitlabRepo <> "` is not finished yet"
wasGitlabRepoChangedSinceImported gitlabRepo bitbucketProject bitbucketRepo = do
  config <- ask
  runGitlabApi $ anyLastCommit gitlabRepo 20 (isCommitPesentInBitbucket config)
  where
    isCommitPesentInBitbucket config c =
      isNothing <$> runReaderT (BB.findCommit bitbucketProject bitbucketRepo (gitlabCommitId c)) config

data ListMode = ListImported
              | ListNotImpoted
              deriving (Show, Eq)
data ListConfig = ListConfig
  { listConfigBitbucketAccessToken :: String
  , listConfigBitbucketUrlBase :: URI
  , listConfigGitlabAccessToken :: String
  , listConfigGitlabUrlBase :: URI
  , listConfigBitbucketProjectName :: String
  , listConfigGitlabNamespaceName :: String
  , listConfigRepoNames :: Maybe [String]
  , listConfigNoRepoNames :: Maybe [String]
  , listConfigListMode :: ListMode
  , listConfigOnlyChanged :: Bool
  , listConfigSkipChanged :: Bool
  , listConfigBitbucketSkipDocpart :: Bool
  , listConfigBitbucketOnlyDocpart :: Bool
  , listConfigBitbucketSkipSubtreeDocpart :: Bool
  , listConfigBitbucketOnlySubtreeDocpart :: Bool
  } deriving Show

list' :: ListConfig -> IO ()
list' cfg = do
  bitbucketCfg <- newBitbucketApiConfig (listConfigBitbucketUrlBase cfg) (listConfigBitbucketAccessToken cfg)
  gitlabCfg <- newGitlabApiConfig (listConfigGitlabUrlBase cfg) (listConfigGitlabAccessToken cfg)
  runReaderT (runReaderT doIt bitbucketCfg) gitlabCfg
  where
    doIt :: CommonApi ()
    doIt =
      withNamespaces
        Nothing
        (listConfigBitbucketProjectName cfg) (listConfigGitlabNamespaceName cfg) $
        \bitbucketProject gitlabNamespace ->
          case listConfigListMode cfg of
            ListImported -> do
              bitbucketRepos' <- runBitbucketApi (BB.listProjectRepos bitbucketProject)
              imported <-
                runGitlabApi (GL.findRepos gitlabNamespace Nothing)
                >>=
                filterM
                  (\(glr, bbr) -> do
                       wasChanged <- wasGitlabRepoChangedSinceImported glr bitbucketProject bbr
                       return $
                         (not (listConfigOnlyChanged cfg) || wasChanged)
                         &&
                         (not (listConfigSkipChanged cfg) || not wasChanged))
                    . sortOn (gitlabRepoPath . fst)
                    . filter (filterGitlabReposByCommandLineOption True (listConfigRepoNames cfg) . fst)
                    . filter (not . filterGitlabReposByCommandLineOption False (listConfigNoRepoNames cfg) . fst)
                    . mapMaybe (\glr -> findTheSameByName bitbucketRepos' glr >>= \bbr -> Just (glr, bbr))
                >>=
                filterM
                  (\(_, bbr) ->
                      if listConfigBitbucketOnlySubtreeDocpart cfg
                        then isBitbucketOnlySubtreeDocpart bitbucketProject bitbucketRepos' bbr
                        else return True
                  )
                >>=
                filterM
                  (\(_, bbr) ->
                      if listConfigBitbucketSkipSubtreeDocpart cfg
                        then isBitbucketSkipSubtreeDocpart bitbucketProject bitbucketRepos' bbr
                        else return True
                  )

              liftIO $ forM_ imported $ putStrLn' . gitlabRepoPath . fst
            ListNotImpoted -> do
              gitlabRepos' <- sortOn gitlabRepoPath <$> runGitlabApi (GL.findRepos gitlabNamespace Nothing)
              bitbucketRepos' <- runBitbucketApi (BB.listProjectRepos bitbucketProject)
              notImported <-
                filterM
                  (\bbr ->
                      if listConfigBitbucketOnlySubtreeDocpart cfg
                        then isBitbucketOnlySubtreeDocpart bitbucketProject bitbucketRepos' bbr
                        else return True
                  )
                  (sortOn bitbucketRepoSlug
                  . filter (\bbr -> not (listConfigBitbucketSkipDocpart cfg && isDocPartRepo bitbucketRepos' bbr))
                  . filter (\bbr -> not (listConfigBitbucketOnlyDocpart cfg) || isDocPartRepo bitbucketRepos' bbr)
                  . filter (filterBitbucketReposByCommandLineOption True (listConfigRepoNames cfg))
                  . filter (not . filterBitbucketReposByCommandLineOption False (listConfigNoRepoNames cfg))
                  . filter (not . filterBBSameName gitlabRepos')
                  $ bitbucketRepos')
                >>=
                filterM
                  (\bbr ->
                      if listConfigBitbucketSkipSubtreeDocpart cfg
                        then isBitbucketSkipSubtreeDocpart bitbucketProject bitbucketRepos' bbr
                        else return True
                  )
              liftIO $ forM_ notImported $ putStrLn' . bitbucketRepoSlug


isBitbucketOnlySubtreeDocpart :: BitbucketProject -> [BitbucketRepo] -> BitbucketRepo -> CommonApi Bool
isBitbucketOnlySubtreeDocpart bitbucketProject bitbucketRepos' repo = do
  case findMainRepoForDocPartRepo bitbucketRepos' repo of
    Nothing -> return False
    Just mainRepo -> do
      maybeCommit <- runBitbucketApi $ BB.findFirstCommit bitbucketProject repo
      case maybeCommit of
        Nothing -> return False
        Just commit -> isJust <$> runBitbucketApi (BB.findCommit bitbucketProject mainRepo (bitbucketCommitId commit))

isBitbucketSkipSubtreeDocpart :: BitbucketProject -> [BitbucketRepo] -> BitbucketRepo -> CommonApi Bool
isBitbucketSkipSubtreeDocpart bitbucketProject bitbucketRepos' repo = do
  case findMainRepoForDocPartRepo bitbucketRepos' repo of
    Nothing -> return True
    Just mainRepo -> do
      maybeCommit <- runBitbucketApi $ BB.findFirstCommit bitbucketProject repo
      case maybeCommit of
        Nothing -> return True
        Just commit -> isNothing <$> runBitbucketApi (BB.findCommit bitbucketProject mainRepo (bitbucketCommitId commit))


data PostProcessingConfig = PostProcessingConfig
  { postProcessingConfigBitbucketAccessToken :: String
  , postProcessingConfigBitbucketUrlBase :: URI
  , postProcessingConfigGitlabAccessToken :: String
  , postProcessingConfigGitlabUrlBase :: URI
  , postProcessingConfigBitbucketProjectName :: String
  , postProcessingConfigGitlabNamespaceName :: String
  , postProcessingConfigRepoNames :: Maybe [String]
  , postProcessingConfigNoRepoNames :: Maybe [String]
  , postProcessingConfigAll :: Bool
  , postProcessingConfigImportBranchPermissions :: Bool
  , postProcessingConfigCorrectRepoPath :: Bool
  } deriving Show

postProcessing' :: PostProcessingConfig -> IO ()
postProcessing' cfg = do
  checkActions
  bitbucketCfg <- newBitbucketApiConfig (postProcessingConfigBitbucketUrlBase cfg) (postProcessingConfigBitbucketAccessToken cfg)
  gitlabCfg <- newGitlabApiConfig (postProcessingConfigGitlabUrlBase cfg) (postProcessingConfigGitlabAccessToken cfg)
  runReaderT (runReaderT doIt bitbucketCfg) gitlabCfg
  allDoneReport
  where
    checkActions = do
      unless ( postProcessingConfigAll cfg
               || postProcessingConfigImportBranchPermissions cfg
               || postProcessingConfigCorrectRepoPath cfg
             )
        $ liftIO $ error' "No action is specified for post-processing"

    doIt :: CommonApi ()
    doIt =
      withNamespaces
        (Just ("Applying extra settings to existing repositories of `" <> postProcessingConfigGitlabNamespaceName cfg <> "` GitLab namespace that were imported from `" <> postProcessingConfigBitbucketProjectName cfg <> "` BitBucket project"))
        (postProcessingConfigBitbucketProjectName cfg)
        (postProcessingConfigGitlabNamespaceName cfg)
        $ \bitbucketProject gitlabNamespace -> do
          gitlabRepos' <- sortOn gitlabRepoPath <$> runGitlabApi (GL.findRepos gitlabNamespace Nothing)
          bitbucketRepos' <- runBitbucketApi (BB.listProjectRepos bitbucketProject)
          reposToProcess <-
            let
              reposToProcess' =
                sortOn (bitbucketRepoSlug . fst)
                . filter (filterBitbucketReposByCommandLineOption True (postProcessingConfigRepoNames cfg) . fst)
                . filter (not . filterBitbucketReposByCommandLineOption False (postProcessingConfigNoRepoNames cfg) . fst)
                . map (second fromJust)
                . filter (isJust . snd)
                . map (\bbRepo -> (bbRepo, find (isTheSameRepos bbRepo) gitlabRepos'))
                $ bitbucketRepos'
            in
              fmap catMaybes
                $ flip evalStateT
                    (ProcessingContext 0 (length reposToProcess') ProcessingStateCommon)
                    $ (\(b,g) -> ((b,) <$>) <$> prompt g) `mapM` reposToProcess'
          let
             processOneRepo (bbRepo, glRepo) = do
              let allActions = postProcessingConfigAll cfg

              liftIO $ putStrLn' $ "Post-processing `" <> gitlabRepoName glRepo <> "` repository..."

              when (allActions || postProcessingConfigImportBranchPermissions cfg)
                $ importBranchPermissions bitbucketProject bbRepo glRepo

              when (allActions || postProcessingConfigCorrectRepoPath cfg)
                $ correctRepoPath glRepo

              liftIO $ putStrLn' $ "Post-processing `" <> gitlabRepoName glRepo <> "` repository. Done."

          when (null reposToProcess) $ liftIO . throwIO . NothingToDo $ "Nothing selected"
          do
            t <- liftIO $ case reposToProcess of
              [_] -> promptYorN "Shall I post-process one selected repository"
              _ -> promptYorN $ "Shall I post-process " <> show (length reposToProcess) <> " selected repositories"
            unless t $ liftIO . throwIO . ActionCanceledByUser $ "Post-processing canceled by user"
          
          mapM_ processOneRepo reposToProcess
      where
        prompt gitlabRepo = do
          incrementNo
          ProcessingContext n c s <- get
          case s of
            ProcessingStateAllRemaining -> return (Just gitlabRepo)
            ProcessingStateSkipRemaining -> return Nothing
            ProcessingStateCommon -> do
              let promptConfig = PromptVariantConfig
                    { promptVariantConfigPrompt = "Shall I post-process repo `" <> gitlabRepoName gitlabRepo <> "`? (" <> show n <> "/" <> show c <> ")"
                    , promptVariantConfigItems =
                        [ PostProcessPromptItemYes
                        , PostProcessPromptItemNo
                        , PostProcessPromptItemYesAllTheRest
                        , PostProcessPromptItemNoAllTheRest
                        , PostProcessPromptItemCancel
                        ]
                    , promptVariantConfigDefault = Just PostProcessPromptItemNo
                    }
              promptResult <- liftIO $ promptChar' promptConfig
              case promptResult of
                PostProcessPromptItemYes           -> return $ Just gitlabRepo
                PostProcessPromptItemNo            -> return Nothing
                PostProcessPromptItemYesAllTheRest -> putNewState ProcessingStateAllRemaining  >> return (Just gitlabRepo)
                PostProcessPromptItemNoAllTheRest  -> putNewState ProcessingStateSkipRemaining >> return Nothing
                PostProcessPromptItemCancel        -> liftIO . throwIO $ ActionCanceledByUser "Post-processing canceled by user"
          where
            incrementNo = modify $ \p -> p { processingContextItemNo = 1 + processingContextItemNo p }
            putNewState s = modify $ \p -> p { processingContextState = s }

data PostProcessPromptItem = PostProcessPromptItemYes -- ^ Обработать этот репозиторий
                           | PostProcessPromptItemNo -- ^ Не обрабатывать этот репозиторий
                           | PostProcessPromptItemNoAllTheRest -- ^ Обработать все оставшиеся в списке репозитории
                           | PostProcessPromptItemYesAllTheRest -- ^ Не обрабатывать все оставшиеся в списке репозитории
                           | PostProcessPromptItemCancel -- ^ Прекратить обработку
                           deriving Eq
instance PromptVariant PostProcessPromptItem where
  promptVariantChar PostProcessPromptItemYes           = 'y'
  promptVariantChar PostProcessPromptItemNo            = 'n'
  promptVariantChar PostProcessPromptItemNoAllTheRest  = 's'
  promptVariantChar PostProcessPromptItemYesAllTheRest = 'a'
  promptVariantChar PostProcessPromptItemCancel        = 'q'

  promptVariantHelp PostProcessPromptItemYes           = "post-process this repository"
  promptVariantHelp PostProcessPromptItemNo            = "don't post-process this repository"
  promptVariantHelp PostProcessPromptItemYesAllTheRest = "post-process all the remaining repositories"
  promptVariantHelp PostProcessPromptItemNoAllTheRest  = "post-process selected repositories, skipping all the remaining repositories"
  promptVariantHelp PostProcessPromptItemCancel        = "cancel post-processing"

  promptVariantIsAdvanced _                        = False
