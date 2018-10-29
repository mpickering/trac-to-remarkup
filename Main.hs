{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Data.Char
import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Error.Class
import Control.Monad.Trans.Class
import Control.Monad.Reader.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Concurrent
import Control.Concurrent.Async (mapConcurrently_)
import Data.Default (def)
import Data.Foldable
import Data.Function
import Data.Functor.Identity
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.IO
import System.Directory

import Database.PostgreSQL.Simple
import Network.HTTP.Client.TLS as TLS
import Network.Connection (TLSSettings(..))
import Network.HTTP.Types.Status
import Servant.Client

import GitLab.Tickets
import GitLab.Common
import GitLab.Project
import GitLab.UploadFile
import GitLab.Users
import qualified Trac.Web
import Trac.Db as Trac
import Trac.Db.Types as Trac
import qualified Trac.Convert

project :: ProjectId
project = ProjectId 1

tlsSettings :: TLSSettings
tlsSettings = def { settingDisableCertificateValidation = True }

gitlabToken :: AccessToken
gitlabBaseUrl :: BaseUrl

-- staging
--gitlabBaseUrl = BaseUrl Https "gitlab.ghc.smart-cactus.org" 443 "/api/v4"
--gitlabToken = "zZChwxtdAZ4TS52FWw4K"

-- ben-server
gitlabBaseUrl = BaseUrl Https "gitlab.home.smart-cactus.org" 443 "/api/v4"
gitlabToken = "1CfgUx6ox3wBf11z1KWT"

tracBaseUrl :: BaseUrl
tracBaseUrl = BaseUrl Https "ghc.haskell.org" 443 "/trac/ghc"

type MilestoneMap = M.Map Text MilestoneId

openStateFile :: forall a. (Ord a, Read a, Show a)
              => FilePath -> IO (S.Set a, a -> IO ())
openStateFile stateFile = do
    stateFileExists <- doesFileExist stateFile
    !finished <-
        if stateFileExists
        then S.fromList . map read . lines <$> readFile stateFile
        else return mempty

    stateFile <- openFile stateFile AppendMode
    hSetBuffering stateFile LineBuffering
    let finishItem :: a -> IO ()
        finishItem = hPutStrLn stateFile . show
    return (finished, finishItem)


attachmentStateFile :: FilePath
attachmentStateFile = "attachments.state"

ticketStateFile :: FilePath
ticketStateFile = "tickets.state"

main :: IO ()
main = do
    conn <- connectPostgreSQL ""
    mgr <- TLS.newTlsManagerWith $ TLS.mkManagerSettings tlsSettings Nothing
    let env = mkClientEnv mgr gitlabBaseUrl
    getUserId <- mkUserIdOracle env
    milestoneMap <- either (error . show) id <$> runClientM (makeMilestones conn) env
    (finishedTickets, finishTicket) <- openStateFile ticketStateFile
    tickets <- filter (\t -> not $ ticketNumber t `S.member` finishedTickets)
               <$> Trac.getTickets conn
    let makeTickets' ts = do
            let finishTicket' = finishTicket . ticketNumber
            runClientM (makeTickets conn milestoneMap getUserId finishTicket' ts) env >>= print
            putStrLn "makeTickets' done"
    --mapConcurrently_ makeTickets' (divide 10 tickets)
    putStrLn "Making attachments"
    runClientM (makeAttachments conn getUserId) env >>= print

divide :: Int -> [a] -> [[a]]
divide n xs = map f [0..n-1]
  where
    f i = mapMaybe (\(j,x) -> if j `mod` n == i then Just x else Nothing)
          $ zip [0..] xs

type Username = Text

knownUsers :: M.Map Username Username
knownUsers = M.fromList
    [ "Krzysztof Gogolewski <krz.gogolewski@gmail.com>" .= "int-index"
    , "Ben Gamari <ben@smart-cactus.org>" .= "bgamari"
    , "Austin Seipp <aust@well-typed.com>" .= "thoughtpolice"
    , "ian@well-typed.com" .= "igloo"
    , "Jan Stolarek <jan.stolarek@p.lodz.pl>" .= "jstolarek"
    , "marlowsd@gmail.com" .= "simonmar"
    , "Richard Eisenberg <eir@cis.upenn.edu>" .= "goldfire"
    , "p.capriotti@gmail.com" .= "pcapriotti"
    , "Thomas Miedema <thomasmiedema@gmail.com>" .= "thomie"
    , "pho@cielonegro.org" .= "pho_at_cielonegro.org"
    , "Favonia" .= "favonia"
    , "andygill" .= "AndyGill"
    ]
  where (.=) = (,)

sanitizeUsername :: Username -> Username
sanitizeUsername n
  | Just _ <- T.find (== '<') n =
    sanitizeUsername $ T.takeWhile (/= '>') $ T.tail $ T.dropWhile (/= '<') n
  | otherwise =
    T.map fixChars $ T.takeWhile (/= '@') n
  where
    fixChars '_' = '_'
    fixChars '-' = '-'
    fixChars '.' = '.'
    fixChars c
      | isLetter c = c
      | isDigit c  = c
    fixChars c = '_'

type UserIdOracle = Username -> ClientM UserId

withMVarState :: forall s m a. (MonadMask m, MonadIO m)
              => MVar s -> StateT s m a -> m a
withMVarState var action =
    fst . fst <$> generalBracket (liftIO $ takeMVar var) release (runStateT action)
  where
    release :: s -> ExitCase (a, s) -> m ()
    release s0 exit = do
        liftIO $ putMVar var $ case exit of
          ExitCaseSuccess (_, s') -> s'
          _ -> s0

type UserLookupM = MaybeT (StateT UserIdCache ClientM)

type UserIdCache = M.Map Username UserId

mkUserIdOracle :: ClientEnv -> IO UserIdOracle
mkUserIdOracle clientEnv = do
    cacheVar <- newMVar mempty
    let runIt :: Username -> StateT UserIdCache IO (Maybe UserId)
        runIt username = StateT $ \cache -> do
            res <- runClientM (runStateT (runMaybeT $ getUserId $ T.strip username) cache) clientEnv
            either throwM pure res
    return $ liftIO
           . fmap (fromMaybe $ error "couldn't resolve user id")
           . withMVarState cacheVar
           . runIt
  where
    getUserId :: Username -> UserLookupM UserId
    getUserId username =
            tryCache
        <|> cacheIt tryLookupName
        <|> cacheIt tryCreate
      where
        cuUsername
          | Just u <- M.lookup username knownUsers = u
          | otherwise = "trac-"<>sanitizeUsername username

        tryCache :: UserLookupM UserId
        tryCache = do
            cache <- lift get
            MaybeT $ pure $ M.lookup username cache

        tryLookupName :: UserLookupM UserId
        tryLookupName =
            fmap userId $ MaybeT $ lift $ findUserByUsername gitlabToken cuUsername

        tryCreate :: UserLookupM UserId
        tryCreate = lift $ do
            let cuEmail = "trac+"<>cuUsername<>"@haskell.org"
                cuName = username
                cuSkipConfirmation = True
            liftIO $ putStrLn $ "Creating user " <> show username
            uid <- lift $ createUser gitlabToken CreateUser {..}
            lift $ addProjectMember gitlabToken project uid Reporter
            return uid

        cacheIt :: UserLookupM UserId -> UserLookupM UserId
        cacheIt action = do
            uid <- action
            lift $ modify' $ M.insert username uid
            return uid

makeMilestones :: Connection -> ClientM MilestoneMap
makeMilestones conn = do
    milestones <- liftIO $ Trac.getMilestones conn
    --mconcat <$> mapM createMilestone' milestones
    foldMap (\(GitLab.Tickets.Milestone a b) -> M.singleton a b)
        <$> listMilestones gitlabToken project
  where
    createMilestone' :: Trac.Milestone -> ClientM MilestoneMap
    createMilestone' Trac.Milestone{..} = do
        mid <- createMilestone gitlabToken Nothing project
            $ CreateMilestone { cmTitle = mName
                              , cmDescription = mDescription
                              , cmDueDate = Just mDue
                              , cmStartDate = Nothing
                              }
        return $ M.singleton mName mid

makeAttachment :: UserIdOracle -> Attachment -> ClientM ()
makeAttachment getUserId (Attachment{..})
  | TicketAttachment ticketNum <- aResource = do
        liftIO $ putStrLn $ "Attachment " ++ show (aResource, aFilename)
        mgr <- manager <$> ask
        content <- liftIO $ Trac.Web.fetchTicketAttachment tracBaseUrl ticketNum aFilename
        uid <- getUserId aAuthor
        msg <- if ".hs" `T.isSuffixOf` aFilename
            then mkSnippet uid ticketNum content
            else mkAttachment uid ticketNum content
        mkComment uid (IssueIid $ fromIntegral $ getTicketNumber ticketNum) msg
  | otherwise = return ()
  where
    mkSnippet, mkAttachment :: UserId -> TicketNumber -> BS.ByteString -> ClientM Text
    mkSnippet uid ticketNum content = do
        let cs = CreateSnippet { csTitle = T.unwords [ aFilename, "from ticket"
                                                     , T.pack $ show $ getTicketNumber ticketNum
                                                     ]
                               , csFileName = aFilename
                               , csDescription = Just aDescription
                               , csCode = TE.decodeUtf8 content
                               , csVisibility = Public
                               }
        sid <- GitLab.Project.createSnippet gitlabToken (Just uid) project cs
        return $ T.unlines
            [ "Attached file `" <> aFilename <> "` ($" <> T.pack (show $ getSnippetId sid) <> ")."
            , ""
            , aDescription
            ]
    mkAttachment uid ticketNum content = do
        url <- GitLab.UploadFile.uploadFile gitlabToken (Just uid) project aFilename content
        return $ T.unlines
            [ "Attached file `" <> aFilename <> "` ([download](" <> url <> "))."
            , ""
            , aDescription
            ]

    mkComment :: UserId -> IssueIid -> Text -> ClientM ()
    mkComment uid iid msg = do
        let note = CreateIssueNote { cinBody = msg
                                   , cinCreatedAt = Just aTime
                                   }
        void $ createIssueNote gitlabToken (Just uid) project iid note

makeAttachments :: Connection -> UserIdOracle -> ClientM ()
makeAttachments conn getUserId = do
    attachments <- liftIO $ getAttachments conn
    (finishedAttachments, finishAttachment) <-
        liftIO $ openStateFile attachmentStateFile
    let makeAttachment' a
          | aIdent `S.member` finishedAttachments = return ()
          | otherwise = do
            makeAttachment getUserId a
            liftIO $ finishAttachment aIdent
          where aIdent = (aResource a, aFilename a, aTime a)
    mapM_ makeAttachment' $ attachments

makeTickets :: Connection
            -> MilestoneMap
            -> UserIdOracle
            -> (Ticket -> IO ())
            -> [Trac.Ticket]
            -> ClientM ()
makeTickets conn milestoneMap getUserId finishTicket tickets = do
    mapM_ createTicket' tickets
  where
    createTicket' t = handleAll onError $ flip catchError onError $ do
        iid <- createTicket milestoneMap getUserId t
        tcs <- liftIO $ Trac.getTicketChanges conn (ticketNumber t)
        let groups = groupBy ((==) `on` (\tc -> (changeTime tc, changeAuthor tc))) tcs
        mapM_ (createTicketChanges milestoneMap getUserId iid . collapseChanges) groups
        liftIO $ finishTicket t
      where
        onError :: (MonadIO m, Show a) => a -> m ()
        onError err =
            liftIO $ putStrLn $ "Failed to create ticket " ++ show t ++ ": " ++ show err

collapseChanges :: [TicketChange] -> TicketChange
collapseChanges tcs = TicketChange
    { changeTime = changeTime $ head tcs
    , changeAuthor = changeAuthor $ head tcs
    , changeFields = foldl1 collapseFields (map changeFields tcs)
    , changeComment = listToMaybe $ catMaybes $ map changeComment tcs
    }

tracToMarkdown :: TicketNumber -> Text -> Text
tracToMarkdown (TicketNumber n) src =
      T.pack $ Trac.Convert.convert (fromIntegral n) mempty (T.unpack src)

createTicket :: MilestoneMap -> UserIdOracle
             -> Ticket -> ClientM IssueIid
createTicket milestoneMap getUserId t = do
    liftIO $ print $ ticketNumber t
    creatorUid <- getUserId $ ticketCreator t
    let extraRows = [] -- [ ("Reporter", ticketCreator t) ]
        description = T.unlines
            [ tracToMarkdown (ticketNumber t) $ runIdentity $ ticketDescription (ticketFields t)
            , ""
            , fieldsTable extraRows fields
            ]
        fields = ticketFields t
        iid = case ticketNumber t of
                TicketNumber n -> IssueIid $ fromIntegral n
        issue = CreateIssue { ciIid = Just iid
                            , ciTitle = runIdentity $ ticketSummary fields
                            , ciLabels = Just $ fieldLabels $ hoistFields (Just . runIdentity) fields
                            , ciCreatedAt = Just $ ticketCreationTime t
                            , ciDescription = Just description
                            , ciMilestoneId = Just $ M.lookup (runIdentity $ ticketMilestone fields) milestoneMap
                            , ciWeight = Just $ prioToWeight $ runIdentity $ ticketPriority fields
                            }
    let handle404 (FailureResponse resp)
          | 404 <- statusCode $ responseStatusCode resp
          = return ()
        handle404 e
          = throwError e
    deleteIssue gitlabToken Nothing project iid `catchError` handle404
    ir <- createIssue gitlabToken (Just creatorUid) project issue
    liftIO $ print ir
    return $ irIid ir

createTicketChanges :: MilestoneMap -> UserIdOracle
                    -> IssueIid -> TicketChange -> ClientM ()
createTicketChanges milestoneMap getUserId iid tc = do
    authorUid <- getUserId $ changeAuthor tc
    let body = T.unlines
            [ tracToMarkdown ticketNumber $ fromMaybe mempty $ changeComment tc
            , ""
            , fieldsTable {-[("User", changeAuthor tc)]-} [] (changeFields tc)
            ]
        note = CreateIssueNote { cinBody = body
                               , cinCreatedAt = Just $ changeTime tc
                               }
        ticketNumber = case iid of IssueIid n -> TicketNumber $ fromIntegral n

    case changeComment tc of
      Just c | not $ T.null $ T.strip c ->
               void $ createIssueNote gitlabToken (Just authorUid) project iid note
      _ -> return ()

    let status = case ticketStatus $ changeFields tc of
                   Nothing         -> Nothing
                   Just New        -> Just ReopenEvent
                   Just Assigned   -> Just ReopenEvent
                   Just Patch      -> Just ReopenEvent
                   Just Merge      -> Just ReopenEvent
                   Just Closed     -> Just CloseEvent
                   Just InfoNeeded -> Just ReopenEvent
                   Just Upstream   -> Just ReopenEvent

        notNull :: Maybe Text -> Maybe Text
        notNull (Just s) | T.null s = Nothing
        notNull s = s

        fields = changeFields tc
        edit = EditIssue { eiTitle = notNull $ ticketSummary fields
                         , eiDescription = tracToMarkdown ticketNumber <$> ticketDescription fields
                         , eiMilestoneId = fmap (`M.lookup` milestoneMap) (ticketMilestone fields)
                         , eiLabels = Just $ fieldLabels fields
                         , eiStatus = status
                         , eiUpdateTime = Just $ changeTime tc
                         , eiWeight = prioToWeight <$> ticketPriority fields
                         }
    liftIO $ print edit
    when (not $ nullEditIssue edit)
        $ void $ editIssue gitlabToken (Just authorUid) project iid edit
    return ()

-- | Maps Trac keywords to labels
keywordLabels :: M.Map Text Labels
keywordLabels = M.fromList
    $ [ passthru "newcomer"
      , ("newcomers", "newcomer")
      , passthru "TypeInType"
      , passthru "TypeFamilies"
      , passthru "PatternSynonyms"
      , passthru "Deriving"
      , passthru "Generics"
      , passthru "PatternMatchWarnings"
      , passthru "Inlining"
      , passthru "QuantifiedConstraints"
      , passthru "TypeApplications"
      , passthru "LevityPolymorphism"
      , passthru "CodeGen"
      , passthru "GADTs"
      , passthru "JoinPoints"
      , passthru "Typeable"
      , ("Typeable", "typeable")
      , ("ORF", "OverloadedRecordFields")
      , ("hs-boot", "hs-boot")
      , passthru "SpecConstr"
      , passthru "ApplicativeDo"
      , passthru "FunDeps"
      , passthru "TypedHoles"
      , passthru "CSE"
      , ("TypeCheckerPlugins", "Plugins")
      , ("deriving-perf", "Deriving" <> "CompilerPerformance")
      , passthru "CUSKs"
      , passthru "PolyKinds"
      , ("performance", "RuntimePerformance")
      , ("ci-breakage", "CI-Breakage")

      , ("DWARF", "DebugInformation")
      , passthru "SafeHaskell"
      , passthru "CustomTypeErrors"
      , passthru "StaticPointers"
      , passthru "Unicode"
      , ("warnings", "ErrorMessages")
      , passthru "Arrows"
      , passthru "SIMD"
      , passthru "TemplateHaskell"
      ]
  where
    passthru x = (x, mkLabel x)

typeOfFailureLabels :: TypeOfFailure -> Labels
typeOfFailureLabels t =
    case t of
      BuildingGhcFailed         -> "GhcBuildFailure"
      CompileTimeCrash          -> "CompilerCrash"
      CompileTimePerformance    -> "CompilerPerformance"
      IncorrectDebugInformation -> "DebugInformation"
      DocumentationBug          -> "Documentation"
      InvalidProgramAccepted    -> mempty
      GhcDoesn'tWork            -> mempty
      GhciCrash                 -> "GHCi"
      ValidProgramRejected      -> mempty
      IncorrectAPIAnnotation    -> "APIAnnotations"
      PoorErrorMessage          -> "ErrorMessages"
      IncorrectWarning          -> "ErrorMessages"
      IncorrectResultAtRuntime  -> "IncorrectResultAtRuntime"
      InstallationFailure       -> mempty
      RuntimeCrash              -> "RuntimeCrash"
      RuntimePerformance        -> "RuntimePerformance"
      OtherFailure              -> mempty

fieldLabels :: Fields Maybe -> Labels
fieldLabels fields =
    "TracImport" <> keywordLbls <> failureLbls
  where
    keywordLbls = mconcat
        [ lbl
        | Just keywords <- pure $ ticketKeywords fields
        , kw <- keywords
        , Just lbl <- pure $ M.lookup kw keywordLabels
        ]

    failureLbls :: Labels
    failureLbls = maybe mempty typeOfFailureLabels $ ticketTypeOfFailure fields

fieldsTable :: forall f. (Functor f, Foldable f)
            => [(Text, Text)] -> Fields f -> T.Text
fieldsTable extraRows (Fields{..})
  | null rows = ""
  | otherwise = T.unlines $
    [ "<details><summary>Trac metadata</summary>"
    , ""
    , table
    , "</details>"
    ]
  where
    one :: f a -> Maybe a
    one = listToMaybe . toList

    unless :: (a -> Bool) -> Maybe a -> Maybe a
    unless p (Just x) | p x = Nothing
    unless _ x = x

    row :: Text -> Maybe Text -> Maybe (Text, Text)
    row name val = fmap (\v -> (name,v)) val

    rows :: [(Text, Text)]
    rows =
        catMaybes
        [ row "Version" $ one ticketVersion
        , row "Test case" $ unless T.null $ one ticketTestCase
        , row "Differential revisions" $ one ticketDifferentials >>= renderTicketDifferentials
        ] ++ extraRows

    header :: (Text, Text)
    header = ("Trac field", "Value")

    widths = ( maximum $ map (T.length . fst) (header:rows)
             , maximum $ map (T.length . snd) (header:rows)
             )

    table = T.unlines $
        [ renderRow header
        , renderRow (T.replicate (fst widths) "-", T.replicate (snd widths) "-")
        ] ++ map renderRow rows

    renderRow :: (Text, Text) -> Text
    renderRow (a,b) = mconcat
        [ "| "
        , T.justifyLeft (fst widths) ' ' a
        , " | "
        , T.justifyLeft (snd widths) ' ' b
        , " |"
        ]

renderTicketDifferentials :: [Differential] -> Maybe Text
renderTicketDifferentials [] = Nothing
renderTicketDifferentials diffs = Just $ T.intercalate ", " $ map toLink diffs
  where
    toLink (Differential n) =
        T.pack $ "[D"++show n++"](https://phabricator.haskell.org/D"++show n++")"

toPriorityLabel :: Priority -> Labels
toPriorityLabel p = case p of
  PrioLowest  -> "P-lowest"
  PrioLow     -> "P-low"
  PrioNormal  -> "P-normal"
  PrioHigh    -> "P-high"
  PrioHighest -> "P-highest"

prioToWeight :: Priority -> Weight
prioToWeight PrioLowest  = Weight 0
prioToWeight PrioLow     = Weight 3
prioToWeight PrioNormal  = Weight 5
prioToWeight PrioHigh    = Weight 7
prioToWeight PrioHighest = Weight 10
