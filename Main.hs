{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}

module Main where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Error.Class
import Data.Default (def)
import Data.Foldable
import Data.Function
import Data.Functor.Identity
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Map as M
import qualified Data.Text as T

import Database.PostgreSQL.Simple
import Network.HTTP.Client.TLS as TLS
import Network.Connection (TLSSettings(..))
import Network.HTTP.Types.Status
import Servant.Client

import GitLab.Tickets
import Trac.Db
import Trac.Db.Types
import qualified Trac.Convert

gitlabToken :: AccessToken
gitlabToken = "eT4mt9KK1CgeYoMo-5Nx"

project :: ProjectId
project = ProjectId 1

main :: IO ()
main = do
    conn <- connectPostgreSQL ""
    tickets <- getTickets conn
    --mapM_ print tickets
    mapM_ (getTicketChanges conn >=> print) $ map ticketNumber tickets

    let tlsSettings = def { settingDisableCertificateValidation = True }
    mgr <- TLS.newTlsManagerWith $ TLS.mkManagerSettings tlsSettings Nothing
    let clientEnv = mkClientEnv mgr (BaseUrl Https "gitlab.ghc.smart-cactus.org" 443 "/api/v4")

    let createTicket' t = do
            iid <- createTicket t
            tcs <- liftIO $ getTicketChanges conn (ticketNumber t)
            let groups = groupBy ((==) `on` (\tc -> (changeTime tc, changeAuthor tc))) tcs
            mapM_ (createTicketChanges iid . collapseChanges) groups
            return ()
    res <- runClientM (mapM_ createTicket' $ take 1 tickets) clientEnv
    print res
    return ()

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

createTicket :: Ticket -> ClientM IssueIid
createTicket t = do
    liftIO $ print $ ticketNumber t
    let extraRows = [ ("Reporter", ticketCreator t) ]
        description = T.unlines
            [ fieldsTable extraRows fields
            , ""
            , tracToMarkdown (ticketNumber t) $ runIdentity $ ticketDescription (ticketFields t)
            ]
        fields = ticketFields t
        iid = case ticketNumber t of
                TicketNumber n -> IssueIid $ fromIntegral n
        issue = CreateIssue { ciIid = Just iid
                            , ciTitle = runIdentity $ ticketSummary fields
                            , ciLabels = Just $ fieldLabels $ hoistFields (Just . runIdentity) fields
                            , ciCreatedAt = Just $ ticketCreationTime t
                            , ciDescription = Just description
                            }
    let handle404 (FailureResponse resp)
          | 404 <- statusCode $ responseStatusCode resp
          = return ()
        handle404 e
          = throwError e
    deleteIssue gitlabToken project iid `catchError` handle404
    ir <- createIssue gitlabToken project issue
    liftIO $ print ir
    return $ irIid ir

createTicketChanges :: IssueIid -> TicketChange -> ClientM ()
createTicketChanges iid tc = do
    let body = T.unlines
            [ fieldsTable [("User", changeAuthor tc)] (changeFields tc)
            , ""
            , fromMaybe mempty (changeComment tc)
            ]
        note = CreateIssueNote { cinBody = body
                               , cinCreatedAt = Just $ changeTime tc
                               }
    createIssueNote gitlabToken project iid note
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
    keywordLbls <> failureLbls
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
