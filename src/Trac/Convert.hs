module Trac.Convert (convert, CommentMap) where

import qualified Trac.Parser as R
import Trac.Writer
import Data.List
import qualified Data.Map as M
import Debug.Trace
import Data.Maybe


convert :: Int -> CommentMap -> String -> String
convert n cm s =
    writeRemarkup
    $ convertBlocks n cm
    $ either (\err -> [R.Para [R.Str "NO PARSE: ", R.Str $ show err, R.Str s]]) id
    $ R.parseTrac
    $ s

convertWithError n cm s = writeRemarkup . convertBlocks n cm <$> R.parseTrac s

convertBlocks :: Int -> CommentMap -> [R.Block] -> [Block]
convertBlocks n cm = map (convertBlock n cm)

convertBlock :: Int -> CommentMap -> R.Block -> Block
convertBlock n cm (R.Header nlev is bs)
  = Header nlev (convertInlines n cm is) (convertBlocks n cm bs)
convertBlock n cm (R.Para is)        = Para (convertInlines n cm is)
convertBlock n cm (R.List _ bs)      = List Style (map (:[]) (convertBlocks n cm bs))
convertBlock n cm (R.DefnList _)     = convertDefnListToTable
convertBlock n cm (R.Code ty s)      = CodeBlock ty s
convertBlock n cm (R.BlockQuote bs)  = Quote [Para (convertInlines n cm bs)]
convertBlock n cm (R.Discussion bs)  = Quote (convertBlocks n cm bs)
convertBlock _ _ R.Table            = Table
convertBlock _ _ R.HorizontalLine   = HorizontalLine

convertDefnListToTable :: Block
convertDefnListToTable = Para []

convertInlines n cm = map (convertInline n cm)

convertInline :: Int -> CommentMap -> R.Inline -> Inline
convertInline n cm (R.Bold is) = Bold (convertInlines n cm is)
convertInline n cm (R.Monospaced is) = Monospaced is
convertInline n cm (R.Italic is) = Italic (convertInlines n cm is)
convertInline n cm (R.WikiStyle is) = Italic (convertInlines n cm is)
convertInline n cm (R.Link url is) = WebLink (intersperse Space (map Str is)) url
convertInline n cm (R.Str s) = Str s
convertInline _ _ (R.LineBreak)  = LineBreak
convertInline _ _ (R.Space)      = Space
convertInline _ _ (R.TracTicketLink n) = TicketLink n Nothing
convertInline n cm (R.CommentLink mt c) =
  let ticketN = fromMaybe n mt
  in case M.lookup (ticketN, c) cm of
      Just t -> TicketLink ticketN (Just t)
      Nothing -> traceShow ("COULD NOT FIND", n, mt, c, cm) (TicketLink ticketN Nothing)
convertInline _ _ e = error $ "not handled"  ++ show e
