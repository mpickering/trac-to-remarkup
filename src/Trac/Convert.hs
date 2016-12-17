module Trac.Convert where

import qualified Trac.Parser as R
import Trac.Writer
import Data.List

convert :: String -> String
convert = writeRemarkup . convertBlocks . either (const ([R.Para [(R.Str "NO PARSE")]])) id . R.parseTrac

convertWithError s = writeRemarkup . convertBlocks <$> R.parseTrac s

convertBlocks :: [R.Block] -> [Block]
convertBlocks = map convertBlock

convertBlock :: R.Block -> Block
convertBlock (R.Header n is bs) = Header n (convertInlines is) (convertBlocks bs)
convertBlock (R.Para is)        = Para (convertInlines is)
convertBlock (R.List _ bs)      = List Style (map (:[]) (convertBlocks bs))
convertBlock (R.DefnList _)     = convertDefnListToTable
convertBlock (R.Code ty s)      = CodeBlock ty s
convertBlock (R.BlockQuote bs)  = Quote [Para (convertInlines bs)]
convertBlock (R.Discussion bs)  = Quote (convertBlocks bs)
convertBlock R.Table            = Table
convertBlock R.HorizontalLine   = HorizontalLine

convertDefnListToTable :: Block
convertDefnListToTable = Para []

convertInlines = map convertInline

convertInline :: R.Inline -> Inline
convertInline (R.Bold is) = Bold (convertInlines is)
convertInline (R.Monospaced is) = Monospaced is
convertInline (R.Italic is) = Italic (convertInlines is)
convertInline (R.WikiStyle is) = Italic (convertInlines is)
convertInline (R.Link url is) = WebLink (intersperse Space (map Str is)) url
convertInline (R.Str s) = Str s
convertInline (R.LineBreak)  = LineBreak
convertInline (R.Space)      = Space
convertInline (R.TracLink n) = TicketLink n
convertInline e = error $ "not handled"  ++ show e
