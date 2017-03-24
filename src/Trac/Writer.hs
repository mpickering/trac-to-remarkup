module Trac.Writer where

import Data.List
import Trac.Pretty
import qualified Data.Map as M

type CommentMap = M.Map (Int, Int) Int

data Inline = Bold Inlines
             | Italic Inlines
             | Monospaced String
             | Deleted Inlines
             | Underlined Inlines
             | Highlighted Inlines
             | ObjectLink Ref
             | TicketLink Int (Maybe Int) -- Potential comment
             | Str String
             | Space
             | ImageLink
             | UserMention
             | ProjectMention
             | WikiLink
             | WebLink Inlines String
             | LineBreak

type Inlines = [Inline]

data Ref = Ref

type Blocks = [Block]

data Style = Style

data Block = Header Int Inlines Blocks
           | Quote Blocks
           | Para Inlines
           | List Style [Blocks]
           | CodeBlock (Maybe Lang) String
           | Table
           | HorizontalLine

type Lang = String


writeRemarkup :: [Block] -> String
writeRemarkup = render (Just 80) . blocks

blocks bs = (foldr (<>) empty (intersperse blankline (map block bs)))

repeatP :: Int -> Doc -> Doc
repeatP n s = foldr (<>) empty (replicate n s)

equals = char  '='

block :: Block -> Doc
block (Header n h bs)
  = repeatP n equals <+> inlines h <+> repeatP n equals $+$ blocks bs
block (Quote is)    =
  prefixed "> " (blocks is)
block (Para is)     = inlines is
block (List s iss)   =
  vcat (map (oneListItem "-") iss)
block (CodeBlock ml s) =
  vcat [text "```", mlang, text s, text "```"]
  where
    mlang = maybe empty ((text "lang=" <>) . text) ml
block Table = undefined
block HorizontalLine = text "---"

oneListItem :: String -> Blocks -> Doc
oneListItem mark is = text mark $$ nest 4 (blocks is)

inlines = hcat . map inline

inline :: Inline -> Doc
inline (Bold is) = inside (text "**") (text "**") (inlines is)
inline (Italic is) = inside (text "//") (text "//") (inlines is)
inline (Monospaced is) = inside (text "`") (text "`") (text is)
inline (Deleted is) = inside (text "~~") (text "~~") (inlines is)
inline (Underlined is) = inside (text "__") (text "__") (inlines is)
inline (Highlighted is) = inside (text "!!") (text "!!") (inlines is)
inline (Str str) = text str
inline Space = space
inline (WebLink is url) = brackets (inlines is) <> parens (text url)
inline (ObjectLink Ref) = empty
inline (ImageLink) = empty
inline LineBreak = cr
inline (TicketLink n mc) = char 'T' <> text (show n) <>
                              maybe empty (\n -> char '#' <> text (show n)) mc
inline _ = empty

