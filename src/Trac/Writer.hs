{-# LANGUAGE OverloadedStrings #-}

module Trac.Writer where

import Data.List
import qualified Data.Text as T
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
           | Table [TableRow]
           | HorizontalLine

type Lang = String

type TableRow = [TableCell]

data TableCell = TableHeaderCell Inlines
               | TableCell Inlines

tableCellContents :: TableCell -> Inlines
tableCellContents (TableCell is) = is
tableCellContents (TableHeaderCell is) = is


writeRemarkup :: [Block] -> String
writeRemarkup = render (Just 80) . blocks

blocks bs = (foldr (<>) empty (intersperse blankline (map block bs)))

repeatP :: Int -> Doc -> Doc
repeatP n s = foldr (<>) empty (replicate n s)

equals = char  '='

block :: Block -> Doc
block (Header n h bs)
  = repeatP n (char '#') <+> inlines h $+$ blocks bs
block (Quote is)    =
  prefixed "> " (blocks is)
block (Para is)     = inlines is
block (List s iss)   =
  vcat (map (oneListItem "-") iss)
block (CodeBlock ml s) =
  vcat [text "```" <> mlang, text s, text "```"]
  where
    mlang = case ml of
      Just lang -> text lang
      Nothing   -> empty
block (Table rs) =
  table rs
block HorizontalLine = text "---"

table :: [TableRow] -> Doc
table rs
  | isProper rs = niceTable rs'
  | otherwise = htmlTable rs'
  where
    rs' = normalizeTable rs

isProper :: [TableRow] -> Bool
isProper [] = True
isProper (x:xs) =
  (isProperHeaderRow x || isProperBodyRow x) && all isProperBodyRow xs

isProperHeaderRow :: TableRow -> Bool
isProperHeaderRow = all isHeaderCell

isProperBodyRow :: TableRow -> Bool
isProperBodyRow = all (not . isHeaderCell)

isHeaderCell :: TableCell -> Bool
isHeaderCell (TableHeaderCell {}) = True
isHeaderCell (TableCell {}) = False

-- | Render a \"nice\" table (native remarkup)
niceTable :: [TableRow] -> Doc
niceTable = vcat . map niceRow

-- | Normalize the table so that all rows have the same column count.
normalizeTable :: [TableRow] -> [TableRow]
normalizeTable rows =
  map doOne rows
  where
    columns = maximum (1 : map length rows)
    doOne :: TableRow -> TableRow
    doOne xs =
      -- If the row contains only header cells, pad it with header cells,
      -- otherwise pad it with regular cells.
      --
      -- We pick this choice in order to preserve "proper-ness" of tables, that
      -- is, if a table meets the criteria for rendering in remarkup style
      -- before normalization, it should also meet the criteria after
      -- normalization; for "improper" tables, the padding choice doesn't
      -- matter much, but it is reasonable to assume that if a row contains
      -- any non-header cells, then padding it with more non-header cells is
      -- acceptable.
      let emptyCell =
            if not (null xs) && isProperHeaderRow xs then
              TableHeaderCell []
            else
              TableCell []
      in take columns $ xs ++ repeat emptyCell

niceRow :: TableRow -> Doc
niceRow row =
  if isProperHeaderRow row then
    vcat [ niceRowCells row, niceHeaderUnderline (length row) ]
  else
    niceRowCells row

niceHeaderUnderline :: Int -> Doc
niceHeaderUnderline n =
  niceRowRaw (replicate n $ text "-----")

niceRowCells :: [TableCell] -> Doc
niceRowCells cells =
  niceRowRaw (map (inlines . tableCellContents) cells)

niceRowRaw :: [Doc] -> Doc
niceRowRaw items =
  cat (text "|" : [ text " " <> i <> text " |" | i <- items ])


-- | Render an HTML-style table (using HTML tags)
htmlTable :: [TableRow] -> Doc
htmlTable rs =
  vcat [ text "<table>" , htmlTableRows rs , "</table>" ]

htmlTableRows :: [TableRow] -> Doc
htmlTableRows = vcat . map htmlTableRow

htmlTableRow :: TableRow -> Doc
htmlTableRow tr = vcat [ "<tr>", htmlTableCells tr, "</tr>" ]

htmlTableCells :: [TableCell] -> Doc
htmlTableCells = vcat . map htmlTableCell

htmlTableCell :: TableCell -> Doc
htmlTableCell (TableHeaderCell is) = vcat [ "<th>", inlines is, "</th>" ]
htmlTableCell (TableCell is) = vcat [ "<td>", inlines is, "</td>" ]

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

