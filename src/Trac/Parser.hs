{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
module Trac.Parser where

--import Pandoc.Types
import Text.Megaparsec hiding (space)
import Text.Megaparsec.String
import Text.Megaparsec.Combinator
--import Text.Pandoc.Parsing
--import Debug.Trace
import Debug.NoTrace

import Control.Applicative (empty)
import Control.Monad (void)
import Text.Megaparsec hiding (space)
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

import Control.Applicative ((<|>), some, optional)
import Control.Monad (void)
import Data.Char (readLitChar)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (listToMaybe, fromMaybe, isJust)

import Text.Megaparsec.Combinator
import Text.Megaparsec.Error
import Text.Megaparsec.Pos
import Text.Megaparsec.Prim
import qualified Text.Megaparsec.Char as C
import Data.Maybe

normaliseNewlines :: String -> String
normaliseNewlines ('\r':'\n':xs) = '\n': normaliseNewlines xs
normaliseNewlines (c:xs) = c : normaliseNewlines xs
normaliseNewlines [] =  "\n"

data Inline = Bold Inlines
             | Italic Inlines
             | WikiStyle Inlines
             | Monospaced String
             | Link String [String]
             | TracLink Int
             | CommentLink (Maybe Int) Int
             | Anchor
             | Image
             | Comment
             | Str String
             | LineBreak
             | Space deriving Show

type Inlines = [Inline]

type Blocks = [Block]
type Type = Maybe String

data Block = Header Int Inlines Blocks
            | Para Inlines
            | List ListType [Block]
            | DefnList [(Inlines, [Inlines])]
            | Code Type String
            | BlockQuote Inlines
            | Discussion [Block]
            | Table
            | HorizontalLine deriving Show


type Document = [Block]

parseTrac :: String -> Either (ParseError Char Dec) [Block]
parseTrac s = (runParser (blocks) "" (normaliseNewlines s))


testParser :: Parser a -> String -> a
testParser p s = either (error . show) id (runParser p "" s)

{-
blocks :: Parser Blocks
blocks = do header <|> para <|> list <|> defn <|> code <|> quote
                   <|> disc <|> table <|> horiz
-}

inlines = some inline

inline :: Parser Inline
inline = inlineNoNL <|> endline

inlineNoNL :: Parser Inline
inlineNoNL = do
             --getInput >>= traceShowM
             choice [ tracLink
                    , commentLink
                    , bold
                    , italic
                    , link
                    , wikiStyle
                    , monospaced
                    , monospaced2
                    , space
                    , str
                    , symbol ]



bold = try bold1 <|> try bold2
bold1 = Bold <$> inlineMarkup 3
bold2 = Bold <$> try (between (string "**") (string "**") (some inline))

italic = try italic1 <|> italic2
italic1 = Italic <$> inlineMarkup 2
italic2 = Italic <$> try (between (string "//") (string "//") (some inline))

wikiStyle = WikiStyle <$> inlineMarkup 5

inlineMarkup :: Int -> Parser [Inline]
inlineMarkup n = try $ do
  _ <- count n (char '\'')
  content <- someTill inline (count n (char '\''))
  return (content)

monospaced :: Parser Inline
monospaced =
  Monospaced <$> try (between (char '`') (char '`')
                  (someTill anyChar (lookAhead $ char '`')))

monospaced2 :: Parser Inline
monospaced2 = Monospaced <$> try (between (string "{{{") (string "}}}")
                                (someTill anyChar (lookAhead $ string "}}}")))

str :: Parser Inline
str = Str <$> some (noneOf (reservedChars ++ "\n\r"))

word :: Parser String
word = some (noneOf (reservedChars ++ "\n\r"))

symbol :: Parser Inline
symbol = Str . (: []) <$> oneOf reservedChars

space :: Parser Inline
space = Space <$ oneOf " \t"

number :: Parser Int
number = read <$> some (oneOf "0123456789")

tracLink :: Parser Inline
tracLink = try $ TracLink <$> (char '#' *> number)

commentLink :: Parser Inline
commentLink = do
  mn <- optional (string "ticket:" *> number <* char '#')
  c  <- string "comment:" *> number
  return $ CommentLink mn c


blankline = oneOf "\n\r"

endline :: Parser Inline
endline = try $ do
  newline
  notFollowedBy blankline
  notFollowedBy (pItemListStart)
  notFollowedBy literalBlockStart
  notFollowedBy (char '>')
  notFollowedBy (string "  ")
  return LineBreak

skipSpaces :: Parser ()
skipSpaces = () <$ many (oneOf " \t")

blocks = manyTill (block <* many blankline) eof

block :: Parser Block
block = do
  many blankline
  getInput >>= \s -> traceM "block" >>=  \_ -> traceShowM s
  r <- choice [pList, discussion, blockQuote, literalBlock, defnList,header, para]
  many blankline
  traceShowM r
  getInput >>= traceShowM
  return r

header :: Parser Block
header = try $ do
  getInput >>= \s -> traceShowM ("header", take 5 $ s)
  level <- length <$> some (char '=')
  skipSpaces
  content <- inlines
  bs <- blocks
  return (Header level content bs)

para :: Parser Block
para = Para <$> some inline

anyLine :: Parser String
anyLine = do
 v <- manyTill anyChar newline
 traceShowM v
 return v

literalBlockStart :: Parser (Maybe String)
literalBlockStart = do
  string "{{{"
  mtype <- optional (string "#!" *> word)
  skipSpaces
  some blankline
  return mtype

literalBlock :: Parser Block
literalBlock = try $ do
  getInput >>= \s -> traceShowM ("litBlock", take 5 $ s)
  mtype <- literalBlockStart
  contents <- unlines <$> manyTill anyLine (string "}}}" *> sc)
  return (Code mtype contents)

scn :: Parser ()
scn = L.space (void spaceChar) empty empty

sc :: Parser ()
sc = L.space (void $ oneOf " \t") empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

parser :: Parser [Block]
parser = pItemListStart *> pItemList <* eof

defnList :: Parser Block
defnList = do
  getInput >>= \s -> traceShowM ("defnList", take 5 $ s)
  let getOne = do
        d <- defnItem
        traceM "dlist"
        many blankline
        traceM "dlist1"
        return d
  DefnList <$> some getOne


defnItem :: Parser (Inlines, [Inlines])
defnItem = try $ do
 L.indentGuard sc GT (unsafePos 1)
-- traceM "here"
 defn <- someTill inline (lexeme(string "::"))
-- traceM "here2"
-- traceShowM defn
 mis <- optional (some inlineNoNL)
 traceM "here3"
 traceShowM mis
 optional newline
 traceM "here3a"
 getInput >>= traceShowM
 traceM "here3b"

 iss <- many (do
          notFollowedBy defnItem
          L.indentGuard sc GT (unsafePos 1) >> (some inlineNoNL) <* optional newline)
 traceM "here3c"
-- traceM "here4"
 traceShowM (defn, mis, iss)
 return (defn, maybeToList mis ++ iss)

discussion :: Parser Block
discussion = do
  getInput >>= \s -> traceShowM ("discussion", s)
  ss <- unlines <$> some (string ">" *> sc *> anyLine)
  getInput >>= \ (!s) -> traceShowM (s, ss)
  Discussion <$> parseFromString (many blankline *> many block) ss

data ListType = ListType deriving Show

pList :: Parser Block
pList =  do
  getInput >>= \s -> traceShowM ("pList", s)
  List ListType <$> (try pItemListStart *> pItemList)

pItemListStart = sc *> char '*'

pItemList :: Parser [Block]
pItemList = do
  getInput >>= \s -> traceShowM ("pItemList", s)
  indentBlock2 scn p
  where
    p :: Parser (L.IndentOpt Parser [Block] Block)
    p = do
      getInput >>= traceShowM
      s <- Para <$> some (try inlineNoNL)
      traceShowM ("s", (show s))
      return (L.IndentMany Nothing (\ss -> return (s:ss))
                (Para <$> some inlineNoNL <|> pList))

printList :: Int -> Block -> String
printList n (List _ (el:els)) =
  unlines (("* " ++ printList n el): map ((replicate (2 * n) ' '  ++) .  printList (n + 1)) els)
printList _ (Para is) = concatMap printInlines is

printInlines (Str s) = s
printInlines Space = " "

url :: Parser String
url = manyTill anyChar spaceChar

link :: Parser Inline
link = try $ do
  char '['
  l <- url
  desc <- words <$> manyTill (noneOf "]\n") (char ']')
  return $ Link l desc




indentBlock2 :: (Show s, MonadParsec e s m, Token s ~ Char)
  => m ()              -- ^ How to consume indentation (white space)
  -> m (L.IndentOpt m a b) -- ^ How to parse “reference” token
  -> m a
indentBlock2 sc r = do
  sc
  ref <- L.indentLevel
  traceShowM ref
  a   <- r
  getInput >>= \s -> traceShowM ("pItemList after r", s)
  ref' <- L.indentLevel
  traceShowM ref'
  case a of
    L.IndentNone x -> return x
    L.IndentMany indent f p -> do
      mlvl <- optional . try $ C.eol *> L.indentGuard sc EQ ref
      traceShowM mlvl
      case mlvl of
        Nothing  -> sc *> f []
        Just lvl -> indentedItems ref (fromMaybe lvl indent) sc p >>= f
    L.IndentSome indent f p -> do
      lvl <- C.eol *> L.indentGuard sc GT ref
      indentedItems ref (fromMaybe lvl indent) sc p >>= f

indentedItems :: Show s => MonadParsec e s m
  => Pos               -- ^ Reference indentation level
  -> Pos               -- ^ Level of the first indented item ('lookAhead'ed)
  -> m ()              -- ^ How to consume indentation (white space)
  -> m b               -- ^ How to parse indented tokens
  -> m [b]
indentedItems ref lvl sc p = do
  traceShow (ref, lvl) go
  where
    go = (sc *> L.indentLevel) >>= re . traceShowId
    re pos
      | pos < ref = return []
      | pos == lvl = (:) <$> try p <*> go
      | otherwise  = do
          getInput >>= traceShowM
          traceShowM (ref, lvl)
          traceM "otherwise"
          done <- isJust <$> optional eof
          if done
            then return []
            else L.incorrectIndent EQ lvl pos

blockQuote :: Parser Block
blockQuote = try $ do
  getInput >>= \s -> traceShowM ("blockQuote", take 5 $ s)
  ss <- unlines <$> some (string "  " *> anyLine)
  traceM ss
  BlockQuote <$> parseFromString inlines ss

parseFromString :: Parser b -> String -> Parser b
parseFromString parser str = do
  oldPos <- getPosition
  oldInput <- getInput
  setInput str
  result <- parser
  traceM "success"
  skipSpaces
  traceM "success"
  eof
  setInput oldInput
  setPosition oldPos
  return result





list :: Parser Block
list = bulletList -- <|> numberedList

bulletList :: Parser Block
bulletList = do
  startPos <- getPosition
  char '*'
  skipSpaces
  return undefined








reservedChars :: [Char]
reservedChars = "\'`*/!{}>|[]#: "






