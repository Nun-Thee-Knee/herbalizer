{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Applicative ((<$>), (<*>), (<*), (<$>))
import Control.Monad (liftM)
import Control.Monad.State (State, runState)
import Control.Applicative ((<*))
import Text.Parsec hiding (State)
import Text.Parsec.Indent
import Text.Parsec.Pos
import Data.List (isPrefixOf, isInfixOf, intercalate, intersperse, tails)
import qualified Data.Map as M
import Text.Regex.Posix
import System.Environment
import Control.Monad.Identity (Identity)

type IParser a = ParsecT String () (IndentT Identity) a

iParse :: IParser a -> SourceName -> String -> Either ParseError a
iParse p s inp = runIndent $ runParserT p () s inp

data Tree = Tree Expression [Tree]
    deriving (Show)

type Attrs = [(String, String)]
data InlineContent = RubyInlineContent String 
    | PlainInlineContent String
    | NullInlineContent
    | HamlFilterContent String
    deriving (Show, Eq)

type IsFormTag = Bool

data Expression = 
      DocType String
    | Comment String 
    | PlainText String
    | RubyStartBlock String IsFormTag
    | RubyMidBlock String
    | RubySideEffect String
    | RubyExp String
    | Tag String Attrs InlineContent
    | GenericExpression String 
    deriving (Show)

container :: IParser Tree
container = do
  b <- withBlock Tree expression container
  spaces
  return b

expression :: IParser Expression
expression = (try escapeHtmlExpr <|> docType) <|> comment <|> hamlFilter <|> startPlainText <|> rubyBlock <|> rubyExp <|> tag <|> genericExpression

rubyBlock = do
    char '-'
    spaces
    k <- rubyKeyword
    rest <- manyTill anyChar newline <* spaces
    if (k `elem` midBlockKeywords)
    then return (RubyMidBlock $ k ++ rest)
    -- TODO : we need to recognize Ruby expression expression included purely for a side effect,
    -- e.g. "- localvar = Time.now"
    else return (RubyStartBlock (k ++ rest) False)
  where midBlockKeywords = ["else", "elsif", "rescue", "ensure", "when", "end"]

escapeHtmlExpr = do
  char '!'
  line <- ((:) <$> char '=' >> spaces >> manyTill anyChar newline <* spaces)
  return $ RubyExp $ "raw(" ++ line ++ ")"

rubyExp = do
  line <- ((:) <$> char '=' >> spaces >> manyTill anyChar newline <* spaces)
  return (RubyExp line)

tag :: IParser Expression
tag = do
    tag <- explicitTag <|> return "div"
    as <- many (dotClass <|> idHash)
    hs <- option [] (hashAttrs)
    many $ oneOf " \t"
    c <- parseInlineContent 
    spaces
    return $ Tag tag (attrs as hs) c
  where 
    attrs as hs = filter (\(k, v) -> v /= "") $ 
      M.toList $ 
      M.unionWith (\a b -> intercalate " " (filter (/= "") [a,b]))
        (M.fromList hs)
        (M.fromList (makeClassIdAttrs as)) 
    parseInlineContent = (RubyInlineContent <$> (char '=' >> spaces >> manyTill anyChar newline)) <|> 
        (PlainInlineContent <$> (manyTill anyChar newline)) 
        <|> return NullInlineContent

makeClassIdAttrs :: [String] -> [(String, String)]
makeClassIdAttrs cs = classes : [("id", ids)]
    where classes = ("class", intercalate " " $ map tail $ filter ("." `isPrefixOf`) cs )
          ids = intercalate " " $ map tail $ filter (isPrefixOf "#") cs

explicitTag = do
  char '%'
  tag <- many alphaNum
  return tag

dotClass = (:) <$> char '.' <*> cssClassOrId
idHash = (:) <$> char '#' <*> cssClassOrId

hashAttrs = do
  char '{' 
  xs <- kvPair `sepBy` (spaces >> char ',' >> spaces)
  char '}'
  return xs

cssClassOrId = many (alphaNum <|> oneOf "-_")
rubyIdentifier = many (alphaNum <|> char '_')

rubyKeyword = many alphaNum

singleQuotedStr = do
    between (char '\'') (char '\'') (many stringChar)
  where stringChar = ('\'' <$ string "\\'") <|> (noneOf "'")

doubleQuotedStr = do
    between (char '"') (char '"') (many stringChar)
  where stringChar = ('"' <$ string "\\\"") <|> (noneOf "\"")

--- Ruby interpolation delimiters crudely replaced by ERB style
rubyString = do
    between (char '"') (char '"') rString
  where 
    rString = liftM replaceInterpolationDelim $ many stringChar
    stringChar = ('"' <$ string "\\\"") <|> (noneOf "\"") 
    replaceInterpolationDelim = (replace "#{" "<%= ") . (replace "}" " %>")

rubySymbol =  do
      char ':' 
      xs <- (char '"' >> many stringChar2 <* char '"') <|> (char '\'' >> many stringChar1 <* char '\'') <|> rubyIdentifier 
      return xs
  where stringChar1 = ('\'' <$ string "\\'") <|> (noneOf "'")
        stringChar2 = ('"' <$ string "\\\"") <|> (noneOf "\"")

rubySymbolKey = rubyIdentifier <* char ':'

-- really, we need to parse full-blown Ruby expressions
rubyValue = do
    xs <- many (noneOf "},([ \t")  <* spaces
    rest <- ((lookAhead (oneOf ",}") >> return ""))
            <|> (betweenStuff '(' ')' )
            <|> (betweenStuff '[' ']' )
    return $ "<%= " ++ xs ++ rest ++ " %>"
  where 
    betweenStuff x y = do
      xs' <- between (char x) (char y) (many $ noneOf [y])
      return $ [x] ++ xs' ++ [y]

rocket = spaces >> string "=>" >> spaces 
aKey = (singleQuotedStr <* rocket)
  <|> (doubleQuotedStr <* rocket)
  <|> (rubySymbol <* rocket)
  <|> (rubySymbolKey <* spaces)

aValue = singleQuotedStr <|> rubyString <|> many1 digit <|> rubyValue

kvPair :: IParser (String, String)
kvPair = do
  k <- (many $ oneOf " \t") >> aKey 
  v <- spaces >> aValue <* (many $ oneOf " \t")
  return (k, v)

-- TODO HTML Comments are not rendered like HAML renders them
-- also HAML -# style comments could be rendered
comment :: IParser Expression
comment = do
  char '/' 
  s <- manyTill anyChar newline
  spaces
  return $ Comment s

docType :: IParser Expression
docType = do
    string "!!!"
    many $ char ' '
    s <- option [] $ many alphaNum
    newline
    return $ DocType s

filterBlock :: (Stream s (IndentT m) z, Monad m) => ParsecT s u (IndentT m) a -> IndentParserT s u m [a]
filterBlock p = withPos $ do
    r <- many (checkIndent >> p)
    return r

hamlFilter = do
  withPos $ do
    char ':' 
    s <- many $ alphaNum
    many (oneOf " \t")
    newline
    xs <- indentedOrBlank
    return $ Tag (convertToTag s) [] (HamlFilterContent $ concat xs)
  where convertToTag "javascript" = "script"
        convertToTag s = s

indentedOrBlank = many1 (try blankLine <|> try indentedLine)

indentedLine :: IParser String
indentedLine = do
    a <- indented
    s <- many (noneOf "\n")
    newline
    return $ case a of
        () -> s  -- Return s directly if indented consumed nothing
        _  -> ""  -- Handle the case where indented succeeds without consuming input

blankLine :: IParser String
blankLine = many (oneOf " \t") <* newline

-- Adjusting startPlainText to handle indented content
startPlainText :: IParser Expression
startPlainText = do
    t <- indentedLine
    return $ PlainText t

-- Adjusting genericExpression to handle indented content
genericExpression :: IParser Expression
genericExpression = do
    t <- indentedLine
    return $ GenericExpression t


replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace from to xs = join' to . split' from $ xs
    where join' j = foldr1 (\a b -> a ++ j ++ b)
          split' [] _ = error "empty"
          split' delim' str = split'' str
              where split'' [] = [[]]
                    split'' l@(x:xs)
                        | delim' `isPrefixOf` l = [] : split'' (drop (length delim') l)
                        | otherwise = let (y:ys) = split'' xs in (x : y) : ys

main :: IO ()
main = do
  args <- getArgs
  file <- readFile $ head args
  case iParse container "" file of
    Left err -> print err
    Right x -> print x
