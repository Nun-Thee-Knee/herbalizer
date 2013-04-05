module Main where
import Control.Applicative ((<$>), (<*>), (<*), (<$))
import Control.Monad (liftM)
import Control.Monad.State 
import Control.Applicative ((<*))
import Data.List (intercalate)
import Text.Parsec hiding (State)
import Text.Parsec.Indent
import Data.List (isPrefixOf)
import qualified Data.Map as M
import Data.List.Utils

type IParser a = ParsecT String () (State SourcePos) a

iParse :: IParser a -> SourceName -> String -> Either ParseError a
iParse p s inp = runIndent s $ runParserT p () s inp

data Tree = Tree Expression [Tree]
    deriving (Show)

type Attrs = [(String, String)]
data InlineContent = RubyInlineContent String 
    | PlainInlineContent String
    | NullInlineContent
    deriving (Show, Eq)


type IsFormTag = Bool

data Expression = Comment String 
    | PlainText String
    | RubyStartBlock String IsFormTag
    | RubyMidBlock String
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
expression = comment <|> startPlainText <|> (try rubyFormBlock <|> rubyBlock) <|> rubyExp <|> tag <|> genericExpression
  
rubyBlock = do
    char '-'
    spaces
    k <- rubyKeyword
    rest <- manyTill anyChar newline <* spaces
    if (k `elem` midBlockKeywords)
    then return (RubyMidBlock $ k ++ rest)
    else return (RubyStartBlock (k ++ rest) False) 
  where midBlockKeywords = ["else", "elsif", "rescue", "ensure", "when", "end"]

rubyFormBlock = do
    char '='
    spaces 
    k <- string "form"
    rest <- manyTill anyChar newline <* spaces
    return (RubyStartBlock (k ++ rest) True) 

rubyExp = RubyExp <$> ((:) <$> char '=' >> spaces >> manyTill anyChar newline <* spaces)

tag :: IParser Expression
tag = do
    tag <- explicitTag <|> return "div"
    as <- many (dotClass <|> idHash)
    hs <- option [] (hashAttrs)
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
makeClassIdAttrs cs = classes ++ [("id", ids)]
    where classes = map (\x -> ("class", x)) $ map tail $ filter ("." `isPrefixOf`) cs 
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

--- Ruby interpolation delimiters crudley replaced by ERB style
rubyString = do
    between (char '"') (char '"') rString
  where 
    rString = liftM replaceInterpolationDelim $ many stringChar
    stringChar = ('"' <$ string "\\\"") <|> (noneOf "\"") 
    replaceInterpolationDelim = (replace "#{" "<%= ") . (replace "}" " %>")

rubySymbol =  char ':' >> rubyIdentifier
rubySymbolKey = rubyIdentifier <* char ':'

{-
rubyInlineCode = do
    identifier <- rubyIdentifier <|> return ""
    xs <- (between (char '(') (char ')') stuffInsideParens) <|> (return "")
    return $ "<%= " ++ (identifier ++ xs) ++ " %>"
  where 
    stuffInsideParens = do
        -- (++) <$> (many (noneOf "()\n") ) <*> (between (char '(') (char ')') stuffInsideParens)
        -- (many (noneOf "()\n") ) 
        a <- many (noneOf "(") 
        -- b <- (between (char '(') (char ')') stuffInsideParens) <|> return "") 
        -- c <- many (noneOf ")\n")
        return a
-}


rubyValue = do
    xs <- many (noneOf ",(") 
    rest <- ((lookAhead (char ',') >> return ""))
            <|> (includeParens (many (noneOf ")")))
    return $ "<%= " ++ xs ++ rest ++ " %>"
  where 
    includeParens p = do 
        xs' <- between (char '(') (char ')') p
        return $ "(" ++ xs' ++ ")"

rocket = spaces >> string "=>" >> spaces 
aKey = (singleQuotedStr <* rocket)
  <|> (doubleQuotedStr <* rocket)
  <|> (rubySymbol <* rocket)
  <|> (rubySymbolKey <* spaces)

aValue = singleQuotedStr <|> rubyString <|> rubyValue

kvPair :: IParser (String, String)
kvPair = do
  k <- aKey
  v <- aValue 
  return (k, v)


comment :: IParser Expression
comment = do
  char '/' 
  s <- manyTill anyChar newline
  return $ Comment s

startPlainText = do 
  spaces
  a <- noneOf "-=.#%" 
  b <- manyTill anyChar newline
  spaces
  return $ PlainText (a:b)

genericExpression :: IParser Expression
genericExpression = do
  spaces
  s <- manyTill anyChar newline
  spaces
  return $ GenericExpression s


------------------------------------------------------------------------
-- output ERB
-- turn tree structure into an array of lines, including closing tags and indentation level


type Nesting = Int

erb ::  Nesting -> Tree -> [String]

erb n tree@(Tree (Tag t a i) []) 
    | t `elem` selfClosingTags = [pad n ++ selfClosingTag tree]
    -- basically ignores inline content
  where selfClosingTags = ["br", "img", "hr"]

erb n tree@(Tree (Tag t a i) []) = [pad n ++ startTag tree ++ endTag tree]
erb n tree@(Tree (Tag t a i) xs) = (pad n ++ startTag tree) : (processChildren n xs ++ [pad n ++ endTag tree])

erb n tree@(Tree (RubyStartBlock s isform) xs) = 
    (pad n ++ (starttag isform) ++ s ++ " %>") : processChildren n xs
  where 
    starttag True = "<%= "
    starttag False = "<% "
erb n tree@(Tree (RubyMidBlock s) xs) = 
    (pad n ++ "<% " ++ s ++ " %>") : processChildren n xs

erb n tree@(Tree (RubyExp s) _) = [pad n ++ "<%= " ++ s ++ " %>"] 
erb n tree@(Tree (PlainText s) _) = [pad n ++ s] 
erb n tree@(Tree (Comment s) _) = [pad n ++ "<%#" ++ s ++ " %>"] 

erb n x@_ = [pad n ++ show x]

processChildren n xs = concat $ map (erb (n + 1)) $ endtags xs

-- Try to insert "<% end %>" tags correctly
endtags (x@(Tree (RubyStartBlock _ _) _):y@(Tree (RubyMidBlock _) _):xs) = 
    x:(endtags (y:xs))  -- just shift the cursor to the right
endtags (x@(Tree (RubyMidBlock _) _):y@(Tree (RubyMidBlock _) _):xs) = 
    x:(endtags (y:xs))
endtags (x@(Tree (RubyStartBlock _ _) _):xs) = x:(Tree (PlainText "<% end %>") []):(endtags xs)
endtags (x@(Tree (RubyMidBlock _) _):xs) = x:(Tree (PlainText "<% end %>") []):(endtags xs)

-- Move inline Ruby expressions to child tree
endtags (x@(Tree (Tag t a (RubyInlineContent s)) ts):xs) = 
  (Tree (Tag t a NullInlineContent) ((Tree (RubyExp s) []):ts)):(endtags xs)

endtags (x:xs) = x : (endtags xs)
endtags [] = []



startTag :: Tree -> String
startTag (Tree (Tag t a i) _) = "<" ++ t ++ showAttrs a ++ ">" ++ showInlineContent i

endTag :: Tree -> String
endTag (Tree (Tag t _ _) _) = "</" ++ t ++ ">"

selfClosingTag :: Tree -> String
selfClosingTag (Tree (Tag t a _) _) = "<" ++ t ++ showAttrs a ++ "/>"

showAttrs xs = case map makeAttr xs of 
      [] -> ""
      xs' -> " " ++ intercalate " " xs'
    where makeAttr (k,v) =  intercalate "=" [k, "\"" ++ v ++ "\"" ]

showInlineContent (PlainInlineContent s) = s
showInlineContent (RubyInlineContent s) = "RUBY: " ++ s
showInlineContent (NullInlineContent) = ""
    
pad :: Int -> String
pad n = take (n * 2) $ repeat ' ' 

------------------------------------------------------------------------

main = do
    s <- getContents
    case iParse container "" s of 
        Left err -> putStrLn (show err)
        Right s' -> do 
            -- putStrLn (show s')
            putStrLn . unlines $ erb 0 s'




