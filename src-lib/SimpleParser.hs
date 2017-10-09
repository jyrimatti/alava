{-# LANGUAGE NoImplicitPrelude, DeriveFunctor, GeneralizedNewtypeDeriving, ScopedTypeVariables, OverloadedStrings, GADTs #-}
module SimpleParser where

import Foundation(($),(.),(+),fromList,toList,Char,Bool,Maybe(Just,Nothing),Functor(fmap),not,(||),(&&),(==),(/=),elem,(<>),foldr,foldl',filter,notElem)

import Control.Applicative (Applicative,Alternative(empty,(<|>)),pure,some,many,(<*>),(*>),(<*),(<$>),optional)
import Data.Traversable (traverse)
import Data.Text.Lazy (Text,uncons)
import Data.Char (isSpace)
import Data.Monoid (Monoid,mconcat,mempty)

import Syntax (Term(Type,Var,Lam,App,Pi,Ann,Let,Sig,Def,Comment,Paren,Pos,Prod,Sigma,Case),SourcePos(SourcePos),AnnType(UserGiven))

-- http://www.itu.dk/people/carsten/courses/f02/handouts/MonadicParserCombinators.pdf

-- $setup
-- >>> :set -XOverloadedStrings

newtype Parser thing = Parser {parseWithPos :: Text -> SourcePos -> [(thing, Text, SourcePos)]} deriving (Functor,Monoid)

parse :: Parser thing -> Text -> [(thing, Text, SourcePos)]
parse p input = parseWithPos p input (SourcePos 1 1)

instance Applicative Parser where
  pure x = Parser $ \input pos -> [(x, input, pos)]
  Parser af <*> Parser aa = Parser $ \input pos -> [(f a, input2, pos2) | (f, input1, pos1) <- af input pos,
                                                                          (a, input2, pos2) <- aa input1 pos1]

instance Alternative Parser where
  empty = mempty
  p <|> q = Parser $ \input pos ->
    case parseWithPos p input pos of
      [] -> parseWithPos q input pos
      r  -> r

withPos :: Parser Term -> Parser Term
withPos p = Parser $ \input pos -> [(toPos pos a, b, c) | (a, b, c) <- parseWithPos p input pos]
  where toPos _ a@(Pos _ _) = a
        toPos pos a = Pos pos a

getPos :: Term -> (Term -> Term)
getPos (Pos pos _) = Pos pos
getPos (Lam _ Nothing c) = getPos c
getPos (Lam _ (Just b) _) = getPos b
getPos (App a _)   = getPos a
getPos (Pi _ b _)  = getPos b
getPos (Ann _ b _) = getPos b
getPos (Let (a:_) _)   = getPos a
getPos (Sig _ b)   = getPos b
getPos (Def _ b)   = getPos b
getPos (Paren a)   = getPos a
getPos (Sigma _ Nothing (Just c)) = getPos c
getPos (Sigma _ (Just b) _) = getPos b
getPos (Prod (Just a) _) = getPos a
getPos (Case a _ _) = getPos a

reservedChars :: [Char]
reservedChars = ".,-{}():=\\>"

reservedWords :: [[Char]]
reservedWords = ["Type","let","in","->"]

eat :: (Char -> Bool) -> Parser Char
eat predicate = Parser $ \input pos -> case uncons input of
      Just (first, rest) | predicate first -> [(first,rest,updatePos first pos)]
      _ -> []
  where updatePos '\n' (SourcePos line _) = SourcePos (line+1) 0
        updatePos _    (SourcePos line col) = SourcePos line (col+1)



isIdentifierChar :: Char -> Bool
isIdentifierChar c = not (isSpace c || elem c reservedChars)

isNewline :: Char -> Bool
isNewline = (== '\n')



-- |
-- >>> parse (char 'x') "x"
-- [('x',"",...)]
--
-- >>> parse (char 'x') "y"
-- []
char :: Char -> Parser Char
char c = eat (== c)

-- |
-- >>> parse (notChar 'x') "x"
-- []
--
-- >>> parse (notChar 'x') "y"
-- [('y',"",...)]
notChar :: Char -> Parser Char
notChar c = eat (/= c)

-- |
-- >>> parse (text "a") "a"
-- [("a","",...)]
--
-- >>> parse (text "a") "ab"
-- [("a","b",...)]
text :: Text -> Parser Text
text = fmap fromList . traverse char . toList

-- |
-- >>> parse newline "\n"
-- [("\n","",...)]
--
-- >>> parse newline "\n\n"
-- [("\n\n","",...)]
--
-- >>> parse newline "x"
-- []
newline :: Parser Text
newline = fmap fromList $ some $ char '\n'

-- |
-- >>> parse whitespace ""
-- [("","",...)]
--
-- >>> parse whitespace "  "
-- [("  ","",...)]
--
-- >>> parse whitespace " a"
-- [(" ","a",...)]
whitespace :: Parser Text
whitespace = fmap fromList $ many $ eat isSpace

-- |
-- >>> parse ws ""
-- [([],"",...)]
--
-- >>> parse ws "  "
-- [([],"",...)]
--
-- >>> parse ws "a"
-- [([],"a",...)]
--
-- >>> parse ws " a"
-- [([],"a",...)]
--
-- >>> parse ws " {-a-}--b"
-- [([*Comment "a",*Comment "b"],"",...)]
ws :: Parser [Term]
ws = many (whitespace *> comment) <* whitespace

wsNoNewline :: Parser [Term]
wsNoNewline = many (eat (\c -> not (isNewline c) && isSpace c)) *> many comment

-- |
-- >>> parse singlelineComment "-- a"
-- [(*Comment " a","",...)]
--
-- >>> parse singlelineComment "-- a\nb"
-- [(*Comment " a","\nb",...)]
singlelineComment :: Parser Term
singlelineComment = withPos $ Comment . fromList <$> (text "--" *> many (eat $ not . isNewline))

-- |
-- >>> parse multilineComment "{- foo -}"
-- [(*Comment " foo ","",...)]
--
-- >>> parse multilineComment "{- foo\nbar -}"
-- [(*Comment " foo\nbar ","",...)]
multilineComment :: Parser Term
multilineComment = withPos $ Comment . fromList <$> (text "{-" *> content <* text "-}")
  where content = mconcat <$> many(
                               (\c -> [c])      <$> notChar '-' <|>
                               ((\a b -> [a,b]) <$> char '-' <*> notChar '}'))

-- |
-- >>> parse comment "--a  "
-- [(*Comment "a  ","",...)]
comment :: Parser Term
comment = singlelineComment <|> multilineComment

-- |
-- >>> parse (parens var) "(x)"
-- [(Paren (*Var "x"),"",...)]
--
-- >>> parse (parens (parens var)) "((x))"
-- [(Paren (Paren (*Var "x")),"",...)]
parens :: Parser Term -> Parser Term
parens x = Paren <$> (char '(' *> x <* char ')')




expr :: Parser Term
expr = ws *> plet <* ws

-- |
-- >>> parse term "x"
-- [(*Var "x","",...)]
--
-- >>> parse term "(x)"
-- [(Paren (*Var "x"),"",...)]
--
-- >>> parse term "x -> y -> z"
-- [(*Pi Nothing (*Var "x") (*Pi Nothing (*Var "y") (*Var "z")),"",...)]
--
-- >>> parse term "x y -> z"
-- [(*Pi Nothing (*App (*Var "x") (*Var "y")) (*Var "z"),"",...)]
term :: Parser Term
term = pi

-- |
-- >>> parse app "x y"
-- [(*App (*Var "x") (*Var "y"),"",...)]
--
-- >>> parse app "x y z"
-- [(*App (*App (*Var "x") (*Var "y")) (*Var "z"),"",...)]
--
-- >>> parse app "(x y) z"
-- [(*App (Paren (*App (*Var "x") (*Var "y"))) (*Var "z"),"",...)]
--
-- >>> parse app "x (y z)"
-- [(*App (*Var "x") (Paren (*App (*Var "y") (*Var "z"))),"",...)]
app :: Parser Term
app = foldl' toApp <$> factor <*> many (wsNoNewline *> factor)
  where factor = ptype <|> var <|> record <|> recordcon <|> parens term
        toApp a = getPos a . App a

-- |
-- >>> parse ptype "Type"
-- [(*Type,"",...)]
ptype :: Parser Term
ptype = withPos $ text "Type" *> pure Type

-- |
-- >>> parse var "x"
-- [(*Var "x","",...)]
-- 
-- >>> parse var "xy"
-- [(*Var "xy","",...)]
--
-- >>> parse var "x y"
-- [(*Var "x"," y",...)]
--
-- >>> parse var "x.y"
-- [(*Var "x",".y",...)]
--
-- >>> parse var "Type"
-- []
var :: Parser Term
var = withPos $ Var . fromList <$> (Parser $ \input pos -> filter (\(ident,_,_) -> (ident `notElem` reservedWords)) $ parseWithPos (some (eat isIdentifierChar)) input pos)

ann :: Parser Term
ann = withPos $ Ann <$> (char '(' *> ws *> var <* ws <* char ':' <* ws)
           <*> (term <* ws <* char ')')
           <*> pure UserGiven

-- |
-- >>> parse lam "\\x.x"
-- [(*Lam "x" Nothing (*Var "x"),"",...)]
--
-- >>> parse lam "\\x. \\y. x"
-- [(*Lam "x" Nothing (*Lam "y" Nothing (*Var "x")),"",...)]
--
-- >>> parse lam "\\x. Type"
-- [(*Lam "x" Nothing *Type,"",...)]
--
-- >>> parse lam "\\(x:y).x"
-- [(*Lam "x" (Just (*Var "y")) (*Var "x"),"",...)]
--
-- >>> parse lam "\\x y. x"
-- [(*Lam "x" Nothing (*Lam "y" Nothing (*Var "x")),"",...)]
lam :: Parser Term
lam = toLam <$> (char '\\' *> many (wsNoNewline *> (var <|> ann)))
            <*> (ws *> char '.' *> ws *> term)
  where toLam (Pos pos (Var a) : as) c                           = Pos pos $ Lam a Nothing $ toLam as c
        toLam (Pos pos (Ann (Pos _ (Var a)) b UserGiven) : as) c = Pos pos $ Lam a (Just b) $ toLam as c
        toLam [] c = c

-- |
-- >>> parse pi "x -> y"
-- [(*Pi Nothing (*Var "x") (*Var "y"),"",...)]
--
-- >>> parse pi "(x y) -> z"
-- [(*Pi Nothing (Paren (*App (*Var "x") (*Var "y"))) (*Var "z"),"",...)]
--
-- >>> parse pi "x y -> z"
-- [(*Pi Nothing (*App (*Var "x") (*Var "y")) (*Var "z"),"",...)]
--
-- >>> parse pi "x -> y -> z"
-- [(*Pi Nothing (*Var "x") (*Pi Nothing (*Var "y") (*Var "z")),"",...)]
--
-- >>> parse pi "(x : y) -> z"
-- [(*Pi (Just "x") (*Var "y") (*Var "z"),"",...)]
--
-- >>> parse pi "(p : (x : y) -> z) -> q"
-- [(*Pi (Just "p") (*Pi (Just "x") (*Var "y") (*Var "z")) (*Var "q"),"",...)]
--
-- >>> parse pi "(p : x) -> (q : y) -> z"
-- [(*Pi (Just "p") (*Var "x") (*Pi (Just "q") (*Var "y") (*Var "z")),"",...)]
--
-- >>> parse pi "(c: Type) -> (p -> q -> c) -> d"
-- [(*Pi (Just "c") *Type (*Pi Nothing (Paren (*Pi Nothing (*Var "p") (*Pi Nothing (*Var "q") (*Var "c")))) (*Var "d")),"",...)]
pi :: Parser Term
pi = foldr toPi <$> factor1 <*> many (ws *> factor2)
  where factor1 = ws *> (plet <|> lam <|> app <|> ann <|> var <|> ptype <|> record <|> recordcon <|> parens term)
        factor2 = ws *> text "->" *> ws *> term
        toPi b c@(Pos _ (Ann (Pos _ (Var a)) cc UserGiven)) = getPos c $ Pi (Just a) cc b
        toPi b c                                            = getPos c $ Pi Nothing c b

-- |
-- >>> parse valdef "a = b"
-- [(*Def "a" (*Var "b"),"",...)]
valdef :: Parser Term
valdef = toDef <$> (var <* ws <* char '=' <* ws) <*> term
  where toDef (Pos pos (Var a)) b = Pos pos $ Def a b

-- |
-- >>> parse recorddef "a = {b:Type}"
-- [([*Def "a" (*Sigma (Just "b") (Just *Type) Nothing),*Sig "a_" (Pi Nothing (*Sigma (Just "b") (Just *Type) Nothing) (*Sigma (Just "b") (Just *Type) Nothing)),*Def "a_" (Lam "_" Nothing (Var "_"))],...)]
--
-- >>> parse recorddef "a = {b:Type, c:Type}"
-- [([*Def "a" (*Sigma (Just "b") (Just *Type) (Just (*Sigma (Just "c") (Just *Type) Nothing))),*Sig "a_" (Pi Nothing (*Sigma (Just "b") (Just *Type) (Just (*Sigma (Just "c") (Just *Type) Nothing))) (*Sigma (Just "b") (Just *Type) (Just (*Sigma (Just "c") (Just *Type) Nothing)))),*Def "a_" (Lam "_" Nothing (Var "_"))],...)]
--
-- >>> parse recorddef "a (B:Type) = {b:B}"
-- [([*Def "a" (Lam "B" Nothing (*Sigma (Just "b") (Just (*Var "B")) Nothing)),*Sig "a_" (*Pi (Just "B") *Type (Pi Nothing (*Sigma (Just "b") (Just (*Var "B")) Nothing) (*Sigma (Just "b") (Just (*Var "B")) Nothing))),*Def "a_" (Lam "_" Nothing (Lam "_" Nothing (Var "_")))],...)]
--
-- >>> parse recorddef "a B C = {b: B C}"
-- [([*Def "a" (Lam "B" Nothing (Lam "C" Nothing (*Sigma (Just "b") (Just (*App (*Var "B") (*Var "C"))) Nothing))),*Sig "a_" (*Pi Nothing (*Var "B") (*Pi Nothing (*Var "C") (Pi Nothing (*Sigma (Just "b") (Just (*App (*Var "B") (*Var "C"))) Nothing) (*Sigma (Just "b") (Just (*App (*Var "B") (*Var "C"))) Nothing)))),*Def "a_" (Lam "_" Nothing (Lam "_" Nothing (Lam "_" Nothing (Var "_"))))],...)]
recorddef :: Parser [Term]
recorddef = toDef <$> var <*> many (ws *> factor) <* (ws <* char '=' <* ws) <*> record
  where factor = ptype <|> var <|> ann
        toPi (Pos pos (Ann (Pos _ (Var a)) c UserGiven)) b = Pos pos $ Pi (Just a) c b
        toPi b c                                           = getPos b $ Pi Nothing b c
        name Type = Lam "_" Nothing
        name (Var x) = Lam x Nothing
        name (Pos _ x) = name x
        name (Ann x _ _) = name x
        toDef (Pos pos (Var a)) ts rec = [ --Pos pos $ Def a $ foldr toPi rec ts
                                              Pos pos $ Def a $ foldr name rec ts
                                            , Pos pos $ Sig (a<>"_") $ foldr toPi (Pi Nothing rec rec) ts
                                            , Pos pos $ Def (a<>"_") $ foldr (\_ b -> Lam "_" Nothing b) (Lam "_" Nothing $ Var "_") ts
                                            ]
        --lamChain r (Sigma Nothing Nothing Nothing) []             = Prod Nothing Nothing (Just r)
        --lamChain r (Sigma Nothing Nothing Nothing) (x:xs)         = foldr (\a b -> Prod (Just a) (Just b) (Just r)) x xs
        --lamChain r (Sigma (Just a) (Just b) Nothing) xs           = Lam a (Just b) $ lamChain r (Sigma Nothing Nothing Nothing) $ Ann (Var a) b UserGiven : xs
        --lamChain r (Sigma (Just a) (Just b) (Just c)) xs          = Lam a (Just b) $ lamChain r c $ Ann (Var a) b UserGiven : xs
        



-- |
-- >>> parse sigdef "x : Type -> Type -> Type"
-- [(*Sig "x" (*Pi Nothing *Type (*Pi Nothing *Type *Type)),"",...)]
sigdef :: Parser Term
sigdef = toSig <$> var <*> (ws *> char ':' *> ws *> term)
  where toSig (Pos pos (Var a)) b = Pos pos $ Sig a b

-- |
-- >>> parse letExpr "a = b"
-- [([*Def "a" (*Var "b")],"",...)]
--
-- >>> parse letExpr "x : Type"
-- [([*Sig "x" *Type],"",...)]
letExpr ::  Parser [Term]
letExpr = recorddef <|> ((\x -> [x]) <$> valdef) <|> ((\x -> [x]) <$> sigdef)

-- |
-- >>> parse plet "let a = \\x.x in a"
-- [(*Let [*Def "a" (*Lam "x" Nothing (*Var "x"))] (*Var "a"),"",...)]
--
-- >>> parse plet "let a = b in a"
-- [(*Let [*Def "a" (*Var "b")] (*Var "a"),"",...)]
--
-- >>> parse plet "let a = b\n--foo\nc = d in a"
-- [(*Let [*Def "a" (*Var "b"),*Def "c" (*Var "d")] (*Var "a"),"",...)]
--
-- >>> parse plet "let {-c-}a = b in a"
-- [(*Let [*Def "a" (*Var "b")] (*Var "a"),"",...)]
plet :: Parser Term
plet = withPos $ (\as bss c -> Let (mconcat (as : bss)) c) <$> (text "let" *> ws *> letExpr)
                                                           <*> many (wsNoNewline *> newline *> ws *> letExpr)
                                                           <*> (ws *> text "in" *> ws *> term)

-- |
-- >>> parse record "{ a: Type }"
-- [(*Sigma (Just "a") (Just *Type) Nothing,...)]
--
-- >>> parse record "{ a: Type, b: Type, c: Type }"
-- [(*Sigma (Just "a") (Just *Type) (Just (*Sigma (Just "b") (Just *Type) (Just (*Sigma (Just "c") (Just *Type) Nothing)))),...)]
--
-- >>> parse record "{ a: Type, b: Type -> Type }"
-- [(*Sigma (Just "a") (Just *Type) (Just (*Sigma (Just "b") (Just (*Pi Nothing *Type *Type)) Nothing)),...)]
--
-- >>> parse record "{}"
-- [(*Sigma Nothing Nothing Nothing,...)]
--
-- >>> parse record "{ a: A, b: (T A) }"
-- [(*Sigma (Just "a") (Just (*Var "A")) (Just (*Sigma (Just "b") (Just (Paren (*App (*Var "T") (*Var "A")))) Nothing)),...)]
record :: Parser Term
record = withPos $ rec <$> (char '{' *> optional (ws *> sigdef)) <*> many (ws *> char ',' *> ws *> sigdef) <* ws <* char '}'
  where rec :: Maybe Term -> [Term] -> Term
        rec Nothing [] = Sigma Nothing Nothing Nothing
        rec (Just (Pos pos (Sig n t))) [] = Pos pos $ Sigma (Just n) (Just t) Nothing
        rec (Just (Pos pos (Sig n t))) (x:xs) = Pos pos $ Sigma (Just n) (Just t) (Just $ rec (Just x) xs)

-- |
-- >>> parse recordcon "{ a = Type }"
-- [(*Prod (Just (*Def "a" *Type)) Nothing,...)]
--
-- >>> parse recordcon "{ a = Type, b = Type }"
-- [(*Prod (Just (*Def "a" *Type)) (Just (Prod (Just (*Def "b" *Type)) Nothing)),...)]
--
-- >>> parse recordcon "{}"
-- [(*Prod Nothing Nothing,...)]
recordcon :: Parser Term
recordcon = withPos $ rec <$> (char '{' *> optional (ws *> valdef)) <*> many (ws *> char ',' *> ws *> valdef) <* ws <* char '}'
  where rec :: Maybe Term -> [Term] -> Term
        rec Nothing        [] = Prod Nothing Nothing
        rec d@(Just _)     [] = Prod d Nothing
        rec d@(Just _) (x:xs) = Prod d (Just $ rec (Just x) xs)
