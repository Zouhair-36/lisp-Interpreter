-- TP-1  --- Implantation d'une sorte de Lisp          -*- coding: utf-8 -*-
{-# OPTIONS_GHC -Wall #-}
--
-- Ce fichier défini les fonctionalités suivantes:
-- - Analyseur lexical
-- - Analyseur syntaxique
-- - Pretty printer
-- - Implantation du langage

---------------------------------------------------------------------------
-- Importations de librairies et définitions de fonctions auxiliaires    --
---------------------------------------------------------------------------

import Text.ParserCombinators.Parsec -- Bibliothèque d'analyse syntaxique.
import Data.Char                -- Conversion de Chars de/vers Int et autres.
import System.IO                -- Pour stdout, hPutStr

---------------------------------------------------------------------------
-- La représentation interne des expressions de notre language           --
---------------------------------------------------------------------------
data Sexp = Snil                        -- La liste vide
          | Ssym String                 -- Un symbole
          | Snum Int                    -- Un entier
          | Snode Sexp [Sexp]           -- Une liste non vide
          -- Génère automatiquement un pretty-printer et une fonction de
          -- comparaison structurelle.
          deriving (Show, Eq)

-- Exemples:
-- (+ 2 3) ==> Snode (Ssym "+")
--                   [Snum 2, Snum 3]
--
-- (/ (* (- 68 32) 5) 9)
--     ==>
-- Snode (Ssym "/")
--       [Snode (Ssym "*")
--              [Snode (Ssym "-")
--                     [Snum 68, Snum 32],
--               Snum 5],
--        Snum 9]

---------------------------------------------------------------------------
-- Analyseur lexical                                                     --
---------------------------------------------------------------------------

pChar :: Char -> Parser ()
pChar c = do { _ <- char c; return () }

-- Les commentaires commencent par un point-virgule et se terminent
-- à la fin de la ligne.
pComment :: Parser ()
pComment = do { pChar ';'; _ <- many (satisfy (\c -> not (c == '\n')));
                (pChar '\n' <|> eof); return ()
              }
-- N'importe quelle combinaison d'espaces et de commentaires est considérée
-- comme du blanc.
pSpaces :: Parser ()
pSpaces = do { _ <- many (do { _ <- space ; return () } <|> pComment);
               return () }

-- Un nombre entier est composé de chiffres.
integer     :: Parser Int
integer = do c <- digit
             integer' (digitToInt c)
          <|> do _ <- satisfy (\c -> (c == '-'))
                 n <- integer
                 return (- n)
    where integer' :: Int -> Parser Int
          integer' n = do c <- digit
                          integer' (10 * n + (digitToInt c))
                       <|> return n

-- Les symboles sont constitués de caractères alphanumériques et de signes
-- de ponctuations.
pSymchar :: Parser Char
pSymchar    = alphaNum <|> satisfy (\c -> c `elem` "!@$%^&*_+-=:|/?<>")
pSymbol :: Parser Sexp
pSymbol= do { s <- many1 (pSymchar);
              return (case parse integer "" s of
                        Right n -> Snum n
                        _ -> Ssym s)
            }

---------------------------------------------------------------------------
-- Analyseur syntaxique                                                  --
---------------------------------------------------------------------------

-- La notation "'E" est équivalente à "(quote E)"
pQuote :: Parser Sexp
pQuote = do { pChar '\''; pSpaces; e <- pSexp;
              return (Snode (Ssym "quote") [e]) }

-- Une liste est de la forme:  ( {e} [. e] )
pList :: Parser Sexp
pList  = do { pChar '('; pSpaces;
              ses <- pTail;
                    return (case ses of [] -> Snil
                                        se : ses' -> Snode se ses')
            }
pTail :: Parser [Sexp]
pTail  = do { pChar ')'; return [] }
     -- <|> do { pChar '.'; pSpaces; e <- pSexp; pSpaces;
     --          pChar ')' <|> error ("Missing ')' after: " ++ show e);
     --          return e }
     <|> do { e <- pSexp; pSpaces; es <- pTail; return (e : es) }

-- Accepte n'importe quel caractère: utilisé en cas d'erreur.
pAny :: Parser (Maybe Char)
pAny = do { c <- anyChar ; return (Just c) } <|> return Nothing

-- Une Sexp peut-être une liste, un symbol ou un entier.
pSexpTop :: Parser Sexp
pSexpTop = do { pSpaces;
                pList <|> pQuote <|> pSymbol
                <|> do { x <- pAny;
                         case x of
                           Nothing -> pzero
                           Just c -> error ("Unexpected char '" ++ [c] ++ "'")
                       }
              }

-- On distingue l'analyse syntaxique d'une Sexp principale de celle d'une
-- sous-Sexp: si l'analyse d'une sous-Sexp échoue à EOF, c'est une erreur de
-- syntaxe alors que si l'analyse de la Sexp principale échoue cela peut être
-- tout à fait normal.
pSexp :: Parser Sexp
pSexp = pSexpTop <|> error "Unexpected end of stream"

-- Une séquence de Sexps.
pSexps :: Parser [Sexp]
pSexps = do pSpaces
            many (do e <- pSexpTop
                     pSpaces
                     return e)

-- Déclare que notre analyseur syntaxique peut-être utilisé pour la fonction
-- générique "read".
instance Read Sexp where
    readsPrec _p s = case parse pSexp "" s of
                      Left _ -> []
                      Right e -> [(e,"")]

---------------------------------------------------------------------------
-- Sexp Pretty Printer                                                   --
---------------------------------------------------------------------------

showSexp' :: Sexp -> ShowS
showSexp' Snil = showString "()"
showSexp' (Snum n) = showsPrec 0 n
showSexp' (Ssym s) = showString s
showSexp' (Snode h t) =
    let showTail [] = showChar ')'
        showTail (e : es) =
            showChar ' ' . showSexp' e . showTail es
    in showChar '(' . showSexp' h . showTail t

-- On peut utiliser notre pretty-printer pour la fonction générique "show"
-- (utilisée par la boucle interactive de GHCi).  Mais avant de faire cela,
-- il faut enlever le "deriving Show" dans la déclaration de Sexp.
{-
instance Show Sexp where
    showsPrec p = showSexp'
-}

-- Pour lire et imprimer des Sexp plus facilement dans la boucle interactive
-- de Hugs/GHCi:
readSexp :: String -> Sexp
readSexp = read
showSexp :: Sexp -> String
showSexp e = showSexp' e ""

---------------------------------------------------------------------------
-- Représentation intermédiaire Lexp                                     --
---------------------------------------------------------------------------

type Var = String

data Lexp = Lnum Int             -- Constante entière.             
          | Lbool Bool           -- Constante Booléenne.             
          | Lvar Var             -- Référence à une variable.      
          | Ltest Lexp Lexp Lexp -- Expression conditionelle.      
          | Lfob [Var] Lexp      -- Construction de fobjet.        
          | Lsend Lexp [Lexp]    -- Appel de fobjet.               
          | Llet Var Lexp Lexp   -- Déclaration non-récursive.     
          -- Déclaration d'une liste de variables qui peuvent être 
          -- mutuellement récursives.
          | Lfix [(Var, Lexp)] Lexp
          deriving (Show, Eq)

fromSsym :: Sexp -> Var
fromSsym (Ssym s) = s
fromSsym _ = error "Expected a symbole"

fromLvar :: Lexp -> Var
fromLvar (Lvar var) = var
fromLvar _ = error "Expected a variable"


snodeSep :: [Sexp] -> [(Var, Lexp)]
snodeSep [] = []
-- Cas des fonctions avec plusieurs arguments
snodeSep (Snode (Snode (Ssym f) params) body : rest) = case s2l (head params) of
    Lnum n   -> (f, Lnum n) : snodeSep body ++ snodeSep rest
    Lvar arg -> (f, Lfob (map fromSsym params) (s2l (head body))) : snodeSep rest
    _        -> error "cas plusieurs arg"
    
-- Cas des variables (sans arguments)
snodeSep (Snode (Ssym var) [body] : rest) =
    (var, s2l body) : snodeSep rest

-- Cas des fonctions mutuellement recursives 
snodeSep (Snode sexp (x:xs): rest) = snodeSep [sexp] ++ snodeSep (x:xs) ++ snodeSep rest
snodeSep _ = error "Invalid fix declaration"


-- Première passe simple qui analyse une Sexp et construit une Lexp équivalente.
s2l :: Sexp -> Lexp
s2l (Snum n) = Lnum n
s2l (Ssym s) = case s of
    "true"  -> Lbool True
    "false" -> Lbool False
    _       -> Lvar s

s2l (Snode e1 []) = s2l e1

s2l (Snode (Ssym "if") [cond, ethen, eelse]) =
    Ltest (s2l cond) (s2l ethen) (s2l eelse)

s2l (Snode (Ssym "fob") args) = Lfob (map (fromLvar . s2l) (init args)) (s2l (last args))

s2l (Snode (Ssym "let") [Ssym x, e1, e2]) =
    Llet x (s2l e1) (s2l e2)

s2l (Snode (Ssym "fix") decls) =
    Lfix (snodeSep (init decls)) (s2l (last decls))

-- Pour les appels de fonction
s2l (Snode f args) = Lsend (s2l f) (map s2l args)

s2l se = error ("Expression Psil inconnue: " ++ showSexp se)

---------------------------------------------------------------------------
-- Représentation du contexte d'exécution                                --
---------------------------------------------------------------------------

-- Type des valeurs manipulées à l'exécution.
data Value = Vnum Int
           | Vbool Bool
           | Vbuiltin ([Value] -> Value)
           | Vfob VEnv [Var] Lexp

instance Show Value where
    showsPrec p (Vnum n) = showsPrec p n
    showsPrec p (Vbool b) = showsPrec p b
    showsPrec _ (Vbuiltin _) = showString "<primitive>"
    showsPrec _ (Vfob _ _ _) = showString "<fobjet>"

type VEnv = [(Var, Value)]

-- L'environnement initial qui contient les fonctions prédéfinies.
env0 :: VEnv
env0 = let binop f op =
              Vbuiltin (\vs -> case vs of
                         [Vnum n1, Vnum n2] -> f (n1 `op` n2)
                         [_, _] -> error "Pas un nombre"
                         _ -> error "Nombre d'arguments incorrect")

          in [("+", binop Vnum (+)),
              ("*", binop Vnum (*)),
              ("/", binop Vnum div),
              ("-", binop Vnum (-)),
              ("<", binop Vbool (<)),
              (">", binop Vbool (>)),
              ("≤", binop Vbool (<=)),
              ("≥", binop Vbool (>=)),
              ("=", binop Vbool (==)),
              ("true",  Vbool True),
              ("false", Vbool False)]

---------------------------------------------------------------------------
-- Évaluateur                                                            --
---------------------------------------------------------------------------

elookup :: Var -> VEnv -> Value
elookup x ((x1,v1):env) =
    if x == x1
    then v1
    else elookup x env
elookup _ [] = error "Variable non trouvée"


eval :: VEnv -> Lexp -> Value
eval _ (Lnum n) = Vnum n
eval _ (Lbool b) = Vbool b
eval env (Lvar x) = elookup x env
eval env (Ltest e1 e2 e3) = case eval env e1 of
    Vbool b  -> if b   then eval env e2   else eval env e3
    _        -> error "first expression is not a boolean"

eval env (Lfob args lexp) = Vfob env args lexp

eval env (Lsend f args) = case eval env f of
    Vfob closure (x:xs) body ->
        if length (x:xs) == length args
        then let newEnv = zip (x:xs) (map (eval env) args) ++ closure
             in eval newEnv body
        else error "Nombre d'arguments incorrect pour la fonction"
    Vbuiltin primitive    -> primitive (map (eval env) args) 
    _ -> error "Tentative d'appel sur une valeur non fonctionnelle"

eval env (Llet x e1 e2) = eval ((x, eval env e1) : env) e2

eval env (Lfix decla body) =
    let -- Création de l'environnement récursif
        recursiveEnv = map (\(var, lexp) -> 
            case lexp of
                Lfob args bodyLexp -> (var, Vfob recursiveEnv args bodyLexp)
                _                  -> (var, eval recursiveEnv lexp)
            ) decla ++ env 
        finalEnv = recursiveEnv ++ env

    in eval finalEnv body

---------------------------------------------------------------------------
-- Toplevel                                                              --
---------------------------------------------------------------------------

evalSexp :: Sexp -> Value
evalSexp = eval env0 . s2l

-- Lit un fichier contenant plusieurs Sexps, les évalues l'une après
-- l'autre, et renvoie la liste des valeurs obtenues.
run :: FilePath -> IO ()
run filename =
    do inputHandle <- openFile filename ReadMode
       hSetEncoding inputHandle utf8
       s <- hGetContents inputHandle
       (hPutStr stdout . show)
           (let sexps s' = case parse pSexps filename s' of
                             Left _ -> [Ssym "#<parse-error>"]
                             Right es -> es
            in map evalSexp (sexps s))
       hClose inputHandle

sexpOf :: String -> Sexp
sexpOf = read

lexpOf :: String -> Lexp
lexpOf = s2l . sexpOf

valOf :: String -> Value
valOf = evalSexp . sexpOf

test :: Lexp
test =  s2l (readSexp "(fix (((even x) (if (= x 0) true (odd (- x 1)))) ((odd x) (if (= x 0) false (even (- x 1))))) (odd 42))")


