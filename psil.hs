-- TP-1  --- Implantation d'une sorte de Lisp          -*- coding: utf-8 -*-
{-# OPTIONS_GHC -Wall #-}

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
          | Scons Sexp Sexp             -- Une paire
          | Ssym String                 -- Un symbole
          | Snum Int                    -- Un entier
          -- Génère automatiquement un pretty-printer et une fonction de
          -- comparaison structurelle.
          deriving (Show, Eq)

-- Exemples:
-- (+ 2 3)  ==  (((() . +) . 2) . 3)
--          ==>  Scons (Scons (Scons Snil (Ssym "+"))
--                            (Snum 2))
--                     (Snum 3)
--                   
-- (/ (* (- 68 32) 5) 9)
--     ==  (((() . /) . (((() . *) . (((() . -) . 68) . 32)) . 5)) . 9)
--     ==>
-- Scons (Scons (Scons Snil (Ssym "/"))
--              (Scons (Scons (Scons Snil (Ssym "*"))
--                            (Scons (Scons (Scons Snil (Ssym "-"))
--                                          (Snum 68))
--                                   (Snum 32)))
--                     (Snum 5)))
--       (Snum 9)

---------------------------------------------------------------------------
-- Analyseur lexical                                                     --
---------------------------------------------------------------------------

pChar :: Char -> Parser ()
pChar c = do { _ <- char c; return () }

-- Les commentaires commencent par un point-virgule et se terminent
-- à la fin de la ligne.
pComment :: Parser ()
pComment = do { pChar ';'; _ <- many (satisfy (\c -> not (c == '\n')));
                pChar '\n'; return ()
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
pSymchar    = alphaNum <|> satisfy (\c -> not (isAscii c)
                                          || c `elem` "!@$%^&*_+-=:|/?<>")
pSymbol :: Parser Sexp
pSymbol= do { s <- many1 (pSymchar);
              return (case parse integer "" s of
                        Right n -> Snum n
                        _ -> Ssym s)
            }

---------------------------------------------------------------------------
-- Analyseur syntaxique                                                  --
---------------------------------------------------------------------------

-- La notation "'E" est équivalente à "(shorthand-quote E)"
-- La notation "`E" est équivalente à "(shorthand-backquote E)"
-- La notation ",E" est équivalente à "(shorthand-comma E)"
pQuote :: Parser Sexp
pQuote = do { c <- satisfy (\c -> c `elem` "'`,"); pSpaces; e <- pSexp;
              return (Scons
                      (Scons Snil
                             (Ssym (case c of
                                     ',' -> "shorthand-comma"
                                     '`' -> "shorthand-backquote"
                                     _   -> "shorthand-quote")))
                      e) }

-- Une liste (Tsil) est de la forme ( [e .] {e} )
pTsil :: Parser Sexp
pTsil = do _ <- char '('
           pSpaces
           (do { _ <- char ')'; return Snil }
            <|> do hd <- (do e <- pSexp
                             pSpaces
                             (do _ <- char '.'
                                 pSpaces
                                 return e
                              <|> return (Scons Snil e)))
                   pLiat hd)
    where pLiat :: Sexp -> Parser Sexp
          pLiat hd = do _ <- char ')'
                        return hd
                 <|> do e <- pSexp
                        pSpaces
                        pLiat (Scons hd e)

-- Accepte n'importe quel caractère: utilisé en cas d'erreur.
pAny :: Parser (Maybe Char)
pAny = do { c <- anyChar ; return (Just c) } <|> return Nothing

-- Une Sexp peut-être une liste, un symbol ou un entier.
pSexpTop :: Parser Sexp
pSexpTop = do { pSpaces;
                pTsil <|> pQuote <|> pSymbol
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
    readsPrec _ s = case parse pSexp "" s of
                      Left _ -> []
                      Right e -> [(e,"")]

---------------------------------------------------------------------------
-- Sexp Pretty Printer                                                   --
---------------------------------------------------------------------------

showSexp' :: Sexp -> ShowS
showSexp' Snil = showString "()"
showSexp' (Snum n) = showsPrec 0 n
showSexp' (Ssym s) = showString s
showSexp' (Scons e1 e2) = showHead (Scons e1 e2) . showString ")"
    where showHead (Scons Snil e') = showString "(" . showSexp' e'
          showHead (Scons e1' e2')
            = showHead e1' . showString " " . showSexp' e2'
          showHead e = showString "(" . showSexp' e . showString " ."

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
-- Représentation intermédiaire                                          --
---------------------------------------------------------------------------

type Var = String
type Constructor = Var
type Lpat = Maybe (Constructor, [Var])
type Stype = String                     -- Le nom du type sous forme de String.
type Lcons = (Constructor, [Stype])     -- Définition de constructeur.
type TVar = Var

data Ltype = Lint               -- Le type des nombres entiers.
           | Limply Ltype Ltype -- Le type d'une abstraction.
           | Ldatatype TVar     -- Le nom d'un type algébrique.
           | Lerror String      -- Erreur de typage.
          deriving (Show, Eq)

data Lexp = Lnum Int                    -- Constante entière.
          | Lvar Var                    -- Référence à une variable.
          | Labs (Var, Stype) Lexp      -- Fonction anonyme prenant un argument.
          | Lapply Lexp Lexp            -- Appel de fonction, avec un argument.
          | Lnew Constructor [Lexp]
          | Lfilter Lexp [(Lpat, Lexp)] -- Filtrage.
          -- Déclaration d'une liste de variables qui peuvent être
          -- mutuellement récursives.
          | Ldef [((Var, Maybe Stype), Lexp)] Lexp
          | Ladt TVar [Lcons] Lexp      -- Définition de type algébrique.
          deriving (Show, Eq)

---------------------------------------------------------------------------
-- Conversion de Sexp à Lexp                                             --
---------------------------------------------------------------------------

sexp2revlist :: Sexp -> [Sexp]
sexp2revlist Snil = []
sexp2revlist (Scons ses se) = se : sexp2revlist ses
sexp2revlist se = error ("Pas une liste: " ++ show se)
            

-- Première passe simple qui analyse un Sexp et construit une Lexp équivalente.
s2l :: Sexp -> Lexp
s2l (Snum n) = Lnum n
s2l (Ssym s) = Lvar s
s2l se@(Scons _ _) = case reverse (sexp2revlist se) of
  [Ssym "abs", sargs, sbody] -> 
      let mkabs (Scons (Scons Snil (Ssym arg)) (Ssym stype)) 
              = Labs (arg, stype) (s2l sbody)
          mkabs (Scons se 
                    (Scons (Scons Snil (Ssym arg)) (Ssym stype)))
              = Labs (arg, stype) (mkabs se)
          mkabs sabs 
              = error ("Argument formel non reconnu: " ++ show sabs)
      in mkabs sargs
  [Ssym "def", decls, sbody] ->
      let s2d (Scons (Scons Snil (Ssym var)) sdef) = ((var, Nothing), s2l sdef)
          s2d (Scons (Scons (Scons Snil (Ssym var)) (Ssym stype)) sdef) 
              = ((var, Just stype), s2l sdef) -- déclaration avec type
          s2d (Scons (Scons 
                        (Scons (Scons Snil (Ssym var)) sargs) (Ssym rtype)) 
                    sdef)
              = ((var, Just rtype),  -- rtype est el type de retour
                    s2l (Scons (Scons (Scons Snil (Ssym "abs")) sargs) sdef))
          s2d decl = error ("Declaration non reconnue: " ++ show decl)
      in Ldef (map s2d (reverse (sexp2revlist decls))) (s2l sbody)
  [Ssym "if", scond, sthen, selse] ->
      Lfilter (s2l scond)
              [(Just ("true:", []), s2l sthen),
               (Just ("false:", []), s2l selse)]
  Ssym "new" : Ssym cons : sargs -> Lnew cons (map s2l sargs)
  Ssym "filter" : starget : sbranches ->
      let s2v (Ssym v) = v
          s2v sv = error ("Variable de motif non reconnue: " ++ show sv)
          s2p (Ssym "_") = Nothing
          s2p (Ssym cons) = Just (cons, [])
          s2p spat@(Scons _ _) = case reverse (sexp2revlist spat) of
            (Ssym cons) : sargs -> Just (cons, map s2v sargs)
            ses -> error ("constructeur non reconnu: " ++ show (head ses))
          s2p spat = error ("Motif non reconnu: " ++ show spat)
          s2b (Scons (Scons Snil spat) sbody) = (s2p spat, s2l sbody)
          s2b sbranch = error ("Branche non reconnue: " ++ show sbranch)
      in Lfilter (s2l starget) (map s2b sbranches)
  [Ssym "adt", Ssym dt, tags, sbody] ->   -- définition de type algébrique
      let s2lcons (c : stypes) = (c, stypes)
          s2lcons a = error ("Défintion de constructeur inconnue: " ++ show a)
          s2a Snil = []
          s2a (Scons se a) = s2lcons (reverse (sexp2revlist a)): s2a se
          s2a tag = error ("Défintion de constructeur inconnue: " ++ show tag)
      in Ladt dt (reverse (s2a tags)) (s2l sbody)
  sfun : sargs ->
      foldl (\ l sarg -> Lapply l (s2l sarg)) (s2l sfun) sargs
  [] -> error "Impossible"
s2l se = error ("Expression Psil inconnue: " ++ (showSexp se))

---------------------------------------------------------------------------
-- Environnement initial et valeurs                                      --
---------------------------------------------------------------------------

-- Type des valeurs manipulées par l'évaluateur.
data Value = Vnum Int
           | Vcons Constructor [Value]
           | Vprim (Value -> Value)

instance Show Value where
    showsPrec p (Vnum n) = showsPrec p n
    showsPrec p (Vcons s args) =
        showString "[" . showString s .
                   showValues (reverse args) . showString "]"
        where showValues [] = showString ""
              showValues (v:vs) = showString " " . showsPrec p v . showValues vs
    showsPrec _ (Vprim _) = showString "<function>"

-- Type de l'environnemnt initial, qui donne autant le type que les valeurs
-- des éléments prédéfinis.
type Env = [(Var, Ltype, Value)]

-- Environnemnts qui gardent les définitions de types salgébriques.
type DTEnv = [(TVar, [(Constructor, [Ltype])])]

dtenv0 :: DTEnv
dtenv0 = [("Bool", [("true:", []), ("false:", [])])
          -- ("ListInt", [("nil:", []),
          --              ("cons:", [Lint, Ldatatype "ListInt"])])
         ]

valbool :: Bool -> Value
valbool x = if x then Vcons "true:"  [] else Vcons "false:" []

-- L'environnement initial qui contient les fonctions prédéfinies.
env0 :: Env
env0 = [("true", bool, valbool True),
        ("false", bool, valbool False),
        ("not", b2b,
              Vprim (\v -> case v of
                            Vcons "true:" [] -> valbool False
                            Vcons "false:" [] -> valbool True
                            _ -> error ("Pas un booléen: " ++ show v))),
        ("+", i2i2i, prim (+)),
        ("-", i2i2i, prim (-)),
        ("*", i2i2i, prim (*)),
        ("/", i2i2i, prim div),
        ("<",  i2i2b, primb (<)),
        ("<=", i2i2b, primb (<=)),
        (">",  i2i2b, primb (>)),
        ("==", i2i2b, primb (==)),
        (">=", i2i2b, primb (>=))]
    where bool = (Ldatatype "Bool")
          i2i2i = Limply Lint (Limply Lint Lint)
          i2i2b = Limply Lint (Limply Lint bool)
          b2b = Limply bool bool

          prim op = Vprim (\v1 -> case v1 of
                           Vnum x -> Vprim (\v2 -> case v2 of
                                            Vnum y -> Vnum (x `op` y)
                                            _ -> error ("Pas un entier: "
                                                       ++ show v2))
                           _ -> error ("Pas un entier: " ++ show v1))
          primb op = Vprim (\v1 -> case v1 of
                            Vnum x -> Vprim (\v2 -> case v2 of
                                             Vnum y -> valbool (x `op` y)
                                             _ -> error ("Pas un entier: "
                                                        ++ show v2))
                            _ -> error ("Pas un entier: " ++ show v1))

---------------------------------------------------------------------------
-- Vérification des types                                                --
---------------------------------------------------------------------------

-- Environnement de typage, qui contient les types des variables.
type TEnv = [(Var, Ltype)]

tenv0 :: TEnv
tenv0 = map (\(x,t,_v) -> (x,t)) env0

-- Fonction central de la vérification des types.
-- Elle implémente le jugement `Δ;Γ ⊢ e : τ`.
-- Δ, Γ, et e sont les trois arguments et elle renvoie le τ correspondant.
check :: DTEnv -> TEnv -> Lexp -> Ltype
check _ _ (Lnum _) = Lint
-- COMPLÉTER ICI!!
check _ _ _ = error("check")

---------------------------------------------------------------------------
-- Interpréteur/compilateur                                              --
---------------------------------------------------------------------------

-- Environnement utilisé à l'exécution, qui contient les valeurs des variables.
type VEnv = [(Var, Value)]

-- La fonction d'évaluation principale.
-- On prend l'environnment en 2 temps, de manière à ce que
-- `eval XS LEXP : [Value] -> Value` compile LEXP vers une
-- fonction Haskell équivalente où [Value] contient les valeurs des
-- variables, dans le même ordre qu'elles étaient dans XS.
-- ATTENTION: Assurez-vous que la fonction renvoyée ne fasse référence
-- ni à XS ni à LEXP, vu que cela signifierait que le code n'a pas
-- vraiment été compilé.
eval :: [Var] -> Lexp -> ([Value] -> Value)
eval _ (Lnum n) = \_vs -> Vnum n
eval xs (Lvar x)
    -- La position de la valeur de X sera la même dans VS qu'elle est
    -- dans XS, donc on la calcule pendant la compilation.
    = let x2pos (y : ys) = if x == y then 0 else 1 + x2pos ys
          x2pos [] = error ("Variable non reconnue: " ++ x)
          pos = x2pos xs
      -- À l'exécution, on peut directement extraire notre valeur de VS
      -- sans comparer des noms de variables.
      in \vs -> vs !! pos
-- COMPLÉTER ICI!!
eval _ _ = error("eval")
---------------------------------------------------------------------------
-- Toplevel                                                              --
---------------------------------------------------------------------------

evalSexp :: Sexp -> Value
evalSexp se = let -- Converti à Lexp
                  le = s2l se
                  -- Compile vers Haskell
                  f = eval (map (\(x,_t,_v) -> x) env0) le
              -- Exécute le code
              in f (map (\(_x,_t,v) -> v) env0)

sexpOf :: String -> Sexp
sexpOf = read

lexpOf :: String -> Lexp
lexpOf = s2l . sexpOf

valOf :: String -> Value
valOf = evalSexp . sexpOf

checkSexp :: Sexp -> Ltype
checkSexp = check dtenv0 tenv0 . s2l
        
typeOf :: String -> Ltype
typeOf = checkSexp . sexpOf


-- Lit un fichier contenant plusieurs Sexps, les évalues l'une après
-- l'autre, et renvoie la liste des valeurs obtenues.
run :: FilePath -> IO ()
run filename =
    let print_list [] = return ()
        print_list ((v,t) : vs)
            = do (hPutStr stdout . show) v
                 hPutStr stdout " : "
                 (hPutStr stdout . show) t
                 hPutStr stdout "\n"
                 print_list vs
    in do inputHandle <- openFile filename ReadMode 
          hSetEncoding inputHandle utf8
          s <- hGetContents inputHandle
          print_list
              (let sexps s' = case parse pSexps filename s' of
                                Left _ -> [Ssym "#<parse-error>"]
                                Right es -> es
               in map (\le -> case checkSexp le of
                               err@(Lerror _) -> (Vcons "(ERROR)" [], err)
                               t -> (evalSexp le, t))
                      (sexps s))
          hClose inputHandle
