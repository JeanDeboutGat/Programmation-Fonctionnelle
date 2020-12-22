-- Interprete
-- Jean Debout Gatari



module Main where

import Parser
import Data.Maybe
import System.IO
import Control.Monad
import Data.Either

type Nom = String

data Expression = Lam Nom Expression
                | App Expression Expression
                | Var Nom
                | Lit Litteral
                deriving (Show,Eq)

data Litteral = Entier Integer
              | Bool   Bool
              deriving (Show,Eq)


-- 1 Parseur qui consomme tout les éspaces
-- au début de la chaine analysé
espacesP :: Parser ()
espacesP = (many  (car ' ') ) >>= \_ -> pure ()


-- 2  Un analyseur qui analyse le premier nom dans une chaine
smallChar x = x `elem` ['a' .. 'z']

nomP :: Parser Nom
nomP = do nom <- some (carQuand smallChar)
          espacesP
          pure nom

-- 3 parsing an expression
varP :: Parser Expression
varP = do varName <- nomP
          pure (Var varName)



-- 4 Applique
applique' :: [Expression] -> Expression
applique' [e] = e
applique' [e1, e2] = App e1 e2
applique' (e1:e2:es) =  let h = applique' [e1, e2] in applique'(h:es)


-- Applique avec foldl
applique :: [Expression] -> Expression
applique = foldl1 (\x y -> App x y)


-- expression parsers
exprP :: Parser Expression
exprP = varP <|> lambdaP <|> exprParentheseeP <|> nombreP <|> booleenP

exprsP :: Parser Expression
exprsP = some exprP >>= \l -> pure $ applique l


