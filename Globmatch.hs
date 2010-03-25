module Globmatch (match) where

data MEChar = Unesc Char
            | Esc Char
            deriving (Show)
              
type Pattern = String

match :: Pattern -> String -> Bool
match p s = match' p' s
  where p' = parseEscaping p

parseEscaping :: String -> [MEChar]
parseEscaping []          = []
parseEscaping ('\\':x:xs) = Esc x : parseEscaping xs
parseEscaping ('\\':_)    = error "Escape char at end of string."
parseEscaping (x:xs)      = Unesc x : parseEscaping xs
              
match' :: [MEChar] -> String -> Bool
match' [] [] =
  True
match' (Unesc '*' : ps) (x:xs) =
  match' ps (x:xs)             ||
  match' ps xs                 ||
  match' (Unesc '*' : ps) xs
match' (Unesc '[' : ps) (x:xs) =
  let (cs, ps') = parseGroup ps
  in x `elem` cs &&
     match' ps' xs
match' (Unesc '?' : ps) (x:xs) =
  match' ps xs
match' (p:ps) (x:xs) =
  let p' = whatever p
  in p' == x &&
     match' ps xs
match' _ _ =
  False

whatever :: MEChar -> Char
whatever (Esc x)   = x
whatever (Unesc x) = x

parseGroup :: [MEChar] -> ([Char], [MEChar])
parseGroup (Unesc ']' : gs) = (']':gs', rs)
  where (gs', rs) = parseGroup' [] gs
parseGroup gs = parseGroup' [] gs

parseGroup' :: [Char] -> [MEChar] -> ([Char], [MEChar])
parseGroup' acc (Unesc ']' : gs) =
  (acc, gs)
parseGroup' acc (g:gs) =
  parseGroup' (whatever g : acc) gs
parseGroup' _ _ =
  error "Character group missing end ]."
