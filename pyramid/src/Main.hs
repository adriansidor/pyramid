module Main where
import Data.Char

data Parser a = P (String -> [(a, String)])

-- parser 'return' i operator sekwencji
-- (od wersji ghc 7.10 klasa Monad jest podklasa klasy Applicative)

instance Functor Parser where
  fmap f m = m >>= pure . f

instance Applicative Parser where
  pure v = P (\inp -> [(v, inp)])
  f1 <*> f2 = f1 >>= \v1 -> f2 >>= (pure . v1)

instance Monad Parser where
  return = pure
  p >>= f  = P (\inp -> case parse p inp of
                          [] -> []
                          [(v, out)] -> parse (f v) out)

-- podstawowe parsery

item  :: Parser Char
item =  P (\inp -> case inp of
                     []     -> []
                     (x:xs) -> [(x,xs)])

failure :: Parser a
failure = P (\inp -> [])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = P (\inp -> case parse p inp of
                       [] -> parse q inp
                       [(v, out)] -> [(v, out)])

-- dalsze prymitywy

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else failure

digit :: Parser Char
digit = sat isDigit

char :: Char -> Parser Char
char x = sat (== x)

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do v <- p
             vs <- many p
             return (v:vs)
         
--value :: [(a,String)] -> a
value inp = case inp of
                [(n, _)] -> n
rest inp = case inp of
                [(_,n)] -> n
main::IO()
a :: Parser (Char, Char)
a = do x <- item
       item
       y <- item
       return (x, y)
       
word :: Parser String
word = do v <- item
          case v of
               ' ' -> return []
               _ -> do vs <- word
                       return (v:vs)
term :: Parser String
term = do v <- item
          case v of
                ' ' -> do vs <- term
                          return vs
                ',' -> return []
                ']' -> return []
                '[' -> do vs <- term
                          return vs
                _ -> do vs <- term
                        return (v:vs)

--decode :: Parser String                     
decode = do c <- item
            case c of
--                '[' -> do terms <- many term
--                          let tab = toTable terms
--                          vs <- decode
--                          --return (terms:vs)
--                          return (tab:vs)
                '[' -> do terms <- term
                          let tab = toTable terms
                          vs <- decode
                          --return (terms:vs)
                          return (tab:vs)
                ' ' -> do vs <- decode
                          return vs
           +++
            --return []
            return []
p :: Parser String
p = do d <- digit
       ds <- many (do char ','
                      digit)
       char ']'
       return (d:ds)
moi [] = []
moi (x:xs) = [x]:moi xs
test = do x <- item
          case x of
               '[' -> do v <- p
                         vs <- test
                         return (v:vs)
               ' ' -> do vs <- test
                         return vs
         +++
          return []
toTable [] = []
toTable (x:xs) = case x of
                        "Nothing" -> 0:toTable xs
                        _ -> do case (isDigit d) of
                                    True -> (digitToInt d):toTable xs
                                    False -> [] --tu jest cos zle, nie zwraca pustej listy
                                where d = last x 
main = do
    content <- readFile "pyramid.txt"
    putStrLn content
    let p = parse word content
    let a = value p
    print a
    let b = parse decode (rest p)
    let c = value b
    print c
    putStrLn "bla"
--    putStrLn w
--    case w of
--         "Piramidy" -> do putStrLn content
    --print . value w