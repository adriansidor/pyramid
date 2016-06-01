module Parser where
import Data.Char

data Parser a = P (String -> [(a, String)])

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

-- zwraca część, którą udało się sparsować        
value :: [(a,String)] -> a
value inp = case inp of
                [(n, _)] -> n

-- zwraca część nieskonsumowaną przez parser                
unconsumed :: [(a, String)] -> String
unconsumed inp = case inp of
                [(_,n)] -> n

-- parser, który czyta jedno słowo       
word :: Parser String
word = do v <- item
          case v of
               ' ' -> return []
               _ -> do vs <- word
                       return (v:vs)
                       
-- parser, który czyta jedną wartość ze String typu "[ wartość1, wartość2, wartość3]"
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

-- parser, który czyta wszystkie wartości ze String typu "[v1, v2, v3] [v4, v5, v6] ..."
-- zwraca je w formie tablicy wartości "[v1,v2,v3,v4,v5,v6,...]
terms :: Parser [String]                    
terms  = do c <- item
            case c of
                '[' -> do t <- many term
                          return t
                ' ' -> do vs <- terms
                          return vs
           +++
            return []

-- zamienia wartości typu String na typ numeryczny i zwraca je na liście
-- String "Nothing" zamienia na 0, String "_______x" zamienia na x jeśli x jest cyfrą
decodeHights :: [String]->[Int]
decodeHights [] = []
decodeHights (x:xs) = case x of
                        "Nothing" -> 0:decodeHights xs
                        _ -> do case (isDigit d) of
                                    True -> (digitToInt d):decodeHights xs
                                    False -> []
                                where d = last x 