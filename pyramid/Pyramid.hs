module Pyramid where
import Data.List

-- własny typ Piramidy
-- Pyramid 
-- [a] <- górne ograniczenie wysokości piramid
-- [a] <- dolne ograniczenie
-- [a] <- lewe ograniczenie
-- [a] <- prawe ograniczenie
-- (Board a) <- plansza gry
data Pyramid a = Pyramid [a] [a] [a] [a] (Board a) --deriving Show

instance Show a => Show (Pyramid a) where
    show (Pyramid top down left right board) = "ograniczenie gorne " ++ show top ++ "\n" ++
                                               "ograniczenie dolne " ++ show down ++ "\n" ++
                                               "ograniczenie lewe  " ++ show left ++ "\n" ++
                                               "ograniczenie prawe " ++ show right ++ "\n" ++
                                               "plansza gry" ++ "\n" ++ show board


-- własny typ planszy gry
-- plansza gry [[[a]]] ma postać [wiersz[kolumna[val1,val2,...]]]
data Board a = Board [[[a]]] --deriving Show --[[[Integer]]] --deriving Show

-- wypisuje plansze gry
showBoard :: Show a => Board a -> String
showBoard (Board []) = ""
showBoard (Board (x:xs)) = show x ++ "\n" ++ showBoard (Board xs)

instance Show a => Show (Board a) where
    show (Board a) = showBoard (Board a)
    
-- tworzy nowy wiersz planszy gry w formie listy pól gry
-- nowe pole gry składa się ze wszystkich możliwych wartości jakie pole może przyjąć
-- wygląd wiersza planszy gry o 4 polach gry
-- [[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]]
newBoardRow _ 0 = []
newBoardRow a n = [x | x<- [1 .. a] ]:newBoardRow a (n-1)

-- tworzy nową plansze gry
-- wygląd planszy gry 4x4
--[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]]
--[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]]
--[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]]
--[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]]
newBoard _ _ 0 = []
newBoard a b n = (newBoardRow a b):(newBoard a b (n-1))

-- rozmiar Piramidy
-- Piramida składa się z pól nxn, funkcja zwraca n
sizeP (Pyramid a _ _ _ _) = length a

-- zwraca górne ograniczenie Piramidy
topP (Pyramid a _ _ _ _) = a

-- zwraca dole ograniczenie Piramidy
downP (Pyramid _ a _ _ _) = a

-- zwraca lewe ograniczenie Piramidy
leftP (Pyramid _ _ a _ _) = a

-- zwraca prawe ograniczenie Piramidy
rightP (Pyramid _ _ _ a _) = a

-- zwraca plansze gry
boardP (Pyramid _ _ _ _ (Board a)) = a

-- zwraca piramide z nową planszą gry
setBoard (Pyramid a b c d _) board = Pyramid a b c d (Board board)

-- zwraca konkretne pole planszy gry
getField x y a = ((a !! x) !! y)

-- zwraca wiersz planszy gry
getRow r a = a !! r

-- zwraca kolumne planszy gry
getColumn c [] = []
getColumn c (x:xs) = (x !! c):getColumn c xs
       
-- sprawdza czy plansza gry jest rozwiązana          
isSolved [] = True
isSolved ((x:xs):fd) = case (isSolved' (x:xs)) of
                                True -> isSolved fd
                                False -> False
                                
-- podmetoda, sprawdza długość pola gry
-- pole o długości 1 (posiada 1 wartość) jest rozwiązane
isSolved' [] = True
isSolved' (x:xs) = case (length x) of
                            1 -> isSolved' xs
                            otherwise -> False
                              
-- zwraca nową plansze gry z uaktualnionym polem gry
updateField t x y l [] = []
updateField t x y l ((s:xs):fd) = case x == xn of
                                        True -> (updateField' t x y l (s:xs)):updateField t x y l fd
                                        False -> (s:xs):updateField t x y l fd
                                    where xn = l - length fd
                                    
--updatePyramid t x y (Pyramid a b c d (Board board)) = Pyramid a b c d (Board (updateField t x y (length a) board))

-- podmetoda uaktualniajaca wiersz gry
updateField' t x y l [] = []
updateField' t x y l (s:xs) = case y == yn of
                                False -> s:updateField' t x y l xs
                                True -> t:updateField' t x y l xs
                            where yn = l - length xs  

-- dostosowuje plansze gry do wszystkich ograniczeń
-- przykład: posiadając wiersz
-- 3 [[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]] , gdzie 3 z lewej strony jest ograniczeniem widoczności piramid
-- aby widzieć 3 piramidy to na pewno w polu 1 nie może być 3 i 4
-- a w polu 2 nie może być 4
-- otrzymamy:
-- [[1,2],[1,2,3],[1,2,3,4],[1,2,3,4]]
applyConstraints pyramid = do let board1 = applyTopConstraint (topP pyramid) (sizeP pyramid) (boardP pyramid)
                              let board2 = applyLeftConstraint (leftP pyramid) (sizeP pyramid) board1
                              let board3 = applyDownConstraint (downP pyramid) (sizeP pyramid) board2
                              let board4 = applyRightConstraint (rightP pyramid) (sizeP pyramid) board3
                              setBoard pyramid board4

-- dostosowuje plansze do ograniczenia, tzn usuwa z pól gry wartości, których nie może być
-- ze względu na ograniczenie
--direction oznacza, z ktorej strony planszy uwzlgedniamy ograniczenia
--0 oznacza od gory, 1 z dołu, 2 od lewej, 3 z prawej
applyConstraint _ 0 x y l board = board
applyConstraint direction n x y l board = do let rev = (reverse (getField (x-1) (y-1) board))
                                             let revlen = length rev
                                             let size = length board
                                             let dropcount = dropCount n revlen size
                                             case direction of
                                                    0 -> applyConstraint direction (n-1) (x+1) y l (updateField (reverse (drop dropcount rev)) x y l board)
                                                    1 -> applyConstraint direction (n-1) (x-1) y l (updateField (reverse (drop dropcount rev)) x y l board)
                                                    2 -> applyConstraint direction (n-1) x (y+1) l (updateField (reverse (drop dropcount rev)) x y l board)
                                                    3 -> applyConstraint direction (n-1) x (y-1) l (updateField (reverse (drop dropcount rev)) x y l board)

-- oblicza ile elementów pola powinno zostać usuniętych
-- ze względu na ograniczenie, rozmiar pola, rozmiar Piramidy
dropCount a l s = case (b>0) of
                            False -> 0
                            True -> b
                  where k = s - (a - 1)
                        b = l - k

-- dostosowuje plansze do górnego ograniczenia
-- usuwając elementy z pól, które na pewno nie mogły by być rozwiązaniem        
applyTopConstraint [] _ board = board
applyTopConstraint (a:xs) l board = case a of
                                         0 -> applyTopConstraint xs l board
                                         1 -> applyTopConstraint xs l (updateField [l] 1 y l board)
                                         otherwise -> applyTopConstraint xs l (applyConstraint 0 a 1 y l board)
                                        where y = l - length xs
                                        
-- dostosowuje plansze do dolnego ograniczenia
-- usuwając elementy z pól, które na pewno nie mogły by być rozwiązaniem                                    
applyDownConstraint [] _ board = board
applyDownConstraint (a:xs) l board = case a of
                                         0 -> applyDownConstraint xs l board
                                         1 -> applyDownConstraint xs l (updateField [l] l y l board)
                                         otherwise -> applyDownConstraint xs l (applyConstraint 1 a l y l board)
                                        where y = l - length xs

--dostosowuje plansze do lewego ograniczenia      
-- usuwając elementy z pól, które na pewno nie mogły by być rozwiązaniem                                   
applyLeftConstraint [] _ board = board
applyLeftConstraint (a:xs) l board = case a of
                                         0 -> applyLeftConstraint xs l board
                                         1 -> applyLeftConstraint xs l (updateField [l] x 1 l board)
                                         otherwise -> applyLeftConstraint xs l (applyConstraint 2 a x 1 l board)
                                        where x = l - length xs

-- dostosowuje plansze do prawego ograniczenia      
-- usuwając elementy z pól, które na pewno nie mogły by być rozwiązaniem                                    
applyRightConstraint [] _ board = board
applyRightConstraint (a:xs) l board = case a of
                                         0 -> applyRightConstraint xs l board
                                         1 -> applyRightConstraint xs l (updateField [l] x l l board)
                                         otherwise -> applyRightConstraint xs l (applyConstraint 3 a x l l board)
                                        where x = l - length xs

-- przeszukuje wiersz planszy gry w celu znaleznia pola o rozmiarze 1 (z jednym elementem)
searchRowForSingle _ [] s _= -1
searchRowForSingle w (x:xs) s l = case ((length x == 1) && (not (elem [w,n] s))) of
                            True -> n
                            False -> searchRowForSingle w xs s l
                           where n = l - length xs

-- przeszukuje plansze gry w celu znalezieniu pól z pojedyńczymi elementami                         
findSingles [] s _ = ([],s)
findSingles ((x:xs):fd) s l = case n of
                                  -1 -> findSingles fd s l
                                  otherwise -> ([w,n],[w,n]:s)
                                where w = l - length fd
                                      n = searchRowForSingle w (x:xs) s l


--searchRowsForDist :: [[[Integer]]]->Int->[[[Integer]]]
-- przeszukuje wiersze planszy gry w celu znalezienia pola z 'wyodrębnionym elementem'
-- wyodrębniony element to taki, który występuje tylko raz w danym wierszu
searchRowsForDistinct [] l = []
searchRowsForDistinct ((x:xs):fd) l = case list of
                                        [] -> searchRowsForDistinct fd l
                                        otherwise -> [a:b | a<-[r], b<-list]:searchRowsForDistinct fd l
                                  where r = l - length fd
                                        list = searchForDistinct (x:xs) l

-- przeszukuje kolumny w celu znalezienia pola z 'wyodrębnionym elementem'
-- wyodrębniony element to taki, który występuje tylko raz w danej kolumnie                                        
searchColsForDistinct board = searchColsForDistinct' board board

-- podmetoda searchColsForDistinct
searchColsForDistinct' _ [] = []
searchColsForDistinct' board (x:xs) = case list of
                                      [] -> searchColsForDistinct' board xs
                                      otherwise -> [a:b | a<-[c], b<-list]:searchColsForDistinct' board xs
                                    where l = length board
                                          c = l - length xs
                                          col = getColumn (c-1) board
                                          list = searchForDistinct col l
                                          
-- szuka w liście pól gry (wierszu lub kolumnie) elementów 'wyodrębnionych'
-- zwraca listę z elementami typu [a,b]
-- gdzie, a - pozycja na której znalezniono element wyodrępniony
--        b - element wyodrębniony  
searchForDistinct _ 0 = []                            
searchForDistinct (x:xs) n = case (length oc) of
                                1 -> [head oc,n]:searchForDistinct (x:xs) (n-1)
                                otherwise -> searchForDistinct (x:xs) (n-1)
                            where oc = occurrence (x:xs) n (length (x:xs))
 
-- liczy wystąpienie danego elementu w liście pól gry (wierszu lub kolumnie)
-- jeśli występuje tylko raz to zwraca indeks tego pola na liście                           
occurrence [] n l = []
occurrence (x:xs) n l = case (elem n x) of
                                True -> index:occurrence xs n l
                                False -> occurrence xs n l
                             where index = l - length xs
                             
-- dostosowuje plansze do elementów 'wyodrębnionych'
-- tzn, usuwa z pól planszy gry gdzie znalezniono element 'wyodrębniony' inne elementy
-- pozostawiając w polu tylko element 'wyodrębniony'
applyDistinct _ [] board = board
applyDistinct d ((x:xs):fd) board = do let b = applyDistinct' d (x:xs) board
                                       applyDistinct d fd b
                                   
-- podmetoda applyDistinct
applyDistinct' _ [] board = board
applyDistinct' d (x:xs) board = case d of
                                0 -> do let b = updateField t r c l board
                                        applyDistinct' d xs b
                                1 -> do let b = updateField t c r l board
                                        applyDistinct' d xs b
                             where t = [x !! 2]
                                   r = x !! 0
                                   c = x !! 1
                                   l = length board
                                     
-- usuwa z planszy gry, w danej kolumnie podany element                        
deleteFromColumn _ board _ 0 = board             
deleteFromColumn c board v l = deleteFromColumn c (updateField newField l c (length board) board) v (l-1)
                                    where newField = case (length field) of
                                                            1 -> field
                                                            otherwise -> delete v field
                                          field = (getField (l-1) (c-1) board)
                                          
-- usuwa z planszy gry, w danym wierszu podany element
deleteFromRow _ board _ 0 = board             
deleteFromRow r board v l = deleteFromRow r (updateField newField r l (length board) board) v (l-1)
                                    where newField = case (length field) of
                                                            1 -> field
                                                            otherwise -> delete v field
                                          field = (getField (r-1) (l-1) board)

-- sprawdza czy na liście znajdują się zduplikowane elementy                                          
hasDuplicate _ 0 = False                   
hasDuplicate (x:xs) l = case hasElement of
                             False -> hasDuplicate (xs++[x]) (l-1)
                             True -> True
                          where hasElement = elem x xs
                          
-- usuwa elementy z listy, które zawierają w sobie zduplikowane wartości            
removeIfDuplicateElements a = [x | x<-a, not (hasDuplicate x (length x))]
                                                       
-- uaktualnia plansze gry do ograniczenia
-- usuwając elementy gry które nie występują w możliwych rozwiązaniach
matchCondition _ [] board = board                       
matchCondition d (x:xs) board = case d of
                                0 -> case x of
                                        0 -> matchCondition d xs board
                                        1 -> matchCondition d xs board
                                        4 -> matchCondition d xs board
                                        otherwise -> case (length comb) of
                                                            1 -> matchCondition d xs board
                                                            otherwise -> do let ps = removeIfDuplicateElements comb --possible solutions
                                                                            let correct = getCorrectSolutions ps 0 x
                                                                            case (length correct) of
                                                                                    0 -> matchCondition d xs board
                                                                                    1 -> matchCondition d xs (updateCol (head correct) n board)    
                                                                                    otherwise -> do let av = reverse (availableElements correct (length board))
                                                                                                    let av2 = removeDuplicateElements av
                                                                                                    matchCondition d xs (updateCol' av2 n board)             
                                                         where comb = sequence (getColumn (n-1) board)
                                1 -> case x of
                                        0 -> matchCondition d xs board
                                        1 -> matchCondition d xs board
                                        --l -> matchCondition d xs board
                                        otherwise -> case (length comb) of
                                                            1 -> matchCondition d xs board
                                                            otherwise -> do let ps = removeIfDuplicateElements comb --possible solutions
                                                                            let correct = getCorrectSolutions ps 1 x
                                                                            case (length correct) of
                                                                                    0 -> matchCondition d xs board
                                                                                    1 -> matchCondition d xs (updateCol (head correct) n board)    
                                                                                    otherwise -> do let av = reverse (availableElements correct (length board))
                                                                                                    let av2 = removeDuplicateElements av
                                                                                                    matchCondition d xs (updateCol' av2 n board)                            
                                                         where comb = sequence (getColumn (n-1) board)
                                2 -> case x of
                                        0 -> matchCondition d xs board
                                        1 -> matchCondition d xs board
                                        --l -> matchCondition d xs board
                                        otherwise -> case (length comb) of
                                                            1 -> matchCondition d xs board
                                                            otherwise -> do let ps = removeIfDuplicateElements comb --possible solutions
                                                                            let correct = getCorrectSolutions ps 0 x
                                                                            case (length correct) of
                                                                                    0 -> matchCondition d xs board
                                                                                    1 -> matchCondition d xs (updateRow (head correct) n board)    
                                                                                    otherwise -> do let av = reverse (availableElements correct (length board))
                                                                                                    let av2 = removeDuplicateElements av
                                                                                                    matchCondition d xs (updateRow' av2 n board)                              
                                                         where comb = sequence (getRow (n-1) board)
                                3 -> case x of
                                        0 -> matchCondition d xs board
                                        1 -> matchCondition d xs board
                                        --l -> matchCondition d xs board
                                        otherwise -> case (length comb) of
                                                            1 -> matchCondition d xs board
                                                            otherwise -> do let ps = removeIfDuplicateElements comb --possible solutions
                                                                            let correct = getCorrectSolutions ps 1 x
                                                                            case (length correct) of
                                                                                    0 -> matchCondition d xs board
                                                                                    1 -> matchCondition d xs (updateRow (head correct) n board)    
                                                                                    otherwise -> do let av = reverse (availableElements correct (length board))
                                                                                                    let av2 = removeDuplicateElements av
                                                                                                    matchCondition d xs (updateRow' av2 n board)
                                                         where comb = sequence (getRow (n-1) board)
                            where l = length board
                                  n = l - length xs

-- uaktualnia całą kolumne w planszy gry
-- gdy do pól uaktualnianej kolumny wstawiamy pojedyńcze elementy
updateCol [] _ board = board
updateCol (s:xs) c board = updateCol xs c (updateField [s] n c l board)
                                    where l = length board
                                          n = l - length xs

-- uaktualnia całą kolumne w planszy gry
-- gdy do pól uaktualnianej kolumny wstawiamy liste elementów                                          
updateCol' [] _ board = board
updateCol' (s:xs) c board = updateCol' xs c (updateField s n c l board)
                                    where l = length board
                                          n = l - length xs

-- uaktualnia cały wiersz w planszy gry
-- gdy do pól uaktualnianego wiersza wstawiamy pojedyńcze elementy                                          
updateRow [] _ board = board
updateRow (s:xs) r board = updateRow xs r (updateField [s] r n l board)
                                    where l = length board
                                          n = l - length xs

-- uaktualnia cały wiersz w planszy gry
-- gdy do pól uaktualnianego wiersza wstawiamy listę elementów                                         
updateRow' [] _ board = board
updateRow' (s:xs) r board = updateRow' xs r (updateField s r n l board)
                                    where l = length board
                                          n = l - length xs
                                          
-- zwraca z podanej listy możliwych rozwiązań te które spełniają podane ograniczenie
getCorrectSolutions [] d h = []
getCorrectSolutions (x:xs) d h= case d of
                            -- od lewej do prawej
                            0 -> case ((countHight x) == h) of
                                        False -> getCorrectSolutions xs d h
                                        True -> x:getCorrectSolutions xs d h
                            --od prawej do lewej
                            1 -> case ((countHight (reverse x)) == h) of
                                        False -> getCorrectSolutions xs d h
                                        True -> x:getCorrectSolutions xs d h

-- zwraca liczbe piramid które widać dla danego rozwiązania
countHight (x:[]) = 1                                  
countHight (x:y:xs) = case (x<y) of
                               True -> 1 + countHight (y:xs)
                               False -> countHight (x:xs)

-- tworzy listę pól z możliwymi elementami na podstawie możliwych rozwiązań
availableElements b 0 = []
availableElements b n = (availableElements' b n):availableElements b (n-1)

--podmetoda availableElements
availableElements' [] n = [] 
availableElements' (x:xs) n = (x !! (n-1)):availableElements' xs n

-- usuwa zduplikowane elementy z podanej listy pól
removeDuplicateElements [] = []
removeDuplicateElements (x:xs) = (removeDuplicateElement x []):removeDuplicateElements xs

-- podmetoda removeDuplicateElements
-- usuwa zduplikowane elementy z pola
removeDuplicateElement [] b = sort(reverse b)
removeDuplicateElement (x:xs) b = case (elem x b) of
                              False -> removeDuplicateElement xs (x:b)
                              True -> removeDuplicateElement xs b
                              
-- rozwiązuje plansze gry piramidy
-- limit ogranicza liczbę iteracji pętli
-- Algorytm jest następujący
-- 1. jeśli limit jest różny od 0 przejdź do kroku 2.
-- 2. znajdź elementy 'wyodrębnione' i dostosuj plansze
-- 3. usuń elementy nadmiarowe
-- 4. dopasuj pola gry do ograniczeń
-- 5. sprawdź czy piramida została rozwiązana
--        jesli tak wyświetl rozwiązanie
--        jeśli nie przejdź do kroku 1 i zmniejsz limit o 1    
solvePyramid _ 0 = print "Nie znaleziono rozwiazania"                        
solvePyramid pyramid limit = do let p1 = findDistinctElements pyramid
                                p2 <- removeRedundantElements p1 []
                                let p3 = matchToConditions p2
                                case (isSolved (boardP p3)) of
                                        True -> do print "Rozwiazanie"
                                                   print p3
                                        False -> solvePyramid p3 (limit-1)

-- szuka na planszy elementów 'wyodrębnionych'
-- w polu gdzie znajduje się element 'wyodrębniony' usuwane są inne elementy
-- w polu pozostaje tylko element 'wyodrębniony'
-- przykład: posiadając wiersz
-- [[1,2,3,4],[1,2,3],[1,2],[1,2,3]]
-- elementem 'wyodrębniony' jest 4 w polu 1 (nie występuje ona w innych polach w wierszu)
-- usuwamy z tego pola inne elementy
-- otrzymamy:
-- [[4],[1,2,3],[1,2],[1,2,3]]
findDistinctElements pyramid = do let distinctInRows = searchRowsForDistinct (boardP pyramid) (sizeP pyramid)
                                  let board1 = applyDistinct 0 distinctInRows (boardP pyramid)
                                  let distinctInCols = searchColsForDistinct board1
                                  let board2 = applyDistinct 1 distinctInCols board1
                                  setBoard pyramid board2

-- usuwa elementy powtarzające się z planszy gry
-- element powtarzający się to taki element, który występuje w polu danego wiersza lub kolumny
-- gdzie nie może występować bo jest rozwiązaniem innego pola w danym wierszu lub kolumnie
-- przykład: posiadając wiersz
-- [[4],[1,2,3,4],[1,2],[1,2,3,4]]
-- usuniemy powtarzające się 4 w wierszu bo 4 jest rozwiązaniem pola 1 w wierszu
-- otrzymamy:
-- [[4],[1,2,3],[1,2],[1,2,3]]
removeRedundantElements pyramid s = case single of
                                            ([],_) -> return pyramid
                                            (a,b) -> do let b1 = deleteFromColumn (head (tail (head b))) (boardP pyramid) v l
                                                        let b2 = deleteFromRow (head (head b)) b1 v l
                                                        removeRedundantElements (setBoard pyramid b2) b
                                                    where v = head (getField ((head a)-1) ((head (tail a))-1) (boardP pyramid))
                                        where single = findSingles (boardP pyramid) s l
                                              l = (length (boardP pyramid))
                                              
-- uaktualnia plansze gry ze względu na wszystkie ograniczenia
-- przykład: posiadając wiersz
-- 3 [[1,2],[1,2],[3],[4]] , gdzie 3 z lewej strony jest ograniczeniem widoczności piramid
-- mamy dwie możliwości:
-- [[1],[2],[3],[4]] widzimy 4 piramidy
-- [[2],[1],[3],[4]] widzimy 3 piramidy
-- wybierzemy rozwiązanie 2 bo spełnia ograniczenie na widoczność 3 piramid
-- otrzymamy:
-- [[2],[1],[3],[4]]
matchToConditions (Pyramid a b c d (Board board)) = do let boarda = matchCondition 0 a board
                                                       let boardb = matchCondition 1 b boarda
                                                       let boardc = matchCondition 2 c boardb
                                                       let boardd = matchCondition 3 d boardc
                                                       Pyramid a b c d (Board boardd)