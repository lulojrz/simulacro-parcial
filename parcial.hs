mapeo = [('a','b'),('c','d')]
frase = ['c','a','p','i','t','a']

--ejercicio 1
hayQueCodificar :: Char -> [(Char,Char)] -> Bool
hayQueCodificar _ [] = False
hayQueCodificar r ((x,y):xs)| r == x = True
                            | otherwise = hayQueCodificar  r xs 

--ejercicio 2
cuantasVecesHayQueCodificar:: Char ->[Char] -> [(Char,Char)] -> Int
cuantasVecesHayQueCodificar _ [] _ = 0
cuantasVecesHayQueCodificar caracter frase map   | hayQueCodificar caracter map == False  && contarLetra caracter frase == 0  = 0 
                                                 |hayQueCodificar caracter map == True  && contarLetra caracter frase /= 0 = contarLetra caracter frase
                                         


contarLetra :: Char -> [Char] -> Int
contarLetra _ [] =0
contarLetra letra (x:xs)   | letra == x = 1 + contarLetra letra xs 
                           | otherwise = contarLetra letra xs



--ejercicio 3
laQueMasHayQueCodificar :: [Char] -> [(Char,Char)] -> Char
laQueMasHayQueCodificar (x:xs) lista | hayQueCodificar x lista == True = comparar (x:xs)
                                     | otherwise = laQueMasHayQueCodificar xs lista


comparar::[Char] -> Char
comparar (x:y:xs) | contarLetra x (x:y:xs) > contarLetra y (x:y:xs) = x
                  | contarLetra x (x:xs) == contarLetra y (y:xs) = x 
                  | otherwise = comparar(y:xs) 

--ejercicio 4
codificarFrase :: [Char] -> [(Char,Char)] -> [Char]
codificarFrase _ [] = []
codificarFrase (x:xs) mapeo | hayQueCodificar x mapeo == True  = (reemplazarLetra x mapeo : codificarFrase xs mapeo) 
                            | otherwise = codificarFrase xs mapeo

reemplazarLetra :: Char -> [(Char,Char)] -> Char
reemplazarLetra letra ((x,y):xs) | letra == x = y
                                 | otherwise = x