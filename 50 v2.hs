module Test50questions where

-- 1.
entreDoisLimites' :: Int -> Int -> [Int]
entreDoisLimites'  _ 0 = []
entreDoisLimites' n m = n : entreDoisLimites' (n + 1) (m - 1)

-- 3.
concatena :: [a] -> [a] -> [a]
concatena listaA [] = listaA
concatena [] listaB = listaB
concatena (x:y) (z:u) = x : concatena y (z:u)

-- 4.
elementoNaPosicao :: [a] -> Int -> a
elementoNaPosicao [] _ = error "A lista não tem elementos!"
elementoNaPosicao (x:y) 0 = x
elementoNaPosicao (x:y) n = elementoNaPosicao y (n - 1)

-- 5.
reverterLista :: [a] -> [a]
reverterLista [] = []
reverterLista (x:y) = reverterLista y ++ [x]

-- 6.
removerPos :: Int -> [a] -> [a]
removerPos _ [] = []
removerPos 1 (x:y) = [x]
removerPos n (x:y) = x : removerPos (n - 1) y

-- 7.
libertarElementos :: Int -> [a] -> [a]
libertarElementos _ [] = []
libertarElementos 0 listaA = listaA
libertarElementos n (x:y) = libertarElementos (n - 1) y

-- 8.
concatenarEmPares :: [a] -> [b] -> [(a,b)]
concatenarEmPares [] _ = []
concatenarEmPares _ [] = []
concatenarEmPares (x:y) (z:u) = (x,z) : concatenarEmPares y u

-- 9.
replicarElemento :: Int -> a -> [a]
replicarElemento 0 ele = []
replicarElemento rep ele = ele : replicarElemento (rep - 1) ele

-- 10.
intercalado :: Int -> [a] -> [a]
intercalado _ [] = []
