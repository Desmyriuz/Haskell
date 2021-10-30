-- Exercício 2

doubleElements :: [Float] -> [Float]
doubleElements [] = []
doubleElements (ah:at) = (2*ah) : doubleElements at

occursInString :: Char -> String -> Int
occursInString _ [] = 0
occursInString char (ah:at)
   | char == ah = 1 + occursInString char (at)
   | otherwise = occursInString char (at)

justPositive :: [Int] -> Bool
justPositive [] = True
justPositive (ah:at)
   | ah >= 0 = justPositive at
   | otherwise = False

onlyPositive :: [Int] -> [Int]
onlyPositive [] = []
onlyPositive (ah:at)
   | ah >= 0 = ah : onlyPositive at
   | otherwise = onlyPositive at

sumNegative :: [Int] -> Int
sumNegative [] = 0
sumNegative (ah:at)
   | ah >= 0 = sumNegative at
   | otherwise = ah + sumNegative at

-- Exercício 4

type Polinomio = [Monomio]
type Monomio = (Float, Int)

conta :: Int -> Polinomio -> Int
conta _ [] = 0
conta grau ((coef, expo):eqt)
   | grau == expo = 1 + conta grau (eqt)
   | otherwise = conta grau (eqt)

grau :: Polinomio -> Int
grau [] = 0
grau [(coef, expo)] = expo
grau ((coef, expo):(coef2, expo2):eqt)
   | coef >= coef2 = grau ((coef, expo):eqt)
   | otherwise = grau ((coef2, expo2):eqt)

selgrau :: Int -> Polinomio -> Polinomio
selgrau _ [] = []
selgrau expo ((coef, expn):eqt) = if (expo == expn)
                                  then (coef, expn) : selgrau expo eqt
                                  else selgrau expo eqt

derivada :: Polinomio -> Polinomio
derivada [] = []
derivada ((coef, expo):eqt)
   | expo > 0 = (coef * (fromIntegral(expo)) , expo - 1) : derivada eqt
   | otherwise = (0,0) : derivada eqt

calcula :: Float -> Polinomio -> Float
calcula _ [] = 0
calcula num ((coef, expo):eqt) = coef * (num ^expo) + calcula num eqt

noZeroInCoef :: Polinomio -> Polinomio
noZeroInCoef [] = []
noZeroInCoef ((coef, expo):eqt) = if (coef == 0)
                                  then noZeroInCoef eqt
                                  else (coef, expo) : noZeroInCoef eqt

mult :: Monomio -> Polinomio -> Polinomio
mult _ [] = []
mult (moncoef, monexpo) ((coef, expo):eqt) = [(moncoef * coef, monexpo + expo)] ++ mult (moncoef, monexpo) eqt

normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza [(coef, expo)] = [(coef, expo)]
normaliza ((coef, expo):(coef2, expo2):eqt)
   | expo == expo2 = normaliza ((coef + coef2, expo):eqt)
   | conta expo eqt == 0 = (coef, expo) : normaliza ((coef2, expo2):eqt)
   | otherwise = normaliza ((coef, expo):eqt ++ [(coef2, expo2)])

soma :: Polinomio -> Polinomio -> Polinomio
soma [] poli = poli
soma poli [] = poli
soma poli1 poli2 = normaliza (poli1 ++ poli2)

produto :: Polinomio -> Polinomio -> Polinomio
produto [] _ = []
produto _ [] = []
produto ((coef,expo):eqt) polinomio = mult (coef,expo) polinomio ++ produto eqt polinomio

ordena :: Polinomio -> Polinomio
ordena [] = []
ordena [(coef, expo)] = [(coef, expo)]
ordena ((coef, expo):(coef2,expo2):eqt)
   | expo >= expo2 = (coef2,expo2) : ordena ((coef,expo):eqt)
   | otherwise = (coef, expo) : ordena ((coef2, expo2):eqt)

equiv :: Polinomio -> Polinomio -> Bool
equiv poli1 poli2
   | poli1 == normaliza poli2 = True
   | normaliza poli1 == poli2 = True
   | otherwise = False
