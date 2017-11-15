module Skyline where

-- Cabecera del programa Skyline.hs
-- David Pastor Sanz
-- Práctica de Teoría de los Lenguajes de Programación
-- Curso 2015-2016

-- Tipos
type Edificio = (Int,Int,Int)
type Coordenada = (Int,Int)
type Skyline = [Coordenada]

-- Funciones
-- Recibe una lista de edificios y devuelve un valor de tipo Skyline
resuelveSkyline :: [Edificio] -> Skyline
resuelveSkyline []     = []
resuelveSkyline (x:[]) = edificioAskyline x
resuelveSkyline xs     =
  let left  = resuelveSkyline (fst div)
      right = resuelveSkyline (snd div)
  in combina left right
  where
    div = divide xs
    
-- Recibe un único edificio y devuelve su línea horizonte
edificioAskyline :: Edificio -> Skyline
edificioAskyline (x,y,z)
  | x < y && z /= 0 = [(x,z),(y,0)]
  | otherwise       = error( "Revise las coordenadas: \n -La primera coordenada debe ser mayor que la segunda \n -La altura (tercera coordenada) debe ser mayor que 0" )

-- Divide en dos mitades la lista de edificios. Devuelve una dupla con dos listas de edificios
divide :: [Edificio] -> ([Edificio],[Edificio])
divide xs = divideAux xs ([],[])
  where
    divideAux [] acumulator  = acumulator
    divideAux (x:[]) (a,b)   = (x:a,b)
    divideAux (x:y:[]) (a,b) = (x:a,y:b)
    divideAux (x:y:xs) (a,b) = divideAux xs (x:a,y:b)

-- Combina las soluciones parciales de los subproblemas
combina :: Skyline -> Skyline -> Skyline
combina xs ys = combinaAux xs ys 0 0 0 []
  where
    combinaAux [] [] _ _ _ sal = sal
    combinaAux lx@((x,hx):xs) [] _ _ ph sal
      | hx /= ph  = sal ++ lx
      | otherwise = sal ++ xs 
    combinaAux [] ly@((y,hy):ys) _ _ ph sal
      | hy /= ph  = sal ++ ly
      | otherwise = sal ++ ys
    combinaAux lx@((x,hx):xs) ly@((y,hy):ys) phx phy ph sal
      | x == y && (difh hx hy)     = combinaAux xs ys hx hy  (max hx hy)  (sal ++ [(x,(max hx hy))])
      | x == y && not(difh hx hy)  = combinaAux xs ys hx hy ph sal
      | x < y  && (difh hx phy)    = combinaAux xs ly hx phy (max hx phy) (sal ++ [(x,(max hx phy))])
      | x < y  && not(difh hx phy) = combinaAux xs ly hx phy ph sal
      | x > y  && (difh phx hy)    = combinaAux lx ys phx hy (max phx hy) (sal ++ [(y,(max phx hy))])
      | x > y  && not(difh phx hy) = combinaAux lx ys phx hy ph sal
        where
         difh h1 h2 = ph /= (max h1 h2)

-- Dibujar skyline. Imprime un skyline en pantalla
dibujaSkyline :: Skyline -> String
dibujaSkyline p@((a,b):xs) = dibujaSkylineAux listh (maximum listh) " "
  where
    dibujaSkylineAux [] 0 result       = result ++ "-"
    dibujaSkylineAux (ph:phx) 0 result = dibujaSkylineAux phx 0 (result ++ "-")
    dibujaSkylineAux [] 1 result       = dibujaSkylineAux listh 0 (result ++ " \n" ++ "-")
    dibujaSkylineAux [] n result       = dibujaSkylineAux listh (n-1) (result ++ " \n ")
    dibujaSkylineAux (ph:phx) n result
      | ph >= n   = dibujaSkylineAux phx n (result ++ "*")
      | otherwise = dibujaSkylineAux phx n (result ++ " ")
    listh = (listhAux p (take (a-1) (repeat 0)))
      where
        listhAux [] sol                      = sol
        listhAux (head:[]) sol               = sol
        listhAux ((a1,b1):(a2,b2):tail) sol  = listhAux ((a2,b2):tail) (sol ++ (take (a2-a1) (repeat b1)))