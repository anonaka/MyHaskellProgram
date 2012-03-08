a = [1,2,3,4,5] :: [Int]

myaccum :: [Int] -> [Int]
myaccum xs = myaccum_it xs []

myaccum_it :: [Int] -> [Int] -> [Int]
myaccum_it [] rslt = reverse rslt
myaccum_it (x:xs) [] = myaccum_it xs [x]
myaccum_it (x:xs) rslt = myaccum_it xs ((x + head rslt):rslt)
       
myaccum2 :: [Int] -> [Int]
myaccum2 x = reverse $ foldl fun1 [] x

fun1 :: [Int] -> Int -> [Int]
fun1 b a = if b == []
           then [a]
           else (a + head b):b
     