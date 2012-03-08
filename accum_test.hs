a = [1,2,3,4,5] :: [Int]

myaccum :: [Int] -> [Int] -> [Int]
myaccum [] rslt = reverse rslt
myaccum (x:xs) [] = myaccum xs [x]
myaccum (x:xs) rslt = myaccum xs ((x + head rslt):rslt)
       
myaccum2 :: [Int] -> [Int]
myaccum2 x = reverse $ foldl fun1 [] x

fun1 :: [Int] -> Int -> [Int]
fun1 b a = if b == []
           then [a]
           else (a + head b):b
     