sum' [] = 0
sum' (x:xs) = x + sum' xs

product' [] = 1
product' (x:xs) = x * product' xs

take' _ [] = []
take' n _ | n < 1 = []
take' n (x:xs) = x : take' (n-1) xs

drop' n xs | n < 1 = xs
drop' n (x:xs) = drop' (n-1) xs

reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

fact' n = product [1..n]

main = do
    print $ sum' [1..4]
    print $ product' [1..4]
    print $ take' 3 [1..5]
    print $ drop' 3 [1..6]
    print $ reverse' [1..7]

    print $ fact' 4
