addOneIfOdd n = case odd n of
    True -> (\n -> n + 1) n
    False -> n

addFive x y = (\n -> n + 5) (if x > y then y else x)

mflip f x y = f y x