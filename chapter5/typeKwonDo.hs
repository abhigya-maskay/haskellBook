f :: Int -> String
f = show

g :: String -> Char
g = head 

h :: Int -> Char
h = head . show

data A 
data B
data C

aToB :: A -> B
aToB = undefined

bToC :: B -> C
bToC = undefined

aToC :: A -> C
aToC = undefined

q :: A -> B
q = aToB

w :: B -> C
w = bToC

e :: A -> C
e = aToC

data X
data Y
data Z

xToZ :: X -> Z
xToZ = undefined

yToZ :: Y -> Z
yToZ = undefined

xz :: X -> Z
xz = xToZ

yz :: Y -> Z
yz = yToZ

xform :: (X,Y) -> (Z, Z)
xform (x,y) = (xz x, yz y)

munge :: (x -> y) -> (y -> (w,z)) -> x -> w
munge xToY yToWz = fst . yToWz . xToY