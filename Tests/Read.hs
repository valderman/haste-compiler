module Tests.Read where

data ADT = Foo | Bar | Baz Int deriving (Read, Eq)

{-# NOINLINE float #-}
{-# NOINLINE double #-}
{-# NOINLINE string #-}
{-# NOINLINE list #-}
{-# NOINLINE adt #-}
{-# NOINLINE strFloat #-}
{-# NOINLINE strDouble #-}
{-# NOINLINE strString #-}
{-# NOINLINE strList #-}
{-# NOINLINE strADT #-}
float :: Float
float = 4395389.944
strFloat = "4395389.944"

double :: Double
double = 453.234098
strDouble = "453.234098"

string :: String
string = "hsifsdlkf"
strString = "\"hsifsdlkf\""

list :: [Int]
list = [32,7,6,49,0,9,9]
strList = "[32,7,6,49,0,9,9]"

adt :: [ADT]
adt = [Foo, Foo, Baz 99, Bar, Foo]
strADT = "[Foo, Foo, Baz 99, Bar, Foo]"

runTest :: IO [Bool]
runTest = return [
    read strFloat  == float,
    read strDouble == double,
    read strString == string,
    read strList   == list,
    read strADT    == adt
  ]
