-- #hide
module Data.Time.Calendar.Private where

import Data.Fixed

type NumericPadOption = Maybe Char

pad1 :: NumericPadOption -> String -> String
pad1 (Just c) s = c:s
pad1 _ s = s

padN :: Int -> Char -> String -> String
padN i _ s | i <= 0 = s
padN i c s = (replicate i c) ++ s

show2Fixed :: NumericPadOption -> Pico -> String
show2Fixed opt x | x < 10 = pad1 opt (showFixed True x)
show2Fixed _ x = showFixed True x

showPaddedMin :: (Num t,Ord t,Show t) => Int -> NumericPadOption -> t -> String
showPaddedMin _ Nothing i = show i
showPaddedMin pl opt i | i < 0 = '-':(showPaddedMin pl opt (negate i))
showPaddedMin pl (Just c) i =
  let s = show i in 
    padN (pl - (length s)) c s

show2 :: (Num t,Ord t,Show t) => NumericPadOption -> t -> String
show2 = showPaddedMin 2

show3 :: (Num t,Ord t,Show t) => NumericPadOption -> t -> String
show3 = showPaddedMin 3

show4 :: (Num t,Ord t,Show t) => NumericPadOption -> t -> String
show4 = showPaddedMin 4

mod100 :: (Integral i) => i -> i
mod100 x = mod x 100

div100 :: (Integral i) => i -> i
div100 x = div x 100

clip :: (Ord t) => t -> t -> t -> t
clip a _ x | x < a = a
clip _ b x | x > b = b
clip _ _ x = x

clipValid :: (Ord t) => t -> t -> t -> Maybe t
clipValid a _ x | x < a = Nothing
clipValid _ b x | x > b = Nothing
clipValid _ _ x = Just x
