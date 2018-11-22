module Util where

import Control.Applicative
import Data.Foldable


choice :: Alternative f => [a] -> f a
choice = asum . map pure

rotate :: Int -> [a] -> [a]
rotate n x = uncurry (flip (++)) (splitAt (mod n (length x)) x)