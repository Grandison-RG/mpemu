module ZipperList ( ZipperList
                  , zipForward
                  , zipBackward
                  , zipHead
                  ) where

type ZipperList a = ([a],[a])

zipForward :: ZipperList a -> ZipperList a
zipForward (xs, y:ys) = (y:xs, ys)
zipForward l = l

zipBackward :: ZipperList a -> ZipperList a
zipBackward (x:xs, ys) = (xs, x:ys)
zipBackward l = l

zipHead :: ZipperList a -> a
zipHead = head . snd
