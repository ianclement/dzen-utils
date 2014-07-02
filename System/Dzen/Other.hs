-- |
-- Module      :  System.Dzen.Interaction
-- Copyright   :  (c) 2014 Ian Clement
-- License     :  GPL 3 (see the LICENSE file in the distribution)
--
-- Maintainer  :  ian.clement@gmail.com
-- Stability   :  experimental
-- Portability :  
--
-- TODO

module System.Dzen.Other
    (-- * Clickable area
     Button(..)
    ,ca
    ) where

data Button = LeftButton | MiddleButton | RightButton deriving (Eq, Enum)

ca :: Transform a => Button -> String -> a -> a
ca btn cmd = transform $ \s -> rawStr ("^ca(" ++ buttonStr ++ "," ++ cmd ++ ")") +++ s +++ rawStr "^ca()"
  where buttonStr = show $ fromEnum btn + 1

-- TODO actions as commands 
