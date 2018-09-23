{-# LANGUAGE
    FlexibleInstances
  , UndecidableInstances
  , TypeFamilies
  , FlexibleContexts #-}

module UnescapingPrint (unEscapingShow, ushow, unEscapingPrint, uprint) where 

import Prelude(Char, String, IO, putStrLn, ShowS, showString,(.),map,(>))
import GHC.Show (Show(..), showLitChar, showChar)
import Unsafe.Coerce (unsafeCoerce)

newtype UnescapingChar = UnescapingChar {unescapingChar :: Char}

type family ToUnescapingTF a where
  ToUnescapingTF Char = UnescapingChar
  ToUnescapingTF (x a b c d e f g h) = x (ToUnescapingTF a) (ToUnescapingTF b)
                                         (ToUnescapingTF c) (ToUnescapingTF d)
                                         (ToUnescapingTF e) (ToUnescapingTF f)
                                         (ToUnescapingTF g) (ToUnescapingTF h)
  ToUnescapingTF (x a b c d e f g) = x (ToUnescapingTF a) (ToUnescapingTF b)
                                       (ToUnescapingTF c) (ToUnescapingTF d)
                                       (ToUnescapingTF e) (ToUnescapingTF f)
                                       (ToUnescapingTF g)
  ToUnescapingTF (x a b c d e f) = x (ToUnescapingTF a) (ToUnescapingTF b)
                                     (ToUnescapingTF c) (ToUnescapingTF d)
                                     (ToUnescapingTF e) (ToUnescapingTF f)
  ToUnescapingTF (x a b c d e) = x (ToUnescapingTF a) (ToUnescapingTF b)
                                   (ToUnescapingTF c) (ToUnescapingTF d)
                                   (ToUnescapingTF e)
  ToUnescapingTF (x a b c d) = x (ToUnescapingTF a) (ToUnescapingTF b)
                                 (ToUnescapingTF c) (ToUnescapingTF d)
  ToUnescapingTF (x a b c) = x (ToUnescapingTF a) (ToUnescapingTF b)
                               (ToUnescapingTF c)
  ToUnescapingTF (x a b) = x (ToUnescapingTF a) (ToUnescapingTF b)
  ToUnescapingTF (x a) = x (ToUnescapingTF a)
  ToUnescapingTF a = a

class Show a => ToUnescaping a where
    toUnescaping :: a -> ToUnescapingTF a

instance ToUnescaping Char where
    toUnescaping = UnescapingChar

instance Show a => ToUnescaping a where
    toUnescaping = unsafeCoerce

unEscapingShow, ushow :: (ToUnescaping t, Show (ToUnescapingTF t)) => t -> String
unEscapingShow = show . toUnescaping
ushow = unEscapingShow

unEscapingPrint, uprint :: (ToUnescaping t, Show (ToUnescapingTF t)) => t -> IO ()
unEscapingPrint = putStrLn . unEscapingShow
uprint = unEscapingPrint

--------------------------------------------------------------------------

instance  Show UnescapingChar  where
    showsPrec _ (UnescapingChar '\'') = showString "'\\''"
    showsPrec _ (UnescapingChar c)    = showChar '\'' . showLitChar' c . showChar '\''

    showList cs = showChar '"' . showLitString' (map unescapingChar cs) . showChar '"'

showLitChar'                :: Char -> ShowS
showLitChar' c s | c > '\DEL' =  showChar c s
showLitChar' c s = showLitChar c s

showLitString' :: String -> ShowS
showLitString' []         s = s
showLitString' ('"' : cs) s = showString "\\\"" (showLitString' cs s)
showLitString' (c   : cs) s = showLitChar' c (showLitString' cs s)
