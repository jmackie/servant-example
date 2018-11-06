-- |
-- Module      : Data.String.Multiline
-- Description : Quasiquoter for nicer multiline strings
--
module Data.String.Multiline
    ( string
    , stringWith
    )
where

import Prelude

import qualified Language.Haskell.TH as TH

import Language.Haskell.TH.Quote (QuasiQuoter(..))


string :: QuasiQuoter
string = stringWith id


stringWith :: (String -> String) -> QuasiQuoter
stringWith f = QuasiQuoter
    { quoteExp  = pure . TH.LitE . TH.StringL . f . normaliseNewlines
    , quotePat  = \_ ->
        fail
            "illegal raw string QuasiQuote (allowed as expression only, used as a pattern)"
    , quoteType = \_ ->
        fail
            "illegal raw string QuasiQuote (allowed as expression only, used as a type)"
    , quoteDec  = \_ ->
        fail
            "illegal raw string QuasiQuote (allowed as expression only, used as a declaration)"
    }


-- | Linux FTW.
normaliseNewlines :: String -> String
normaliseNewlines []                 = []
normaliseNewlines ('\r' : '\n' : cs) = '\n' : normaliseNewlines cs
normaliseNewlines (c           : cs) = c : normaliseNewlines cs
