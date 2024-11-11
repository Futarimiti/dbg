-- | More convenient @trace@, stolen from rust
module Debug.Trace.Dbg
  ( dbg
  , dbgM
  ) where

import Debug.Trace
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.Meta.Parse
import Text.Printf

-- | @[dbg|\<expr\>|]@ evaluates @\<expr\>@ to @\<value\>@,
-- trace it in form of @"\<expr\> = \<value\>"@ and gives @\<value\>@.
--
-- @dbg@ uses @<Debug.Trace.trace>@ under the hood.
-- It's like @<trace>@ but with auto labelling.
--
-- Type of @\<expr\>@ must be an instance of @<Show>@.
--
-- ==== __Examples__
-- >>> :{
-- factorial n
--   | [dbg|n <= 1|] = [dbg|1|]
--   | otherwise = [dbg|n * factorial (n - 1)|]
-- :}
-- >>> factorial 4
-- [<interactive>:1:10] n <= 1 = False
-- [<interactive>:1:10] n <= 1 = False
-- [<interactive>:1:10] n <= 1 = False
-- [<interactive>:1:10] n <= 1 = True
-- [<interactive>:1:26] 1 = 1
-- [<interactive>:2:22] n * factorial (n - 1) = 2
-- [<interactive>:2:22] n * factorial (n - 1) = 6
-- [<interactive>:2:22] n * factorial (n - 1) = 24
-- 24
--
-- Like @<trace>@, evaluation will be carried out lazily:
--
-- >>> const 1 [dbg|undefined|]
-- 1
dbg :: QuasiQuoter
dbg = QuasiQuoter
  { quoteExp = dbgExp
  , quotePat = unimplemented "quotePat"
  , quoteType = unimplemented "quoteType"
  , quoteDec = unimplemented "quoteDec"
  } where
    dbgExp :: String -> Q Exp
    dbgExp str = do
      loc <- location
      let file = loc_filename loc
          (line, col) = loc_start loc
      case parseExp str of
        Left err -> fail $ printf "Parse error in |%s|: %s" str err
        Right e -> [| let result = $(pure e) in trace
          (printf "[%s:%d:%d] %s = %s"
            (file :: String)
            (line :: Int)
            (col :: Int)
            (str :: String)
            (show result :: String))
          result |]

-- | Like @dbg@ but gives @m ()@ where @m@ is an arbitrary Applicative context.
-- Allows for convenient use in do-notation.
--
-- @dbg@ uses @<Debug.Trace.traceM>@ under the hood.
-- It's like @<traceM>@ but with auto labelling.
--
-- ==== __Examples__
-- >>> :{
-- do
--   x <- Just 3
--   y <- pure 2
--   [dbgM|(x,y)|]
--   pure (x*2 + y)
-- :}
-- [<interactive>:5:9] (x,y) = (3,2)
-- Just 8
dbgM :: QuasiQuoter
dbgM = QuasiQuoter
  { quoteExp = dbgExp_
  , quotePat = unimplemented "quotePat"
  , quoteType = unimplemented "quoteType"
  , quoteDec = unimplemented "quoteDec"
  } where
    dbgExp_ :: String -> Q Exp
    dbgExp_ str = do
      loc <- location
      let file = loc_filename loc
          (line, col) = loc_start loc
      case parseExp str of
        Left err -> fail $ printf "Parse error in |%s|: %s" str err
        Right e -> [| let result = $(pure e) in traceM
          (printf "[%s:%d:%d] %s = %s"
            (file :: String)
            (line :: Int)
            (col :: Int)
            (str :: String)
            (show result :: String)) |]

unimplemented :: String -> a
unimplemented what = error $ printf "%s is not implemented" what
