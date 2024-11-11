-- | Interoperability for the monad-logger library.
module Control.Monad.Logger.Dbg
  ( dbgML
  -- * Specialised TH logging
  -- For convenience
  , dbgDebug
  , dbgInfo
  , dbgWarn
  , dbgError
  ) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.Meta.Parse
import Control.Monad.Logger
import Text.Printf
import qualified Data.Text as Text

-- | Log with chosen @<LogLevel>@.
--
-- Not intended to be used directly;
-- use specialised @QuasiQuoter@s below.
--
-- >>> [dbgML LevelDebug|9 * 9|]
-- *** Parse error
dbgML :: LogLevel -> QuasiQuoter
dbgML lvl = QuasiQuoter
  { quoteExp = dbgExpML lvl
  , quotePat = unimplemented "quotePat"
  , quoteType = unimplemented "quoteType"
  , quoteDec = unimplemented "quoteDec"
  } where unimplemented sth = error $ "unimplemented: " ++ sth

dbgExpML :: LogLevel -> String -> Q Exp
dbgExpML lvl str = do
  case parseExp str of
    Left err -> fail $ printf "Failed to parse expression |%s|: %s" str err
    Right e -> [| let result = $(pure e) in $(log')
      (Text.pack (printf "%s = %s"
        (str :: String)
        (show result :: String))) |]
  where log' = case lvl of
                 LevelDebug   -> logDebug
                 LevelInfo    -> logInfo
                 LevelWarn    -> logWarn
                 LevelError   -> logError
                 LevelOther t -> logOther t

dbgDebug, dbgInfo, dbgWarn, dbgError :: QuasiQuoter
dbgDebug = dbgML LevelDebug
dbgInfo = dbgML LevelInfo
dbgWarn = dbgML LevelWarn
dbgError = dbgML LevelError
