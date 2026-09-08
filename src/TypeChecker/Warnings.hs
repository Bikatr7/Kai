module TypeChecker.Warnings (reportWarnings, renderWarning) where

import System.IO (hPutStrLn, stderr)
import TypeChecker.Types (TypeWarning)
import Diagnostics (renderTypeWarning)

reportWarnings :: [TypeWarning] -> IO ()
reportWarnings = mapM_ (hPutStrLn stderr . renderWarning)

renderWarning :: TypeWarning -> String
renderWarning = renderTypeWarning
