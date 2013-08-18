module Tests.EscapeSequences where

runTest :: IO String
runTest = return "\\some\\text\tww\SP\BEL\r\n\32more text\x1234\&56"
