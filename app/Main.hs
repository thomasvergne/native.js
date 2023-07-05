module Main where
import qualified Language.NativeJS.Parser.Parser as P

main :: IO ()
main = do
  let file = "example/test.js"
  x <- decodeUtf8 <$> readFileBS file
  y <- P.parseNative file x
  case y of
    Left err -> print err
    Right ast -> print ast
