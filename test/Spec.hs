import TicLib
import System.FilePath.Glob (glob)
import Test.DocTest (doctest)

main :: IO ()
main = do
  putStrLn "\nSrc Tests:"
  glob "src/**/*.hs" >>= doctest
  putStrLn "App Tests:"
  glob "app/**/*.hs" >>= doctest
