import TicLib
import Control.Monad (forM_)
import System.FilePath.Glob (glob)
import Test.DocTest (doctest)

main :: IO ()
main = do
  files <- (++) <$> glob "src/**/*.hs" 
                <*> glob "app/**/*.hs"
  forM_ files $ \f -> doctest [f]
