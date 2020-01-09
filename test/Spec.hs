{-# LANGUAGE BlockArguments #-}


import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import System.FilePath
import System.FilePath.Glob

import Test.Tasty
import Test.Tasty.Golden as G

import Pipes.XML
import qualified Pipes.Prelude as P
import Pipes
import Text.Pretty.Simple
import Protolude


main :: IO ()
main = do
  tokenizer <- listTestFiles "tokenizer" >>=  traverse (mkGoldenTest produceTokens)
  defaultMain 
    do testGroup "Tests" tokenizer

listTestFiles :: FilePath -> IO [FilePath]
listTestFiles t = globDir1 pat $ "test/tests" </> t
  where pat = compile "*.xml"

mkGoldenTest :: Show a => Pipe ByteString a IO () -> FilePath -> IO TestTree
mkGoldenTest test path = do
  let testName   = takeBaseName path
      goldenPath = replaceExtension path ".golden"
  pure $ goldenVsString testName goldenPath 
    do 
      v <- BS.readFile path
      actual <- P.toListM $ yield v >-> test
      return $ toS $ pShowNoColor actual