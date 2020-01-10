{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}


import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import System.FilePath
import System.FilePath.Glob

import Test.Tasty
import Test.Tasty.Golden as G

import Pipes.XML
import Pipes.XML.Token
import qualified Pipes.Prelude as P
import Pipes
import Pipes.Core
import Text.Pretty.Simple
import Protolude
import qualified Data.Text as T


listTestFiles :: FilePath -> IO [FilePath]
listTestFiles t = globDir1 pat $ "test/tests" </> t
  where pat = compile "*.xml"

mkGoldenTest :: Show a => Pipe ByteString a IO () 
    -> Text
    -> FilePath 
    -> IO TestTree
mkGoldenTest test name path = do
    let testName   =  takeBaseName path <> ": " <> toS name
        goldenPath = replaceExtension path $ "." 
            <> toS (T.replace " " "_" name) 
            <> ".golden"
    pure $ goldenVsString
        testName
        goldenPath
        do
            v      <- BS.readFile path
            actual <- P.toListM $ yield v >-> test
            return $ toS $ pShowNoColor actual

testFile n t f = mkGoldenTest f n $ "test/tests" </>  t
testFiles n t f =  listTestFiles t  >>=  traverse (mkGoldenTest f n)

onPlants n f = testFile n "dsl/plant_catalog.xml"  
        do produceTokens >-> f

select_plants :: Functor m =>Pipe Token ByteString m ()
select_plants = void $ unPipe $ do
    tag "CATALOG"
    tags "PLANT"
    tag "COMMON"
    pipe $ getText Just //> yield
    stop

select_1_plant :: Functor m =>Pipe Token ByteString m ()
select_1_plant = void $ unPipe $ do
    tag "CATALOG"
    tag "PLANT"
    tag "COMMON"
    pipe $ getText Just //> yield
    stop

select_3rd_plant :: Functor m =>Pipe Token ByteString m ()
select_3rd_plant = void $ unPipe $ do
    tag "CATALOG"
    stimes 2 (tag "PLANT" >> stop)
        <> do
            tag "COMMON"
            pipe $ getText Just //> yield
            stop


main :: IO ()
main = do
    tokenizer <- testFiles "tokenizer" "tokenizer" produceTokens 
    multi_select <- onPlants  "multi select" select_plants 
    single_select <- onPlants "single select" select_1_plant
    pick_plant <- onPlants "3rd select" select_3rd_plant
    defaultMain 
        do testGroup "Tests"
            [ do testGroup "Tokenizer" tokenizer
            , do testGroup "DSL" 
                [ single_select
                , multi_select
                , pick_plant
                ]
            ]
