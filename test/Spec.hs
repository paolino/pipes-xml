{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, NoImplicitPrelude #-}


import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import System.FilePath
import System.FilePath.Glob

import Test.Tasty
import Test.Tasty.Golden as G
import Test.Tasty.Golden.Advanced as G

import Pipes.XML
import Pipes.XML.Token
import qualified Pipes.Prelude as P
import Pipes
import Pipes.Core
import Text.Pretty.Simple
import Protolude hiding (to)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Control.Lens
import Data.Attoparsec.ByteString.Char8
import qualified Pipes.Prelude as P
import Control.Monad.Fail

listTestFiles :: FilePath -> IO [FilePath]
listTestFiles t = globDir1 pat $ "test/tests" </> t
  where pat = compile "*.xml"


testPipe 
    :: Show a 
    => Pipe Token a IO () 
    -> Text
    -> FilePath 
    -> TestTree 
testPipe test name input = goldenTest testName
    do BS.readFile goldenPath
    do
        v      <- BS.readFile input
        actual <- P.toListM $ yield v >-> produceTokens >-> test
        return $ toS $ pShowNoColor actual
    do \g t -> if g == t then pure Nothing 
                else do 
                    BS.writeFile failedPath t
                    pure $ Just ("failed: " <> failedPath)
    do BS.writeFile goldenPath 
    where   testName   =  takeBaseName input <> " - " <> toS name
            goldenPath = replaceExtension input $ "." 
                <> toS (T.replace " " "_" name) 
                <> ".golden"
            failedPath = replaceExtension input $ "." 
                <> toS (T.replace " " "_" name) 
                <> ".failed"

testFile n t f = testPipe f n $ "test/tests" </>  t
testFiles n ts f =  map (\t -> testPipe f n t) ts

onPlants n = testFile n "dsl/plant_catalog.xml"  


select_plants :: Functor m =>Pipe Token ByteString m ()
select_plants = void $ renderPipe $ do
    tag "CATALOG"
    tags "PLANT"
    tag "COMMON"
    pipe $ getText Just //> yield
    stop

select_1_plant :: Functor m => Pipe Token ByteString m ()
select_1_plant = void $ renderPipe $ do
    tag "CATALOG"
    tag "PLANT"
    tag "COMMON"
    pipe $ getText Just //> yield
    stop

select_3rd_plant :: Functor m => Pipe Token ByteString m ()
select_3rd_plant = void $ renderPipe $ do
    tag "CATALOG"
    stimes 2 (tag "PLANT" >> stop)
        <> do
            tag "COMMON"
            pipe $ getText Just //> yield
            stop

select_zone4_plants :: MonadFail m => Pipe Token Text m ()
select_zone4_plants = 
    (>->)
        do  void $ renderPipe $ do
                tag "CATALOG"
                tags "PLANT"
                mconcat 
                    [ do 
                        tag "COMMON" 
                        pipe $ getText (preview $ to T.decodeUtf8' . _Right) //> yield . Left 
                        stop
                    , do 
                        tag "ZONE"
                        let t x = Just 4 == x ^? to (parseOnly decimal) . _Right
                        pipe $ getText Just  //> yield . Right . t
                        stop
                    ]
        do  forever $ do
                Left plant <- await
                Right zone4 <- await
                when zone4 $ yield plant




main :: IO ()
main = do
    tokenizer_files <- listTestFiles "tokenizer"
    defaultMain 
        do testGroup "Tests"
            [ do testGroup "Tokenizer" 
                $ testFiles "tokenizer" tokenizer_files cat
            , do testGroup "DSL" 
                        [ onPlants "multi select" select_plants
                        , onPlants "single select" select_1_plant
                        , onPlants "3rd select" select_3rd_plant
                        , onPlants "zone 4 plants" select_zone4_plants
                        ]
            ]
