# pipes-xml


Streaming xml retokenizer combinator library based on pipes

## example

retokenizing a veeam soap API response

```haskell

parseVM :: Text -> Maybe Int
parseVM = preview _Right . parseOnly 
            do  skipSpace 
                char '"'
                string $ "urn:VMware:Vm:" 
                takeTill (=='.')
                char '.'
                string "vm-" 
                n <- decimal
                char '"'
                pure n

data OVM = RP | VM Int | RPT (S.Set Bool) deriving Show

makePrisms ''OVM

isLTFile = S.singleton . B.isInfixOf "month" 

getVMs :: forall m . MonadIO m => Pipe Token OVM m Loop
getVMs  = unPipe $ do 
        tag "VmRestorePoints" 
        tags "VmRestorePoint"
        pipe $ yield RP
        mappend
            do
                tag "Links"
                m <- tags "Link" 
                let mr =  do 
                        "BackupFileReference" <- m ^? ix "Type"
                        m ^? ix "Name" 
                case mr of 
                    Nothing -> pure () 
                    Just x -> pipe $ yield (RPT $ isLTFile x)
                stop
            do
                tag "HierarchyObjRef"
                pipe $ getText (parseVM . toS) //> yield . VM
                stop
```

## TODO

Drop Xeno dependency for a real streaming tokenizer
