{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Configuration.Dotenv ( loadFile, defaultConfig )
import           Control.Monad ( void, mapM_ )
import qualified Data.ByteString            as B
import qualified Data.Text                  as T
import           Network.FTP.Client ( RTypeCode (TA) )
import qualified Network.FTP.Client         as FTP
import qualified System.Directory           as D
import           System.Environment ( getEnv )

data Credentials = Credentials { user :: String, pass :: String, host :: String }

getCredentials :: IO Credentials
getCredentials = do
    void $ loadFile defaultConfig
    Credentials <$> getEnv "FTP_USER"
                <*> getEnv "FTP_PASS"
                <*> getEnv "FTP_HOST"

main :: IO ()
main = do
    putStrLn ""
    Credentials { user, pass, host } <- getCredentials
    handleFtp user pass host

isTextFile :: FilePath -> Bool
isTextFile f = (==) ".txt" $ T.takeEnd 4 $ T.pack f

txtFilePaths :: IO [FilePath]
txtFilePaths = filter isTextFile <$> D.listDirectory "."

handleFtp :: String -> String -> String -> IO ()
handleFtp u p host = FTP.withFTP host 5021 $ \h welcome -> do
    initFtp h u p
    tp <- txtFilePaths
    putStrLn $ "Found " ++ show (length tp) ++ " file(s) to upload"
    mapM_ (uploadFile h) tp
    closeFtp h

initFtp :: FTP.Handle -> String -> String -> IO ()
initFtp h u p = do
    FTP.login h u p
    void $ FTP.pasv h

closeFtp :: FTP.Handle -> IO ()
closeFtp = do
    void . FTP.quit

uploadFile :: FTP.Handle -> FilePath -> IO ()
uploadFile h f = do
    putStrLn $ " - Uploading " ++ show f
    bs <- B.readFile f
    FTP.stor h f bs TA