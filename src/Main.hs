{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Configuration.Dotenv ( loadFile, defaultConfig )
import           Control.Exception ( Exception, throw )
import           Control.Monad ( void, mapM_ )
import qualified Data.ByteString            as B
import           Data.Char ( isSpace )
import           Data.Data ( Typeable )
import qualified Data.Text                  as T
import           Network.FTP.Client ( RTypeCode (TA) )
import qualified Network.FTP.Client         as FTP
import qualified System.Directory           as D
import           System.Environment ( lookupEnv )
import           Text.Read ( readMaybe )

data Credentials = Credentials { user :: String, pass :: String, host :: String, port :: Int }

newtype CredentialsException = CredentialsException String
    deriving (Show, Typeable)

instance Exception CredentialsException

getCredentials :: IO Credentials
getCredentials = do
    _ <- loadFile defaultConfig
    u <- nonEmptyString <$> lookupEnv "FTP_USER"
    p <- nonEmptyString <$> lookupEnv "FTP_PASS"
    h <- nonEmptyString <$> lookupEnv "FTP_HOST"
    pt <- parsePort <$> lookupEnv "FTP_PORT"

    case Credentials <$> u <*> p <*> h <*> pt of
        Just c -> return c
        _ -> throw $ CredentialsException "Invalid FTP credentials"

nonEmptyString :: Maybe String -> Maybe String
nonEmptyString s = case dropWhile isSpace <$> s of
    Just [] -> Nothing
    Just s -> Just s
    _ -> Nothing

parsePort :: Maybe String -> Maybe Int
parsePort Nothing = Nothing
parsePort (Just p) = case readMaybe p :: Maybe Int of
    Just pt
        | pt > 0 -> Just pt
        | otherwise -> Nothing
    _ -> Nothing

main :: IO ()
main = do
    putStrLn ""
    Credentials { user, pass, host, port } <- getCredentials
    handleFtp user pass host port

isTextFile :: FilePath -> Bool
isTextFile f = (==) ".txt" $ T.takeEnd 4 $ T.pack f

txtFilePaths :: IO [FilePath]
txtFilePaths = filter isTextFile <$> D.listDirectory "."

handleFtp :: String -> String -> String -> Int -> IO ()
handleFtp u p host pt = FTP.withFTP host pt $ \h welcome -> do
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
