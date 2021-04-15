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
import           System.Environment ( lookupEnv, getArgs )
import           Text.Read ( readMaybe )

data Credentials = Credentials { user :: String, pass :: String, host :: String, port :: Int }

newtype CredentialsException = CredentialsException String
    deriving (Show, Typeable)

instance Exception CredentialsException

main :: IO ()
main = do
    a <- getArgs
    case a of
        [] -> getCredentialsFromEnv >>= handleFtp
        [a, b, c, d] -> getCredentialsFromArgs >>= handleFtp
        _ -> help

getCredentialsFromEnv :: IO Credentials
getCredentialsFromEnv = do
    _ <- loadFile defaultConfig
    u <- nonEmptyString <$> lookupEnv "FTP_USER"
    p <- nonEmptyString <$> lookupEnv "FTP_PASS"
    h <- nonEmptyString <$> lookupEnv "FTP_HOST"
    pt <- parsePort <$> lookupEnv "FTP_PORT"

    case Credentials <$> u <*> p <*> h <*> pt of
        Just c -> return c
        _ -> throw $ CredentialsException "Invalid FTP credentials in .env file"

getCredentialsFromArgs :: IO Credentials
getCredentialsFromArgs = do
    u  <- nonEmptyString . pluck 0 <$> getArgs
    p  <- nonEmptyString . pluck 1 <$> getArgs
    h  <- nonEmptyString . pluck 2 <$> getArgs
    pt <- parsePort      . pluck 3 <$> getArgs
    case Credentials <$> u <*> p <*> h <*> pt of
        Just c -> return c
        _ -> throw $ CredentialsException "Invalid FTP credentials given as arguments"

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

isTextFile :: FilePath -> Bool
isTextFile f = (==) ".txt" $ T.takeEnd 4 $ T.pack f

txtFilePaths :: IO [FilePath]
txtFilePaths = filter isTextFile <$> D.listDirectory "."

handleFtp :: Credentials -> IO ()
handleFtp Credentials { user, pass, host, port } = FTP.withFTP host port $ \h welcome -> do
    initFtp h user pass
    tp <- txtFilePaths
    putStrLn $ "Found " ++ show (length tp) ++ " file(s) to upload"
    mapM_ (uploadFile h) tp
    closeFtp h

initFtp :: FTP.Handle -> String -> String -> IO ()
initFtp h u p = do
    FTP.login h u p
    void $ FTP.pasv h

closeFtp :: FTP.Handle -> IO ()
closeFtp = void . FTP.quit

uploadFile :: FTP.Handle -> FilePath -> IO ()
uploadFile h f = do
    putStr $ " - Uploading " ++ show f ++ "..."
    bs <- B.readFile f
    FTP.stor h f bs TA
    putStrLn "  Success!"

help :: IO ()
help = putStrLn $
    "ftp-upload-txt\n\n" 
    ++ "This script is used to upload all adjacent .txt files to a target FTP server. "
    ++ "There are two ways provide this script with FTP credentials:\n"
    ++ "  1. Using an adjacent .env file (FTP_USER, FTP_PASS, FTP_HOST, FTP_PORT)\n"
    ++ "  2. Via command line arguments (order: user, pass, host, port)\n\n"
    ++ "Example: `ftp-upload-txt devuser GoodPassword123 ftp.my.server.com 21`\n\n"
    ++ "For more information please review the README."

pluck :: Int -> [a] -> Maybe a
pluck i arr
    | length arr <= i = Nothing
    | otherwise = Just (arr !! i)
