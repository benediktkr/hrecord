{-# LANGUAGE OverloadedStrings #-}

module Password 
  ( genSalt
  , makePassword 
  --, makeHashedPassword
  ) where

import Data.Text (Text)
import Data.Text.Encoding
import qualified Data.Text as T
import qualified Data.ByteString.Base64 as Base64
import System.IO
import Data.ByteString
import Crypto.Hash

staticSalt :: Text
staticSalt = "Benedikt rokkar"

-- Pw :: Text -> Password
newtype Password = Password Text deriving Show

goodPassword :: Text -> Bool
goodPassword passwd = T.length passwd > 7

makePassword :: Text -> Maybe Password
makePassword text 
  | goodPassword text = Just $ Password text
  | otherwise         = Nothing

getPasswordBytes :: Password -> ByteString
getPasswordBytes (Password pass) = encodeUtf8 pass

-- Slt :: ByteString -> Salt
newtype Salt = Salt ByteString deriving Show

genSalt :: Int -> IO Salt
genSalt bytes = do 
  f <- openFile "/dev/urandom" ReadMode
  salt <- hGet f bytes
  return $ Salt $ Base64.encode salt

data HashedPassword = HashedPassword { salt :: Salt
                                     , hashed_password :: Digest SHA512 }

makeHashedPassword :: Salt -> Password -> HashedPassword
makeHashedPassword salt pass = do
  let passwd = getPasswordBytes pass 
  hash_ <- hash passwd  :: Digest SHA512 -- Add salt and stuff
  return $ HashedPassword salt hash_


--instance Show HashedPassword where 
--    show (HashedPassword salt hashed_password) = "sha512$" ++ hashed_password ++ "$" ++ salt
--
-- Example usage
--createNewHashedPassword :: Text -> IO HashedPassword
--createNewHashedPassword pass = do
--    salt <- genSalt 32 
--    passwd <- makePassword pass
--    return $ makeHashedPassword salt passwd
