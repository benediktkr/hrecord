module Model where

import Yesod
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Quasi
import Database.Persist.MongoDB hiding (master)
import Language.Haskell.TH.Syntax
import Data.Typeable (Typeable)
import Control.Applicative
import Control.Monad
import Prelude

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
let mongoSettings = (mkPersistSettings (ConT ''MongoBackend))
                        { mpsGeneric = False
                        }
 in share [mkPersist mongoSettings]
    $(persistFileWith upperCaseSettings "config/models")

goodPassword :: Text -> Bool
goodPassword passwd = T.length passwd > 7

goodUsername :: Text -> Bool
goodUsername name 
  | Text.length name <= 1                                    = False
  | ' ' `elem` name                                       = False
  | elem (map Text.toLower name) ["server", "ca", "lokun"]   = False
  | not Text.all isAlphaNum name                            = False
  | otherwise                                             = True

instance ToJSON LokUser where
  toJSON (LokUser ide pass email isk btc left end) = object
   [ "ident"      .= ide
   , "email"      .= email
   , "credit_isk" .= isk
   , "credit_btc" .= btc
   , "dl_left"    .= left
   , "sub_end"    .= end
   ]

instance FromJSON LokUser where
  parseJSON (Object o) = LokUser
      <$> o .: "username"
      <*> o .: "password"
      <*> o .: "email"
      <*> o .: "credit_isk"
      <*> o .: "credit_btc"
      <*> o .: "dl_left"
      <*> o .: "sub_end"

  parseJSON _ = mzero


  
  
