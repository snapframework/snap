{-# LANGUAGE DeriveDataTypeable #-}

{-|

  Defines functions for generating and checking salted hashes.  Salted hashes
  are used to store passwords in a way that prevents passwords from being
  deduced even if the user/password database is disclosed.

-}

module Snap.Snaplet.Auth.Password
  ( SaltedHash(..)
  , Salt(..)
  , buildSaltAndHash
  , hashPassword
  , checkSalt
  , HashFunc
  , defaultHash
  ) where

import           Numeric
import           Random

import           Codec.Utils
import           Data.ByteString.Internal (c2w)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Digest.SHA512
import           Data.Generics hiding ((:+:))


------------------------------------------------------------------------------
-- | Type alias for hash functions.
type HashFunc = [Octet] -> [Octet]


------------------------------------------------------------------------------
-- | Salt newtype gives us type safety and allows us to control how salts are
-- accessed.
newtype Salt = Salt { unSalt :: [Octet] }
  deriving (Read,Show,Ord,Eq,Typeable,Data)


------------------------------------------------------------------------------
-- | Data structure representing a salted hash.
data SaltedHash = SaltedHash 
  { shSalt :: Salt
  , shHash :: [Octet]
  } deriving (Read,Show,Ord,Eq,Typeable,Data)


------------------------------------------------------------------------------
-- | The length of our salts.
sALT_LENGTH :: Int
sALT_LENGTH = 16


------------------------------------------------------------------------------
-- | Converts a String to an array of Octets.
--strToOctets :: String -> [Octet]
--strToOctets = listToOctets . (map c2w)


------------------------------------------------------------------------------
-- | An slow, iterated SHA512 hash function to make dictionary attacks more
-- difficult.
defaultHash :: HashFunc
defaultHash a = (iterate hash a) !! 512


------------------------------------------------------------------------------
-- | Generates a random salt.
randomSalt :: IO Salt
randomSalt = do
    chars <- sequence $ take sALT_LENGTH $ repeat $
        randomRIO (0::Int,15) >>= return . flip showHex ""
    return $ Salt $ map c2w $ concat chars

------------------------------------------------------------------------------
-- | Generates a random salt, hashes it, and returns a 'SaltedHash'.
buildSaltAndHash :: HashFunc -> ByteString -> IO SaltedHash
buildSaltAndHash hf str = do
    salt <- randomSalt
    return $ hashPassword hf salt str


------------------------------------------------------------------------------
-- | Hash the given salt ++ password and wrap into a 'SaltedHash'.
hashPassword :: HashFunc -> Salt -> ByteString -> SaltedHash
hashPassword hf s pwd = SaltedHash s h
  where h = hf ((unSalt s) ++ pwd')
        pwd' = B.unpack pwd


------------------------------------------------------------------------------
-- | Checks that the input string is the same as the SaltedHash.
checkSalt :: HashFunc -> ByteString -> SaltedHash -> Bool
checkSalt hf str (SaltedHash (Salt salt) h) =
    h == (hf $ salt++(B.unpack str))

