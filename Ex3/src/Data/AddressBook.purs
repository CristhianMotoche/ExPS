module Data.AddressBook where

import Prelude

import Data.List
import Data.Maybe
import Control.Plus (empty)

-- Types

type Entry =
  {
      firstName :: String
    , lastName  :: String
    , address   :: Address
  }

type Address =
  {
      street :: String
    , city   :: String
    , state  :: String
  }

type AddressBook = List Entry

-- Functions

emptyBook :: AddressBook
emptyBook = empty

showEntry :: Entry -> String
showEntry entry = entry.firstName ++ ", " ++
              entry.lastName ++ ", " ++
              showAddress entry.address

showAddress :: Address -> String
showAddress address = address.street ++ ", " ++
                      address.city ++ ", " ++
                      address.state ++ ". "

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName book = head $ filter filterEntry book
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

looksUP :: String -> AddressBook -> Maybe Entry
looksUP street book = head $ filter filterEntry book
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.address.street == street

isInAddressBook :: String -> AddressBook -> Boolean
isInAddressBook name book = not null $ filter filterEntry book
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.firstName == name

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubBy filterEntry
  where
    filterEntry :: Entry -> Entry -> Boolean
    filterEntry e1 e2 = e1.firstName == e2.firstName && e1.lastName == e2.lastName
