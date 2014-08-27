{-# LANGUAGE TemplateHaskell, TypeFamilies, RecordWildCards, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hive.Master.Persistent
  where

-------------------------------------------------------------------------------

import Control.Monad.Reader (ask)
import Control.Monad.State  (get, put)

-------------------------------------------------------------------------------

import Data.Acid
import Data.IxSet
import Data.Typeable
import Data.Data
import Data.SafeCopy

import Hive.Types

-------------------------------------------------------------------------------

data Entry = Entry { ticket   :: Ticket
                   , problem  :: Problem
                   , solution :: Maybe Solution
                   }
  deriving (Eq, Ord, Data, Typeable)

data Database = Database { ticketSeq :: Ticket
                         , entries   :: IxSet Entry
                         }
  deriving (Data, Typeable)

$(deriveSafeCopy 0 'base ''Problem)
deriveSafeCopy 0 'base ''ProblemType
deriveSafeCopy 0 'base ''Instance
deriveSafeCopy 0 'base ''Solution
deriveSafeCopy 0 'base ''Ticket
deriveSafeCopy 0 'base ''Entry
deriveSafeCopy 0 'base ''Database

-------------------------------------------------------------------------------

instance Indexable Ticket where
  empty = ixSet [ ixFun $ \t -> [unTicket t] ]

instance Indexable Entry where
  empty = ixSet [ ixFun $ \e -> [ticket   e]
                , ixFun $ \e -> [problem  e]
                , ixFun $ \e -> [solution e]
                ]

-------------------------------------------------------------------------------

mkEntry :: Ticket -> Problem -> Maybe Solution -> Entry
mkEntry = Entry

-------------------------------------------------------------------------------

initDatabase :: Database
initDatabase = Database { ticketSeq = mkTicket 0
                        , entries   = empty
                        }

getDatabase :: Query Database Database
getDatabase = ask

putDatabase :: Database -> Update Database ()
putDatabase = put

getTicketSeq :: Query Database Ticket
getTicketSeq = do
  Database{..} <- ask
  return ticketSeq

updateTicketSeq :: Ticket -> Update Database ()
updateTicketSeq t = do
  db@Database{..} <- get
  put $ db { ticketSeq = t }

insertEntry :: Entry -> Update Database ()
insertEntry e = do
  db@Database{..} <- get
  put $ db { entries = updateIx (ticket e) e entries }

updateEntry :: Entry -> Update Database ()
updateEntry = insertEntry

getEntry :: Ticket -> Query Database (Maybe Entry)
getEntry t = do
  Database{..} <- ask
  return . getOne $ entries @= t

-------------------------------------------------------------------------------
{-
acidQuery :: (QueryEvent event, MethodState event ~ EcomState) => event -> Handler (EventResult event)
acidQuery q = do
  state <- getEcomState <$> getYesod
  query' state q

acidUpdate :: (UpdateEvent event, MethodState event ~ EcomState) => event -> Handler (EventResult event)
acidUpdate q = do
  state <- getEcomState <$> getYesod
  update' state q
-}
-------------------------------------------------------------------------------

makeAcidic ''Database [ 'getDatabase
                      , 'putDatabase
                      , 'getTicketSeq
                      , 'updateTicketSeq
                      , 'insertEntry
                      , 'updateEntry
                      , 'getEntry
                      ]