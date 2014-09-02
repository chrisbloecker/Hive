{-# LANGUAGE TemplateHaskell, TypeFamilies, RecordWildCards, DeriveDataTypeable, DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hive.Master.Persistent
  where

-------------------------------------------------------------------------------

import qualified Control.Monad.Reader as R  (ask)
import qualified Control.Monad.State  as ST (get, put)

-------------------------------------------------------------------------------

import Data.Acid
import Data.IxSet
import Data.Data
import Data.SafeCopy

import Hive.Types

-------------------------------------------------------------------------------

data Database = Database { ticketSeq :: Ticket
                         , entries   :: IxSet Entry
                         }
  deriving (Data, Typeable)

$(deriveSafeCopy 0 'base ''Problem)
$(deriveSafeCopy 0 'base ''ProblemType)
$(deriveSafeCopy 0 'base ''Solution)
$(deriveSafeCopy 0 'base ''Ticket)
$(deriveSafeCopy 0 'base ''Entry)
$(deriveSafeCopy 0 'base ''Database)

-------------------------------------------------------------------------------

instance Indexable Ticket where
  empty = ixSet [ ixFun $ \t -> [unTicket t] ]

instance Indexable Entry where
  empty = ixSet [ ixFun $ \e -> [ticket   e]
                , ixFun $ \e -> [problem  e]
                , ixFun $ \e -> [solution e]
                ]

-------------------------------------------------------------------------------

initDatabase :: Database
initDatabase = Database { ticketSeq = mkTicket 0
                        , entries   = empty
                        }

getTicketSeq :: Query Database Ticket
getTicketSeq = do
  Database{..} <- R.ask
  return ticketSeq

updateTicketSeq :: Ticket -> Update Database ()
updateTicketSeq t = do
  db@Database{..} <- ST.get
  ST.put $ db { ticketSeq = t }

insertEntry :: Entry -> Update Database ()
insertEntry e = do
  db@Database{..} <- ST.get
  ST.put $ db { entries = updateIx (ticket e) e entries }

updateEntry :: Entry -> Update Database ()
updateEntry = insertEntry

getEntry :: Ticket -> Query Database (Maybe Entry)
getEntry t = do
  Database{..} <- R.ask
  return . getOne $ entries @= t

getHistory :: Ticket -> Ticket -> Query Database [Entry]
getHistory fromTicket toTicket = do
  Database{..} <- R.ask
  return . toDescList (Proxy :: Proxy Ticket) $ entries @>= fromTicket @<= toTicket

-------------------------------------------------------------------------------

makeAcidic ''Database [ 'getTicketSeq
                      , 'updateTicketSeq
                      , 'insertEntry
                      , 'updateEntry
                      , 'getEntry
                      , 'getHistory
                      ]