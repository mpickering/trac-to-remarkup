{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad

import Database.PostgreSQL.Simple
import Trac.Db
import Trac.Db.Types

main :: IO ()
main = do
    conn <- connectPostgreSQL ""
    tickets <- getTickets conn
    --mapM_ print tickets
    mapM_ (getTicketChanges conn >=> print) $ map ticketNumber tickets
