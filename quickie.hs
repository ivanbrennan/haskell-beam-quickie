{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Database.Beam (Beamable, Columnar, Database, DatabaseSettings, Generic,
                      Identity, PrimaryKey, Table, TableEntity, aggregate_, all_,
                      as_, asc_, countAll_, defaultDbSettings, desc_, group_,
                      insert, insertValues, liftIO, orderBy_, primaryKey,
                      runInsert, runSelectReturningList, runSelectReturningOne,
                      select)
import Database.Beam.Sqlite (runBeamSqliteDebug)
import Database.SQLite.Simple (open)
import Data.Text (Text)


data UserT f = User
  { _userEmail     :: Columnar f Text
  , _userFirstName :: Columnar f Text
  , _userLastName  :: Columnar f Text
  , _userPassword  :: Columnar f Text
  } deriving (Generic, Beamable)

type User = UserT Identity
type UserId = PrimaryKey UserT Identity

deriving instance Show User
deriving instance Eq User

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f Text) deriving (Generic, Beamable)
  primaryKey = UserId . _userEmail


data ShoppingCartDb f = ShoppingCartDb
  { _shoppingCartUsers :: f (TableEntity UserT)
  } deriving (Generic, Database backend)

shoppingCartDb :: DatabaseSettings backend ShoppingCartDb
shoppingCartDb = defaultDbSettings


main :: IO ()
main = do
  conn <- open "shoppingcart.db"

  runBeamSqliteDebug putStrLn conn $
    runInsert $
      insert (_shoppingCartUsers shoppingCartDb) $
        insertValues
          [ User
              "james@example.com"
              "James"
              "Smith"
              "b4cc344d25a2efe540adbf2678e2304c"
          , User
              "betty@example.com"
              "Betty"
              "Jones"
              "82b054bd83ffad9b6cf8bdb98ce3cc2f"
          , User
              "sam@example.com"
              "Sam"
              "Taylor"
              "332532dcfaa1cbf61e2a266bd723612c"
          ]

  let allUsers = all_ (_shoppingCartUsers shoppingCartDb)

  runBeamSqliteDebug putStrLn conn $ do
    users <- runSelectReturningList $ select allUsers
    mapM_ (liftIO . putStrLn . show) users

  let sortUsersByName = orderBy_
                          (\u ->
                            ( asc_ (_userFirstName u)
                            , desc_ (_userLastName u)
                            )
                          )
                          allUsers

  runBeamSqliteDebug putStrLn conn $ do
    users <- runSelectReturningList $ select sortUsersByName
    mapM_ (liftIO . putStrLn . show) users

  let userCount = aggregate_ (\_ -> as_ @Int countAll_) allUsers

  runBeamSqliteDebug putStrLn conn $ do
    Just c <- runSelectReturningOne $ select userCount
    liftIO $ putStrLn ("We have " ++ show c ++ " users in the database")

  runBeamSqliteDebug putStrLn conn $
    runInsert $
      insert (_shoppingCartUsers shoppingCartDb) $
        insertValues [ User
                         "james@pallo.com"
                         "James"
                         "Pallo"
                         "b4cc344d25a2efe540adbf2678e2304c"
                     , User
                         "betty@sims.com"
                         "Betty"
                         "Sims"
                         "82b054bd83ffad9b6cf8bdb98ce3cc2f"
                     , User
                         "james@oreily.com"
                         "James"
                         "O'Reily"
                         "b4cc344d25a2efe540adbf2678e2304c"
                     , User
                         "sam@sophitz.com"
                         "Sam"
                         "Sophitz"
                         "332532dcfaa1cbf61e2a266bd723612c"
                     , User
                         "sam@jely.com"
                         "Sam"
                         "Jely"
                         "332532dcfaa1cbf61e2a266bd723612c"
                     ]

  let numberOfUsersByName = aggregate_
                              (\u ->
                                ( group_ (_userFirstName u)
                                , as_ @Int countAll_
                                )
                              )
                              allUsers

  runBeamSqliteDebug putStrLn conn $ do
    countedByName <- runSelectReturningList $ select numberOfUsersByName
    mapM_ (liftIO . putStrLn . show) countedByName
