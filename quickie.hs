{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ImpredicativeTypes   #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- tableLenses, dbLenses
{-# OPTIONS_GHC -Wno-missing-signatures #-}

import Database.Beam (Beamable, C, Columnar, Database, DatabaseSettings, Generic,
                      Identity, LensFor(LensFor), PrimaryKey, Table, TableEntity,
                      TableLens(TableLens), aggregate_, all_, as_, asc_,
                      countAll_, dbLenses, defaultDbSettings, desc_,
                      dbModification, fieldNamed, group_, insert, insertValues,
                      liftIO, modifyTableFields, orderBy_, primaryKey, runInsert,
                      runSelectReturningList, runSelectReturningOne, select,
                      setEntityName, tableLenses, tableModification,
                      withDbModification, default_, insertExpressions, pk, val_)
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


data AddressT f = Address
  { _addressId    :: C f Int
  , _addressLine1 :: C f Text
  , _addressLine2 :: C f (Maybe Text)
  , _addressCity  :: C f Text
  , _addressState :: C f Text
  , _addressZip   :: C f Text
  , _addressForUser :: PrimaryKey UserT f
  } deriving (Generic, Beamable)

type Address = AddressT Identity

deriving instance Show (PrimaryKey UserT Identity)
deriving instance Show Address

instance Table AddressT where
  data PrimaryKey AddressT f = AddressId (C f Int) deriving (Generic, Beamable)
  primaryKey = AddressId . _addressId


User (LensFor userEmail)
     (LensFor userFirstName)
     (LensFor userLastName)
     (LensFor userPassword) = tableLenses

Address (LensFor addressId)
        (LensFor addressLine1)
        (LensFor addressLine2)
        (LensFor addressCity)
        (LensFor addressState)
        (LensFor addressZip)
        (UserId (LensFor addressForUserId)) = tableLenses

ShoppingCartDb (TableLens shoppingCartUsers)
               (TableLens shoppingCartUserAddresses) = dbLenses


data ShoppingCartDb f = ShoppingCartDb
  { _shoppingCartUsers         :: f (TableEntity UserT)
  , _shoppingCartUserAddresses :: f (TableEntity AddressT)
  } deriving (Generic, Database backend)

shoppingCartDb :: DatabaseSettings backend ShoppingCartDb
shoppingCartDb =
  defaultDbSettings `withDbModification` dbModification
    { _shoppingCartUserAddresses =
        ( setEntityName "addresses"
        <> modifyTableFields tableModification
             { _addressLine1 = fieldNamed "address1"
             , _addressLine2 = "address2" -- OverloadedStrings
             }
        )
    }


main :: IO ()
main = do
  conn <- open "shoppingcart.db"

  let james = User "james@example.com"
                   "James"
                   "Smith"
                   "b4cc344d25a2efe540adbf2678e2304c"

      betty = User "betty@example.com"
                   "Betty"
                   "Jones"
                   "82b054bd83ffad9b6cf8bdb98ce3cc2f"

      sam = User "sam@example.com"
                 "Sam"
                 "Taylor"
                 "332532dcfaa1cbf61e2a266bd723612c"

  runBeamSqliteDebug putStrLn conn $
    runInsert $
      insert (_shoppingCartUsers shoppingCartDb) $
        insertValues [ james, betty, sam ]

  let addresses = [ Address
                      default_
                      (val_ "123 Little Street")
                      (val_ Nothing)
                      (val_ "Boston")
                      (val_ "MA")
                      (val_ "12345")
                      (val_ (pk james))
                  , Address
                      default_
                      (val_ "222 Main Street")
                      (val_ (Just "Ste 1"))
                      (val_ "Houston")
                      (val_ "TX")
                      (val_ "8888")
                      (val_ (pk betty))
                  , Address
                      default_
                      (val_ "9999 Residence Ave")
                      (val_ Nothing)
                      (val_ "Sugarland")
                      (val_ "TX")
                      (val_ "8989")
                      (val_ (pk betty))
                  ]

  runBeamSqliteDebug putStrLn conn $
    runInsert $
      insert (_shoppingCartUserAddresses shoppingCartDb) $
        insertExpressions addresses

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
