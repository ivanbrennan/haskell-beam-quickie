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
                      Identity, LensFor(LensFor), Nullable, PrimaryKey, Table,
                      TableEntity, TableLens(TableLens), aggregate_, all_, as_, asc_,
                      countAll_, dbLenses, defaultDbSettings, desc_,
                      dbModification, fieldNamed, group_, insert, insertValues,
                      liftIO, modifyTableFields, orderBy_, primaryKey, runInsert,
                      runSelectReturningList, runSelectReturningOne, select,
                      setEntityName, tableLenses, tableModification,
                      withDbModification, default_, insertExpressions, pk, val_)
import Database.Beam.Sqlite (runBeamSqliteDebug)
import Database.SQLite.Simple (open)
import Data.Text (Text)
import Data.Time (LocalTime)
import Lens.Micro ((^.))


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


data ProductT f = Product
  { _productId          :: C f Int
  , _productTitle       :: C f Text
  , _productDescription :: C f Text
  , _productPrice       :: C f Int {- Price in cents -}
  } deriving (Generic, Beamable)

type Product = ProductT Identity
deriving instance Show Product

instance Table ProductT where
  data PrimaryKey ProductT f = ProductId (C f Int) deriving (Generic, Beamable)
  primaryKey = ProductId . _productId


deriving instance Show (PrimaryKey AddressT Identity)

data OrderT f = Order
  { _orderId            :: C f Int
  , _orderDate          :: C f LocalTime
  , _orderForUser       :: PrimaryKey UserT f
  , _orderShipToAddress :: PrimaryKey AddressT f
  , _orderShippingInfo  :: PrimaryKey ShippingInfoT (Nullable f)
  } deriving (Generic, Beamable)

type Order = OrderT Identity
deriving instance Show Order

instance Table OrderT where
  data PrimaryKey OrderT f = OrderId (C f Int) deriving (Generic, Beamable)
  primaryKey = OrderId . _orderId


data ShippingCarrier
  = USPS
  | FedEx
  | UPS
  | DHL
  deriving (Show, Read, Eq, Ord, Enum)

data ShippingInfoT f = ShippingInfo
  { _shippingInfoId             :: C f Int
  , _shippingInfoCarrier        :: C f ShippingCarrier
  , _shippingInfoTrackingNumber :: C f Text
  } deriving (Generic, Beamable)

type ShippingInfo = ShippingInfoT Identity
deriving instance Show ShippingInfo

instance Table ShippingInfoT where
  data PrimaryKey ShippingInfoT f = ShippingInfoId (C f Int) deriving (Generic, Beamable)
  primaryKey = ShippingInfoId . _shippingInfoId

deriving instance Show (PrimaryKey ShippingInfoT (Nullable Identity))

deriving instance Show (PrimaryKey OrderT Identity)
deriving instance Show (PrimaryKey ProductT Identity)

data LineItemT f = LineItem
  { _lineItemInOrder    :: PrimaryKey OrderT f
  , _lineItemForProduct :: PrimaryKey ProductT f
  , _lineItemQuantity   :: C f Int
  } deriving (Generic, Beamable)

type LineItem = LineItemT Identity
deriving instance Show LineItem

instance Table LineItemT where
  data PrimaryKey LineItemT f = LineItemId (PrimaryKey OrderT f) (PrimaryKey ProductT f)
                                deriving (Generic, Beamable)
  primaryKey = LineItemId <$> _lineItemInOrder <*> _lineItemForProduct


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

LineItem _ _ (LensFor lineItemQuantity) = tableLenses

Product (LensFor productId)
        (LensFor productTitle)
        (LensFor productDescription)
        (LensFor productPrice) = tableLenses

ShoppingCartDb (TableLens shoppingCartUsers)
               (TableLens shoppingCartUserAddresses)
               (TableLens shoppingCartProducts)
               (TableLens shoppingCartOrders)
               (TableLens shoppingCartShippingInfos)
               (TableLens shoppingCartLineItems) = dbLenses


data ShoppingCartDb f = ShoppingCartDb
  { _shoppingCartUsers         :: f (TableEntity UserT)
  , _shoppingCartUserAddresses :: f (TableEntity AddressT)
  , _shoppingCartProducts      :: f (TableEntity ProductT)
  , _shoppingCartOrders        :: f (TableEntity OrderT)
  , _shoppingCartShippingInfos :: f (TableEntity ShippingInfoT)
  , _shoppingCartLineItems     :: f (TableEntity LineItemT)
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
    , _shoppingCartProducts = setEntityName "products"
    , _shoppingCartOrders =
        ( setEntityName "orders"
        <> modifyTableFields tableModification
             { _orderShippingInfo = ShippingInfoId "shipping_info__id"
             }
        )
    , _shoppingCartShippingInfos =
        ( setEntityName "shipping_info"
        <> modifyTableFields tableModification
             { _shippingInfoId = "id"
             , _shippingInfoCarrier = "carrier"
             , _shippingInfoTrackingNumber = "tracking_number"
             }
        )
    , _shoppingCartLineItems = setEntityName "line_items"
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

  -- let addresses = [ Address
  --                     default_
  --                     (val_ "123 Little Street")
  --                     (val_ Nothing)
  --                     (val_ "Boston")
  --                     (val_ "MA")
  --                     (val_ "12345")
  --                     (val_ (pk james))
  --                 , Address
  --                     default_
  --                     (val_ "222 Main Street")
  --                     (val_ (Just "Ste 1"))
  --                     (val_ "Houston")
  --                     (val_ "TX")
  --                     (val_ "8888")
  --                     (val_ (pk betty))
  --                 , Address
  --                     default_
  --                     (val_ "9999 Residence Ave")
  --                     (val_ Nothing)
  --                     (val_ "Sugarland")
  --                     (val_ "TX")
  --                     (val_ "8989")
  --                     (val_ (pk betty))
  --                 ]

  -- runBeamSqliteDebug putStrLn conn $
  --   runInsert $
  --     insert (_shoppingCartUserAddresses shoppingCartDb) $
  --       insertExpressions addresses

  -- let products = [ Product default_ (val_ "Red Ball") (val_ "A bright red, very spherical ball") (val_ 1000)
  --                , Product default_ (val_ "Math Textbook") (val_ "Contains a lot of important math theorems and formulae") (val_ 2500)
  --                , Product default_ (val_ "Intro to Haskell") (val_ "Learn the best programming language in the world") (val_ 3000)
  --                , Product default_ (val_ "Suitcase") "A hard durable suitcase" 15000
  --                ]


  addresses' <- runBeamSqliteDebug putStrLn conn $
                runSelectReturningList $
                select (all_ (shoppingCartDb ^. shoppingCartUserAddresses))

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
