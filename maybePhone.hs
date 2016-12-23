import qualified Data.Map as M 

type PersonName = String
type PhoneNumber = String
type BillingAddress = String

data MobileCarrier = Network1 | Network2 | Network3
                      deriving (Eq, Ord, Show)

findCarrierBillingAddress :: PersonName
  -> M.Map PersonName PhoneNumber
  -> M.Map PhoneNumber MobileCarrier
  -> M.Map MobileCarrier BillingAddress
  -> Maybe BillingAddress

-- v1
findCarrierBillingAddress person phoneMap carrierMap addressMap =
  case M.lookup person phoneMap of
    Nothing -> Nothing
    -- Just number -> return number
    Just number -> case M.lookup number carrierMap of
      Nothing -> Nothing
      -- Just carrier -> return $ show $ carrier
      Just carrier -> M.lookup carrier addressMap

customers = M.fromList $ [("Bill", "123"), ("Sam", "456")]
numbers = M.fromList $ [("123", Network1), ("456", Network2)]
carriers = M.fromList $ [(Network1, "high st"), (Network2, "main st")]

test = findCarrierBillingAddress "Bill" customers numbers carriers

findCarrierBillingAddress2 :: PersonName
  -> M.Map PersonName PhoneNumber
  -> M.Map PhoneNumber MobileCarrier
  -> M.Map MobileCarrier BillingAddress
  -> Maybe BillingAddress

findCarrierBillingAddress2 person phoneMap carrierMap addressMap =
  M.lookup person phoneMap >>= \num ->
    -- return num
    M.lookup num carrierMap >>= \carrier ->
      -- return $ show carrier
      M.lookup carrier addressMap

test2 = findCarrierBillingAddress2 "Bill" customers numbers carriers


