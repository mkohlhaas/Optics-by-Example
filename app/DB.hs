-- We don't need to export DbFields or it's record accessors at all.
-- It exists solely to define the fields needed by the module.
module DB (DatabaseUrl, HasDatenbankUrl (..), connectDatenbank) where

import Control.Lens
import Control.Monad.Reader

type DatabaseUrl = String

-- `DbFields` is a DUMMY RECORD which exposes the fields used in other records.
-- The field prefix `dbFields` must be spelled exactly like this or `makeFields` will ignore it.
-- The prefix must be the exact name of the record, but with the first letter lowercased.
newtype DbFields = DbFields {_dbFieldsDatenbankUrl ∷ DatabaseUrl}
-- creates HasDatenbankUrl (and more)
makeFields ''DbFields

-- If we're not worried about record-accessor conflicts we can use `makeFieldsNoPrefix`.
-- newtype DbFields = DbFields {_datenbankUrl ∷ String}
-- makeFieldsNoPrefix ''DbFields

-- We can define each field's type class in this module.
-- Then just implement those fields in our objects (i.e. EnvDB) in the Main module.
-- Thus we are negating any need for dependencies between the two modules.

connectDatenbank ∷ (MonadIO m, HasDatenbankUrl e DatabaseUrl, MonadReader e m) ⇒ m ()
connectDatenbank = do
  url ← view datenbankUrl
  liftIO $ putStrLn ("connecting to db at: " <> url)
