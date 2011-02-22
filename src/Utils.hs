module Utils (
  ndResponse,
  seeOtherN,
  getDataOr
  ) where

import Happstack.Server (ok, toResponse, nullDir, getDataFn, seeOther)

-- | Ensures that the path ends
ndResponse a = nullDir >> (ok $ toResponse a)

-- | Redirects with no message
seeOtherN uri = seeOther uri $ toResponse ""

-- | Tries to get data with rqData, if it fails it passes the error to
-- handler.
getDataOr rqData handler = getDataFn rqData >>= either handler return