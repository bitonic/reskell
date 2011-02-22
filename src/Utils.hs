module Utils (
  ndResponse,
  seeOtherN,
  getDataOr
  ) where

import Happstack.Server (ok, toResponse, nullDir, getDataFn, seeOther)

ndResponse a = nullDir >> (ok $ toResponse a)

seeOtherN uri = seeOther uri $ toResponse ""

getDataOr rqData handler = getDataFn rqData >>= either handler return