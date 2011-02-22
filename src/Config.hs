module Config (
  resourcesDir,
  sessionCookieName,
  appPolicy
  ) where

import Happstack.Server (BodyPolicy, defaultBodyPolicy)

resourcesDir :: FilePath
resourcesDir = "/home/astroboy/src/hsnews/resources"

sessionCookieName :: String
sessionCookieName = "hsnews-session"

appPolicy :: BodyPolicy
appPolicy = (defaultBodyPolicy "/tmp/" 0 1000 1000)