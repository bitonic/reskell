module Config where

import Happstack.Server (BodyPolicy, defaultBodyPolicy)

import Database.Redis.Redis (Redis)
import qualified Database.Redis.Redis as R

resourcesDir :: FilePath
resourcesDir = "/home/astroboy/src/hsnews/resources"

sessionCookieName :: String
sessionCookieName = "hsnews-session"

appPolicy :: BodyPolicy
appPolicy = (defaultBodyPolicy "/tmp/" 0 1000 1000)

redisPrefix :: String
redisPrefix = "hsnews"

redisConn :: IO Redis
redisConn = R.connect R.localhost R.defaultPort
