{-#LANGUAGE OverloadedStrings #-}
module Settings where

import GitLab.Common (ProjectId (..), AccessToken (..))
import Network.Connection (TLSSettings(..))
import Servant.Client (BaseUrl (..), Scheme (..))
import Data.Default (def)
import Data.ByteString (ByteString)

project :: ProjectId
project = ProjectId 1

tlsSettings :: TLSSettings
tlsSettings = def { settingDisableCertificateValidation = True }

gitlabToken :: AccessToken
gitlabBaseUrl :: BaseUrl

-- tobias local
gitlabBaseUrl = BaseUrl Http "gitlabghc.nibbler" 80 "/api/v4"
gitlabToken = "WWHmsVF943KX8sK3zfD2"

dsn :: ByteString
dsn = "dbname=trac_ghc"

tracBaseUrl :: BaseUrl
tracBaseUrl = BaseUrl Https "ghc.haskell.org" 443 "/trac/ghc"


-- staging
--gitlabBaseUrl = BaseUrl Https "gitlab.ghc.smart-cactus.org" 443 "/api/v4"
--gitlabToken = "zZChwxtdAZ4TS52FWw4K"
--dsn = ""

-- ben-server
-- gitlabBaseUrl = BaseUrl Https "gitlab.home.smart-cactus.org" 443 "/api/v4"
-- gitlabToken = "1CfgUx6ox3wBf11z1KWT"
-- dsn = ""

