{-# LANGUAGE OverloadedStrings #-}

module SemanticParserAPI.Compute.Network where

{- 
import           Data.ByteString.Char8                     (ByteString)
import qualified Data.ByteString.Lazy.Char8          as BL
import qualified Data.Text.Encoding                  as TE
import qualified Data.Text.IO                        as TIO
import           Network.Connection                        (TLSSettings(..))
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types                        (Method)

simpleHttpClient :: Bool -> Method -> String -> Maybe ByteString -> IO BL.ByteString
simpleHttpClient isurlenc mth url mbstr = do
  request0' <- parseRequest url
  let request0 = request0' { requestHeaders = requestHeaders request0' ++ [ ("Accept","application/json") ] }
  let request' = maybe (request0 { method = mth }) (\bstr -> request0 { method = mth, requestBody = RequestBodyBS bstr }) mbstr
      request = if isurlenc then urlEncodedBody [] request' else request'
  print (requestHeaders request)
  case mbstr of
    Nothing -> return ()
    Just bstr -> TIO.putStrLn (TE.decodeUtf8 bstr)
  manager <- if (secure request)
               then do
                 let tlssetting = TLSSettingsSimple True False False
                     mansetting = mkManagerSettings tlssetting Nothing
                 newManager mansetting
               else newManager defaultManagerSettings
  response <- httpLbs request manager
  return (responseBody response)
-}
