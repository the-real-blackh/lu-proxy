{-# LANGUAGE OverloadedStrings #-}

import Network.Wai.Handler.Warp
import Network.Wai (Application)
import qualified Network.Wai as Wai
import Network.HTTP.Types
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Conduit as HK
import qualified Network.Wai.Conduit as WK
import qualified Data.ByteString.Char8 as BS
import qualified Network.HTTP.Client.Conduit as HCC
import qualified Data.CaseInsensitive as CI
import Data.Conduit (Flush (..), Sink, Source, ($$), mapOutput, yield)
import Blaze.ByteString.Builder (fromByteString)


main = do
    manager <- HC.newManager HC.defaultManagerSettings
    run 3038 (app manager)

app :: HC.Manager -> Application
app mgr mwreq respond = do
{-
    print $ req {
        requestHeaders = requestHeader req
      }
      -}
    {-
    let req' = defaultRequest {
            method = Wai.requestMethod req
          }
    -}
    let url = "http://www.liberationunleashed.com" <> Wai.rawPathInfo mwreq <> Wai.rawQueryString mwreq
    BS.putStrLn url
    hreq0 <- HC.parseRequest $ BS.unpack url
    let hreq = hreq0
            { HC.method = Wai.requestMethod mwreq
            , HC.requestHeaders = modifyRequestHeaders $ Wai.requestHeaders mwreq
            , HC.redirectCount = 0 -- Always pass redirects back to the client.
            , HC.requestBody =
                case Wai.requestBodyLength mwreq of
                    Wai.ChunkedBody ->
                        HK.requestBodySourceChunkedIO (WK.sourceRequestBody mwreq)
                    Wai.KnownLength l ->
                        HK.requestBodySourceIO (fromIntegral l) (WK.sourceRequestBody mwreq)
            -- Do not touch response body. Otherwise there may be discrepancy
            -- between response headers and the response content.
            , HC.decompress = const False
            , HC.responseTimeout =  
            }
            
    -- handle (respond . errorResponse) $
    print hreq
    HC.withResponse hreq mgr $ \res -> do
        let body = mapOutput (Chunk . fromByteString) . HCC.bodyReaderSource $ HC.responseBody res
            headers = filter dropResponseHeader (HC.responseHeaders res)
        print (HC.responseStatus res)
        print headers
        --print body
        respond $ WK.responseSource (HC.responseStatus res) headers body
    {-
    print hreq
    resp <- HC.httpLbs hreq mgr
    respond $ Wai.responseLBS status200 [] "Hello World"
    -}
  where
    modifyRequestHeaders hdrs =
        ("host", "www.liberationunleashed.com") :
        filter dropRequestHeader (
            filter (\(k, _) -> k /= "host") hdrs
        )
    dropRequestHeader (k, _) = k `notElem`
        [ "content-encoding"
        , "content-length"
        ]
    dropResponseHeader (k, _) = k `notElem` []
