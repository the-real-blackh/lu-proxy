{-# LANGUAGE OverloadedStrings #-}

import Network.Wai.Handler.Warp
import Network.Wai (Application)
import qualified Network.Wai as Wai
import Network.HTTP.Types
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Conduit as HK
import qualified Network.Wai.Conduit as WK
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import qualified Network.HTTP.Client.Conduit as HCC
import qualified Data.CaseInsensitive as CI
import Data.Conduit (Flush (..), Sink, Source, ($$), mapOutput, yield, sourceToList)
--import qualified Data.Attoparsec.ByteString.Char8 as A
import Control.Exception (finally)
import Data.Maybe
import Blaze.ByteString.Builder (fromByteString)
import Control.Concurrent.MVar
import Data.Char (chr)


main = do
    manager <- HC.newManager HC.defaultManagerSettings
    mutex <- newMVar ()
    run 3038 (app manager mutex)

app :: HC.Manager -> MVar () -> Application
app mgr mutex mwreq respond = do
    takeMVar mutex   -- ###
    do
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

        BS.putStrLn $ "------ " <> url
        hreq0 <- HC.parseRequest $ BS.unpack url
        case Wai.requestBodyLength mwreq of
            Wai.ChunkedBody -> putStrLn "Wai.ChunkedBody"
            Wai.KnownLength l -> putStrLn $ "Wai.KnownLength "++show l
        case HCC.requestBody hreq0 of
            HCC.RequestBodyLBS text           -> putStrLn $ "RequestBodyLBS TEXT=" ++ (map (chr . fromIntegral) $ BL.unpack text)
            HCC.RequestBodyBS _               -> putStrLn "RequestBodyBS"
            HCC.RequestBodyBuilder len stream -> putStrLn "RequestBodyBuilder"
            HCC.RequestBodyStream len stream -> putStrLn "RequestBodyStream"
            HCC.RequestBodyStreamChunked stream -> putStrLn "RequestBodyStreamChunked"
            HCC.RequestBodyIO stream -> putStrLn "RequestBodyIO"
        let reqBody = WK.sourceRequestBody mwreq
        reqBodyStr <- mconcat <$> sourceToList reqBody
        putStrLn $ "reqBodyStr="++show reqBodyStr

        let origRequestHeaders = Wai.requestHeaders mwreq
            hreq = hreq0
                { HC.method = Wai.requestMethod mwreq
                , HC.requestHeaders = modifyRequestHeaders origRequestHeaders
                , HC.redirectCount = 0 -- Always pass redirects back to the client.
                , HC.requestBody =
                    HCC.RequestBodyBS reqBodyStr
                    -- HK.requestBodySourceChunkedIO (WK.sourceRequestBody mwreq)
                {-
                    case Wai.requestBodyLength mwreq of
                        Wai.ChunkedBody ->
                            HK.requestBodySourceChunkedIO (WK.sourceRequestBody mwreq)
                        Wai.KnownLength l ->
                            HK.requestBodySourceIO (fromIntegral l) (WK.sourceRequestBody mwreq)
                -}
                -- Do not touch response body. Otherwise there may be discrepancy
                -- between response headers and the response content.
                , HC.decompress = const False
                , HC.responseTimeout = HC.responseTimeoutMicro 180000000
                }

        -- handle (respond . errorResponse) $
        print hreq
        HC.withResponse hreq mgr $ \res -> do
            let body = mapOutput (Chunk . fromByteString) . HCC.bodyReaderSource $ HC.responseBody res
                headers = filter dropResponseHeader (HC.responseHeaders res)
            print (HC.responseStatus res)
            let headers' = modifyResponseHeaders origRequestHeaders headers
            print headers'
            --print body
            respond $ WK.responseSource (HC.responseStatus res) headers' body
      `finally` do
        BS.putStrLn "<<<<"     -- ###
        putMVar mutex ()   -- ###
    {-
    print hreq
    resp <- HC.httpLbs hreq mgr
    respond $ Wai.responseLBS status200 [] "Hello World"
    -}
  where
    getHeader name hdrs =
        fmap snd $ listToMaybe $ filter (\(n, v) -> n == name) hdrs
    setHeader name value hdrs =
        (name, value) :
        filter dropRequestHeader (
            filter (\(k, _) -> k /= name) hdrs
        )
    modifyRequestHeaders = setHeader "host" "www.liberationunleashed.com"
    {-
    ("Set-Cookie","phpbb3_qkcy3_u=1; expires=Wed, 01-Jul-2020 05:23:54 GMT; path=/; domain=liberationunleashed.com; HttpOnly")
    ("Location","https://www.liberationunleashed.com/nation/index.php?sid=70d156e237184b8ef52a83784addadca")
    -}
    modifyResponseHeaders origRequestHeaders =
        case mHost of
            Just host ->
                map (\(n, v) -> if n == "Set-Cookie"
                    then (n, correctCookie host v)
                    else (n, v)) .
                map (\(n, v) -> if n == "Location"
                    then (n, correctLocation host v)
                    else (n, v))
            Nothing   -> id
      where
        mHost = getHeader "host" origRequestHeaders
    dropRequestHeader (k, _) = k `notElem` ["content-encoding", "content-length"]
    dropResponseHeader (k, _) = k `notElem` []

    correctCookie :: BS.ByteString -> BS.ByteString -> BS.ByteString
    correctCookie host = semi . map fix . unsemi
      where
        semi = BS.intercalate "; "
        unsemi text = case BS.breakSubstring "; " text of
            (start, end) | BS.null end -> [start]
            (start, end) -> start : unsemi (BS.drop 2 end)
        fix text | CI.mk (BS.take 7 text) == "domain=" = "domain=" <> host
                 | otherwise                           = text

    correctLocation :: BS.ByteString -> BS.ByteString -> BS.ByteString
    correctLocation host url | https `BS.isPrefixOf` url
        = "/" <> BS.drop (BS.length https) url
      where
        https = "https://www.liberationunleashed.com/"
