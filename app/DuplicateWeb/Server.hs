{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module DuplicateWeb.Server
  ( runServer
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception (try, SomeException)
import Control.Monad (unless)
import Data.Aeson (encode, object, (.=), Value, decode, withObject, (.:), (.:?), (.!=))
import Data.Aeson.Types (parseJSON, FromJSON(..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (runSettings, setPort, setHost, defaultSettings)
import System.IO (hGetContents, hClose)
import System.Posix.Files (getFileStatus, getSymbolicLinkStatus, fileSize, isDirectory, isSymbolicLink, isRegularFile, FileStatus)
import System.Process (createProcess, proc, StdStream(..), std_out, waitForProcess)
import Text.Read (readMaybe)

import DuplicateChecker.Analyser (AnalyserConfig(..), findDuplicates)
import DuplicateChecker.FsType (MountInfo, loadMountInfo, isBlacklistedFs)
import DuplicateChecker.Stats (Stats, newStats, recordStat)
import DuplicateScorer.Scoring
    ( WithReason(..), ScoringFunction, selectBestFile
    , pathLengthScore, modificationTimeScore, pathDepthScore
    , createGoldenScore, createTrashScore
    )

data RunParams = RunParams
  { rpDirs   :: [String]
  , rpGolden :: [String]
  , rpTrash  :: [String]
  , rpJobs   :: Int
  } deriving (Generic, Show)

instance FromJSON RunParams where
  parseJSON = withObject "RunParams" $ \o -> RunParams
    <$> o .:  "dirs"
    <*> o .:? "golden" .!= []
    <*> o .:? "trash"  .!= []
    <*> o .:? "jobs"   .!= 8

type EventChan = TChan (Maybe String)
type Sessions  = TVar (Map.Map Int EventChan)

newSessions :: IO Sessions
newSessions = newTVarIO Map.empty

runServer :: Int -> IO ()
runServer port =
  let settins = setPort port $ setHost "*" $ defaultSettings
  in
    do
    sessions <- newSessions
    counter  <- newTVarIO (0 :: Int)
    putStrLn $ "Listening on http://localhost:" ++ show port
    runSettings settins (application counter sessions)

application :: TVar Int -> Sessions -> Application
application counter sessions req respond =
  case (requestMethod req, pathInfo req) of
    ("GET",  [])              -> serveIndex respond
    ("POST", ["run"])         -> handleRun counter sessions req respond
    ("GET",  ["stream", sid]) -> handleStream sessions sid respond
    _                         -> respond $ responseLBS status404 [] "Not found"

serveIndex :: (Response -> IO ResponseReceived) -> IO ResponseReceived
serveIndex respond =
  respond $ responseLBS status200
    [(hContentType, "text/html; charset=utf-8")]
    htmlPage

handleRun :: TVar Int -> Sessions -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleRun counter sessions req respond = do
  body <- readRequestBody req
  case decode body :: Maybe RunParams of
    Nothing     -> respond $ responseLBS status400 [] "Bad request"
    Just params -> do
      (sid, chan) <- atomically $ do
        n <- readTVar counter
        let n' = n + 1
        writeTVar counter n'
        ch <- newTChan
        modifyTVar' sessions (Map.insert n' ch)
        return (n', ch)
      _ <- forkIO $ runPipeline params chan
      respond $ responseLBS status200
        [(hContentType, "application/json")]
        (encode $ object ["sessionId" .= sid])

handleStream :: Sessions -> Text -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleStream sessions sid respond =
  case readMaybe (T.unpack sid) :: Maybe Int of
    Nothing -> respond $ responseLBS status404 [] "Not found"
    Just n  -> do
      mchan <- atomically $ Map.lookup n <$> readTVar sessions
      case mchan of
        Nothing   -> respond $ responseLBS status404 [] "Not found"
        Just chan  ->
          respond $ responseStream status200
            [ (hContentType, "text/event-stream")
            , ("Cache-Control", "no-cache")
            , ("X-Accel-Buffering", "no")
            , ("Access-Control-Allow-Origin", "*")
            ]
            $ \write flush -> do
                let loop = do
                      msg <- atomically $ readTChan chan
                      case msg of
                        Nothing  -> do
                          write (BB.byteString "data: {\"type\":\"done\"}\n\n")
                          flush
                        Just line -> do
                          write (BB.byteString $ BSC.pack $ "data: " ++ line ++ "\n\n")
                          flush
                          loop
                loop

runPipeline :: RunParams -> EventChan -> IO ()
runPipeline params chan = do
  let sendLog msg = atomically $ writeTChan chan $ Just $
        BSLC.unpack $ encode $ object ["type" .= ("log" :: String), "msg" .= msg]

  sendLog $ "Finding files in " ++ show (length (rpDirs params)) ++ " director(ies)..."

  mountInfo <- loadMountInfo
  stats     <- newStats

  (_, Just hout, _, ph) <- createProcess
    (proc "find" (rpDirs params ++ ["-type", "f"])) { std_out = CreatePipe }
  allLines <- lines <$> hGetContents hout
  _ <- waitForProcess ph

  sendLog $ "Found " ++ show (length allLines) ++ " paths, filtering..."

  pairs <- catMaybes <$> mapM (processPath stats mountInfo) allLines

  sendLog $ "Analysing " ++ show (length pairs) ++ " files for duplicates..."

  groups <- findDuplicates (AnalyserConfig { numWorkers = rpJobs params }) stats pairs

  sendLog $ "Found " ++ show (length groups) ++ " duplicate group(s), scoring..."

  let scoringFns =
        map (createGoldenScore . T.pack) (rpGolden params)
        ++ map (createTrashScore  . T.pack) (rpTrash  params)
        ++ [modificationTimeScore, pathDepthScore, pathLengthScore]

  scored <- mapM (scoreGroup scoringFns) groups

  let script = unlines $
        [ "#!/bin/bash"
        , "# Generated by duplicate-dedup-web"
        , ""
        ] ++ concatMap formatGroup scored

  atomically $ writeTChan chan $ Just $
    BSLC.unpack $ encode $ object ["type" .= ("script" :: String), "content" .= script]

  atomically $ writeTChan chan Nothing

processPath :: Stats -> MountInfo -> FilePath -> IO (Maybe (FilePath, Integer))
processPath stats mountInfo path = do
  linkResult <- try (getSymbolicLinkStatus path) :: IO (Either SomeException FileStatus)
  case linkResult of
    Left _          -> recordStat stats "stat-error" >> return Nothing
    Right linkStatus ->
      if isSymbolicLink linkStatus
        then recordStat stats "skipped-symlink" >> return Nothing
        else do
          result <- try (getFileStatus path) :: IO (Either SomeException FileStatus)
          case result of
            Left _     -> recordStat stats "stat-error" >> return Nothing
            Right status
              | isDirectory status        -> recordStat stats "skipped-directory"  >> return Nothing
              | not (isRegularFile status) -> recordStat stats "skipped-special"    >> return Nothing
              | otherwise -> do
                  let sz = fromIntegral (fileSize status) :: Integer
                  if sz == 0
                    then recordStat stats "skipped-zero-size" >> return Nothing
                    else if isBlacklistedFs mountInfo path
                      then recordStat stats "skipped-blacklisted-fs" >> return Nothing
                      else return (Just (path, sz))

scoreGroup :: [ScoringFunction] -> [FilePath] -> IO (WithReason FilePath, [WithReason FilePath])
scoreGroup _    []    = error "empty group"
scoreGroup _    [f]   = return (WithReason f ["sole file"], [])
scoreGroup fns  files = selectBestFile fns files

formatGroup :: (WithReason FilePath, [WithReason FilePath]) -> [String]
formatGroup (keep, removes) =
  [ "# KEEP: " ++ value keep ++ "  (" ++ unwords (reasons keep) ++ ")" ]
  ++ [ "rm -v " ++ show (value r) ++ "  # " ++ unwords (reasons r) | r <- removes ]
  ++ [""]

readRequestBody :: Request -> IO BSL.ByteString
readRequestBody req = go []
  where
    go acc = do
      chunk <- getRequestBodyChunk req
      if BS.null chunk
        then return $ BSL.fromChunks (reverse acc)
        else go (chunk : acc)

htmlPage :: BSL.ByteString
htmlPage = BSL.fromStrict $ BSC.pack $ unlines
  [ "<!DOCTYPE html>"
  , "<html lang=\"en\">"
  , "<head>"
  , "<meta charset=\"UTF-8\">"
  , "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">"
  , "<title>Duplicate File Deduplicator</title>"
  , "<style>"
  , "  *, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; }"
  , "  body {"
  , "    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;"
  , "    background: #1a1a2e;"
  , "    color: #e0e0f0;"
  , "    min-height: 100vh;"
  , "    padding: 2rem 1rem;"
  , "  }"
  , "  .container { max-width: 860px; margin: 0 auto; }"
  , "  header { text-align: center; margin-bottom: 2rem; }"
  , "  header h1 { font-size: 2rem; color: #e94560; margin-bottom: 0.4rem; }"
  , "  header p  { color: #8888aa; font-size: 1rem; }"
  , "  .card {"
  , "    background: #16213e;"
  , "    border-radius: 10px;"
  , "    padding: 1.5rem;"
  , "    margin-bottom: 1.5rem;"
  , "    border: 1px solid #0f3460;"
  , "  }"
  , "  .card h2 { font-size: 1.1rem; color: #a0a8d0; margin-bottom: 1rem; text-transform: uppercase; letter-spacing: 0.05em; }"
  , "  .section { margin-bottom: 1.2rem; }"
  , "  .section label { display: block; font-size: 0.85rem; color: #8888aa; margin-bottom: 0.5rem; }"
  , "  .dir-row { display: flex; gap: 0.5rem; margin-bottom: 0.4rem; }"
  , "  .dir-row input {"
  , "    flex: 1;"
  , "    background: #0f1b35;"
  , "    border: 1px solid #0f3460;"
  , "    border-radius: 6px;"
  , "    padding: 0.5rem 0.75rem;"
  , "    color: #e0e0f0;"
  , "    font-size: 0.9rem;"
  , "  }"
  , "  .dir-row input:focus { outline: none; border-color: #e94560; }"
  , "  .btn-sm {"
  , "    background: #0f3460;"
  , "    color: #e0e0f0;"
  , "    border: none;"
  , "    border-radius: 6px;"
  , "    padding: 0.5rem 0.75rem;"
  , "    cursor: pointer;"
  , "    font-size: 0.85rem;"
  , "    white-space: nowrap;"
  , "  }"
  , "  .btn-sm:hover { background: #1a4a80; }"
  , "  .add-btn {"
  , "    background: none;"
  , "    border: 1px dashed #0f3460;"
  , "    color: #8888aa;"
  , "    border-radius: 6px;"
  , "    padding: 0.4rem 0.9rem;"
  , "    cursor: pointer;"
  , "    font-size: 0.82rem;"
  , "    margin-top: 0.3rem;"
  , "  }"
  , "  .add-btn:hover { border-color: #e94560; color: #e94560; }"
  , "  .jobs-row { display: flex; align-items: center; gap: 0.75rem; }"
  , "  .jobs-row input {"
  , "    width: 80px;"
  , "    background: #0f1b35;"
  , "    border: 1px solid #0f3460;"
  , "    border-radius: 6px;"
  , "    padding: 0.5rem 0.75rem;"
  , "    color: #e0e0f0;"
  , "    font-size: 0.9rem;"
  , "  }"
  , "  .jobs-row input:focus { outline: none; border-color: #e94560; }"
  , "  .btn-run {"
  , "    display: block;"
  , "    width: 100%;"
  , "    margin-top: 1.2rem;"
  , "    background: #e94560;"
  , "    color: #fff;"
  , "    border: none;"
  , "    border-radius: 8px;"
  , "    padding: 0.75rem;"
  , "    font-size: 1rem;"
  , "    font-weight: 600;"
  , "    cursor: pointer;"
  , "    letter-spacing: 0.03em;"
  , "  }"
  , "  .btn-run:hover:not(:disabled) { background: #c73050; }"
  , "  .btn-run:disabled { opacity: 0.5; cursor: not-allowed; }"
  , "  #progress-log {"
  , "    background: #0a0f1e;"
  , "    border-radius: 6px;"
  , "    padding: 0.75rem 1rem;"
  , "    height: 180px;"
  , "    overflow-y: auto;"
  , "    font-family: 'SFMono-Regular', Consolas, 'Liberation Mono', Menlo, monospace;"
  , "    font-size: 0.82rem;"
  , "    color: #a0f0a0;"
  , "    white-space: pre-wrap;"
  , "    word-break: break-all;"
  , "  }"
  , "  .script-actions { display: flex; gap: 0.75rem; margin-bottom: 1rem; }"
  , "  .btn-action {"
  , "    background: #0f3460;"
  , "    color: #e0e0f0;"
  , "    border: none;"
  , "    border-radius: 6px;"
  , "    padding: 0.5rem 1rem;"
  , "    cursor: pointer;"
  , "    font-size: 0.875rem;"
  , "    text-decoration: none;"
  , "  }"
  , "  .btn-action:hover { background: #1a4a80; }"
  , "  pre {"
  , "    background: #0a0f1e;"
  , "    border-radius: 6px;"
  , "    padding: 1rem;"
  , "    max-height: 500px;"
  , "    overflow: auto;"
  , "    font-family: 'SFMono-Regular', Consolas, 'Liberation Mono', Menlo, monospace;"
  , "    font-size: 0.8rem;"
  , "    color: #c0e0ff;"
  , "    white-space: pre;"
  , "  }"
  , "</style>"
  , "</head>"
  , "<body>"
  , "<div class=\"container\">"
  , "  <header>"
  , "    <h1>Duplicate File Deduplicator</h1>"
  , "    <p>Scan directories, find duplicate files, and generate a clean-up script.</p>"
  , "  </header>"
  , ""
  , "  <div class=\"card\">"
  , "    <h2>Configuration</h2>"
  , ""
  , "    <div class=\"section\">"
  , "      <label>Input Directories</label>"
  , "      <div id=\"input-dirs\">"
  , "        <div class=\"dir-row\">"
  , "          <input type=\"text\" placeholder=\"/path/to/scan\">"
  , "          <button class=\"btn-sm\" onclick=\"this.parentElement.remove()\">&#x2715;</button>"
  , "        </div>"
  , "      </div>"
  , "      <button class=\"add-btn\" onclick=\"addDirInput('input-dirs', '/path/to/scan')\">+ Add Directory</button>"
  , "    </div>"
  , ""
  , "    <div class=\"section\">"
  , "      <label>Golden Directories <span style=\"color:#8888aa;font-size:0.8em\">(prefer keep)</span></label>"
  , "      <div id=\"golden-dirs\"></div>"
  , "      <button class=\"add-btn\" onclick=\"addDirInput('golden-dirs', '/golden/path')\">+ Add</button>"
  , "    </div>"
  , ""
  , "    <div class=\"section\">"
  , "      <label>Trash Directories <span style=\"color:#8888aa;font-size:0.8em\">(prefer delete)</span></label>"
  , "      <div id=\"trash-dirs\"></div>"
  , "      <button class=\"add-btn\" onclick=\"addDirInput('trash-dirs', '/trash/path')\">+ Add</button>"
  , "    </div>"
  , ""
  , "    <div class=\"section\">"
  , "      <label>Parallel Jobs</label>"
  , "      <div class=\"jobs-row\">"
  , "        <input type=\"number\" id=\"jobs\" value=\"8\" min=\"1\" max=\"64\">"
  , "        <span style=\"color:#8888aa;font-size:0.85rem\">concurrent workers</span>"
  , "      </div>"
  , "    </div>"
  , ""
  , "    <button class=\"btn-run\" id=\"run-btn\" onclick=\"runPipeline()\">&#x25B6; Run Pipeline</button>"
  , "  </div>"
  , ""
  , "  <div class=\"card\" id=\"progress-card\">"
  , "    <h2>Progress</h2>"
  , "    <div id=\"progress-log\">Ready.</div>"
  , "  </div>"
  , ""
  , "  <div class=\"card\" id=\"script-card\" style=\"display:none\">"
  , "    <h2>Generated Script</h2>"
  , "    <div class=\"script-actions\">"
  , "      <button class=\"btn-action\" id=\"copy-btn\" onclick=\"copyScript()\">Copy to Clipboard</button>"
  , "      <a class=\"btn-action\" id=\"download-btn\" download=\"script.sh\" href=\"#\">Download script.sh</a>"
  , "    </div>"
  , "    <pre><code id=\"script-output\"></code></pre>"
  , "  </div>"
  , "</div>"
  , ""
  , "<script>"
  , "function addDirInput(containerId, placeholder) {"
  , "  const c = document.getElementById(containerId);"
  , "  const row = document.createElement('div');"
  , "  row.className = 'dir-row';"
  , "  row.innerHTML = `<input type=\"text\" placeholder=\"${placeholder}\">`"
  , "                + `<button class=\"btn-sm\" onclick=\"this.parentElement.remove()\">&#x2715;</button>`;"
  , "  c.appendChild(row);"
  , "}"
  , ""
  , "function getDirs(containerId) {"
  , "  return [...document.querySelectorAll('#' + containerId + ' input')]"
  , "    .map(i => i.value.trim()).filter(Boolean);"
  , "}"
  , ""
  , "let es = null;"
  , ""
  , "async function runPipeline() {"
  , "  const dirs = getDirs('input-dirs');"
  , "  if (!dirs.length) { alert('Add at least one input directory.'); return; }"
  , "  const params = {"
  , "    dirs,"
  , "    golden: getDirs('golden-dirs'),"
  , "    trash:  getDirs('trash-dirs'),"
  , "    jobs:   parseInt(document.getElementById('jobs').value) || 8"
  , "  };"
  , "  document.getElementById('run-btn').disabled = true;"
  , "  document.getElementById('script-card').style.display = 'none';"
  , "  document.getElementById('progress-log').textContent = '';"
  , "  log('Starting pipeline...');"
  , ""
  , "  const res = await fetch('/run', {"
  , "    method: 'POST',"
  , "    headers: {'Content-Type': 'application/json'},"
  , "    body: JSON.stringify(params)"
  , "  });"
  , "  const { sessionId } = await res.json();"
  , ""
  , "  if (es) es.close();"
  , "  es = new EventSource('/stream/' + sessionId);"
  , "  es.onmessage = (e) => {"
  , "    const ev = JSON.parse(e.data);"
  , "    if (ev.type === 'log') {"
  , "      log(ev.msg);"
  , "    } else if (ev.type === 'script') {"
  , "      showScript(ev.content);"
  , "    } else if (ev.type === 'done') {"
  , "      log('Done.');"
  , "      document.getElementById('run-btn').disabled = false;"
  , "      es.close();"
  , "    }"
  , "  };"
  , "  es.onerror = () => { document.getElementById('run-btn').disabled = false; };"
  , "}"
  , ""
  , "function log(msg) {"
  , "  const el = document.getElementById('progress-log');"
  , "  el.textContent += msg + '\\n';"
  , "  el.scrollTop = el.scrollHeight;"
  , "}"
  , ""
  , "function showScript(content) {"
  , "  document.getElementById('script-output').textContent = content;"
  , "  const card = document.getElementById('script-card');"
  , "  card.style.display = 'block';"
  , "  const blob = new Blob([content], {type: 'text/x-shellscript'});"
  , "  document.getElementById('download-btn').href = URL.createObjectURL(blob);"
  , "}"
  , ""
  , "function copyScript() {"
  , "  navigator.clipboard.writeText(document.getElementById('script-output').textContent)"
  , "    .then(() => {"
  , "      document.getElementById('copy-btn').textContent = '\\u2713 Copied!';"
  , "      setTimeout(() => document.getElementById('copy-btn').textContent = 'Copy to Clipboard', 2000);"
  , "    });"
  , "}"
  , "</script>"
  , "</body>"
  , "</html>"
  ]
