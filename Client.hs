import Network
import System.IO
import Monitor.Common
import Control.Monad
import Control.Applicative
import Control.Concurrent
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Data.Time
import Data.List

-- all the state
data AllState = AllState {modeState :: ModeState, events :: Events, fullscreen :: Bool}

-- we need which mode we are in, which mode is next, and how we pick which modes are next
data ModeState = ModeState {currentMode :: Mode, nextMode :: Mode, modeTransition :: ModeTransition}
data Mode = PingMode PingState
data ModeTransition = AutoTransition

-- we need GUI information, which depends on what mode we are in
data PingState = PingState

-- we need to keep track of all the events, split into their own piles of info
data Events = Events {pings :: [Event], eventsChanged :: Bool}

main = do
  unsafeInitGUIForThreadedRTS
  -- initial state
  stVar <- newMVar initialState
  forkIO $ connect stVar
  -- main window and draw area
  (w, area) <- mainWindow
  handleInputs area w stVar
  -- thread to perform redraws
  forkIO $ redraw area stVar
  -- and draw the window
  widgetShowAll w
  mainGUI
  return ()

connect :: MVar AllState -> IO ()
connect stVar = do
  h <- connectTo "localhost" (PortNumber 8960)
  hSetBuffering h LineBuffering
  hPutStrLn h "client"
  forever $ do
    estr <- hGetLine h
    event <- parseEvent estr
    st <- takeMVar stVar
    let st' = st {events = addEvent event (events st)}
    let st'' = setChanged True st'
    putMVar stVar st''
  return ()

redraw :: DrawingArea -> MVar AllState -> IO ()
redraw area stvar = do
  done <- newMVar ()
  lastChange <- newEmptyMVar
  putMVar lastChange =<< getCurrentTime
  forever $ do
    changed <- hasChanged <$> readMVar stvar
    when changed $ do
      takeMVar done -- block on drawing the previous frame
      postGUIAsync $ do
        st <- takeMVar stvar
        -- get some stuff
        (x, y) <- widgetGetSize area
        w <- widgetGetDrawWindow area
        -- DRAW!
        renderWithDrawable w (drawState x y st)
        let st' = setChanged False st
        putMVar stvar st'
        putMVar done ()
    threadDelay 16666 -- 60 fps

hasChanged :: AllState -> Bool
hasChanged = eventsChanged . events

setChanged :: Bool -> AllState -> AllState
setChanged b st = st {events = (events st) {eventsChanged = b}}

drawState :: Int -> Int -> AllState -> Render ()
drawState w h st = do
  case currentMode (modeState st) of
    PingMode ps -> drawPings w h ps (pings (events st))

drawPings :: Int -> Int -> PingState -> [Event] -> Render ()
drawPings w h ps es = do
  fillBack w h
  let text = unlines (map prettyPrint es)
  selectFontFace "monospace" FontSlantNormal FontWeightNormal
  layout <- createLayout text
  showLayout layout
  return ()

fillBack w h = do
  setSourceRGB 1 1 1
  rectangle 0 0 (fromIntegral w) (fromIntegral h)
  fill
  setSourceRGB 0 0 0

addEvent :: Event -> Events -> Events
addEvent Invalid st = st
addEvent p@Ping{} st = st {pings = take 20 $ p:pings st}

initialState :: AllState
initialState = AllState
  { modeState = ModeState
      { currentMode = PingMode PingState
      , nextMode = PingMode PingState
      , modeTransition = AutoTransition}
  , events = Events
      { pings = []
      , eventsChanged = True}
  , fullscreen = False}

mainWindow :: IO (Window, DrawingArea)
mainWindow = do
  w <- windowNew
  windowResize w 600 300
  area <- drawingAreaNew
  containerAdd w area
  return (w, area)

handleInputs :: DrawingArea -> Window -> MVar AllState -> IO ()
handleInputs area w stVar = do
  w `on` keyPressEvent $ tryEvent $ do
    [] <- eventModifier
    "F11" <- eventKeyName
    st <- liftIO $ takeMVar stVar
    let fs = fullscreen st
    liftIO $ if fs then windowUnfullscreen w else windowFullscreen w
    liftIO $ putMVar stVar st {fullscreen = not fs}
  w `on` deleteEvent $ tryEvent $ do
    liftIO $ mainQuit
  area `on` configureEvent $ tryEvent $ do
    st <- liftIO $ takeMVar stVar
    let st' = setChanged True st
    liftIO $ putMVar stVar st'
  return ()

