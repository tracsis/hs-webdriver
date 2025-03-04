{-# LANGUAGE ExistentialQuantification #-}

-- | This module exports basic WD actions that can be used to interact with a
-- browser session.
module Test.WebDriver.Commands
       ( -- * Sessions
         createSession, closeSession, sessions, getActualCaps, getSessionCaps
         -- * Browser interaction
         -- ** Web navigation
       , openPage, forward, back, refresh
         -- ** Page info
       , getCurrentURL, getSource, getTitle, saveScreenshot, screenshot, screenshotBase64
         -- * Timeouts
       , setImplicitWait, setScriptTimeout, setPageLoadTimeout
         -- * Web elements
       , Element(..), Selector(..)
         -- ** Searching for elements
       , findElem, findElems, findElemFrom, findElemsFrom
         -- ** Interacting with elements
       , click, submit, getText
         -- *** Sending key inputs to elements
       , sendKeys, sendRawKeys, clearInput
         -- ** Element information
       , ElemRect(..)
       , attr, prop, cssProp
       , elemPos, elemSize, elemRect
       , isSelected, isEnabled, isDisplayed
       , tagName, activeElem
         -- * Javascript
       , executeJS, asyncJS
       , JSArg(..)
         -- * Windows
       , WindowHandle(..), WindowRect(..)
       , getCurrentWindow, closeWindow, windows, focusWindow,  maximize
       , windowRect, getWindowSize, setWindowSize, getWindowPos, setWindowPos
         -- * Focusing on frames
       , focusFrame, FrameSelector(..)
         -- * Cookies
       , Cookie(..), mkCookie
       , cookies, setCookie, deleteCookie, deleteVisibleCookies, deleteCookieByName
         -- * Alerts
       , getAlertText, replyToAlert, acceptAlert, dismissAlert
         -- * Mouse gestures
       , moveTo, moveToCenter, moveToFrom
       , clickWith, MouseButton(..)
       , mouseDown, mouseUp, withMouseDown, doubleClick
         -- * HTML 5 Web Storage
       , WebStorageType(..), storageSize, getAllKeys, deleteAllKeys
       , getKey, setKey, deleteKey
         -- * HTML 5 Application Cache
       , ApplicationCacheStatus(..)
       , getApplicationCacheStatus
         -- * Mobile device support
         -- ** Screen orientation
       , Orientation(..)
       , getOrientation, setOrientation
         -- ** Geo-location
       , getLocation, setLocation
         -- ** Touch gestures
       , touchClick, touchDown, touchUp, touchMove
       , touchScroll, touchScrollFrom, touchDoubleClick
       , touchLongClick, touchFlick, touchFlickFrom
         -- * IME support
       , availableIMEEngines, activeIMEEngine, checkIMEActive
       , activateIME, deactivateIME
         -- * Uploading files to remote server
         -- |These functions allow you to upload a file to a remote server.
         -- Note that this operation isn't supported by all WebDriver servers,
         -- and the location where the file is stored is not standardized.
       , uploadFile, uploadRawFile, uploadZipEntry
         -- * Server information and logs
       , serverStatus
       , getLogs, getLogTypes, LogType, LogEntry(..), LogLevel(..)
       ) where

import Codec.Archive.Zip
import Control.Applicative
import Control.Exception (SomeException)
import Control.Exception.Lifted (throwIO, handle)
import qualified Control.Exception.Lifted as L
import Control.Monad
import Control.Monad.Base
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Base64.Lazy as B64
import Data.ByteString.Lazy as LBS (ByteString, writeFile)
import Data.CallStack
import qualified Data.Foldable as F
import Data.Maybe
import Data.String (fromString)
import Data.Text (Text, append, toUpper, toLower)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TL
import Data.Word
import Network.URI hiding (path)  -- suppresses warnings
import Test.WebDriver.Actions.Internal
import Test.WebDriver.Capabilities
import Test.WebDriver.Class
import Test.WebDriver.Commands.Internal
import qualified Test.WebDriver.Common.Keys as Keys
import Test.WebDriver.Cookies
import Test.WebDriver.Exceptions.Internal
import Test.WebDriver.JSON
import Test.WebDriver.Session
import Test.WebDriver.Utils (urlEncode)

import Prelude -- hides some "unused import" warnings

-- |Create a new session with the given 'Capabilities'. The returned session becomes the \"current session\" for this action.
--
-- Note: if you're using 'runSession' to run your WebDriver commands, you don't need to call this explicitly.
createSession :: (HasCallStack, WebDriver wd) => Capabilities -> wd WDSession
createSession caps = do
  let connect = withAuthHeaders $ doCommand methodPost "/session" . single "capabilities" $ single "alwaysMatch" caps
  resp <- connect `L.catch` \(_ex :: FailedCommand) -> connect
  s <- getSession
  let sessCpas = parseMaybe (.: "capabilities") resp
  let sessId = parseMaybe (.: "sessionId") resp
  putSession s
    { wdSessCreateResponse = sessCpas
    , wdSessId = SessionId <$> sessId
    }
  getSession

-- |Retrieve a list of active sessions and their 'Capabilities'.
sessions :: (HasCallStack, WebDriver wd) => wd [(SessionId, Capabilities)]
sessions = do
  objs <- doCommand methodGet "/sessions" Null
  mapM (parsePair "id" "capabilities" "sessions") objs

-- |Get the actual server-side 'Capabilities' of the current session.
getActualCaps :: (HasCallStack, WebDriver wd) => wd Capabilities
getActualCaps = doSessCommand methodGet "" Null

-- |Get the 'Capabilities' that were sent when the session was creted.
getSessionCaps :: (HasCallStack, WDSessionState s) => s (Maybe Capabilities)
getSessionCaps = do
  caps <- wdSessCreateResponse <$> getSession
  return $ parseMaybe parseJSON =<< caps

-- |Close the current session and the browser associated with it.
closeSession :: (HasCallStack, WebDriver wd) => wd ()
closeSession = do s@WDSession {} <- getSession
                  noReturn $ doSessCommand methodDelete "" emptyObject
                  putSession s { wdSessId = Nothing }


-- |Sets the amount of time (ms) we implicitly wait when searching for elements.
setImplicitWait :: (HasCallStack, WebDriver wd) => Integer -> wd ()
setImplicitWait ms =
  noReturn $ doSessCommand methodPost "/timeouts/implicit_wait" (object msField)
    `L.catch` \(_ :: SomeException) ->
      doSessCommand methodPost "/timeouts" (object allFields)
  where msField   = ["ms" .= ms]
        allFields = ["type" .= ("implicit" :: String)] ++ msField

-- |Sets the amount of time (ms) we wait for an asynchronous script to return a
-- result.
setScriptTimeout :: (HasCallStack, WebDriver wd) => Integer -> wd ()
setScriptTimeout ms =
  noReturn $ doSessCommand methodPost "/timeouts/async_script" (object msField)
    `L.catch` \( _ :: SomeException) ->
      doSessCommand methodPost "/timeouts" (object allFields)
  where msField   = ["ms" .= ms]
        allFields = ["type" .= ("script" :: String)] ++ msField

-- |Sets the amount of time (ms) to wait for a page to finish loading before throwing a 'Timeout' exception.
setPageLoadTimeout :: (HasCallStack, WebDriver wd) => Integer -> wd ()
setPageLoadTimeout ms = noReturn $ doSessCommand methodPost "/timeouts" params
  where params = object ["type" .= ("page load" :: String)
                        ,"ms"   .= ms ]

-- |Gets the URL of the current page.
getCurrentURL :: (HasCallStack, WebDriver wd) => wd String
getCurrentURL = doSessCommand methodGet "/url" Null

-- |Opens a new page by the given URL.
openPage :: (HasCallStack, WebDriver wd) => String -> wd ()
openPage url
  | isURI url = noReturn . doSessCommand methodPost "/url" . single "url" $ url
  | otherwise = throwIO . InvalidURL $ url

-- |Navigate forward in the browser history.
forward :: (HasCallStack, WebDriver wd) => wd ()
forward = noReturn $ doSessCommand methodPost "/forward" emptyObject

-- |Navigate backward in the browser history.
back :: (HasCallStack, WebDriver wd) => wd ()
back = noReturn $ doSessCommand methodPost "/back" emptyObject

-- |Refresh the current page
refresh :: (HasCallStack, WebDriver wd) => wd ()
refresh = noReturn $ doSessCommand methodPost "/refresh" emptyObject

-- |An existential wrapper for any 'ToJSON' instance. This allows us to pass
-- parameters of many different types to Javascript code.
data JSArg = forall a. ToJSON a => JSArg a

instance ToJSON JSArg where
  toJSON (JSArg a) = toJSON a

{- |Inject a snippet of Javascript into the page for execution in the
context of the currently selected frame. The executed script is
assumed to be synchronous and the result of evaluating the script is
returned and converted to an instance of FromJSON.

The first parameter defines a sequence of arguments to pass to the javascript
function. Arguments of type Element will be converted to the
corresponding DOM element. Likewise, any elements in the script result
will be returned to the client as Elements.

The second parameter defines the script itself in the form of a
function body. The value returned by that function will be returned to
the client. The function will be invoked with the provided argument
list and the values may be accessed via the arguments object in the
order specified.

When using 'executeJS', GHC might complain about an ambiguous type in
situations where the result of the executeJS call is ignored/discard.
Consider the following example:

@
	jsExample = do
		e <- findElem (ById "foo")
		executeJS [] "someAction()"
		return e
@

Because the result of the 'executeJS' is discarded, GHC cannot resolve
which instance of the 'fromJSON' class to use when parsing the
Selenium server response. In such cases, we can use the 'ignoreReturn'
helper function located in "Test.WebDriver.JSON". 'ignoreReturn' has
no runtime effect; it simply helps the type system by expicitly providing
a `fromJSON` instance to use.

@
	import Test.WebDriver.JSON (ignoreReturn)
	jsExample = do
		e <- findElem (ById "foo")
		ignoreReturn $ executeJS [] "someAction()"
		return e
@
-}
executeJS :: (HasCallStack, F.Foldable f, FromJSON a, WebDriver wd) => f JSArg -> Text -> wd a
executeJS a s = fromJSON' =<< getResult
  where
    getResult = doSessCommand methodPost "/execute/sync" . pair ("args", "script") $ (F.toList a,s)

{- |Executes a snippet of Javascript code asynchronously. This function works
similarly to 'executeJS', except that the Javascript is passed a callback
function as its final argument. The script should call this function
to signal that it has finished executing, passing to it a value that will be
returned as the result of asyncJS. A result of Nothing indicates that the
Javascript function timed out (see 'setScriptTimeout')
-}
asyncJS :: (HasCallStack, F.Foldable f, FromJSON a, WebDriver wd) => f JSArg -> Text -> wd (Maybe a)
asyncJS a s = handle timeout $ Just <$> (fromJSON' =<< getResult)
  where
    getResult = doSessCommand methodPost "/execute/async" . pair ("args", "script")
                $ (F.toList a,s)
    timeout (FailedCommand Timeout _)       = return Nothing
    timeout (FailedCommand ScriptTimeout _) = return Nothing
    timeout err = throwIO err

-- |Save a screenshot to a particular location
saveScreenshot :: (HasCallStack, WebDriver wd) => FilePath -> wd ()
saveScreenshot path = screenshot >>= liftBase . LBS.writeFile path

-- |Grab a screenshot of the current page as a PNG image
screenshot :: (HasCallStack, WebDriver wd) => wd LBS.ByteString
screenshot = B64.decodeLenient <$> screenshotBase64

-- |Grab a screenshot as a base-64 encoded PNG image. This is the protocol-defined format.
screenshotBase64 :: (HasCallStack, WebDriver wd) => wd LBS.ByteString
screenshotBase64 = TL.encodeUtf8 <$> doSessCommand methodGet "/screenshot" Null

availableIMEEngines :: (HasCallStack, WebDriver wd) => wd [Text]
availableIMEEngines = doSessCommand methodGet "/ime/available_engines" Null

activeIMEEngine :: (HasCallStack, WebDriver wd) => wd Text
activeIMEEngine = doSessCommand methodGet "/ime/active_engine" Null

checkIMEActive :: (HasCallStack, WebDriver wd) => wd Bool
checkIMEActive = doSessCommand methodGet "/ime/activated" Null

activateIME :: (HasCallStack, WebDriver wd) => Text -> wd ()
activateIME = noReturn . doSessCommand methodPost "/ime/activate" . single "engine"

deactivateIME :: (HasCallStack, WebDriver wd) => wd ()
deactivateIME = noReturn $ doSessCommand methodPost "/ime/deactivate" emptyObject


-- |Specifies the frame used by 'Test.WebDriver.Commands.focusFrame'
data FrameSelector = WithIndex Integer
                     -- |focus on a frame by name or ID
                   | WithName Text
                     -- |focus on a frame 'Element'
                   | WithElement Element
                     -- |focus on the first frame, or the main document
                     -- if iframes are used.
                   | DefaultFrame
                   deriving (Eq, Show, Read)

instance ToJSON FrameSelector where
  toJSON s = case s of
    WithIndex i -> toJSON i
    WithName n -> toJSON n
    WithElement e -> toJSON e
    DefaultFrame -> Null

-- |Switch focus to the frame specified by the FrameSelector.
focusFrame :: (HasCallStack, WebDriver wd) => FrameSelector -> wd ()
focusFrame s = noReturn $ doSessCommand methodPost "/frame" . single "id" $ s

-- |Returns a handle to the currently focused window
getCurrentWindow :: (HasCallStack, WebDriver wd) => wd WindowHandle
getCurrentWindow = doSessCommand methodGet "/window" Null

-- |Returns a list of all windows available to the session
windows :: (HasCallStack, WebDriver wd) => wd [WindowHandle]
windows = doSessCommand methodGet "/window/handles" Null

focusWindow :: (HasCallStack, WebDriver wd) => WindowHandle -> wd ()
focusWindow w = noReturn $ doSessCommand methodPost "/window" . single "handle" $ w

-- |Closes the given window
closeWindow :: (HasCallStack, WebDriver wd) => WindowHandle -> wd ()
closeWindow w = do
  cw <- getCurrentWindow
  focusWindow w
  ignoreReturn $ doSessCommand methodDelete "/window" emptyObject
  unless (w == cw) $ focusWindow cw

-- |Maximizes the current  window if not already maximized
maximize :: (HasCallStack, WebDriver wd) => wd ()
maximize = ignoreReturn $ doSessCommand methodPost "/window/maximize" emptyObject

data WindowRect = WindowRect
  { wrectX :: Int
  , wrectY :: Int
  , wrectWidth :: Word
  , wrectHeight :: Word
  } deriving (Eq, Show)

instance FromJSON WindowRect where
  parseJSON = withObject "WindowRect" $ \o -> do
    wrectX <- o .: "x"
    wrectY <- o .: "y"
    wrectWidth <- o .: "width"
    wrectHeight <- o .: "height"
    pure $ WindowRect {..}

-- |Retrieve current window's rect.
windowRect :: (HasCallStack, WebDriver wd) => wd WindowRect
windowRect = doSessCommand methodGet "/window/rect" Null

-- |Set current window's rect.
setWindowRect :: (HasCallStack, WebDriver wd) => Maybe (Int, Int) -> Maybe (Word, Word) -> wd WindowRect
setWindowRect xy wh = do
  doSessCommand methodPost "/window/rect" $
    object
      [ "x" .= fmap fst xy
      , "y" .= fmap snd xy
      , "width" .= fmap fst wh
      , "height" .= fmap snd wh
      ]

-- |Get the dimensions of the current window.
getWindowSize :: (HasCallStack, WebDriver wd) => wd (Word, Word)
getWindowSize = do
  WindowRect {..} <- windowRect
  pure (wrectWidth, wrectHeight)

-- |Set the dimensions of the current window.
setWindowSize :: (HasCallStack, WebDriver wd) => (Word, Word) -> wd ()
setWindowSize wh = void $ setWindowRect Nothing (Just wh)

-- |Get the coordinates of the current window.
getWindowPos :: (HasCallStack, WebDriver wd) => wd (Int, Int)
getWindowPos = do
  WindowRect {..} <- windowRect
  pure (wrectX, wrectY)

-- |Set the coordinates of the current window.
setWindowPos :: (HasCallStack, WebDriver wd) => (Int, Int) -> wd ()
setWindowPos xy = void $ setWindowRect (Just xy) Nothing

-- |Retrieve all cookies visible to the current page.
cookies :: (HasCallStack, WebDriver wd) => wd [Cookie]
cookies = doSessCommand methodGet "/cookie" Null

-- |Set a cookie. If the cookie path is not specified, it will default to \"/\".
-- Likewise, if the domain is omitted, it will default to the current page's
-- domain
setCookie :: (HasCallStack, WebDriver wd) => Cookie -> wd ()
setCookie = noReturn . doSessCommand methodPost "/cookie" . single "cookie"

-- |Delete a cookie. This will do nothing is the cookie isn't visible to the
-- current page.
deleteCookie :: (HasCallStack, WebDriver wd) => Cookie -> wd ()
deleteCookie c = noReturn $ doSessCommand methodDelete ("/cookie/" `append` urlEncode (cookName c)) emptyObject

deleteCookieByName :: (HasCallStack, WebDriver wd) => Text -> wd ()
deleteCookieByName n = noReturn $ doSessCommand methodDelete ("/cookie/" `append` n) emptyObject

-- |Delete all visible cookies on the current page.
deleteVisibleCookies :: (HasCallStack, WebDriver wd) => wd ()
deleteVisibleCookies = noReturn $ doSessCommand methodDelete "/cookie" emptyObject

-- |Get the current page source
getSource :: (HasCallStack, WebDriver wd) => wd Text
getSource = doSessCommand methodGet "/source" Null

-- |Get the title of the current page.
getTitle :: (HasCallStack, WebDriver wd) => wd Text
getTitle = doSessCommand methodGet "/title" Null

-- |Specifies element(s) within a DOM tree using various selection methods.
data Selector = ByTag Text
              | ByLinkText Text
              | ByPartialLinkText Text
              | ByCSS Text
              | ByXPath Text
              deriving (Eq, Show, Ord)

instance ToJSON Selector where
  toJSON s = case s of
    ByTag t             -> selector "tag name" t
    ByLinkText t        -> selector "link text" t
    ByPartialLinkText t -> selector "partial link text" t
    ByCSS t             -> selector "css selector" t
    ByXPath t           -> selector "xpath" t
    where
      selector :: Text -> Text -> Value
      selector sn t = object ["using" .= sn, "value" .= t]

-- |Find an element on the page using the given element selector.
findElem :: (HasCallStack, WebDriver wd) => Selector -> wd Element
findElem = doSessCommand methodPost "/element"

-- |Find all elements on the page matching the given selector.
findElems :: (HasCallStack, WebDriver wd) => Selector -> wd [Element]
findElems = doSessCommand methodPost "/elements"

-- |Return the element that currently has focus.
activeElem :: (HasCallStack, WebDriver wd) => wd Element
activeElem = doSessCommand methodGet "/element/active" Null

-- |Search for an element using the given element as root.
findElemFrom :: (HasCallStack, WebDriver wd) => Element -> Selector -> wd Element
findElemFrom e s
  | isRelative s = doElemCommand methodPost e "/element" s
  | otherwise = fail "Selector in findElemFrom must be relative"

-- |Find all elements matching a selector, using the given element as root.
findElemsFrom :: (HasCallStack, WebDriver wd) => Element -> Selector -> wd [Element]
findElemsFrom e s
  | isRelative s = doElemCommand methodPost e "/elements" s
  | otherwise = fail "Selector in findElemsFrom must be relative"

isRelative :: Selector -> Bool
isRelative (ByXPath t) = not $ "/" `T.isPrefixOf` t
isRelative _ = True

performActions :: (HasCallStack, WebDriver wd) => [Actions] -> wd ()
performActions = noReturn . doSessCommand methodPost "/actions" . single "actions"

releaseActions :: (HasCallStack, WebDriver wd) => wd ()
releaseActions = noReturn $ doSessCommand methodDelete "/actions" emptyObject

-- |Click on an element.
click :: (HasCallStack, WebDriver wd) => Element -> wd ()
click e = noReturn $ doElemCommand methodPost e "/click" emptyObject

-- |Submit a form element. This may be applied to descendents of a form element
-- as well.
submit :: (HasCallStack, WebDriver wd) => Element -> wd ()
submit e = noReturn $ doElemCommand methodPost e "/submit" emptyObject

-- |Get all visible text within this element.
getText :: (HasCallStack, WebDriver wd) => Element -> wd Text
getText e = doElemCommand methodGet e "/text" emptyObject

-- |Send a sequence of keystrokes to an element. All modifier keys are released
-- at the end of the function. Named constants for special modifier keys can be found
-- in "Test.WebDriver.Common.Keys"
sendKeys :: (HasCallStack, WebDriver wd) => Text -> Element -> wd ()
sendKeys t e = noReturn . doElemCommand methodPost e "/value" . single "text" $ t

-- |Similar to sendKeys, but doesn't implicitly release modifier keys
-- afterwards. This allows you to combine modifiers with mouse clicks.
sendRawKeys :: (HasCallStack, WebDriver wd) => Text -> wd ()
sendRawKeys t
  | t == Keys.null =
  releaseActions
  | otherwise =
  performActions
    [ Actions
        { actionsType = ActionsKey
        , actionsId = "keyboard"
        , actionsParameters = Nothing
        , actionsActions =
            flip concatMap (T.chunksOf 1 t) $ \c ->
              if c `elem` [Keys.control, Keys.alt, Keys.shift, Keys.meta]
                then [ keyDownAction c ]
                else [ keyDownAction c, keyUpAction c ]
        }
    ]
                  

-- |Return the tag name of the given element.
tagName :: (HasCallStack, WebDriver wd) => Element -> wd Text
tagName e = doElemCommand methodGet e "/name" emptyObject

-- |Clear a textarea or text input element's value.
clearInput :: (HasCallStack, WebDriver wd) => Element -> wd ()
clearInput e = noReturn $ doElemCommand methodPost e "/clear" emptyObject

-- |Determine if the element is selected.
isSelected :: (HasCallStack, WebDriver wd) => Element -> wd Bool
isSelected e = doElemCommand methodGet e "/selected" emptyObject

-- |Determine if the element is enabled.
isEnabled :: (HasCallStack, WebDriver wd) => Element -> wd Bool
isEnabled e = doElemCommand methodGet e "/enabled" emptyObject

-- |Determine if the element is displayed.
isDisplayed :: (HasCallStack, WebDriver wd) => Element -> wd Bool
isDisplayed e = doElemCommand methodGet e "/displayed" emptyObject

-- |Retrieve the value of an element's attribute
attr :: (HasCallStack, WebDriver wd) => Element -> Text -> wd (Maybe Text)
attr e t = doElemCommand methodGet e ("/attribute/" `append` urlEncode t) emptyObject

-- |Retrieve the value of an element's propery
prop :: (HasCallStack, WebDriver wd) => Element -> Text -> wd (Maybe Text)
prop e t = doElemCommand methodGet e ("/property/" `append` urlEncode t) emptyObject

-- |Retrieve the value of an element's computed CSS property
cssProp :: (HasCallStack, WebDriver wd) => Element -> Text -> wd (Maybe Text)
cssProp e t = doElemCommand methodGet e ("/css/" `append` urlEncode t) emptyObject

-- |Retrieve an element's current position.
elemPos :: (HasCallStack, WebDriver wd) => Element -> wd (Float, Float)
elemPos e = do
  ElemRect{..} <- elemRect e
  pure (rectX, rectY)

-- |Retrieve an element's current size.
elemSize :: (HasCallStack, WebDriver wd) => Element -> wd (Float, Float)
elemSize e = do
  ElemRect{..} <- elemRect e
  pure (rectWidth, rectHeight)

data ElemRect = ElemRect
  { rectX :: Float
  , rectY :: Float
  , rectWidth :: Float
  , rectHeight :: Float
  } deriving (Eq, Show)

instance FromJSON ElemRect where
  parseJSON = withObject "ElemRect" $ \o -> do
    rectX <- o .: "x"
    rectY <- o .: "y"
    rectWidth <- o .: "width"
    rectHeight <- o .: "height"
    pure $ ElemRect {..}

-- |Retrieve an element's current rect.
elemRect :: (HasCallStack, WebDriver wd) => Element -> wd ElemRect
elemRect e = doElemCommand methodGet e "/rect" emptyObject

-- |A screen orientation
data Orientation = Landscape | Portrait
                 deriving (Eq, Show, Ord, Bounded, Enum)

instance ToJSON Orientation where
  toJSON = String . toUpper . fromString . show

instance FromJSON Orientation where
  parseJSON (String jStr) = case toLower jStr of
    "landscape" -> return Landscape
    "portrait"  -> return Portrait
    err         -> fail $ "Invalid Orientation string " ++ show err
  parseJSON v = typeMismatch "Orientation" v

-- |Get the current screen orientation for rotatable display devices.
getOrientation :: (HasCallStack, WebDriver wd) => wd Orientation
getOrientation = doSessCommand methodGet "/orientation" Null

-- |Set the current screen orientation for rotatable display devices.
setOrientation :: (HasCallStack, WebDriver wd) => Orientation -> wd ()
setOrientation = noReturn . doSessCommand methodPost "/orientation" . single "orientation"

-- |Get the text of an alert dialog.
getAlertText :: (HasCallStack, WebDriver wd) => wd Text
getAlertText = doSessCommand methodGet "/alert/text" Null

-- |Sends keystrokes to Javascript prompt() dialog.
replyToAlert :: (HasCallStack, WebDriver wd) => Text -> wd ()
replyToAlert = noReturn . doSessCommand methodPost "/alert/text" . single "text"

-- |Accepts the currently displayed alert dialog.
acceptAlert :: (HasCallStack, WebDriver wd) => wd ()
acceptAlert = noReturn $ doSessCommand methodPost "/alert/accept" emptyObject

-- |Dismisses the currently displayed alert dialog.
dismissAlert :: (HasCallStack, WebDriver wd) => wd ()
dismissAlert = noReturn $ doSessCommand methodPost "/alert/dismiss" emptyObject

-- |Moves the mouse to the given position relative to current mouse position.
moveTo :: (HasCallStack, WebDriver wd) => (Int, Int) -> wd ()
moveTo (xoffset, yoffset) = do
  performActions
    [ Actions
        { actionsType = ActionsPointer
        , actionsId = "pointer"
        , actionsParameters = Just (ActionsParameters { paramsPointerType = Just PointerMouse })
        , actionsActions =
            [ pointerMoveAction (xoffset, yoffset) OriginPointer
            ]
        }
    ]

-- |Moves the mouse to the center of a given element.
moveToCenter :: (HasCallStack, WebDriver wd) => Element -> wd ()
moveToCenter = moveToFrom (0, 0)

-- |Moves the mouse to the given position relative to the given element's top left.
moveToFrom :: (HasCallStack, WebDriver wd) => (Int, Int) -> Element -> wd ()
moveToFrom (x, y) e = do
  performActions
    [ Actions
        { actionsType = ActionsPointer
        , actionsId = "pointer"
        , actionsParameters = Just (ActionsParameters { paramsPointerType = Just PointerMouse })
        , actionsActions =
            [ pointerMoveAction (x, y) (OriginElement e)
            ]
        }
    ]

-- |Click at the current mouse position with the given mouse button.
clickWith :: (HasCallStack, WebDriver wd) => MouseButton -> wd ()
clickWith btn = do
  performActions
    [ Actions
        { actionsType = ActionsPointer
        , actionsId = "pointer"
        , actionsParameters = Just (ActionsParameters { paramsPointerType = Just PointerMouse })
        , actionsActions =
            [ pointerDownAction { actionButton = Just btn }
            , pointerUpAction { actionButton = Just btn }
            ]
        }
    ]

-- |Perform the given action with the left mouse button held down. The mouse
-- is automatically released afterwards.
withMouseDown :: (HasCallStack, WebDriver wd) => wd a -> wd a
withMouseDown wd = mouseDown >> wd <* mouseUp

-- |Press and hold the left mouse button down. Note that undefined behavior
-- occurs if the next mouse command is not mouseUp.
mouseDown :: (HasCallStack, WebDriver wd) => wd ()
mouseDown = do
  performActions
    [ Actions
        { actionsType = ActionsPointer
        , actionsId = "pointer"
        , actionsParameters = Just (ActionsParameters { paramsPointerType = Just PointerMouse })
        , actionsActions =
            [ pointerDownAction { actionButton = Just LeftButton }
            ]
        }
    ]

-- |Release the left mouse button.
mouseUp :: (HasCallStack, WebDriver wd) => wd ()
mouseUp = do
  performActions
    [ Actions
        { actionsType = ActionsPointer
        , actionsId = "pointer"
        , actionsParameters = Just (ActionsParameters { paramsPointerType = Just PointerMouse })
        , actionsActions =
            [ pointerUpAction { actionButton = Just LeftButton }
            ]
        }
    ]

-- |Double click at the current mouse location.
doubleClick :: (HasCallStack, WebDriver wd) => wd ()
doubleClick = do
  performActions
    [ Actions
        { actionsType = ActionsPointer
        , actionsId = "pointer"
        , actionsParameters = Just (ActionsParameters { paramsPointerType = Just PointerMouse })
        , actionsActions =
            [ pointerDownAction { actionButton = Just LeftButton }
            , pointerUpAction { actionButton = Just LeftButton }
            , pointerDownAction { actionButton = Just LeftButton }
            , pointerUpAction { actionButton = Just LeftButton }
            ]
        }
    ]

-- |Single tap on the touch screen at the given element's location.
touchClick :: (HasCallStack, WebDriver wd) => Element -> wd ()
touchClick (Element e) =
  noReturn . doSessCommand methodPost "/touch/click" . single "element" $ e

-- |Emulates pressing a finger down on the screen at the given location.
touchDown :: (HasCallStack, WebDriver wd) => (Int, Int) -> wd ()
touchDown = noReturn . doSessCommand methodPost "/touch/down" . pair ("x","y")

-- |Emulates removing a finger from the screen at the given location.
touchUp :: (HasCallStack, WebDriver wd) => (Int, Int) -> wd ()
touchUp = noReturn . doSessCommand methodPost "/touch/up" . pair ("x","y")

-- |Emulates moving a finger on the screen to the given location.
touchMove :: (HasCallStack, WebDriver wd) => (Int, Int) -> wd ()
touchMove = noReturn . doSessCommand methodPost "/touch/move" . pair ("x","y")

-- |Emulate finger-based touch scroll. Use this function if you don't care where
-- the scroll begins
touchScroll :: (HasCallStack, WebDriver wd) => (Int, Int) -> wd ()
touchScroll = noReturn . doSessCommand methodPost "/touch/scroll" . pair ("xoffset","yoffset")

-- |Emulate finger-based touch scroll, starting from the given location relative
-- to the given element.
touchScrollFrom :: (HasCallStack, WebDriver wd) => (Int, Int) -> Element -> wd ()
touchScrollFrom (x, y) (Element e) =
  noReturn
  . doSessCommand methodPost "/touch/scroll"
  . triple ("xoffset", "yoffset", "element")
  $ (x, y, e)

-- |Emulate a double click on a touch device.
touchDoubleClick :: (HasCallStack, WebDriver wd) => Element -> wd ()
touchDoubleClick (Element e) =
  noReturn
  . doSessCommand methodPost "/touch/doubleclick"
  . single "element" $ e

-- |Emulate a long click on a touch device.
touchLongClick :: (HasCallStack, WebDriver wd) => Element -> wd ()
touchLongClick (Element e) =
  noReturn
  . doSessCommand methodPost "/touch/longclick"
  . single "element" $ e
-- |Emulate a flick on the touch screen. The coordinates indicate x and y
-- velocity, respectively. Use this function if you don't care where the
-- flick starts.
touchFlick :: (HasCallStack, WebDriver wd) => (Int, Int) -> wd ()
touchFlick =
  noReturn
  . doSessCommand methodPost "/touch/flick"
  . pair ("xSpeed", "ySpeed")

-- |Emulate a flick on the touch screen.
touchFlickFrom :: (HasCallStack, WebDriver wd) =>
                  Int           -- ^ flick velocity
                  -> (Int, Int) -- ^ a location relative to the given element
                  -> Element    -- ^ the given element
                  -> wd ()
touchFlickFrom s (x,y) (Element e) =
  noReturn
  . doSessCommand methodPost "/touch/flick" . object $
  ["xoffset" .= x
  ,"yoffset" .= y
  ,"speed"   .= s
  ,"element" .= e
  ]

-- |Get the current geographical location of the device.
getLocation :: (HasCallStack, WebDriver wd) => wd (Int, Int, Int)
getLocation = doSessCommand methodGet "/location" Null
              >>= parseTriple "latitude" "longitude" "altitude" "getLocation"

-- |Set the current geographical location of the device.
setLocation :: (HasCallStack, WebDriver wd) => (Int, Int, Int) -> wd ()
setLocation = noReturn . doSessCommand methodPost "/location"
              . triple ("latitude",
                        "longitude",
                        "altitude")

-- |Uploads a file from the local filesystem by its file path.
-- Returns the remote filepath of the file
uploadFile :: (HasCallStack, WebDriver wd) => FilePath -> wd Text
uploadFile path = uploadZipEntry =<< liftBase (readEntry [] path)

-- |Uploads a raw bytestring with associated file info.
-- Returns the remote filepath of the file
uploadRawFile :: (HasCallStack, WebDriver wd) =>
                 FilePath          -- ^File path to use with this bytestring.
                 -> Integer        -- ^Modification time
                                   -- (in seconds since Unix epoch).
                 -> LBS.ByteString -- ^ The file contents as a lazy ByteString
                 -> wd Text
uploadRawFile path t str = uploadZipEntry (toEntry path t str)


-- |Lowest level interface to the file uploading mechanism.
-- This allows you to specify the exact details of
-- the zip entry sent across network.
-- Returns the remote filepath of the extracted file
uploadZipEntry :: (HasCallStack, WebDriver wd) => Entry -> wd Text
uploadZipEntry = doSessCommand methodPost "/se/file" . single "file"
                 . TL.decodeUtf8 . B64.encode . fromArchive . (`addEntryToArchive` emptyArchive)


-- |Get the current number of keys in a web storage area.
storageSize :: (HasCallStack, WebDriver wd) => WebStorageType -> wd Integer
storageSize s = doStorageCommand methodGet s "/size" emptyObject

-- |Get a list of all keys from a web storage area.
getAllKeys :: (HasCallStack, WebDriver wd) => WebStorageType -> wd [Text]
getAllKeys s = doStorageCommand methodGet s "" emptyObject

-- |Delete all keys within a given web storage area.
deleteAllKeys :: (HasCallStack, WebDriver wd) => WebStorageType -> wd ()
deleteAllKeys s = noReturn $ doStorageCommand methodDelete s "" emptyObject

-- |An HTML 5 storage type
data WebStorageType = LocalStorage | SessionStorage
                    deriving (Eq, Show, Ord, Bounded, Enum)

-- |Get the value associated with a key in the given web storage area.
-- Unset keys result in empty strings, since the Web Storage spec
-- makes no distinction between the empty string and an undefined value.
getKey :: (HasCallStack, WebDriver wd) => WebStorageType -> Text ->  wd Text
getKey s k = doStorageCommand methodGet s ("/key/" `T.append` urlEncode k) emptyObject

-- |Set a key in the given web storage area.
setKey :: (HasCallStack, WebDriver wd) => WebStorageType -> Text -> Text -> wd Text
setKey s k v = doStorageCommand methodPost s "" . object $ ["key"   .= k,
                                                      "value" .= v ]
-- |Delete a key in the given web storage area.
deleteKey :: (HasCallStack, WebDriver wd) => WebStorageType -> Text -> wd ()
deleteKey s k = noReturn $ doStorageCommand methodPost s ("/key/" `T.append` urlEncode k) emptyObject

-- |A wrapper around 'doSessCommand' to create web storage requests.
doStorageCommand :: (HasCallStack, WebDriver wd, ToJSON a, FromJSON b) =>
                     Method -> WebStorageType -> Text -> a -> wd b
doStorageCommand m s path a = doSessCommand m (T.concat ["/", s', path]) a
  where s' = case s of
          LocalStorage -> "local_storage"
          SessionStorage -> "session_storage"

-- |Get information from the server as a JSON 'Object'. For more information
-- about this object see
-- <https://github.com/SeleniumHQ/selenium/wiki/JsonWireProtocol#status>
serverStatus :: (HasCallStack, WebDriver wd) => wd Value   -- todo: make this a record type
serverStatus = doCommand methodGet "/status" Null

-- |A record that represents a single log entry.
data LogEntry =
  LogEntry { logTime  :: Integer  -- ^ timestamp for the log entry. The standard
                                  -- does not specify the epoch or the unit of
                                  -- time.
           , logLevel :: LogLevel -- ^ log verbosity level
           , logMsg   :: Text
           }
  deriving (Eq, Ord, Show, Read)


instance FromJSON LogEntry where
  parseJSON (Object o) =
    LogEntry <$> o .: "timestamp"
             <*> o .: "level"
             <*> (fromMaybe "" <$> o .: "message")
  parseJSON v = typeMismatch "LogEntry" v

type LogType = String

-- |Retrieve the log buffer for a given log type. The server-side log buffer is reset after each request.
--
-- Which log types are available is server defined, but the wire protocol lists these as common log types:
-- client, driver, browser, server
getLogs :: (HasCallStack, WebDriver wd) => LogType -> wd [LogEntry]
getLogs t = doSessCommand methodPost "/log" . object $ ["type" .= t]

-- |Get a list of available log types.
getLogTypes :: (HasCallStack, WebDriver wd) => wd [LogType]
getLogTypes = doSessCommand methodGet "/log/types" Null

data ApplicationCacheStatus = Uncached | Idle | Checking | Downloading | UpdateReady | Obsolete deriving (Eq, Enum, Bounded, Ord, Show, Read)

instance FromJSON ApplicationCacheStatus where
    parseJSON val = do
        n <- parseJSON val
        case n :: Integer of
            0 -> return Uncached
            1 -> return Idle
            2 -> return Checking
            3 -> return Downloading
            4 -> return UpdateReady
            5 -> return Obsolete
            err -> fail $ "Invalid JSON for ApplicationCacheStatus: " ++ show err

getApplicationCacheStatus :: (HasCallStack, WebDriver wd) => wd ApplicationCacheStatus
getApplicationCacheStatus = doSessCommand methodGet "/application_cache/status" Null
