{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, ExistentialQuantification,
             TemplateHaskell, RecordWildCards, FlexibleContexts #-}
-- |This module exports basic WD actions that can be used to interact with a
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
       , attr, cssProp, elemPos, elemSize
       , isSelected, isEnabled, isDisplayed
       , tagName, activeElem, elemInfo
         -- ** Element equality
       , (<==>), (</=>)
         -- * Javascript
       , executeJS, asyncJS
       , JSArg(..)
         -- * Windows
       , WindowHandle(..), currentWindow
       , getCurrentWindow, closeWindow, windows, focusWindow,  maximize
       , getWindowSize, setWindowSize, getWindowPos, setWindowPos
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
import Test.WebDriver.Capabilities
import Test.WebDriver.Class
import Test.WebDriver.Commands.Internal
import Test.WebDriver.Cookies
import Test.WebDriver.Exceptions.Internal
import Test.WebDriver.JSON
import Test.WebDriver.Session
import Test.WebDriver.Utils (urlEncode)
import OpenTelemetry.Eventlog

import Prelude -- hides some "unused import" warnings

-- |Create a new session with the given 'Capabilities'. The returned session becomes the \"current session\" for this action.
--
-- Note: if you're using 'runSession' to run your WebDriver commands, you don't need to call this explicitly.
createSession :: (HasCallStack, WebDriver wd) => Capabilities -> wd WDSession
createSession caps = withSpan_ "WebDriver.createSession" $ do
  let connect = withAuthHeaders $ doCommand methodPost "/session" . single "desiredCapabilities" $ caps
  body <- connect `L.catch` \(_ex :: FailedCommand) -> connect
  s <- getSession
  putSession s { wdSessCreationResponse = Just body }
  getSession

-- |Retrieve a list of active sessions and their 'Capabilities'.
sessions :: (HasCallStack, WebDriver wd) => wd [(SessionId, Capabilities)]
sessions = withSpan_ "WebDriver.sessions" $ do
  objs <- doCommand methodGet "/sessions" Null
  mapM (parsePair "id" "capabilities" "sessions") objs

-- |Get the actual server-side 'Capabilities' of the current session.
getActualCaps :: (HasCallStack, WebDriver wd) => wd Capabilities
getActualCaps = withSpan_ "WebDriver.getActualCaps" $ doSessCommand methodGet "" Null

-- |Get the 'Capabilities' that were sent when the session was creted.
getSessionCaps :: (HasCallStack, WebDriver wd) => wd (Maybe Capabilities)
getSessionCaps = withSpan_ "WebDriver.getSessionCaps" $ do
  caps <- wdSessCreationResponse <$> getSession
  return $ parseMaybe parseJSON =<< caps
  
-- |Close the current session and the browser associated with it.
closeSession :: (HasCallStack, WebDriver wd) => wd ()
closeSession = withSpan_ "WebDriver.closeSession" $ do 
  s@WDSession {} <- getSession
  noReturn $ doSessCommand methodDelete "" Null
  putSession s { wdSessId = Nothing }


-- |Sets the amount of time (ms) we implicitly wait when searching for elements.
setImplicitWait :: (HasCallStack, WebDriver wd) => Integer -> wd ()
setImplicitWait ms = withSpan_ "WebDriver.setImplicitWait" $
  noReturn $ doSessCommand methodPost "/timeouts/implicit_wait" (object msField)
    `L.catch` \(_ :: SomeException) ->
      doSessCommand methodPost "/timeouts" (object allFields)
  where msField   = ["ms" .= ms]
        allFields = ["type" .= ("implicit" :: String)] ++ msField

-- |Sets the amount of time (ms) we wait for an asynchronous script to return a
-- result.
setScriptTimeout :: (HasCallStack, WebDriver wd) => Integer -> wd ()
setScriptTimeout ms = withSpan_ "WebDriver.setScriptTimeout" $
  noReturn $ doSessCommand methodPost "/timeouts/async_script" (object msField)
    `L.catch` \( _ :: SomeException) ->
      doSessCommand methodPost "/timeouts" (object allFields)
  where msField   = ["ms" .= ms]
        allFields = ["type" .= ("script" :: String)] ++ msField

-- |Sets the amount of time (ms) to wait for a page to finish loading before throwing a 'Timeout' exception.
setPageLoadTimeout :: (HasCallStack, WebDriver wd) => Integer -> wd ()
setPageLoadTimeout ms = withSpan_ "WebDriver.setPageLoadTimeout" $
  noReturn $ doSessCommand methodPost "/timeouts" params
  where params = object ["type" .= ("page load" :: String)
                        ,"ms"   .= ms ]

-- |Gets the URL of the current page.
getCurrentURL :: (HasCallStack, WebDriver wd) => wd String
getCurrentURL = withSpan_ "WebDriver.getCurrentURL" $ doSessCommand methodGet "/url" Null

-- |Opens a new page by the given URL.
openPage :: (HasCallStack, WebDriver wd) => String -> wd ()
openPage url
  | isURI url = withSpan_ "WebDriver.openPage" (noReturn . doSessCommand methodPost "/url" . single "url" $ url)
  | otherwise = throwIO . InvalidURL $ url

-- |Navigate forward in the browser history.
forward :: (HasCallStack, WebDriver wd) => wd ()
forward = withSpan_ "WebDriver.forward" (noReturn $ doSessCommand methodPost "/forward" Null)

-- |Navigate backward in the browser history.
back :: (HasCallStack, WebDriver wd) => wd ()
back = withSpan_ "WebDriver.back" (noReturn $ doSessCommand methodPost "/back" Null)

-- |Refresh the current page
refresh :: (HasCallStack, WebDriver wd) => wd ()
refresh = withSpan_ "WebDriver.refresh" (noReturn $ doSessCommand methodPost "/refresh" Null)

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
executeJS a s = withSpan_ "WebDriver.executeJS" (fromJSON' =<< getResult)
  where
    getResult = doSessCommand methodPost "/execute" . pair ("args", "script") $ (F.toList a,s)

{- |Executes a snippet of Javascript code asynchronously. This function works
similarly to 'executeJS', except that the Javascript is passed a callback
function as its final argument. The script should call this function
to signal that it has finished executing, passing to it a value that will be
returned as the result of asyncJS. A result of Nothing indicates that the
Javascript function timed out (see 'setScriptTimeout')
-}
asyncJS :: (HasCallStack, F.Foldable f, FromJSON a, WebDriver wd) => f JSArg -> Text -> wd (Maybe a)
asyncJS a s = withSpan_ "WebDriver.asyncJS" (handle timeout $ Just <$> (fromJSON' =<< getResult))
  where
    getResult = doSessCommand methodPost "/execute_async" . pair ("args", "script")
                $ (F.toList a,s)
    timeout (FailedCommand Timeout _)       = return Nothing
    timeout (FailedCommand ScriptTimeout _) = return Nothing
    timeout err = throwIO err

-- |Save a screenshot to a particular location
saveScreenshot :: (HasCallStack, WebDriver wd) => FilePath -> wd ()
saveScreenshot path = withSpan_ "WebDriver.saveScreenshot" (screenshot >>= liftBase . LBS.writeFile path)

-- |Grab a screenshot of the current page as a PNG image
screenshot :: (HasCallStack, WebDriver wd) => wd LBS.ByteString
screenshot = withSpan_ "WebDriver.screenshot" (B64.decodeLenient <$> screenshotBase64)

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
deactivateIME = noReturn $ doSessCommand methodPost "/ime/deactivate" Null


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
focusFrame s = withSpan_ "WebDriver.focusFram" (noReturn $ doSessCommand methodPost "/frame" . single "id" $ s)

-- |Returns a handle to the currently focused window
getCurrentWindow :: (HasCallStack, WebDriver wd) => wd WindowHandle
getCurrentWindow = withSpan_ "WebDriver.getCurrentWindow" (doSessCommand methodGet "/window_handle" Null)

-- |Returns a list of all windows available to the session
windows :: (HasCallStack, WebDriver wd) => wd [WindowHandle]
windows = withSpan_ "WebDriver.windows" (doSessCommand methodGet "/window_handles" Null)

focusWindow :: (HasCallStack, WebDriver wd) => WindowHandle -> wd ()
focusWindow w = withSpan_ "WebDriver.focusWindow" (noReturn $ doSessCommand methodPost "/window" . single "name" $ w)

-- |Closes the given window
closeWindow :: (HasCallStack, WebDriver wd) => WindowHandle -> wd ()
closeWindow w = withSpan_ "WebDriver.closeWindow" $ do
  cw <- getCurrentWindow
  focusWindow w
  ignoreReturn $ doSessCommand methodDelete "/window" Null
  unless (w == cw) $ focusWindow cw

-- |Maximizes the current  window if not already maximized
maximize :: (HasCallStack, WebDriver wd) => wd ()
maximize = withSpan_ "WebDriver.maximize" (ignoreReturn $ doWinCommand methodPost currentWindow "/maximize" Null)

-- |Get the dimensions of the current window.
getWindowSize :: (HasCallStack, WebDriver wd) => wd (Word, Word)
getWindowSize = withSpan_ "WebDriver.getWindowSize"
  (doWinCommand methodGet currentWindow "/size" Null
  >>= parsePair "width" "height" "getWindowSize")

-- |Set the dimensions of the current window.
setWindowSize :: (HasCallStack, WebDriver wd) => (Word, Word) -> wd ()
setWindowSize = withSpan_ "WebDriver.setWindowSize" .
  ignoreReturn . doWinCommand methodPost currentWindow "/size"
  . pair ("width", "height")

-- |Get the coordinates of the current window.
getWindowPos :: (HasCallStack, WebDriver wd) => wd (Int, Int)
getWindowPos = withSpan_ "WebDriver.getWindowPos"
  (doWinCommand methodGet currentWindow "/position" Null
  >>= parsePair "x" "y" "getWindowPos")

-- |Set the coordinates of the current window.
setWindowPos :: (HasCallStack, WebDriver wd) => (Int, Int) -> wd ()
setWindowPos = withSpan_ "WebDriver.setWindowPos" . ignoreReturn . doWinCommand methodPost currentWindow "/position" . pair ("x","y")

-- |Retrieve all cookies visible to the current page.
cookies :: (HasCallStack, WebDriver wd) => wd [Cookie]
cookies = withSpan_ "WebDriver.cookies" $ doSessCommand methodGet "/cookie" Null

-- |Set a cookie. If the cookie path is not specified, it will default to \"/\".
-- Likewise, if the domain is omitted, it will default to the current page's
-- domain
setCookie :: (HasCallStack, WebDriver wd) => Cookie -> wd ()
setCookie = withSpan_ "WebDriver.setCookie" . noReturn . doSessCommand methodPost "/cookie" . single "cookie"

-- |Delete a cookie. This will do nothing is the cookie isn't visible to the
-- current page.
deleteCookie :: (HasCallStack, WebDriver wd) => Cookie -> wd ()
deleteCookie c = withSpan_ "WebDriver.deleteCookie" (noReturn $ doSessCommand methodDelete ("/cookie/" `append` urlEncode (cookName c)) Null)

deleteCookieByName :: (HasCallStack, WebDriver wd) => Text -> wd ()
deleteCookieByName n = withSpan_ "WebDriver.deleteCookieByName" (noReturn $ doSessCommand methodDelete ("/cookie/" `append` n) Null)

-- |Delete all visible cookies on the current page.
deleteVisibleCookies :: (HasCallStack, WebDriver wd) => wd ()
deleteVisibleCookies = withSpan_ "WebDriver.deleteVisibleCookies" (noReturn $ doSessCommand methodDelete "/cookie" Null)

-- |Get the current page source
getSource :: (HasCallStack, WebDriver wd) => wd Text
getSource = withSpan_ "WebDriver.getSource" (doSessCommand methodGet "/source" Null)

-- |Get the title of the current page.
getTitle :: (HasCallStack, WebDriver wd) => wd Text
getTitle = withSpan_ "WebDriver.getTitle" (doSessCommand methodGet "/title" Null)

-- |Specifies element(s) within a DOM tree using various selection methods.
data Selector = ById Text
              | ByName Text
              | ByClass Text -- ^ (Note: multiple classes are not
                             -- allowed. For more control, use 'ByCSS')
              | ByTag Text
              | ByLinkText Text
              | ByPartialLinkText Text
              | ByCSS Text
              | ByXPath Text
              deriving (Eq, Show, Ord)

instance ToJSON Selector where
  toJSON s = case s of
    ById t              -> selector "id" t
    ByName t            -> selector "name" t
    ByClass t           -> selector "class name" t
    ByTag t             -> selector "tag name" t
    ByLinkText t        -> selector "link text" t
    ByPartialLinkText t -> selector "partial link text" t
    ByCSS t             -> selector "css selector" t
    ByXPath t           -> selector "xpath" (T.replace "[1" "[0+1" t)
    where
      selector :: Text -> Text -> Value
      selector sn t = object ["using" .= sn, "value" .= t]

-- |Find an element on the page using the given element selector.
findElem :: (HasCallStack, WebDriver wd) => Selector -> wd Element
findElem = withSpan_ "WebDriver.findElem" . doSessCommand methodPost "/element"

-- |Find all elements on the page matching the given selector.
findElems :: (HasCallStack, WebDriver wd) => Selector -> wd [Element]
findElems = withSpan_ "WebDriver.findElems" . doSessCommand methodPost "/elements"

-- |Return the element that currently has focus.
activeElem :: (HasCallStack, WebDriver wd) => wd Element
activeElem = withSpan_ "WebDriver.activeElem" $ doSessCommand methodPost "/element/active" Null

-- |Search for an element using the given element as root.
findElemFrom :: (HasCallStack, WebDriver wd) => Element -> Selector -> wd Element
findElemFrom e s 
  | isRelative s = withSpan_ "WebDriver.findElemFrom" $ doElemCommand methodPost e "/element" s
  | otherwise = fail "Selector in findElemFrom must be relative"

-- |Find all elements matching a selector, using the given element as root.
findElemsFrom :: (HasCallStack, WebDriver wd) => Element -> Selector -> wd [Element]
findElemsFrom e s 
  | isRelative s = withSpan_ "WebDriver.findElemsFrom" $ doElemCommand methodPost e "/elements" s
  | otherwise = fail "Selector in findElemsFrom must be relative"

isRelative :: Selector -> Bool
isRelative (ByXPath t) = not $ "/" `T.isPrefixOf` t
isRelative _ = True

-- |Describe the element. Returns a JSON object whose meaning is currently
-- undefined by the WebDriver protocol.
elemInfo :: (HasCallStack, WebDriver wd) => Element -> wd Value
elemInfo e = doElemCommand methodGet e "" Null
{-# DEPRECATED elemInfo "This command does not work with Marionette (Firefox) driver, and is likely to be completely removed in Selenium 4" #-}

-- |Click on an element.
click :: (HasCallStack, WebDriver wd) => Element -> wd ()
click e = withSpan_ "WebDriver.click" (noReturn $ doElemCommand methodPost e "/click" Null)

-- |Submit a form element. This may be applied to descendents of a form element
-- as well.
submit :: (HasCallStack, WebDriver wd) => Element -> wd ()
submit e = withSpan_ "WebDriver.submit" (noReturn $ doElemCommand methodPost e "/submit" Null)

-- |Get all visible text within this element.
getText :: (HasCallStack, WebDriver wd) => Element -> wd Text
getText e = withSpan_ "WebDriver.getText" (T.strip <$> doElemCommand methodGet e "/text" Null)

-- |Send a sequence of keystrokes to an element. All modifier keys are released
-- at the end of the function. Named constants for special modifier keys can be found
-- in "Test.WebDriver.Common.Keys"
sendKeys :: (HasCallStack, WebDriver wd) => Text -> Element -> wd ()
sendKeys t e = withSpan_ "WebDriver.sendKeys" (noReturn . doElemCommand methodPost e "/value" . single "value" $ [t])

-- |Similar to sendKeys, but doesn't implicitly release modifier keys
-- afterwards. This allows you to combine modifiers with mouse clicks.
sendRawKeys :: (HasCallStack, WebDriver wd) => Text -> wd ()
sendRawKeys t = withSpan_ "WebDriver.sendRawKeys" (noReturn . doSessCommand methodPost "/keys" . single "value" $ [t])

-- |Return the tag name of the given element.
tagName :: (HasCallStack, WebDriver wd) => Element -> wd Text
tagName e = withSpan_ "WebDriver.tagName" (doElemCommand methodGet e "/name" Null)

-- |Clear a textarea or text input element's value.
clearInput :: (HasCallStack, WebDriver wd) => Element -> wd ()
clearInput e = withSpan_ "WebDriver.clearInput" (noReturn $ doElemCommand methodPost e "/clear" Null)

-- |Determine if the element is selected.
isSelected :: (HasCallStack, WebDriver wd) => Element -> wd Bool
isSelected e = withSpan_ "WebDriver.isSelected" (doElemCommand methodGet e "/selected" Null)

-- |Determine if the element is enabled.
isEnabled :: (HasCallStack, WebDriver wd) => Element -> wd Bool
isEnabled e = withSpan_ "WebDriver.isEnabled" (doElemCommand methodGet e "/enabled" Null)

-- |Determine if the element is displayed.
isDisplayed :: (HasCallStack, WebDriver wd) => Element -> wd Bool
isDisplayed e = withSpan_ "WebDriver.isDisplayed" (doElemCommand methodGet e "/displayed" Null)

-- |Retrieve the value of an element's attribute
attr :: (HasCallStack, WebDriver wd) => Element -> Text -> wd (Maybe Text)
attr e t = withSpan_ "WebDriver.attr" (doElemCommand methodGet e ("/attribute/" `append` urlEncode t) Null)

-- |Retrieve the value of an element's computed CSS property
cssProp :: (HasCallStack, WebDriver wd) => Element -> Text -> wd (Maybe Text)
cssProp e t = withSpan_ "WebDriver.cssProp" (doElemCommand methodGet e ("/css/" `append` urlEncode t) Null)

-- |Retrieve an element's current position.
elemPos :: (HasCallStack, WebDriver wd) => Element -> wd (Float, Float)
elemPos e = withSpan_ "WebDriver.elemPos" (doElemCommand methodGet e "/location" Null >>= parsePair "x" "y" "elemPos")

-- |Retrieve an element's current size.
elemSize :: (HasCallStack, WebDriver wd) => Element -> wd (Float, Float)
elemSize e = withSpan_ "WebDriver.elemSize"
  (doElemCommand methodGet e "/size" Null
  >>= parsePair "width" "height" "elemSize")

infix 4 <==>
-- |Determines if two element identifiers refer to the same element.
(<==>) :: (HasCallStack, WebDriver wd) => Element -> Element -> wd Bool
e1 <==> (Element e2) = withSpan_ "WebDriver.(<==>)" (doElemCommand methodGet e1 ("/equals/" `append` urlEncode e2) Null)

-- |Determines if two element identifiers refer to different elements.
infix 4 </=>
(</=>) :: (HasCallStack, WebDriver wd) => Element -> Element -> wd Bool
e1 </=> e2 = not <$> (e1 <==> e2)

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
getOrientation = withSpan_ "WebDriver.getOrientiation" $ doSessCommand methodGet "/orientation" Null

-- |Set the current screen orientation for rotatable display devices.
setOrientation :: (HasCallStack, WebDriver wd) => Orientation -> wd ()
setOrientation = withSpan_ "WebDriver.setOrientation" . noReturn . doSessCommand methodPost "/orientation" . single "orientation"

-- |Get the text of an alert dialog.
getAlertText :: (HasCallStack, WebDriver wd) => wd Text
getAlertText = withSpan_ "WebDriver.getAlertText" (doSessCommand methodGet "/alert_text" Null)

-- |Sends keystrokes to Javascript prompt() dialog.
replyToAlert :: (HasCallStack, WebDriver wd) => Text -> wd ()
replyToAlert = withSpan_ "WebDriver.replyToAlert" . noReturn . doSessCommand methodPost "/alert_text" . single "text"

-- |Accepts the currently displayed alert dialog.
acceptAlert :: (HasCallStack, WebDriver wd) => wd ()
acceptAlert = withSpan_ "WebDriver.acceptAlert" (noReturn $ doSessCommand methodPost "/accept_alert" Null)

-- |Dismisses the currently displayed alert dialog.
dismissAlert :: (HasCallStack, WebDriver wd) => wd ()
dismissAlert = withSpan_ "WebDriver.dismissAlert" (noReturn $ doSessCommand methodPost "/dismiss_alert" Null)

-- |Moves the mouse to the given position relative to the active element.
moveTo :: (HasCallStack, WebDriver wd) => (Int, Int) -> wd ()
moveTo = withSpan_ "WebDriver.moveTo" . noReturn . doSessCommand methodPost "/moveto" . pair ("xoffset","yoffset")

-- |Moves the mouse to the center of a given element.
moveToCenter :: (HasCallStack, WebDriver wd) => Element -> wd ()
moveToCenter (Element e) = withSpan_ "WebDriver.moveToCenter" $
  noReturn . doSessCommand methodPost "/moveto" . single "element" $ e

-- |Moves the mouse to the given position relative to the given element.
moveToFrom :: (HasCallStack, WebDriver wd) => (Int, Int) -> Element -> wd ()
moveToFrom (x,y) (Element e) = withSpan_ "WebDriver.moveToFrom" $
  noReturn . doSessCommand methodPost "/moveto"
  . triple ("element","xoffset","yoffset") $ (e,x,y)

-- |A mouse button
data MouseButton = LeftButton | MiddleButton | RightButton
                 deriving (Eq, Show, Ord, Bounded, Enum)

instance ToJSON MouseButton where
  toJSON = toJSON . fromEnum

instance FromJSON MouseButton where
  parseJSON v = do
    n <- parseJSON v
    case n :: Integer of
      0 -> return LeftButton
      1 -> return MiddleButton
      2 -> return RightButton
      err -> fail $ "Invalid JSON for MouseButton: " ++ show err

-- |Click at the current mouse position with the given mouse button.
clickWith :: (HasCallStack, WebDriver wd) => MouseButton -> wd ()
clickWith = withSpan_ "WebDriver.clickWith" . noReturn . doSessCommand methodPost "/click" . single "button"

-- |Perform the given action with the left mouse button held down. The mouse
-- is automatically released afterwards.
withMouseDown :: (HasCallStack, WebDriver wd) => wd a -> wd a
withMouseDown wd = mouseDown >> wd <* mouseUp

-- |Press and hold the left mouse button down. Note that undefined behavior
-- occurs if the next mouse command is not mouseUp.
mouseDown :: (HasCallStack, WebDriver wd) => wd ()
mouseDown = withSpan_ "WebDriver.mouseDown" $ noReturn $ doSessCommand methodPost "/buttondown" Null

-- |Release the left mouse button.
mouseUp :: (HasCallStack, WebDriver wd) => wd ()
mouseUp = withSpan_ "WebDriver.mouseUp" $ noReturn $ doSessCommand methodPost "/buttonup" Null

-- |Double click at the current mouse location.
doubleClick :: (HasCallStack, WebDriver wd) => wd ()
doubleClick = withSpan_ "WebDriver.doubleClick" $ noReturn $ doSessCommand methodPost "/doubleclick" Null

-- |Single tap on the touch screen at the given element's location.
touchClick :: (HasCallStack, WebDriver wd) => Element -> wd ()
touchClick (Element e) = withSpan_ "WebDriver.touchClick" $
  noReturn . doSessCommand methodPost "/touch/click" . single "element" $ e

-- |Emulates pressing a finger down on the screen at the given location.
touchDown :: (HasCallStack, WebDriver wd) => (Int, Int) -> wd ()
touchDown = withSpan_ "WebDriver.touchDown" . noReturn . doSessCommand methodPost "/touch/down" . pair ("x","y")

-- |Emulates removing a finger from the screen at the given location.
touchUp :: (HasCallStack, WebDriver wd) => (Int, Int) -> wd ()
touchUp = withSpan_ "WebDriver.touchUp" . noReturn . doSessCommand methodPost "/touch/up" . pair ("x","y")

-- |Emulates moving a finger on the screen to the given location.
touchMove :: (HasCallStack, WebDriver wd) => (Int, Int) -> wd ()
touchMove = withSpan_ "WebDriver.touchMove" . noReturn . doSessCommand methodPost "/touch/move" . pair ("x","y")

-- |Emulate finger-based touch scroll. Use this function if you don't care where
-- the scroll begins
touchScroll :: (HasCallStack, WebDriver wd) => (Int, Int) -> wd ()
touchScroll = withSpan_ "WebDriver.touchScroll" . noReturn . doSessCommand methodPost "/touch/scroll" . pair ("xoffset","yoffset")

-- |Emulate finger-based touch scroll, starting from the given location relative
-- to the given element.
touchScrollFrom :: (HasCallStack, WebDriver wd) => (Int, Int) -> Element -> wd ()
touchScrollFrom (x, y) (Element e) = withSpan_ "WebDriver.touchScrollFrom" .
  noReturn
  . doSessCommand methodPost "/touch/scroll"
  . triple ("xoffset", "yoffset", "element")
  $ (x, y, e)

-- |Emulate a double click on a touch device.
touchDoubleClick :: (HasCallStack, WebDriver wd) => Element -> wd ()
touchDoubleClick (Element e) = withSpan_ "WebDriver.touchDoubleClick" .
  noReturn
  . doSessCommand methodPost "/touch/doubleclick"
  . single "element" $ e

-- |Emulate a long click on a touch device.
touchLongClick :: (HasCallStack, WebDriver wd) => Element -> wd ()
touchLongClick (Element e) = withSpan_ "WebDriver.touchLongClick" .
  noReturn
  . doSessCommand methodPost "/touch/longclick"
  . single "element" $ e
-- |Emulate a flick on the touch screen. The coordinates indicate x and y
-- velocity, respectively. Use this function if you don't care where the
-- flick starts.
touchFlick :: (HasCallStack, WebDriver wd) => (Int, Int) -> wd ()
touchFlick = withSpan_ "WebDriver.touchFlick" .
  noReturn
  . doSessCommand methodPost "/touch/flick"
  . pair ("xSpeed", "ySpeed")

-- |Emulate a flick on the touch screen.
touchFlickFrom :: (HasCallStack, WebDriver wd) =>
                  Int           -- ^ flick velocity
                  -> (Int, Int) -- ^ a location relative to the given element
                  -> Element    -- ^ the given element
                  -> wd ()
touchFlickFrom s (x,y) (Element e) = withSpan_ "WebDriver.touchFlickFrom" .
  noReturn
  . doSessCommand methodPost "/touch/flick" . object $
  ["xoffset" .= x
  ,"yoffset" .= y
  ,"speed"   .= s
  ,"element" .= e
  ]

-- |Get the current geographical location of the device.
getLocation :: (HasCallStack, WebDriver wd) => wd (Int, Int, Int)
getLocation = withSpan_ "WebDriver.getLocation"
  (doSessCommand methodGet "/location" Null
  >>= parseTriple "latitude" "longitude" "altitude" "getLocation")

-- |Set the current geographical location of the device.
setLocation :: (HasCallStack, WebDriver wd) => (Int, Int, Int) -> wd ()
setLocation = withSpan_ "WebDriver.setLocation" . noReturn . doSessCommand methodPost "/location"
              . triple ("latitude",
                        "longitude",
                        "altitude")

-- |Uploads a file from the local filesystem by its file path.
uploadFile :: (HasCallStack, WebDriver wd) => FilePath -> wd Text
uploadFile path = withSpan_ "WebDriver.uploadFile" (uploadZipEntry =<< liftBase (readEntry [] path))

-- |Uploads a raw bytestring with associated file info.
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
uploadZipEntry :: (HasCallStack, WebDriver wd) => Entry -> wd Text
uploadZipEntry = withSpan_ "WebDriver.uploadZipEntry" . doSessCommand methodPost "/file" . single "file"
                 . TL.decodeUtf8 . B64.encode . fromArchive . (`addEntryToArchive` emptyArchive)


-- |Get the current number of keys in a web storage area.
storageSize :: (HasCallStack, WebDriver wd) => WebStorageType -> wd Integer
storageSize s = withSpan_ "WebDriver.storageSize" $ doStorageCommand methodGet s "/size" Null

-- |Get a list of all keys from a web storage area.
getAllKeys :: (HasCallStack, WebDriver wd) => WebStorageType -> wd [Text]
getAllKeys s = withSpan_ "WebDriver.getAllKeys" $ doStorageCommand methodGet s "" Null

-- |Delete all keys within a given web storage area.
deleteAllKeys :: (HasCallStack, WebDriver wd) => WebStorageType -> wd ()
deleteAllKeys s = withSpan_ "WebDriver.deleteAllKeys" $ noReturn $ doStorageCommand methodDelete s "" Null

-- |An HTML 5 storage type
data WebStorageType = LocalStorage | SessionStorage
                    deriving (Eq, Show, Ord, Bounded, Enum)

-- |Get the value associated with a key in the given web storage area.
-- Unset keys result in empty strings, since the Web Storage spec
-- makes no distinction between the empty string and an undefined value.
getKey :: (HasCallStack, WebDriver wd) => WebStorageType -> Text ->  wd Text
getKey s k = withSpan_ "WebDriver.getKey" $ doStorageCommand methodGet s ("/key/" `T.append` urlEncode k) Null

-- |Set a key in the given web storage area.
setKey :: (HasCallStack, WebDriver wd) => WebStorageType -> Text -> Text -> wd Text
setKey s k v = withSpan_ "WebDriver.setKey" $ doStorageCommand methodPost s "" . object $ ["key"   .= k,
                                                      "value" .= v ]
-- |Delete a key in the given web storage area.
deleteKey :: (HasCallStack, WebDriver wd) => WebStorageType -> Text -> wd ()
deleteKey s k = withSpan_ "WebDriver.deleteKey" $ noReturn $ doStorageCommand methodPost s ("/key/" `T.append` urlEncode k) Null

-- |A wrapper around 'doSessCommand' to create web storage requests.
doStorageCommand :: (WebDriver wd, ToJSON a, FromJSON b) =>
                     Method -> WebStorageType -> Text -> a -> wd b
doStorageCommand m s path a = doSessCommand m (T.concat ["/", s', path]) a
  where s' = case s of
          LocalStorage -> "local_storage"
          SessionStorage -> "session_storage"

-- |Get information from the server as a JSON 'Object'. For more information
-- about this object see
-- <https://github.com/SeleniumHQ/selenium/wiki/JsonWireProtocol#status>
serverStatus :: (WebDriver wd) => wd Value   -- todo: make this a record type
serverStatus = withSpan_ "WebDriver.serverStatus" $ doCommand methodGet "/status" Null

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
getLogs t = withSpan_ "WebDriver.getLogs" $ doSessCommand methodPost "/log" . object $ ["type" .= t]

-- |Get a list of available log types.
getLogTypes :: (HasCallStack, WebDriver wd) => wd [LogType]
getLogTypes = withSpan_ "WebDriver.getLogTypes" $ doSessCommand methodGet "/log/types" Null

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

getApplicationCacheStatus :: (WebDriver wd) => wd ApplicationCacheStatus
getApplicationCacheStatus = withSpan_ "WebDriver.getApplicationCacheStatus" $ doSessCommand methodGet "/application_cache/status" Null
