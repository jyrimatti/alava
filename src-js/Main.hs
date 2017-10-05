{-# LANGUAGE OverloadedStrings #-}
module Main where

import Foundation (($),(.),null,show,getArgs,putStrLn,fmap,fst,snd,IO,(<>),(<$>),(>>=),(=<<),Bool(True),flip,Either(Left,Right),pure,Maybe(Just,Nothing),return,fromString,toList)
import Foundation.Collection (filter,intercalate,sequence)
import Foundation.IO (readFile)
import Foundation.VFS ()
import Foundation.VFS.FilePath (unsafeFilePath,unsafeFileName,Relativity(Relative))
import Foundation.String (toBytes,Encoding(UTF8),fromBytes)

import qualified Prelude as P
import Control.Monad.IO.Class
import Control.Monad ((<=<))
import Data.Text.Lazy (unpack)
import Data.Foldable (foldMap)

import GHCJS.DOM
import GHCJS.DOM.Types
import GHCJS.DOM.Document (getElementsByClassName,getBodyUnchecked)
import GHCJS.DOM.Element (setInnerHTML)
import GHCJS.DOM.HTMLElement ()
import GHCJS.DOM.Node
import GHCJS.DOM.EventM
import GHCJS.DOM.GlobalEventHandlers (click,blur)
import GHCJS.DOM.HTMLTextAreaElement (setValue)
import GHCJS.DOM.HTMLCollection

import JavaScript.Ajax.Async

import Text.Blaze.XHtml5 hiding (main,head)
import Text.Blaze.XHtml5.Attributes
import Text.Blaze.Html.Renderer.String (renderHtml)

import SimpleParser (expr,parse)
import TypeCheck (inferType)
import Error (Error)
import Syntax (SourcePos)
import qualified HtmlPrint as P (renderHtml)
import Environment (emptyEnv)
import Alava (parse, infer)

main :: IO ()
main = helloMain

helloMain :: JSM ()
helloMain = do
    doc <- currentDocumentUnchecked
    body <- getBodyUnchecked doc

    setInnerHTML body $ renderHtml $ do
      header $ h1 "Alava"  
      section $
        ul $ do
          li $ a ! class_ "basic" $ "Basic syntax"
          li $ a ! class_ "dependent" $ "Dependent types"
      section $ pre $ code ! class_ "code" ! contenteditable "true" $ ""
      section ! class_ "err" $ ""

    code      <- find HTMLElement doc "code"
    err       <- find HTMLElement doc "err"
    basic     <- find HTMLElement doc "basic"
    dependent <- find HTMLElement doc "dependent"

    on basic click $ do
        AjaxResponse _ c <- ajaxGet "../../../../tests/basic_syntax.alava"
        setInnerHTML code c
        preventDefault

    on dependent click $ do
        AjaxResponse _ c <- ajaxGet "../../../../tests/dependent.alava"
        setInnerHTML code c
        preventDefault

    on code blur $ do
        liftIO $ putStrLn "compiling..."
        (Just txt) <- getTextContent code
        liftIO $ putStrLn (fromString txt)
        e <- liftIO $ infer (fromString txt)
        case e of
            Right (t,_) -> setInnerHTML code $ unpack $ P.renderHtml t
            Left errors -> setInnerHTML err $ renderHtml $ foldMap showError errors

    return ()

showError :: (Error, SourcePos) -> Html
showError (err, pos) = dl $ do
                        dt $ toHtml $ show pos
                        dd $ toHtml $ show err

ajaxGet url = liftIO $ liftIO =<< wait <$> sendRequestAsync GET url Nothing Nothing

find :: (IsGObject f) => (JSVal -> f) -> Document -> JSString -> JSM f
find f doc = fmap (uncheckedCastTo f) . headItem <=< getElementsByClassName doc

headItem = flip itemUnchecked 0