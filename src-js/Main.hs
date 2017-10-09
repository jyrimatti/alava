{-# LANGUAGE OverloadedStrings #-}
module Main where

import Foundation (($),(.),getArgs,fmap,fst,snd,IO,(<>),(<$>),(>>=),(=<<),Bool(True),flip,Either(Left,Right),pure,Maybe(Just,Nothing),return)
import Foundation.Collection (filter,intercalate,sequence,traverse)
import Foundation.IO (readFile)
import Foundation.VFS ()
import Foundation.VFS.FilePath (unsafeFilePath,unsafeFileName,Relativity(Relative))
--import Foundation.String (toBytes,Encoding(UTF8),fromBytes)

import Prelude (Show)
import qualified Prelude as P (show)

import Control.Monad.IO.Class
import Control.Monad ((<=<))
import Control.Monad.Logger (runNoLoggingT,NoLoggingT)
import Data.Text.Lazy (Text,pack,unpack)
import Data.Text.Lazy.IO (putStrLn)
import Data.Foldable (foldMap)

import GHCJS.DOM
import GHCJS.DOM.Types (JSM,IsGObject,JSVal,Document,JSString,HTMLElement(..),uncheckedCastTo,Element(..))
import GHCJS.DOM.Document (getElementsByClassName,getBodyUnchecked,getHeadUnchecked)
import GHCJS.DOM.Element (getInnerHTML,setInnerHTML)
import qualified GHCJS.DOM.HTMLElement as E (blur)
import GHCJS.DOM.Node
import GHCJS.DOM.EventM 
import GHCJS.DOM.GlobalEventHandlers (click,blur)
import GHCJS.DOM.HTMLTextAreaElement (setValue)
import GHCJS.DOM.HTMLCollection

import JavaScript.Ajax.Async

import Text.Blaze.XHtml5 hiding (main,head,body)
import Text.Blaze.XHtml5.Attributes
import Text.Blaze.Html.Renderer.String (renderHtml)

import SimpleParser (expr,parse)
import TypeCheck (inferType)
import Error (Error)
import Syntax (SourcePos)
import qualified HtmlPrint (html,head,term,footer,error)
import Environment (emptyEnv)
import Alava (parse, infer)

show :: Show a => a -> Text
show = pack . P.show

main :: IO ()
main = helloMain

body :: Html
body = 
  div ! class_ "container" $ do
    header ! class_ "header" $
      h1 "Alava"

    section ! class_ "section" $ do
      h2 "Errors"
      div ! class_ "boxcontent err" $ toHtml noErrorsText
    section ! class_ "section" $
      div ! class_ "boxcontent" $
        pre $
          code ! class_ "code" ! contenteditable "true" $ introContent
    
    section ! class_ "section menu" $ do
      h2 "Documentation"
      div ! class_ "boxcontent" $
        ul $
          li $ a ! class_ "intro" $ "Introduction"
    section ! class_ "section menu" $ do
      h2 "Code examples" 
      div ! class_ "boxcontent" $
        ul $ do
          li $ a ! class_ "basic" $ "Basic syntax"
          li $ a ! class_ "dependent" $ "Dependent types"
    HtmlPrint.footer

introContent :: Html
introContent = div $ "moi"

helloMain :: JSM ()
helloMain = do
    doc <- currentDocumentUnchecked
    head <- getHeadUnchecked doc
    bdy <- getBodyUnchecked doc

    h <- getInnerHTML head
    setInnerHTML head $ h <> renderHtml HtmlPrint.head
    setInnerHTML bdy $ renderHtml body

    [code,err,intro,basic,dependent] <- traverse (find HTMLElement doc) ["code", "err", "intro", "basic", "dependent"]

    on intro click $ do
        setInnerHTML code $ renderHtml introContent
        preventDefault

    on basic click $ do
        AjaxResponse _ c <- ajaxGet "../../../../tests/basic_syntax.alava"
        setInnerHTML code c
        preventDefault
        E.blur code

    on dependent click $ do
        AjaxResponse _ c <- ajaxGet "../../../../tests/dependent.alava"
        setInnerHTML code c
        preventDefault
        E.blur code

    on code blur $ do
        liftIO $ putStrLn "compiling..."
        setInnerHTML err $ unpack noErrorsText
        (Just txt) <- getTextContent code
        liftIO $ putStrLn $ pack txt
        e <- runNoLoggingT . infer $ pack txt
        case e of
            Right (t,_) -> setInnerHTML code $ renderHtml $ HtmlPrint.term t
            Left errors -> setInnerHTML err $ renderHtml $ foldMap showError errors

    return ()

noErrorsText :: Text
noErrorsText = "No errors. Compilation will happen \"on blur\""

showError :: (Error, SourcePos) -> Html
showError (err, pos) = dl $ do
                        dt $ toHtml $ show pos
                        dd $ HtmlPrint.error err

ajaxGet url = liftIO $ liftIO =<< wait <$> sendRequestAsync GET url Nothing Nothing

find :: (IsGObject f) => (JSVal -> f) -> Document -> JSString -> JSM f
find f doc = fmap (uncheckedCastTo f) . headItem <=< getElementsByClassName doc

headItem = flip itemUnchecked 0