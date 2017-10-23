{-# LANGUAGE OverloadedStrings #-}
module Main where

import Foundation (($),(.),getArgs,fmap,fst,snd,IO,(<>),(<$>),(>>=),(=<<),Bool(True),flip,Either(Left,Right),pure,Maybe(Just,Nothing),return)
import Foundation.Collection (filter,intercalate,sequence,traverse)
import Foundation.IO (readFile)
import Foundation.VFS.FilePath (unsafeFilePath,unsafeFileName,Relativity(Relative))
--import Foundation.String (toBytes,Encoding(UTF8),fromBytes)

import Prelude (Show)
import qualified Prelude as P (show)

import Control.Monad.IO.Class (liftIO)
import Control.Monad ((<=<))
import Control.Monad.Logger (runNoLoggingT,NoLoggingT)
import Data.Text.Lazy (Text,pack,unpack)
import Data.Text.Lazy.IO (putStrLn)
import Data.Foldable (foldMap)

import GHCJS.DOM (currentDocumentUnchecked)
import GHCJS.DOM.Types (JSM,IsGObject,JSVal,Document,JSString,HTMLElement(..),uncheckedCastTo,Element(..))
import GHCJS.DOM.Document (getElementsByClassName,getBodyUnchecked,getHeadUnchecked)
import GHCJS.DOM.Element (getInnerHTML,setInnerHTML)
import qualified GHCJS.DOM.HTMLElement as E (blur)
import GHCJS.DOM.Node (getTextContent)
import GHCJS.DOM.EventM (on,preventDefault)
import GHCJS.DOM.GlobalEventHandlers (click,blur)
import GHCJS.DOM.HTMLTextAreaElement (setValue)
import GHCJS.DOM.HTMLCollection (itemUnchecked)

import JavaScript.Ajax.Async (AjaxResponse(..),sendRequestAsync,wait,StdMethod(GET))

import Text.Blaze.XHtml5 hiding (main,head,body)
import Text.Blaze.XHtml5.Attributes (href,class_,contenteditable,type_)
import Text.Blaze.Html.Renderer.String (renderHtml)

import TypeCheck (inferType)
import Error (Error)
import Syntax (SourcePos)
import qualified HtmlPrint (html,head,eterm,footer,error)
import Environment (emptyEnv)
import Alava (parse, infer)

show :: Show a => a -> Text
show = pack . P.show

main :: IO ()
main = helloMain

body :: Html
body = do
  div ! class_ "container" $ do
    header ! class_ "header" $
      h1 $ do
        span $ a ! href "https://alava.lahteenmaki.net" $ "Alava"
        a ! href "https://lahteenmaki.net" $ " - lahteenmaki.net"

    section ! class_ "section errors" $ do
      h2 $
        a ! href "#errors" $ "Errors"
      div ! class_ "boxcontent err" $ toHtml noErrorsText
    section ! class_ "section content" $ do
      h2 $
        a ! href "#content" $ "Code"
      div ! class_ "boxcontent" $
        pre $
          code ! class_ "code" ! contenteditable "true" $ introContent
    
    section ! class_ "section menu documentation" $ do
      h2 $
        a ! href "#documentation" $ "Documentation"
      div ! class_ "boxcontent" $
        ul $
          li $ a ! class_ "intro" $ "Introduction"
    section ! class_ "section menu examples" $ do
      h2 $
        a ! href "#examples" $ "Code examples"
      div ! class_ "boxcontent" $
        ul $ do
          li $ a ! class_ "basic" $ "Basic syntax"
          li $ a ! class_ "dependent" $ "Dependent types"
    HtmlPrint.footer

introContent :: Html
introContent = do
  h3 "Why should you make your own programming language?"
  ul $ do
    li "to learn new stuff"
    li "to see the difficulty in implementing certain features"
    li "to understand what’s possible and what’s not"
    li "to find out if a language property (like dependent types) can make some exotic features trivial to implement"

  h3 "What are dependent types?"
  ul $ do
    li "a type can depend on both other types AND values"
    li $ do
      "vs (in pseudo code)"
      ul $ do
        li "head: Vector T -> T"
        li "head: (length: Nat) (length > 0) (Vector length T) -> T"

  h3 "What might dependent types be good for?"
  ul $ do
    li "they make it possible not to have a separate module language and a core language"
    li "they make the language useful also as a theorem prover"
    li "statically safe head/tail functions and the elimination of array bounds check"
    li "..."

  h3 "Alava at the moment:"
  ul $ do
    li $ a ! href "https://github.com/jyrimatti/alava" $ "https://github.com/jyrimatti/alava"
    li $ do
      "based on Pi-Forall (lecture videos and code) from Stephanie Weirich ("
      a ! href "https://github.com/sweirich/pi-forall" $ "https://github.com/sweirich/pi-forall"
      ") but I threw most stuff away to learn how things are done"
    li "dependently typed"
    li "no primitives, sum types, module system, polymorphism, universes, type inference, code generation, runtime, evaluator…"
    li "in the future hopefully the aforementioned as well as subtyping, totality checker, linear types, Hindley-Milner etc."


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
        AjaxResponse _ c <- ajaxGet "basic_syntax.alava"
        setInnerHTML code c
        preventDefault
        E.blur code

    on dependent click $ do
        AjaxResponse _ c <- ajaxGet "dependent.alava"
        setInnerHTML code c
        preventDefault
        E.blur code

    on code blur $ do
        liftIO $ putStrLn "compiling..."
        setInnerHTML err $ unpack noErrorsText
        (Just txt) <- getTextContent code
        liftIO $ putStrLn $ pack txt
        case parse $ pack txt of
          []     -> setInnerHTML err $ unpack "Parse error! Sorry, no more info due to custom parser ;)"
          expr:_ -> do
            e <- runNoLoggingT . infer $ expr
            case e of
                Right (t,_) -> setInnerHTML code $ renderHtml $ HtmlPrint.eterm t
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