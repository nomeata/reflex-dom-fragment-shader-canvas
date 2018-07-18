{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RecursiveDo #-}
import Reflex.Dom
import Reflex.Dom.FragmentShaderCanvas
import qualified Data.Text as T

main :: IO ()
main = mainWidgetWithHead
    (el "style" (text css) >> el "title" (text "Fragment Shader Demo")) $ mdo
        inp <- divClass "left" $ do
            inp <- textArea $ def
               & textAreaConfig_initialValue .~ trivialFragmentShader
            divClass "error" $ dynText (maybe "" id <$> dError)
            return inp

        dError <- divClass "right" $ fragmentShaderCanvas
            (mconcat [ "width"  =: "1000" , "height" =: "1000" ])
            (_textArea_value inp)
        return ()

css :: T.Text
css = T.unlines
    [ "document {"
    , "  margin: 0;"
    , "  padding: 0;"
    , "  width: 100vw;"
    , "  height: 100vh;"
    , "}"
    , "body {"
    , "  margin: 0;"
    , "  padding: 0;"
    , "  display: flex;"
    , "  align-items: stretch;"
    , "  width: 100vw;"
    , "  height: 100vh;"
    , "}"
    , "textarea {"
    , "  width: 50vw;"
    , "  height: 50vh;"
    , "}"
    , ".error {"
    , "  width: 100%;"
    , "  text-align: left;"
    , "  white-space:pre;"
    , "  font-family:mono;"
    , "  padding-top: 2em;"
    , "  overflow: scrolll"
    , "}"
    , ".left {"
    , "  width:50%;"
    , "  padding:2em;"
    , "}"
    , ".right {"
    , "  width:50%;"
    , "  display: flex;"
    , "}"
    , "canvas {"
    , "  border: 1px solid black;"
    , "  max-width: 80%;"
    , "  max-height: 80%;"
    , "  margin:auto;"
    , "}"
    ]
