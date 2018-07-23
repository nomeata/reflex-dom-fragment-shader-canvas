{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RecursiveDo #-}
import Reflex.Dom
import Reflex.Dom.FragmentShaderCanvas
import qualified Data.Text as T

main :: IO ()
main = mainWidgetWithHead htmlHead $ mdo
    inp <- divClass "left" $ do
        inp <- textArea $ def
           & textAreaConfig_initialValue .~ trivialFragmentShader
        divClass "error" $ dynText (maybe "" id <$> dError)
        return inp

    dError <- divClass "right" $ fragmentShaderCanvas
        -- Here we determine the resolution of the canvas
        -- It would be desireable to do so dynamically, based on the widget
        -- size. But Reflex.Dom.Widget.Resize messes with the CSS layout.
        (mconcat [ "width"  =: "1000" , "height" =: "1000" ])
        (_textArea_value inp)
    return ()
  where
    htmlHead :: DomBuilder t m => m ()
    htmlHead = do
        el "style" (text css)
        el "title" (text "Fragment Shader Demo")

css :: T.Text
css = T.unlines
    [ "html {"
    , "  margin: 0;"
    , "  height: 100%;"
    , "}"
    , "body {"
    , "  display: flex;"
    , "  height: 100%;"
    , "}"
    , ".left {"
    , "  flex:1 1 0;"
    , "  display:flex;"
    , "  flex-direction:column;"
    , "  padding:1em;"
    , "}"
    , ".right {"
    , "  flex:1 1 0;"
    , "}"
    , "textarea {"
    , "  resize:vertical;"
    , "  height:50%;"
    , "}"
    , ".error {"
    , "  margin-top:1em;"
    , "  text-align: left;"
    , "  font-family:mono;"
    , "  width:100%;"
    , "  overflow: auto;"
    , "}"
    , "canvas {"
    , "  height:100%;"
    , "  width:100%;"
    , "  object-fit:contain;"
    , "}"
    ]
