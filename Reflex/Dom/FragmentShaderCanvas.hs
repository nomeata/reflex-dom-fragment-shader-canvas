{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Reflex.Dom.FragmentShaderCanvas (fragmentShaderCanvas, trivialFragmentShader) where

import Data.Map (Map)
import Data.Text as Text (Text, unlines)
import Control.Lens ((^.))
import Control.Monad.IO.Class

import Reflex.Dom

import GHCJS.DOM.Types hiding (Text)
import Language.Javascript.JSaddle.String
import Language.Javascript.JSaddle.Object (js1, js2, jsf, js, js0, new, jsg)

vertexShaderSource :: Text
vertexShaderSource =
  "attribute vec2 a_position;\
  \void main() {\
  \  gl_Position = vec4(a_position, 0, 1);\
  \}"

-- | An example fragment shader program, drawing a red circle
trivialFragmentShader :: Text
trivialFragmentShader = Text.unlines
  [ "precision mediump float;"
  , "uniform vec2 u_windowSize;"
  , "void main() {"
  , "  float s = 2.0 / min(u_windowSize.x, u_windowSize.y);"
  , "  vec2 pos = s * (gl_FragCoord.xy - 0.5 * u_windowSize);"
  , "  // pos is a scaled pixel position, (0,0) is in the center of the canvas"
  , "  // If the position is outside the inscriped circle, paint black"
  , "  if (length(pos) > 1.0) { gl_FragColor = vec4(0,0,0,1); return; }"
  , "  // Otherwise, return red"
  , "  gl_FragColor = vec4(1.0,0.0,0.0,1.0);"
  , "}"
  ]


paintGL :: (MonadJSM m) => JSVal -> (Maybe Text -> m ()) -> Text -> m ()
paintGL canvas printErr fragmentShaderSource = do
  -- adaption of
  -- https://blog.mayflower.de/4584-Playing-around-with-pixel-shaders-in-WebGL.html
  gl <- liftJSM $ canvas ^. js1 ("getContext"::Text) ("experimental-webgl"::Text)
  liftJSM $ gl ^. jsf ("viewport"::Text) (0::Int, 0::Int, gl ^. js ("drawingBufferWidth"::Text), gl ^. js ("drawingBufferHeight"::Text))

  -- gl ^. jsf "clearColor" [1.0, 0.0, 0.0, 1.0 :: Double]
  -- gl ^. js1 "clear" (gl^. js "COLOR_BUFFER_BIT")

  buffer <- liftJSM $ gl ^. js0 ("createBuffer"::Text)
  liftJSM $ gl ^. jsf ("bindBuffer"::Text) (gl ^. js ("ARRAY_BUFFER"::Text), buffer)
  liftJSM $ gl ^. jsf ("bufferData"::Text)
    ( gl ^. js ("ARRAY_BUFFER"::Text)
    , new (jsg ("Float32Array"::Text)) [[
      -1.0, -1.0,
       1.0, -1.0,
      -1.0,  1.0,
      -1.0,  1.0,
       1.0, -1.0,
       1.0,  1.0 :: Double]]
    ,  gl ^. js ("STATIC_DRAW"::Text)
    )
  -- jsg "console" ^. js1 "log" (gl ^. js0 "getError")

  vertexShader <- liftJSM $ gl ^. js1 ("createShader"::Text) (gl ^. js ("VERTEX_SHADER"::Text))
  liftJSM $ gl ^. js2 ("shaderSource"::Text) vertexShader vertexShaderSource
  liftJSM $ gl ^. js1 ("compileShader"::Text) vertexShader
  -- jsg "console" ^. js1 "log" (gl ^. js1 "getShaderInfoLog" vertexShader)

  fragmentShader <- liftJSM $ gl ^. js1 ("createShader"::Text) (gl ^. js ("FRAGMENT_SHADER"::Text))
  liftJSM $ gl ^. js2 ("shaderSource"::Text) fragmentShader fragmentShaderSource
  liftJSM $ gl ^. js1 ("compileShader"::Text) fragmentShader
  -- jsg "console" ^. js1 "log" (gl ^. js1 "getShaderInfoLog" fragmentShader)
  err <- liftJSM $ gl ^. js1 ("getShaderInfoLog"::Text) fragmentShader
  -- liftJSM $ jsg ("console"::Text) ^. js1 ("log"::Text) err
  printErr . fmap textFromJSString =<< liftJSM (fromJSVal err)

  program <- liftJSM $ gl ^. js0 ("createProgram"::Text)
  liftJSM $ gl ^. js2 ("attachShader"::Text) program vertexShader
  liftJSM $ gl ^. js2 ("attachShader"::Text) program fragmentShader
  liftJSM $ gl ^. js1 ("linkProgram"::Text) program
  liftJSM $ gl ^. js1 ("useProgram"::Text) program
  -- jsg "console" ^. js1 "log" (gl ^. js1 "getProgramInfoLog" program)

  positionLocation <- liftJSM $ gl ^. js2 ("getAttribLocation"::Text) program ("a_position"::Text)
  liftJSM $ gl ^. js1 ("enableVertexAttribArray"::Text) positionLocation
  liftJSM $ gl ^. jsf ("vertexAttribPointer"::Text) (positionLocation, 2::Int, gl ^. js ("FLOAT"::Text), False, 0::Int, 0::Int)
  liftJSM $ jsg ("console"::Text) ^. js1 ("log"::Text) program

  windowSizeLocation <- liftJSM $ gl ^. js2 ("getUniformLocation"::Text) program ("u_windowSize"::Text)
  liftJSM $ gl ^. jsf ("uniform2f"::Text) (windowSizeLocation, gl ^. js ("drawingBufferWidth"::Text), gl ^. js ("drawingBufferHeight"::Text))

  liftJSM $ gl ^. jsf ("drawArrays"::Text) (gl ^. js ("TRIANGLES"::Text), 0::Int, 6::Int);
  return ()

fragmentShaderCanvas ::
    (MonadWidget t m) =>
    (Map Text Text) ->
    Dynamic t Text ->
    m (Dynamic t (Maybe Text))
fragmentShaderCanvas attrs fragmentShaderSource = do
  (canvasEl, _) <- elAttr' "canvas" attrs $ blank
  (eError, reportError) <- newTriggerEvent
  pb <- getPostBuild
  performEvent $ (<$ pb) $ do
    e <- liftJSM $ fromJSValUnchecked =<< toJSVal (_element_raw canvasEl)
    src0 <- sample (current fragmentShaderSource)
    paintGL e (liftIO . reportError) src0
  performEvent $ (<$> updated fragmentShaderSource) $ \src -> do
    e <- liftJSM $ fromJSValUnchecked =<< toJSVal (_element_raw canvasEl)
    paintGL e (liftIO . reportError) src
  holdDyn Nothing eError
