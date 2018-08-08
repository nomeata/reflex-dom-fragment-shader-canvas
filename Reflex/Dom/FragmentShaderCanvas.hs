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
import GHCJS.DOM.HTMLCanvasElement
import GHCJS.DOM.WebGLRenderingContextBase
import Language.Javascript.JSaddle.Object (new, jsg, js1)


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
  , "  // If the position is outside the inscribed circle, make it transparent"
  , "  if (length(pos) > 1.0) { gl_FragColor = vec4(0,0,0,0); return; }"
  , "  // Otherwise, return red"
  , "  gl_FragColor = vec4(1.0,0.0,0.0,1.0);"
  , "}"
  ]

paintGL :: MonadDOM m => (Maybe Text -> m ()) -> Text -> HTMLCanvasElement -> m ()
paintGL printErr fragmentShaderSource canvas = do
  -- adaption of
  -- https://blog.mayflower.de/4584-Playing-around-with-pixel-shaders-in-WebGL.html

  gl <- getContextUnsafe canvas ("experimental-webgl"::Text) ([]::[()])
  gl <- unsafeCastTo WebGLRenderingContext gl

  w <- getDrawingBufferWidth gl
  h <- getDrawingBufferHeight gl
  viewport gl 0 0 w h

  buffer <- createBuffer gl
  bindBuffer gl ARRAY_BUFFER (Just buffer)
  array <- liftDOM (new (jsg ("Float32Array"::Text))
        [[ -1.0, -1.0,
            1.0, -1.0,
           -1.0,  1.0,
           -1.0,  1.0,
            1.0, -1.0,
            1.0,  1.0 :: Double]])
    >>= unsafeCastTo Float32Array
  let array' = uncheckedCastTo ArrayBuffer array
  bufferData gl ARRAY_BUFFER (Just array') STATIC_DRAW

  vertexShader <- createShader gl VERTEX_SHADER
  shaderSource gl (Just vertexShader) vertexShaderSource
  compileShader gl (Just vertexShader)
  -- jsg "console" ^. js1 "log" (gl ^. js1 "getShaderInfoLog" vertexShader)

  fragmentShader <- createShader gl FRAGMENT_SHADER
  shaderSource gl (Just fragmentShader) fragmentShaderSource
  compileShader gl (Just fragmentShader)
  -- jsg "console" ^. js1 "log" (gl ^. js1 "getShaderInfoLog" fragmentShader)
  err <- getShaderInfoLog gl (Just fragmentShader)
  printErr err

  program <- createProgram gl
  attachShader gl (Just program) (Just vertexShader)
  attachShader gl (Just program) (Just fragmentShader)
  linkProgram gl (Just program)
  useProgram gl (Just program)
  -- jsg "console" ^. js1 "log" (gl ^. js1 "getProgramInfoLog" program)

  positionLocation <- getAttribLocation gl (Just program) ("a_position" :: Text)
  enableVertexAttribArray gl (fromIntegral positionLocation)
  vertexAttribPointer gl (fromIntegral positionLocation) 2 FLOAT False 0 0
  -- liftJSM $ jsg ("console"::Text) ^. js1 ("log"::Text) program

  windowSizeLocation <- getUniformLocation gl (Just program) ("u_windowSize" :: Text)
  uniform2f gl (Just windowSizeLocation) (fromIntegral w) (fromIntegral h)

  drawArrays gl TRIANGLES 0 6
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

  let eDraw = leftmost
                [ updated fragmentShaderSource
                , tag (current fragmentShaderSource) pb
                ]

  performEvent $ (<$> eDraw) $ \src -> do
    e <- unsafeCastTo HTMLCanvasElement $ _element_raw canvasEl
    paintGL (liftIO . reportError) src e

  holdDyn Nothing eError
