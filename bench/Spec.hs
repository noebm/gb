import           Criterion.Main

import           Control.Lens
import           SDL.Vect

import           Hardware.GPU
import           Hardware.GPU.Drawing
import           Hardware.GPU.OAM               ( defaultOAM )
import           Hardware.GPU.VideoRAM          ( defaultVideoRAM )
import           Hardware.Timer

main :: IO ()
main = defaultMain
  [ bgroup "timer" [bench "updateTimer" $ nf (updateTimer 50) defaultTimer]
  , bgroup
    "drawing"
    [ bench "generateLine nothing"
      $ nf (generateLine defaultGPUControl defaultVideoRAM) defaultOAM
    , bench "generateLine BG" $ nf
      (generateLine (defaultGPUControl & displayBG .~ True) defaultVideoRAM)
      defaultOAM
    , bench "generateLine OBJ" $ nf
      (generateLine (defaultGPUControl & displayOBJ .~ True) defaultVideoRAM)
      defaultOAM
    , bench "generateLine window" $ nf
      (generateLine
        (  defaultGPUControl
        &  displayWindow
        .~ True
        &  gpuWindow
        .  _y
        .~ 0
        &  gpuLine
        .~ 20
        )
        defaultVideoRAM
      )
      defaultOAM
    ]
  ]
