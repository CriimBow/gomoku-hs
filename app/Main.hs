module Main where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center

ui :: Widget ()
ui = withBorderStyle unicode $ borderWithLabel (str "Hello!") $ center (str "Left") <+> vBorder <+> center (str "Right")

main :: IO ()
main = simpleMain ui
