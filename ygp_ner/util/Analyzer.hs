{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.List                 (sort)
--
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
--
import           Util.IO

senDistChart :: [(Double,Double)] -> Renderable ()
senDistChart xs = toRenderable layout
  where
    fnt1   = plot_lines_values .~ [ xs ]
                $ plot_lines_style  . line_color .~ opaque blue
                $ plot_lines_title .~ "YGP Data"
                $ def
    layout = layout_title .~ "Sentence Didstribution"
             $ layout_plots .~ [toPlot fnt1]
             $ layout_x_axis . laxis_generate .~ scaledAxis def (0,500)
             $ layout_y_axis . laxis_generate .~ scaledAxis def (0,500) 
             $ def

main :: IO ()
main = do
  senDist <- readAnnotSenFile "AnnotatedSentences.txt"  
  let (normSenDistData :: [(Double,Double)]) = map (\(i1,i2) -> (fromIntegral i1, fromIntegral i2)) $ sort senDist
  _ <- renderableToFile def "Test.png" (senDistChart normSenDistData)

  return ()
