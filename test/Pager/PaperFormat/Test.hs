{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Pager.PaperFormat.Test where

import           Test.SmallCheck.Series
import           Test.Tasty
import           Test.Tasty.SmallCheck

import           Graphics.PDF

import           Pager.PaperFormat

formats _ = [ A0, A1, A2, A3, A4, A5, A6, A7, A8
            , B0, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10
            ,         C2, C3, C4, C5, C6
            , D0
            , SRA0, SRA1, SRA2, SRA3, SRA4
            ,  RA0,  RA1,  RA2
            ]

instance Monad m => Serial m PaperFormat where
  series = generate formats

paperFormatSuite :: TestTree
paperFormatSuite = testGroup "PaperFormat"
  [ testProperty "Sizes defined" size_prop
  ]

size_prop :: Property IO
size_prop = forAll $ \pf -> case toPDFRect (PaperSize Landscape pf) of
  PDFRect 0 0 w h -> (w, h) > (0, 0)
  PDFRect _ _ _ _ -> False
