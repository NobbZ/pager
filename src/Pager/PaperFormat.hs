{-|
Module      : Pager.PaperFormat
Description : Defines functions and dataformats needed to handle different sizes\/formats of paper
Copyright   : (c) Norbert Melzer, 2014
License     : MIT
Maintainer  : timmelzer@gmail.com
Stability   : experimental
Portability : portable

TODO: Longer description follows later!
-}

module Pager.PaperFormat ( Points     (Points)
                         , MilliMeters(MilliMeters)
                         , MilliInches(MilliInches)
                         , PaperSize  (..)
                         , PaperFormat(..)
                         , Orientation(..)
                         , pt2mm
                         , pt2minch
                         , mm2pt
                         , mm2minch
                         , minch2pt
                         , minch2mm
                         , toPDFRect) where

import           Data.Tuple
import qualified Graphics.PDF   as PDF

import           Pager.Internal
import           Pager.Units

-- | Defines the size of the paper
data PaperSize = PaperSize   !Orientation !PaperFormat -- ^ Defines the size of the paper in respect of ISO
               | PaperRectPt !Points      !Points      -- ^ Defines the size of the paper given by length and height in 'Points'
               | PaperRectMM !MilliMeters !MilliMeters -- ^ Defines the size of the paper given by length and height in 'MilliMeters'
               | PaperRectMI !MilliInches !MilliInches -- ^ Defines the size of the paper given by length and height in 'MilliInches'

-- | Possible predefined paperformats, please see <http://www.prepressure.com/library/paper-size>
data PaperFormat = A0 | A1 | A2 | A3 | A4 | A5 | A6 | A7 | A8
                 | B0 | B1 | B2 | B3 | B4 | B5 | B6 | B7 | B8 | B9 | B10
                 |           C2 | C3 | C4 | C5 | C6
                 | Letter | Legal
                 deriving (Eq, Show)

-- | Describes the orientation of the paper
data Orientation = Landscape -- ^ the paper is oriented in a way that the width is greater than the height
                 | Portrait  -- ^ the paper is oriented in a way that the height is greater than the width

-- | Returns a 'PDFRect' representing the corresponding 'PaperSize'
toPDFRect :: PaperSize -> PDF.PDFRect
toPDFRect (PaperSize   o pf) = case o of
  Landscape -> uncurry (PDF.PDFRect 0 0) (swap $ dimension)
  Portrait  -> uncurry (PDF.PDFRect 0 0) (       dimension)
  where
    dimension = maybe (0, 0) id $ lookup pf dimensionList
toPDFRect (PaperRectPt w h ) = PDF.PDFRect 0 0 (ub              w) (ub              h)
toPDFRect (PaperRectMM w h ) = PDF.PDFRect 0 0 (ub . mm2pt    $ w) (ub . mm2pt    $ h)
toPDFRect (PaperRectMI w h ) = PDF.PDFRect 0 0 (ub . minch2pt $ w) (ub . minch2pt $ h)

dimensionList :: [(PaperFormat, (Int, Int))]
dimensionList = [ (A0,     (2384, 3370))
                , (A1,     (1684, 2384))
                , (A2,     (1190, 1684))
                , (A3,     ( 842, 1190))
                , (A4,     ( 595,  842))
                , (A5,     ( 420,  595))
                , (A6,     ( 298,  420))
                , (A7,     ( 210,  298))
                , (A8,     ( 148,  210))
                , (B0,     (2835, 4008))
                , (B1,     (2004, 2835))
                , (B2,     (1417, 2004))
                , (B3,     (1001, 1417))
                , (B4,     ( 709, 1001))
                , (B5,     ( 499,  709))
                , (B6,     ( 354,  499))
                , (B7,     ( 249,  354))
                , (B8,     ( 176,  249))
                , (B9,     ( 125,  176))
                , (B10,    (  88,  125))
                , (C2,     (1837,  578))
                , (C3,     ( 578,  919))
                , (C4,     ( 919,  649))
                , (C5,     ( 649,  459))
                , (C6,     ( 459,  323))
                , (Letter, ( 612,  792))
                , (Legal,  ( 612, 1008))
                ]

