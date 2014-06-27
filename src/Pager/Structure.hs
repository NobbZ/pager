{-|
Module      : Pager.Structure
Description : The structure of a document
Copyright   : (c) Norbert Melzer, 2014
License     : MIT
Maintainer  : timmelzer@gmail.com
Stability   : experimental
Portability : portable

TODO: Longer description follows later!
-}

{-# LANGUAGE OverloadedStrings #-}

module Pager.Structure where

import           Data.String
import           Data.Text
import           Data.Word

-- | What does the Document contain?
newtype Document = Document [DocUnit]

-- | An identifier
newtype ID = ID Word
           deriving(Eq)

-- | A text used as caption
newtype Caption = Caption Text
                deriving(Eq)

instance IsString Caption where
  fromString s = Caption (fromString s :: Text)

-- | A text that might be used in table of content or similar lists
newtype ShortCaption = ShortCaption Text
                     deriving(Eq)

instance IsString ShortCaption where
  fromString s = ShortCaption (fromString s :: Text)

-- | Smaller parts of a document
data DocUnit = Part          DocUnitData
             | Chapter       DocUnitData
             | Section       DocUnitData
             | SubSection    DocUnitData
             | SubSubSection DocUnitData
             | Paragraph     DocUnitData
             | SubParagraph  DocUnitData
             | Text          Text
             deriving(Eq)

data DocUnitData = DUD ID Bool Caption ShortCaption [DocUnit]
                 deriving(Eq)

instance Enum DocUnit where
  fromEnum (Part          _) = 7
  fromEnum (Chapter       _) = 6
  fromEnum (Section       _) = 5
  fromEnum (SubSection    _) = 4
  fromEnum (SubSubSection _) = 3
  fromEnum (Paragraph     _) = 2
  fromEnum (SubParagraph  _) = 1
  fromEnum (Text          _) = 0
  toEnum 0 = Text          ""
  toEnum 1 = SubParagraph  (DUD (ID 0) False "" "" [])
  toEnum 2 = Paragraph     (DUD (ID 0) False "" "" [])
  toEnum 3 = SubSubSection (DUD (ID 0) False "" "" [])
  toEnum 4 = SubSection    (DUD (ID 0) False "" "" [])
  toEnum 5 = Section       (DUD (ID 0) False "" "" [])
  toEnum 6 = Chapter       (DUD (ID 0) False "" "" [])
  toEnum 7 = Part          (DUD (ID 0) False "" "" [])
  toEnum n = error $ "toEnum " ++ show n ++ " can't produce valid output!"

instance Ord DocUnit where
  l <= r = (fromEnum l) <= (fromEnum r)
