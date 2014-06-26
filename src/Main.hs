module Main where

import           Graphics.PDF

import           Pager.PaperFormat

main :: IO ()
main = do
  runPdf "demo.pdf" (
    standardDocInfo { author     = toPDFString "Norbert Melzer"
                    , compressed = False
                    }
    ) (toPDFRect $ PaperSize Portrait A4) $ do
      myDocument

myDocument :: PDF ()
myDocument = do
  page1 <- addPage Nothing
  newSection (toPDFString "Section") Nothing Nothing $ do
    newSection (toPDFString "Subsection") Nothing Nothing $ do
      createPageContent page1

createPageContent :: PDFReference PDFPage -> PDF ()
createPageContent page = drawWithPage page $ do
  strokeColor red
  setWidth 0.5
  stroke $ Rectangle (0 :+ 0) (300 :+ 420)
