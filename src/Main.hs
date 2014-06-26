module Main where

import           Graphics.PDF

a0rect = PDFRect 0 0 2384 3370
a1rect = PDFRect 0 0 1684 2384
a2rect = PDFRect 0 0 1190 1684
a3rect = PDFRect 0 0  842 1190
a4rect = PDFRect 0 0  595  842
a5rect = PDFRect 0 0  420  595
a6rect = PDFRect 0 0  298  420
a7rect = PDFRect 0 0  210  298
a8rect = PDFRect 0 0  148  210

main :: IO ()
main = do
  runPdf "demo.pdf" (
    standardDocInfo { author     = toPDFString "Norbert Melzer"
                    , compressed = False
                    }
    ) a4rect $ do
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
  stroke $ Rectangle (10 :+ 0) (200 :+ 300)
