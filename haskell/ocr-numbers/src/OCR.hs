module OCR
  ( convert
  )
where

import Data.Char
import Data.List
import Data.List.Split
import Data.Tuple

type OCRDigit = [String]

convert :: String -> String
convert =
  intercalate ","
    . (map . map) ocrDigitToChar
    . ocrChunks
    . lines

-- | @OCRDigit@ to @Int@ table
ocrTable :: [(OCRDigit, Int)]
ocrTable = map swap intToOcr
  where
    intToOcr = zip [0 ..] (ocrLineToChunks ocrs)
    ocrs =
      [ " _     _  _     _  _  _  _  _ "
      , "| |  | _| _||_||_ |_   ||_||_|"
      , "|_|  ||_  _|  | _||_|  ||_| _|"
      , "                              "
      ]

ocrDigitToChar :: OCRDigit -> Char
ocrDigitToChar od = case lookup od ocrTable of
  Nothing -> '?'
  Just v -> chr (v + ord '0')

-- | break a matrix of OCR digits into digits
ocrChunks :: [String] -> [[OCRDigit]]
ocrChunks ls = map ocrLineToChunks $ chunksOf 4 ls

-- | break a single "OCR Line" (4 rows of texts) into @OCRDigit@s
ocrLineToChunks :: [String] -> [OCRDigit]
ocrLineToChunks = transpose . map (chunksOf 3)
