main = interact $ show . sum . map read . drop 1 . words
