{-# LANGUAGE LambdaCase #-}

module Day8 (day08a, day08b) where
import           Data.List                      ( sortOn )
import           Data.List.Split                ( chunksOf )
import           Text.ParserCombinators.ReadP   ( ReadP
                                                , readP_to_S
                                                )
import qualified Text.ParserCombinators.ReadP  as P
import           Util                           ( strip )

-- A single pixel
data Pixel = B | W | T deriving (Eq)

instance Semigroup Pixel where
    T <> p = p
    p <> _ = p

instance Monoid Pixel where
    mempty = T

instance Read Pixel where
    readsPrec _ = readP_to_S pix
    readList = readP_to_S $ P.many1 pix <* P.eof

pix :: ReadP Pixel
pix =
    P.get
        >>= (\case
                '0' -> pure B
                '1' -> pure W
                '2' -> pure T
                _   -> P.pfail
            )

dispPix :: Pixel -> Char
dispPix B = ' '
dispPix W = '█'
dispPix T = '░'

-- A layer
newtype LayerData = LD { getPix :: [Pixel] }

instance Semigroup LayerData where
    (LD as) <> (LD bs) = LD $ zipWith (<>) as bs

instance Monoid LayerData where
    mempty = LD $ repeat T

dispLayD :: LayerData -> String
dispLayD = fmap dispPix . getPix

-- A complete Image
type Size = (Int, Int)

data Image = Img { _size :: Size,
                   layD  :: [LayerData] }

dispImg :: Image -> String
dispImg (Img (w, _) ls) = unlines $ chunksOf w $ dispLayD $ mconcat ls

mkImage :: Size -> String -> Image
mkImage (w, h) = Img (w, h) . fmap LD . chunksOf (w * h) . read

day08a :: String -> String
day08a inp = show $ filtLay W * filtLay T
  where
    sortLay =
        head
            $ sortOn (length . filter (B ==))
            $ fmap getPix
            $ layD
            $ mkImage (25, 6)
            $ strip inp
    filtLay = length . flip filter sortLay . (==)


day08b :: String -> String
day08b = dispImg . mkImage (25, 6) . strip
