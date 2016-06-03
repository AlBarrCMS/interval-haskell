{-|
Module      : ImageWriter
Description : Module for writing to ppm images.
-}

module ImageWriter (
    Color,
    write_to_ppm,
    Image(Image)
) where
import Data.List
import Data.Foldable
import qualified Data.Sequence as S

-- | An image specified by a width and height (in pixels) and a sequence of pixel color
-- values
data Image f = Image Int Int (S.Seq (Color f))

-- | A list to store components of a color (e.g. r,g,b values)
type Color f = [f]

-- | Returns a string with the contents of a ppm file for the given image
write_to_ppm :: (Floating f, RealFrac f) => Int     -- ^ The maximum pixel color value
                                         -> Image f -- ^ The image to print
                                         -> String  -- ^ The ppm string
write_to_ppm max_color (Image w h pixels) = join ["P3", spaces[w, h], show max_color, colors pixels]
    where
        join_with x = concat . Data.List.intersperse x
        join = join_with "\n" 
        spaces = join_with " " . map show
        colors = join . map spaces . map (map round) . toList
