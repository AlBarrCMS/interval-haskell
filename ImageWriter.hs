module ImageWriter (
    Color,
    write_to_ppm,
    Image(Image)
) where
import Data.List
import Data.Foldable
import qualified Data.Sequence as S

data Image f = Image Int Int (S.Seq (Color f))

type Color f = [f]

write_to_ppm :: (Floating f, RealFrac f) => Int -> Image f -> String 
write_to_ppm max_color (Image w h pixels) = join ["P3", spaces[w, h], show max_color, colors pixels]
    where
        join_with x = concat . Data.List.intersperse x
        join = join_with "\n" 
        spaces = join_with " " . map show
        colors = join . map spaces . map (map round) . toList
