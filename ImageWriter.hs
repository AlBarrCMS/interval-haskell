module ImageWriter (
    write_to_ppm,
    Image(Image)
) where
import Data.List

data Image f = Image Int Int [Color f]

type Color f = [f]
data Material f = Material

write_to_ppm :: (Floating f, RealFrac f) => Int -> Image f -> String 
write_to_ppm max_color (Image w h pixels) = join ["P3", spaces[w, h], show max_color, colors pixels]
    where
        join_with x = concat . Data.List.intersperse x
        join = join_with "\n" 
        spaces = join_with " " . map show
        colors = join . map spaces . map (map round) 
