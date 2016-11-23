import Interval

import Control.Lens
import Linear.NURBS
import Linear.V2
import Test.Hspec

main :: IO ()
main = --hspec $
  do
    putStrLn "Done"
    -- describe "evaluate point" $ do
      -- it "should start from first control point" $
        -- (test `eval` 0.0) ≃ (test ^?! wpoints . _head . wpoint)
      -- it "should end at last control point" $
        -- (test `eval` 1.0) ≃ (test ^?! wpoints . _last . wpoint)
    -- describe "insert knot" $ do
      -- it "should not change nurbs curve" $
        -- insertKnots [(1, 0.1), (2, 0.3)] test ≃ test
      -- where
        -- test :: NURBS V2 (Interval Float)
        -- test = nurbs 3 [V2 (Interval 0 0) (Interval 0 0),
                        -- V2 (Interval 10 11) (Interval 0 0),
                        -- V2 (Interval 10 11) (Interval 10 11),
                        -- V2 (Interval 20 22) (Interval 20 22),
                        -- V2 (Interval 0 0) (Interval 20 22),
                        -- V2 (Interval (-20) (-22)) (Interval 0 0)]
