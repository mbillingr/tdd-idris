import Data.List
import Data.List.Views

mergeSort : Ord a => List a -> List a
mergeSort input with (splitRec input)
  mergeSort [] | SplitRecNil = []
  mergeSort [_] | (SplitRecOne x) = [x]
  mergeSort (_ ++ _) | (SplitRecPair lefts rights lrec rrec) = merge (mergeSort _ | lrec) 
                                                                     (mergeSort _ | rrec)
