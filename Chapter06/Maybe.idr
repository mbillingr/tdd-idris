maybeAdd : Maybe Int -> Maybe Int -> Maybe Int
--maybeAdd x y = x >>= \x_val => y >>= \y_val => Just (x_val + y_val)
maybeAdd x y = do x_val <- x
                  y_val <- y
                  Just (x_val + y_val)
