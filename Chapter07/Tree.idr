data Tree item = Empty
               | Node (Tree item) item (Tree item)

Functor Tree where
    map func Empty = Empty
    map func (Node left x right) 
        = Node (map func left)
               (func x)
               (map func right)

Foldable Tree where
    foldr func acc Empty = acc
    foldr func acc (Node left x right) 
          = let leftfold = foldr func acc left
                rightfold = foldr func leftfold right in
                func x rightfold
