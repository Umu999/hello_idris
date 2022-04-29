module BTree

export
data BTree a = Leaf
            | Node (BTree a) a (BTree a)

export
insert : Ord a => BTree a -> a -> BTree a
insert Leaf x = Node Leaf x Leaf
insert (Node l v r) x = case compare x v of
    LT => Node (insert l x) v r
    EQ => Node l v r
    GT => Node l v (insert r x)

export
member : Ord a => BTree a -> a -> Bool
member Leaf x = False
member (Node l v r) x = case compare x v of
    LT => member l x
    EQ => True
    GT => member r x

export
empty : BTree a
empty = Leaf


