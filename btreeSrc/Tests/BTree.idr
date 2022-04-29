module Tests.BTree

import BTree
import Test.Unit.Assertions

testInsertMember : IO ()
testInsertMember =
    let tree = insert empty 1 in
    let tree = insert tree 5 in
    let tree = insert tree 3 in
    do
        assertTrue $ member tree 1
        assertTrue $ member tree 5
        assertTrue $ member tree 3
        pure ()

testNotInsertMember : IO ()
testNotInsertMember =
    let tree = insert empty 1 in
    let tree = insert tree 5 in
    let tree = insert tree 3 in
    do
        assertFalse $ member tree 2
        assertFalse $ member tree 4
        pure ()


export
test : IO ()
test = do
    testInsertMember
    testNotInsertMember
