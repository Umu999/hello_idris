module Main

import Data.String

getInteger : IO Integer
getInteger = do
    putStr "guess a number:"
    n <- getLine
    case parsePositive n of
        Just n => pure n
        Nothing => do
            putStrLn (n ++ "is not a number.")
            getInteger

game : Integer -> IO ()
game secret = do
    n <- getInteger
    case compare n secret of
        LT => do
            putStrLn "Too small"
            game secret
        GT => do
            putStrLn "Too big"
            game secret
        EQ => do
            putStrLn "You got it"

withOpenFile : String -> Mode -> (File -> IO (Either FileError a)) -> IO (Either FileError a)
withOpenFile filename mode f = do
    Right file <- openFile filename mode
        | Left e => pure (Left e)
    ret <- f file
    closeFile file
    pure ret

getRandom : IO (Either FileError Int)
getRandom = withOpenFile "/dev/random" Read fGetInt
where
    stringToInt : String -> Int
    stringToInt chars = foldl (\acc, i => (shiftL acc 8) + i) 0 $ map ord $ unpack chars

    fGetInt : File -> IO (Either FileError Int)
    fGetInt file = do
        Right chars <- fGetChars file 4
            | Left e => pure (Left e)
        pure (Right (stringToInt chars))

main: IO ()
main = do
    eint <- getRandom
    case eint of
        -- エラーが起きたら終了
        Left _ => pure ()
        Right int => game (cast (1 + (mod int 100)))
