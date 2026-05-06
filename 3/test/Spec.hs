import qualified Program

src :: [Char]
src = "" ++
    "read k;" ++
    "read n;" ++
    "m := 1;" ++
    "while n-m do " ++
        "begin " ++
        "if m - m/k*k then " ++
            "skip; " ++
        "else " ++
           " -- note an exponentiation below\n" ++
            "write m^2; " ++
        "m := m + 1; -- an inline comment\n" ++
        "end " ++
    "write -- yet another comment, this time inside a statement!\n" ++
       "m^2^3;" 

main :: IO ()
main = do
    let p = Program.fromString src
    print $ Program.exec p [3, 16]
    
