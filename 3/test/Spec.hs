import qualified Program

src :: String
src = "read k; read n; m := 1; while n-m do begin if m-m/k*k then skip; else write m^2; m := m+1; end write m^2^3;"

main :: IO ()
main = do
    let p = Program.fromString src
    print $ Program.exec p [3, 16]
    
