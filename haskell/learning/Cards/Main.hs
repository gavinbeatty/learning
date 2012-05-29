import qualified Cards as C
import Maybe

main :: IO ()
main = putStrLn (C.strCard (fromJust (C.mkCard C.Diamonds (C.Number 10))))

