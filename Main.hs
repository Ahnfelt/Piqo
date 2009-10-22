module Main where
import qualified Data.Map as Map
import System.Environment
import Interpreter
import Value
import Expression

main = do
    a <- getArgs
    case a of
        [f] -> putStrLn "Not supported yet."
        _ -> runTests 

runTests = do
    l <- mapM runTest tests
    let p = [r | Right r <- l]
    let f = [r | Left r <- l]
    if not (null p)
        then do
            putStrLn "PASSED:"
            mapM (\s -> do putStr "  "; putStrLn s) p
            return ()
        else do
            return ()
    if not (null f)
        then do
            putStrLn "FAILED:"
            mapM failure f
            return ()
        else do
            return ()
    where 
        failure (s, v, v') = do
            putStrLn ("  " ++ s ++ ":")
            putStrLn ("    Expected: " ++ show v)
            putStrLn ("    But got:  " ++ show v')

runTest :: (String, Value, Expression) -> IO (Either (String, Value, Value) String)
runTest (s, v, e) = do
    let v' = evaluate Map.empty e
    let r = case (v, v') of
            (VBoolean n, VBoolean n') -> n == n'
            (VInteger n, VInteger n') -> n == n'
            (VFloat n, VFloat n') -> abs (n - n') < (abs n) / 100000.0
            _ -> False
    return $ if r then Right s else Left (s, v, v')

tests :: [(String, Value, Expression)]
tests = [
    ("Let/scope", float 7.0, 
        ELet "x" (float 5.0) 
            (ELet "x" (float 7.0) 
                (ESequence 
                    (ELet "x" (float 9.0) (EVariable "x")) 
                    (EVariable "x")))
    ),
    ("Lambda/get", integer 42, 
        ECall
            (EField
                (ELambda "x" (EVariable "x"))
                "get")
            (integer 42)
    ),
    ("Integer arithmetic", integer 3, 
        ECall (EField (integer 7) "over") (integer 2)
    ),
    ("Float arithmetic", float 3.0, 
        ECall (EField (float 9.0) "power") (float 0.5)
    ),
    ("Boolean decision", integer 1,
        ECall 
            (ECall
                (EField (boolean True) "thenElse") 
                (ELambda "_" (integer 1)))
            (ELambda "_" (integer 0))
    ),
    ("Super object/fields", integer 8,
        ELet "o"
            (EObject
                (Just (record [("foo", integer 0), ("bar", integer 1), ("baz", integer 2)]))
                (Map.fromList [("foo", integer 7)]))
            (ECall 
                (EField (EField (EVariable "o") "foo") "plus")
                (EField (EVariable "o") "bar"))
    ),
    ("List/range", integer 2,
        ELet "l" 
            (ECall (EField (integer 1) "to") (integer 3))
            (ECall (EField (ECall (EField (EVariable "l") "getTail") void) "getHead") void)
    ),
    ("String", integer 6,
        ECall (EField (string "foobar") "getLength") void
    )]

