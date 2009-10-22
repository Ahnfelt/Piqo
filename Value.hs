module Value where
import qualified Data.Map as Map
import Expression

data Value
    = VNative (Value -> Value)
    | VLambda (Map.Map String Value) String Expression
    | VObject (Maybe Value) (Map.Map String Value)
    | VInteger Integer
    | VFloat Double
    | VBoolean Bool

instance Literals Value where
    void = VObject Nothing Map.empty
    integer n = VInteger n
    float n = VFloat n
    boolean n = VBoolean n
    lambda i e = VLambda Map.empty i e
    record l = VObject Nothing (Map.fromList l)

instance Show Value where
    show (VNative _) = "{}"
    show (VInteger n) = show n
    show (VFloat n) = show n
    show (VBoolean n) = show n
    show (VLambda _ i e) = "{|" ++ i ++ "| ...}"
    show (VObject (Just v) r) = "(:" ++ show v ++ ", " ++ showFields (Map.keys r) ++ ")"
    show (VObject Nothing r) = "(" ++ showFields (Map.keys r) ++ ")"

showFields (x:x':xs) = x ++ ": ..., " ++ showFields (x':xs)
showFields [x] = x ++ ": ..."
showFields [] = ""

object :: Value -> Value
object o@(VObject _ _) = o
object f@(VLambda _ _ _) = record [("get", f)]
object f@(VNative _) = record [("get", f)]
object (VInteger n) = record [
    property "getFloat" $ VFloat $ fromIntegral n,
    property "getAbsolute" $ VInteger $ abs n,
    property "getSign" $ VInteger $ if n > 0 then 1 else if n < 0 then -1 else 0,
    arithmetic "plus" $ \n' -> n + n',
    arithmetic "minus" $ \n' -> n - n',
    arithmetic "times" $ \n' -> n * n',
    arithmetic "over" $ \n' -> n `div` n',
    arithmetic "mod" $ \n' -> n `mod` n',
    arithmetic "power" $ \n' -> n ^ n',
    compare "greater" $ \n' -> n > n',
    compare "less" $ \n' -> n < n',
    compare "greaterEqual" $ \n' -> n >= n',
    compare "lessEqual" $ \n' -> n <= n',
    compare "equal" $ \n' -> n == n',
    compare "notEqual" $ \n' -> n /= n'
    ]
    where
        property i f = (i, VNative $ \_ -> f)
        arithmetic i f = (i, VNative $ \(VInteger n') -> VInteger (f n'))
        compare i f = (i, VNative $ \(VInteger n') -> VBoolean (f n'))
object (VFloat n) = VObject Nothing $ Map.fromList [
    property "round" $ VInteger $ round n,
    property "floor" $ VInteger $ round n,
    property "ceiling" $ VInteger $ round n,
    property "getAbsolute" $ VFloat $ abs n,
    property "getSign" $ VFloat $ if n > 0.0 then 1.0 else if n < 0.0 then -1.0 else 0.0,
    property "getUndefined" $ VBoolean $ isNaN n,
    property "getInfinite" $ VBoolean $ isInfinite n,
    arithmetic "plus" $ \n' -> n + n',
    arithmetic "minus" $ \n' -> n - n',
    arithmetic "times" $ \n' -> n * n',
    arithmetic "over" $ \n' -> n / n',
    arithmetic "power" $ \n' -> n ** n',
    compare "greater" $ \n' -> n > n',
    compare "less" $ \n' -> n < n',
    compare "greaterEqual" $ \n' -> n >= n',
    compare "lessEqual" $ \n' -> n <= n',
    compare "equal" $ \n' -> n == n',
    compare "notEqual" $ \n' -> n /= n'
    ]
    where
        property i f = (i, VNative $ \_ -> f)
        arithmetic i f = (i, VNative $ \(VFloat n') -> VFloat (f n'))
        compare i f = (i, VNative $ \(VFloat n') -> VBoolean (f n'))
object (VBoolean n) = VObject Nothing $ Map.fromList [
    function "getNot" "_" $ EBoolean (not n),
    function "andAlso" "b" $ if n then ECall (EVariable "b") void else EBoolean False,
    function "orElse" "b" $ if n then EBoolean True else ECall (EVariable "b") void,
    function "then" "t" $ if n then ECall (EVariable "t") void else void,
    function "else" "e" $ if n then void else ECall (EVariable "e") void,
    function "thenElse" "t" $ ELambda "e" $ if n then ECall (EVariable "t") void else ECall (EVariable "e") void
    ]
    where
        function i i' e = (i, VLambda Map.empty i' e)

