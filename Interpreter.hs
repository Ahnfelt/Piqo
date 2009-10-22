module Interpreter (evaluate) where
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import Value
import Expression

may f = maybe Nothing (Just . f)

evaluate :: Map.Map String Value -> Expression -> Value
evaluate m (EVariable i) = m Map.! i
evaluate m (ELet i e e') = evaluate (Map.insert i (evaluate m e) m) e'
evaluate m (ESequence e e') = evaluate m e `seq` evaluate m e'
evaluate m (EList) = VList []
evaluate m (EString n) = VString n
evaluate m (EInteger n) = VInteger n
evaluate m (EFloat n) = VFloat n
evaluate m (EBoolean n) = VBoolean n
evaluate m (ELambda i e) =
    let vs = free e Set.\\ (Set.singleton i) in
    let m' = Map.fromList (map (\v -> (v, m Map.! v)) (Set.toList vs)) in
    VLambda m' i e
evaluate m (ECall e e') =
    call m (evaluate m e) e'
evaluate m (EObject e m') =
    let s = may (evaluate m) e in
    VObject s (Map.map (evaluate m) m')
evaluate m (EField e i) =
    field (evaluate m e) i

call :: Map.Map String Value -> Value -> Expression -> Value
call m (VLambda m' i e') e = evaluate (Map.insert i (evaluate m e) m') e'
call m (VNative f) e = f (evaluate m e)
call m v e = call m (field v "get") e

field :: Value -> String -> Value
field (VObject s o) i = case Map.lookup i o of
    Just v -> v
    Nothing -> field (Maybe.fromJust s) i
field o i = field (object o) i

