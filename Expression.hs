module Expression where
import qualified Data.Map as Map
import qualified Data.Set as Set

data Expression
    = ELambda String Expression
    | EObject (Maybe Expression) (Map.Map String Expression)
    | EField Expression String
    | ECall Expression Expression
    | EInteger Integer
    | EFloat Double
    | EBoolean Bool
    | EVariable String
    | ELet String Expression Expression
    | ESequence Expression Expression
    deriving Show

class Literals a where
    void :: a
    integer :: Integer -> a
    float :: Double -> a
    boolean :: Bool -> a
    lambda :: String -> Expression -> a
    record :: [(String, a)] -> a

instance Literals Expression where
    void = EObject Nothing Map.empty
    integer n = EInteger n
    float n = EFloat n
    boolean n = EBoolean n
    lambda i e = ELambda i e
    record l = EObject Nothing (Map.fromList l)

free :: Expression -> Set.Set String
free (ELambda i e) = free e Set.\\ Set.singleton i
free (EObject (Just e) m) = free e `Set.union` free (EObject Nothing m)
free (EObject Nothing m) = foldl Set.union Set.empty (map (\(k, v) -> free v) (Map.toList m))
free (EField e _) = free e
free (ECall e e') = free e `Set.union` free e'
free (EVariable i) = Set.singleton i
free (ELet i e e') = free e `Set.union` (free e' Set.\\ Set.singleton i)
free (ESequence e e') = free e `Set.union` free e'

