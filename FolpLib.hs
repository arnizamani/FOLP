module FolpLib where

import System.IO

import Lexfolp
import Parfolp
import Absfolp
import ErrM
import Data.Char
import Data.List
import Control.Parallel
import Control.Parallel.Strategies
import qualified Control.Parallel
import qualified Control.Concurrent
import Debug.Trace
import Control.DeepSeq

type ParseFun a = [Token] -> Err a
type TPatternMap = [(String, TPatternTypes)] 
type DM = Exp
type SM = Exp
type WM = Exp
type TState = (DM, SM, WM)
type TMemSize = Int
type TProofSize = Int
type TExpOccPair = (Exp, Int)
type TDiscovered = ([(String, String)], TAuxTree, [(String, Int)])
type TSensMap = ([String], [(String, Exp)], TDiscovered)
type TPrevWM = [(WM, [(String, Exp)])]
type TStrnweaksLst = [(Exp, Exp)]
type TSwitchTpl = (TLengthSearch, TOutput, TUnit)
type TRuleFunc = Ctx -> WM -> TProofSize -> TProofSize -> TResult
type TRCostLkup = [(TRule, Int)]
type TPMData = ([TRuleFunc], TRCostLkup)
type TMemSegs = ([Exp], [Exp], [Exp], [Exp], [Exp], (TStrnweaksLst, TStrnweaksLst))
type TModel = [(Int, String, [Int])]
type TResult = (TResultType, [TProof])

newtype Ctx = Ctx (TSensMap, TMemSegs, TPsystem, (TMemSize, TMemSize), TPrevWM, TSwitchTpl, TPMData, TModel, Int)

data TProof = Rule (TState, TRule, TProof) | FoundSol | CutOff TProofSize
              deriving (Show, Eq)
instance NFData TProof where
    rnf t = t `seq` ()

data TAuxTree = Node (String, Int) TAuxTree TAuxTree | TNull

data TRule = FormInspect | TInspect | FInspect | AReduce | EReduce | Rewrite | PiInspectP | SigmaInspectP | DeltaInspect |
             MInspect | TRecall | CRecall | Strengthen | Weaken | ADistribute | EDistribute | EAndComp | EOrComp |
             AAndComp | AOrComp | AArrowComp | AReduceP | EReduceP | NoRule
             deriving (Eq)
             
data TPatternTypes = FullExp Exp | Quantvar Var | Domain ODom
                     deriving (Eq, Show)
                     
data TResultType = WMO | OPrune | Passed
                   deriving (Eq, Show)

instance NFData TResultType where
    rnf t = t `seq` ()
instance NFData Exp where
    rnf t = t `seq` ()

data TPsystem = T | NT | R
                deriving (Eq)

data TOutput = TERMINAL_ | LATEX_
               deriving (Eq, Show)

data TUnit = SIZE_ | WEIGHT_ | VOLUME_
             deriving (Eq, Show)

data TLengthSearch = LENGTH_SEARCH_ | NO_LENGTH_SEARCH_
                     deriving (Eq, Show)
        
instance Show TRule where
    show FormInspect   = "Formula inspect"
    show TInspect      = "Truth inspect"
    show FInspect      = "Falsity inspect"
    show PiInspectP    = "Pi inspect'"
    show SigmaInspectP = "Sigma inspect'"
    show DeltaInspect  = "Delta inspect"
    show MInspect      = "Model inspect"
    show AReduce       = "For-all reduce"
    show EReduce       = "Exists reduce"
    show AReduceP      = "For-all reduce+"
    show EReduceP      = "Exists reduce+"
    show Rewrite       = "Rewrite"
    show TRecall       = "Tautology recall"
    show CRecall       = "Contradiction recall"
    show Strengthen    = "Strengthening"
    show Weaken        = "Weakening"
    show ADistribute   = "All distribute"
    show EDistribute   = "Exists distribute"
    show EAndComp      = "Sigma &-comprehension"
    show EOrComp       = "Sigma |-comprehension"
    show AAndComp      = "Pi &-comprehension"
    show AOrComp       = "Pi |-comprehension"
    show AArrowComp    = "Pi arrowcomprehension"
    show NoRule        = "*"

myLLexer = myLexer
varassignsize = 1
extNull = (ENull, [])
sensNull = ("", ENull)

concat' = foldl' (++) []

depth :: TAuxTree -> (String, Int) -> Int
depth tree (v, i) =
    case tree of
        Node (val, occ) left right -> if (v, i) == (val, occ) then 0 else 1 + max (depth left (v, i)) (depth right (v, i))
        TNull -> minBound::Int

justLift :: [Maybe a] -> [a]
justLift lst = [x | Just x <- lst]

mc :: IO [TProof] -> IO [TProof] -> IO [TProof]
mc x y = do xvals <- x
            yvals <- y
            return $ xvals ++ yvals

mConcat :: [IO [TProof]] -> IO [TProof]
mConcat mlst = foldl' mc (return []) mlst

ruleCost :: TRCostLkup -> TRule -> TProofSize
ruleCost costs rule =
    case lookup rule costs of
        Just cost -> cost
        _         -> error $ "error in rulecost"
        
parseLtm :: String -> IO Ltm
parseLtm str = case (pLtm (myLLexer str)) of
                   Bad s       -> fail s
                   Ok patterns -> return patterns

parseExp :: String -> IO Exp
parseExp str = case (pExp (myLLexer str)) of
                   Bad s -> fail s
                   Ok e  -> return e

comb :: [a] -> Int -> [[a]]
comb [] _ = []
comb xs 1 = [[x] | x <- xs] 
comb (x:xs) k = [x:set | set <- comb xs (k-1)] ++ (comb xs k)

flength :: Exp -> Int
flength e =
    case e of
        EAnd a b       -> 1 + flength a + flength b
        EOr a b        -> 1 + flength a + flength b
        EImplies a b   -> 1 + flength a + flength b
        EEquiv a b     -> 1 + flength a + flength b
        ENot a         -> 1 + flength a
        EForAll v od a -> 2 + flength a
        EExists v od a -> 2 + flength a
        ECtx a c       -> length c + flength a
        _              -> 1

chp :: Exp -> Bool -> String
chp e rightparenthesis =
    case e of
        ESensVar _ _  -> ""
        ENot _        -> ""
        EFalse        -> ""
        ETrue         -> ""
        EPattern _    -> ""
        EPred _       -> ""
        ECtx a _      -> chp a rightparenthesis
        _             -> if rightparenthesis then ")" else "("

printDomain :: ODom -> String
printDomain EmptyOptDom = ""
printDomain (OptDom d) = "{" ++ concat (intersperse "," [show i | i <- d]) ++ "}"

printVCtx :: [VCtx] -> String
printVCtx vctx = "[" ++ (concat (intersperse "," [v ++ "=" ++ (show i) | Ctxt (Var v) i <- vctx])) ++ "]"

printExp :: Exp -> String
printExp e =
    case e of
        EAnd a b             -> (chp a False) ++ printExp a ++ (chp a True) ++ "&" ++ 
                                (chp b False) ++ printExp b ++ (chp b True)
        EOr a b              -> (chp a False) ++ printExp a ++ (chp a True) ++ "|" ++ 
                                (chp b False) ++ printExp b ++ (chp b True)
        EImplies a b         -> (chp a False) ++ printExp a ++ (chp a True) ++ "->" ++ 
                                (chp b False) ++ printExp b ++ (chp b True)
        EEquiv a b           -> (chp a False) ++ printExp a ++ (chp a True) ++ "<->" ++ 
                                (chp b False) ++ printExp b ++ (chp b True)
        EEqual a b           -> {-(chp a False) ++-} printExp a ++ {-(chp a True) ++-} "=" ++ 
                                {-(chp b False) ++-} printExp b {-++ (chp b True)-}
        ENot a               -> "~" ++ (chp a False) ++ printExp a ++ (chp a True)
        ETrue                -> "T"
        EFalse               -> "F"
        ESensVar (Ident v) _ -> v
        EForAll (Var v) od a -> ">" ++ v ++ printDomain od ++ (chp a False) ++ printExp a ++ (chp a True)
        EExists (Var v) od a -> "<" ++ v ++ printDomain od ++ (chp a False) ++ printExp a ++ (chp a True)
        ECtx a vctx          -> (chp a False) ++ printExp a ++ (chp a True) ++ printVCtx vctx
        EPred (EPBlue a)     -> "Blue(" ++ (show a) ++ ")"
        EPred (EPRed a)      -> "Red(" ++ (show a) ++ ")"
        EPred (EPYellow a)   -> "Yellow(" ++ (show a) ++ ")"
        EPred (EPEdge a1 a2) -> "E(" ++ (show a1) ++ "," ++ (show a2) ++ ")"
        ENull                -> "*"
        _                    -> error "pattern variables are not allowed in goals"
    
atomLength :: Atom -> Int
atomLength (AVar (Var v)) = length v
atomLength (AConst c) = length (show c)

domainLength :: ODom -> Int
domainLength (OptDom dom) = 2 + if length dom > 1 then 2 * length dom - 1 else length dom
domainLength EmptyOptDom = 0

expLength :: Exp -> Int
expLength e =
    case e of
        EAnd a b              -> 1 + expLength a + expLength b + 2 * length (chp a False) + 2 * length (chp b False)
        EOr a b               -> 1 + expLength a + expLength b + 2 * length (chp a False) + 2 * length (chp b False)
        EImplies a b          -> 2 + expLength a + expLength b + 2 * length (chp a False) + 2 * length (chp b False)
        EEquiv a b            -> 3 + expLength a + expLength b + 2 * length (chp a False) + 2 * length (chp b False)
        EEqual a b            -> 1 + expLength a + expLength b
        ENot a                -> 1 + expLength a + 2 * length (chp a False)
        EForAll (Var v) dom a -> 1 + length v + domainLength dom + 2 * length (chp a False) + expLength a
        EExists (Var v) dom a -> 1 + length v + domainLength dom + 2 * length (chp a False) + expLength a
        ESensVar (Ident v) _  -> length v
        EPred (EPRed a)       -> 5 + atomLength a
        EPred (EPBlue a)      -> 6 + atomLength a
        EPred (EPYellow a)    -> 8 + atomLength a
        EPred (EPEdge a b)    -> 4 + atomLength a + atomLength b
        ECtx a vctx           -> 1 + expLength a + 2 * length (chp a False) + sum [2 + length v + length (show i) | Ctxt (Var v) i <- vctx]
        _                     -> 1

getProofColumns :: TProof -> ([DM], [SM], [WM], [TRule])
getProofColumns FoundSol = ([], [], [], [])
getProofColumns (CutOff _) = ([], [], [], [])
getProofColumns (Rule ((dm, sm, wm), rule, rest)) =
    let (dms, sms, wms, rules) = getProofColumns rest
    in ((dm:dms), (sm:sms), (wm:wms), (rule:rules))

printColumns :: [DM] -> [SM] -> [WM] -> [TRule] -> (Int, Int, Int, Int) -> String
printColumns (dm:dms) (sm:sms) (exp:wms) (r:rs) (dml, sml, wml, rl) =
    printExp dm ++ replicate (dml - expLength dm) ' ' ++ " | " ++
    printExp sm ++ replicate (sml - expLength sm) ' ' ++ " | " ++
    printExp exp ++ replicate (wml - expLength exp) ' ' ++ " | " ++
    show r ++ "\n" ++ replicate (dml + sml + wml + rl + 9) (if null rs then '=' else '-') ++ "\n" ++
    printColumns dms sms wms rs (dml, sml, wml, rl)
printColumns _ _ _ _ _ = ""
       
printProof :: TProof -> String
printProof FoundSol = ""
printProof (CutOff _) = "\n------\ncut"
printProof proof =
    let (dm, sm, wm, rules) = getProofColumns proof
        dml = maximum (map expLength dm)
        sml = maximum (map expLength sm)
        wml = maximum (map expLength wm)
        rl  = maximum [length (show rule) | rule <- rules]
    in  replicate (dml + sml + wml + rl + 9) '=' ++ "\n" ++
        "DM" ++ replicate (dml-1) ' ' ++ "| VM " ++ replicate (sml-2) ' ' ++ "| WM" ++ replicate (wml-1) ' ' ++ "| PM\n" ++
        replicate (dml + sml + wml + rl + 9) '=' ++ "\n" ++
        printColumns (dm ++ [ENull]) (sm ++ [ENull]) (wm ++ [ENull]) (rules ++ [NoRule]) (dml, sml, wml, rl)        

getVars' :: Bool -> Exp -> [String]
getVars' isSensVar e =
    case e of
        EAnd a b              -> getVars' isSensVar a ++ getVars' isSensVar b
        EOr a b               -> getVars' isSensVar a ++ getVars' isSensVar b
        EImplies a b          -> getVars' isSensVar a ++ getVars' isSensVar b
        EEquiv a b            -> getVars' isSensVar a ++ getVars' isSensVar b
        ENot a                -> getVars' isSensVar a
        EForAll (Var v) dom a -> getVars' isSensVar a
        EExists (Var v) dom a -> getVars' isSensVar a
        ECtx a _              -> getVars' isSensVar a
        ESensVar (Ident v) _  -> if isSensVar then [v] else []
        _                     -> []

getVars :: Bool -> Exp -> [String]
getVars isSensVar e = nub $ getVars' isSensVar e

getSensVars :: Exp -> [(String, Int)]
getSensVars e =
    case e of
        EAnd a b             -> getSensVars a ++ getSensVars b
        EOr a b              -> getSensVars a ++ getSensVars b
        EImplies a b         -> getSensVars a ++ getSensVars b
        EEquiv a b           -> getSensVars a ++ getSensVars b
        ENot a               -> getSensVars a
        EForAll _ _ a        -> getSensVars a
        EExists _ _ a        -> getSensVars a
        ECtx a _             -> getSensVars a
        ESensVar (Ident v) i -> [(v, i)]
        _                    -> []

getPatternVars :: Exp -> [String]
getPatternVars e =
    case e of
        EAnd a b             -> getPatternVars a ++ getPatternVars b
        EOr a b              -> getPatternVars a ++ getPatternVars b
        EImplies a b         -> getPatternVars a ++ getPatternVars b
        EEquiv a b           -> getPatternVars a ++ getPatternVars b
        ENot a               -> getPatternVars a
        EForAll _ _ a        -> getPatternVars a
        EExists _ _ a        -> getPatternVars a
        ECtx a _             -> getPatternVars a
        EPattern (Pattern v) -> [v]
        _                    -> []

indexedList :: Ord t => [t] -> [(t, Int)]
indexedList explst = concat $ map ((flip zip) [1..]) (group $ sort explst)

subformulasRec :: Exp -> Bool -> [Exp]
subformulasRec e atomic =
    case e of
        EAnd a b      -> (subformulasRec a atomic) ++ (subformulasRec b atomic) ++ [e]
        EOr a b       -> (subformulasRec a atomic) ++ (subformulasRec b atomic) ++ [e]
        EImplies a b  -> (subformulasRec a atomic) ++ (subformulasRec b atomic) ++ [e]
        EEquiv a b    -> (subformulasRec a atomic) ++ (subformulasRec b atomic) ++ [e]
        EForAll _ _ a -> (subformulasRec a atomic) ++ [e]
        EExists _ _ a -> (subformulasRec a atomic) ++ [e]
        ENot a        -> (subformulasRec a atomic) ++ [e]
        ECtx a _      -> (subformulasRec a atomic) ++ [e]
        EPred _       -> if atomic then [e] else []
        ESensVar _ _  -> if atomic then [e] else []
        _             -> []

subformulas :: Exp -> Bool -> [TExpOccPair]
subformulas e atomic = 
    indexedList $ subformulasRec e atomic
      
subst' :: Int -> Exp -> [TExpOccPair] -> Exp -> TExpOccPair
subst' occ expr1 expr2s e =
    let (incr, found) = if fst (head expr2s) == e && (any (==occ) (map snd expr2s) || snd (head expr2s) == 0)
                            then (occ + 1, True) 
                            else (if fst (head expr2s) == e then (occ + 1) else occ, False)
    in if found 
        then (expr1, incr)
        else case e of
                 EAnd a b              -> let (e1, i1) = subst' incr expr1 expr2s a
                                              (e2, i2) = subst' i1 expr1 expr2s b
                                          in (EAnd e1 e2, i2)
                 EOr a b               -> let (e1, i1) = subst' incr expr1 expr2s a
                                              (e2, i2) = subst' i1 expr1 expr2s b
                                          in (EOr e1 e2, i2)
                 EImplies a b          -> let (e1, i1) = subst' incr expr1 expr2s a
                                              (e2, i2) = subst' i1 expr1 expr2s b
                                          in (EImplies e1 e2, i2)
                 EEquiv a b            -> let (e1, i1) = subst' incr expr1 expr2s a
                                              (e2, i2) = subst' i1 expr1 expr2s b
                                          in (EEquiv e1 e2, i2)
                 ENot a                -> let (e1, i1) = (subst' incr expr1 expr2s a)
                                          in (ENot e1, i1)
                 EForAll v dom a       -> let (e1, i1) = (subst' incr expr1 expr2s a)
                                          in (EForAll v dom e1, i1)
                 EExists v dom a       -> let (e1, i1) = (subst' incr expr1 expr2s a)
                                          in (EExists v dom e1, i1)
                 ECtx a c              -> let (e1, i1) = (subst' incr expr1 expr2s a)
                                          in (ECtx e1 c, i1)
                 _                     -> (e, incr)

subst :: Exp -> [TExpOccPair] -> Exp -> Exp
subst e1 e2s e3 = fst $ subst' 1 e1 e2s e3

expSize :: Exp -> Int
expSize e =
    case e of
        EAnd a b              -> 1 + expSize a + expSize b
        EOr a b               -> 1 + expSize a + expSize b
        EImplies a b          -> 1 + expSize a + expSize b
        EEquiv a b            -> 1 + expSize a + expSize b
        ENot a                -> 1 + expSize a
        EEqual a b            -> 1 + expSize a + expSize b
        EForAll v dom a       -> 2 + expSize a
        EExists v dom a       -> 2 + expSize a
        ECtx a _              -> varassignsize + expSize a
        ENull                 -> 0
        _                     -> 1
        
ruleApplCost :: TRCostLkup -> TState -> TRule -> TSwitchTpl -> TProofSize
ruleApplCost costs (dm, sm, wm) rule (_, _, u) =
    case u of
        SIZE_ -> expSize wm
        WEIGHT_ -> ruleCost costs rule
        VOLUME_ -> ruleCost costs rule * expSize wm

proofsize :: TRCostLkup -> TSwitchTpl -> TProof -> TProofSize
proofsize _ _ FoundSol = 0
proofsize _ _ (CutOff ps) = ps
proofsize costs st (Rule (state, rule, proof)) = ruleApplCost costs state rule st + proofsize costs st proof 

pathCost :: TRCostLkup -> TState -> TProofSize -> TRule -> TSwitchTpl -> TProofSize
pathCost costs state g rule st@(l, _, _) = 
    case l of
        LENGTH_SEARCH_ -> 1 + g
        _              -> ruleApplCost costs state rule st + g

prooflength :: TProof -> Int
prooflength FoundSol = 0
prooflength (CutOff _) = 0
prooflength (Rule (_, rule, proof)) =
    1 + prooflength proof

smallest :: TRCostLkup -> [TProof] -> TSwitchTpl -> [TProof]
smallest _ [] _ = []
smallest costs proofs st@(l, _, _) = 
    if l == LENGTH_SEARCH_
        then proofs
        else let vals = validSolutions proofs
                 sizelkup = if null vals
                                then zip (map (proofsize costs st) proofs) proofs
                                else zip (map (proofsize costs st) vals) vals
                 minsize  = minimum $ map fst sizelkup
             in map snd $ filter ((==minsize).fst) sizelkup

getPatternMapping :: Exp -> Exp -> TPatternMap -> Err TPatternMap
getPatternMapping (ENot e) (ENot p) c = getPatternMapping e p c
getPatternMapping (EEquiv e1 e2) (EEquiv p1 p2) c =
    do m1 <- getPatternMapping e1 p1 c
       m2 <- getPatternMapping e2 p2 m1
       return m2
getPatternMapping (EImplies e1 e2) (EImplies p1 p2) c = 
    do m1 <- getPatternMapping e1 p1 c
       m2 <- getPatternMapping e2 p2 m1
       return m2
getPatternMapping (EOr e1 e2) (EOr p1 p2) c = 
    do m1 <- getPatternMapping e1 p1 c
       m2 <- getPatternMapping e2 p2 m1
       return m2
getPatternMapping (EAnd e1 e2) (EAnd p1 p2) c = 
    do m1 <- getPatternMapping e1 p1 c
       m2 <- getPatternMapping e2 p2 m1
       return m2
getPatternMapping (EForAll (Var v1) dom1 e) (EForAll (Var v2) dom2 p) c =
    case lookup v2 c of
        Just exp -> if exp == Quantvar (Var v1) && (dom1 == dom2 || dom2 == EmptyOptDom)
                        then getPatternMapping e p c
                        else Bad "no match"
        _        -> if any (==Quantvar (Var v1)) (map snd c) || (dom1 /= dom2 && dom2 /= EmptyOptDom)
                        then Bad $ "non unique variable"
                        else getPatternMapping e p (("@", Domain dom1):(v2, Quantvar (Var v1)):(filter ((/="@").fst) c))
getPatternMapping (EExists (Var v1) dom1 e) (EExists (Var v2) dom2 p) c =
    case lookup v2 c of
        Just exp -> if exp == Quantvar (Var v1) && (dom1 == dom2 || dom2 == EmptyOptDom)
                        then getPatternMapping e p c
                        else Bad "no match"
        _        -> if any (==Quantvar (Var v1)) (map snd c) ||  (dom1 /= dom2 && dom2 /= EmptyOptDom)
                        then Bad $ "non unique variable"
                        else getPatternMapping e p (("@", Domain dom1):(v2, Quantvar (Var v1)):(filter ((/="@").fst) c))
getPatternMapping (ECtx e1 ec1) (ECtx p1 pc1) c =
    do m1 <- if ec1 == pc1
                 then getPatternMapping e1 p1 c
                 else Bad "vcontext mismatch"
       return m1
getPatternMapping EFalse EFalse c = return c
getPatternMapping ETrue ETrue c = return c
getPatternMapping e (EPattern (Pattern v)) c =
    let patternLkUp = (\var ctx -> case (lookup var ctx) of
                                       Just (FullExp exp) -> 
                                           if exp == e 
                                               then return c 
                                               else case (exp, e) of
                                                   (ESensVar (Ident v1) _, ESensVar (Ident v2) _) -> if v1 == v2 
                                                                                                         then return c
                                                                                                         else Bad $ "no match"
                                                   _                                              -> Bad "no match"
                                       Just (Quantvar qv) -> error "error in getPatternMapping"
                                       Nothing -> {- if expSize e > 3
                                                      then Bad "Subexp too large"
                                                      else -} return $ (v, FullExp e):c)
    in patternLkUp v c 
getPatternMapping _ _ _ = Bad "no match"

makeProof :: TState -> [TProof] -> TRule -> [TProof]
makeProof state as rule = 
    [Rule (state, rule, a) | a <- as]

replace :: TPatternMap -> Exp -> Exp
replace m p = 
    case p of
        EEquiv a b            -> EEquiv (replace m a) (replace m b)
        EImplies a b          -> EImplies (replace m a) (replace m b)
        EOr a b               -> EOr (replace m a) (replace m b)
        EAnd a b              -> EAnd (replace m a) (replace m b)
        ENot a                -> ENot (replace m a)
        EForAll (Var v) dom a -> case lookup v m of
                                     Just (Quantvar qv) -> case lookup "@" m of
                                                               Just (Domain domp) -> EForAll qv domp (replace m a)
                                                               _         -> error "error in replace 1"
                                     _                  -> error "error in replace 2" 
        EExists (Var v) dom a -> case lookup v m of
                                     Just (Quantvar qv) -> case lookup "@" m of
                                                               Just (Domain domp) -> EExists qv domp (replace m a)
                                                               _         -> error "error in replace 1"
                                     _                  -> error "error in replace 2"
        
        ECtx a c              -> ECtx (replace m a) c
        EPattern (Pattern v)  -> case lookup v m of
                                     Just (FullExp e)  -> e
                                     _                 -> error "error in replace 3"
        n                  -> n

exprewrite :: Exp -> [Exp] -> [Exp]
exprewrite e ps = 
    let mappings = [(getPatternMapping e a [], b) | EEquiv a b <- ps]
    -- let mappings = (parMap rdeepseq ) (\(EEquiv a b) -> (getPatternMapping e a [], b)) [e | e@(EEquiv a b) <- ps]
    in [replace m b' | (Ok m, b') <- mappings]

couple :: String -> [String] -> [(String, String)]
couple s1 [] = []
couple s1 (s:ss) = (s, s1):(couple s1 ss)

updateDiscovered :: TDiscovered -> (String, Exp) -> [(String, Exp)] -> TDiscovered
updateDiscovered (discovered, aux, occmp) (s1, a) smap =
    let equalsa = map fst $ filter ((==a).snd) smap
    in ((couple s1 equalsa) ++ discovered, aux, occmp)

substForSensVars' :: [(Exp, Int)] -> TSensMap -> [TExpOccPair] -> Exp -> (Exp, TSensMap)
substForSensVars' _ s [] e = (e, s)
substForSensVars' lkup s@(sv:svs, smap, (d, aux, occsmp)) ((sub, idx):subs) e =
    let occs = case lookup sub lkup of
                   Just occs -> occs
                   _         -> 0
        newlkup = (sub, occs + 1) : (deleteBy (\a b -> fst a == fst b) (sub, idx) lkup)
        newexp = subst (ESensVar (Ident sv) 1) [(sub, idx - occs)] e
        varsubpair = (sv, sub)
        discovered = (d, aux, (sv, 1):occsmp)
    in substForSensVars' newlkup (svs, varsubpair:smap, updateDiscovered discovered varsubpair smap) subs newexp

substForSensVars :: TSensMap -> [TExpOccPair] -> Exp -> (Exp, TSensMap)
substForSensVars s subs e = substForSensVars' [] s subs e

mergeComprLists :: [[TExpOccPair]] -> [[TExpOccPair]] -> [[TExpOccPair]]
mergeComprLists lst1 lst2 = tail $ [a ++ b | a <- []:lst1, b <- []:lst2]
     
getComprCombs' :: [TExpOccPair] -> Exp -> ([TExpOccPair], [[TExpOccPair]])
getComprCombs' lkup e =
    let (pt1, pt2) = span ((/=e).fst) lkup
        (newlkup, anexp) = if null pt2 
                               then ((e, 1) : lkup, (e, 1))
                               else let (e', i) : pt2rest = pt2
                                    in (pt1 ++ [(e', i + 1)] ++ pt2rest, (e', i + 1))
                                    
    in case e of
           EAnd a b        -> let (lkup1, subexps1) = getComprCombs' newlkup a
                                  (lkup2, subexps2) = getComprCombs' lkup1 b
                                  merged = mergeComprLists subexps1 subexps2
                              in (lkup2, [[anexp]]  ++ merged)
           EOr a b         -> let (lkup1, subexps1) = getComprCombs' newlkup a
                                  (lkup2, subexps2) = getComprCombs' lkup1 b
                                  merged = mergeComprLists subexps1 subexps2
                              in (lkup2, [[anexp]] ++ merged)
           EImplies a b    -> let (lkup1, subexps1) = getComprCombs' newlkup a
                                  (lkup2, subexps2) = getComprCombs' lkup1 b
                                  merged = mergeComprLists subexps1 subexps2
                              in (lkup2,  [[anexp]] ++ merged)
           EEquiv a b      -> let (lkup1, subexps1) = getComprCombs' newlkup a
                                  (lkup2, subexps2) = getComprCombs' lkup1 b
                                  merged = mergeComprLists subexps1 subexps2
                              in (lkup2, [[anexp]] ++ merged)
           ENot a          -> let (lkup1, subexps1) = getComprCombs' newlkup a
                              in (lkup1, [anexp]:subexps1)
           EForAll v dom a -> let (lkup1, subexps1) = getComprCombs' newlkup a
                              in (lkup1, [anexp]:subexps1)
           EExists v dom a -> let (lkup1, subexps1) = getComprCombs' newlkup a
                              in (lkup1, [anexp]:subexps1)
           ECtx a c        -> let (lkup1, subexps1) = getComprCombs' newlkup a
                              in (lkup1, [anexp]:subexps1)
           _               -> (lkup, [])

getComprCombs :: Exp -> [[TExpOccPair]]
getComprCombs e = tail $ snd $ getComprCombs' [] e 

createAuxTree :: Exp -> TAuxTree
createAuxTree e =
    case e of
        EAnd a b             -> Node ("*", 1) (createAuxTree a) (createAuxTree b)
        EOr a b              -> Node ("*", 1) (createAuxTree a) (createAuxTree b)
        EImplies a b         -> Node ("*", 1) (createAuxTree a) (createAuxTree b)
        EEquiv a b           -> Node ("*", 1) (createAuxTree a) (createAuxTree b)
        ENot a               -> Node ("*", 1) (createAuxTree a) TNull
        EForAll v dom a      -> Node ("*", 1) (createAuxTree a) TNull
        EExists v dom a      -> Node ("*", 1) (createAuxTree a) TNull
        ECtx a c             -> Node ("*", 1) (createAuxTree a) TNull
        ESensVar (Ident v) i -> Node (v, 1) TNull TNull
        _                    -> TNull

substAuxTree :: TAuxTree -> (String, Int) -> TAuxTree -> TAuxTree
substAuxTree  toadd (v, i) (Node (val, occ) left right) =
    if val == v
        then toadd
        else (Node (val, occ) (substAuxTree toadd (v, i) left) (substAuxTree toadd (v, i) right))
substAuxTree _ _ _ = error "error in substAuxTree"

updateAux :: (String, Int) -> (String, Int) -> TAuxTree -> TAuxTree
updateAux (v1, i1) (v2, i2) (Node (val, occ) left right) =
    if (v2, i2) == (val, occ)
        then (Node (v1, i1) left right) 
        else (Node (val, occ) (updateAux (v1, i1) (v2, i2) left) (updateAux (v1, i1) (v2, i2) right))
updateAux _ _ TNull = TNull

----------------------
--  Inference rules --
----------------------

recall :: [TRule] -> Ctx -> WM -> TProofSize -> TProofSize -> TResult
recall usedrules c@(Ctx (_, (ts, nts, cs, ncs, _, _), ps, _, _, sargs, (_, costs), _, _)) e g d =
    let (matches, rule) = case ps of
                              T -> if not (elem TRecall usedrules)
                                       then ([], TRecall)
                                       else ([getPatternMapping e p [] | p <- ts], TRecall)
                              NT -> if not (elem CRecall usedrules)
                                        then ([], CRecall)
                                        else ([getPatternMapping e p [] | p <- nts], CRecall)
    in if not (null [m | Ok m <- matches]) 
           then (Passed, makeProof (e, ENull, e) [FoundSol] rule)
           else (Passed, [])

strengthen :: Ctx -> WM -> TProofSize -> TProofSize -> TResult
strengthen c@(Ctx ((_, smap, _), memsegs, ps, _, _, sargs, (_, costs), model, _)) e g d =
       let (ts, nts, cs, ncs, rws, (strns, _)) = memsegs
           rewrittenexps = justLift [case getPatternMapping e b [] of
                                         Ok m -> Just $ replace m a
                                         _    -> Nothing | (a, b) <- strns]
           res' = [re | re <- rewrittenexps, not $ opposite smap model re ps]
           res = (parMap rdeepseq)
                   (\re -> let  state = (EImplies re e, ENull, e)
                                goal = insertAssignMaps re
                                (rt, solved) = solve c goal (pathCost costs state g Strengthen sargs) d
                           in (rt, makeProof state solved Strengthen)) res'
       in if null res
              then (Passed, [])
              else if all (== OPrune) (map fst res)
                      then (OPrune, [])
                      else (Passed, concat (map snd res))

-- opposite :: [(String, Exp)] -> TModel -> Exp -> TPsystem -> Bool
        --ECtx a c@((Ctxt (Var vc) i):c')  ->
        --        case lookup "@" m of
        --            Just (Domain (OptDom domp)) -> if elem i domp
        --                                    then ECtx (replace m a) c
        --                                    else error "error in replace 4"
        --            _       -> error "error in replace 3"
validDomain :: TPatternMap -> Exp -> Bool
validDomain m exp =
    case exp of
        ECtx a c@((Ctxt (Var vc) i):c')  ->
                case lookup "@" m of
                    Just (Domain (OptDom domp)) -> if elem i domp
                                            then True
                                            else False
                    _       -> True
        _ -> True
weaken :: Ctx -> WM -> TProofSize -> TProofSize -> TResult
weaken c@(Ctx ((_, smap, _), memsegs, ps, _, _, sargs, (_, costs), model, _)) e g d =
       let (ts, nts, cs, ncs, rws, (_, weaks)) = memsegs
           rewrittenexps = justLift [case getPatternMapping e a [] of
                                         Ok m -> Just $ (replace m b, m)
                                         _    -> Nothing | (a, b) <- (weaks)]
           res' = [re | (re, m) <- rewrittenexps, not $ opposite smap model re ps, validDomain m re]
           res = (parMap rdeepseq)
                   (\re -> let state = (EImplies re e, ENull, e)
                               goal = insertAssignMaps re
                               (rt, solved) = solve c goal (pathCost costs state g Weaken sargs) d
                           in (rt, makeProof state solved Weaken))   res'
       in if null res
             then (Passed, [])
             else if all (== OPrune) (map fst res)
                      then (OPrune, [])
                      else (Passed, concat (map snd res))

makeRewriteProof :: [TRule] -> Ctx -> WM -> TProofSize -> TProofSize -> [TExpOccPair] -> [(Exp, [Exp])] -> TResult
makeRewriteProof usedrules c@(Ctx (_, _, ps, _, _, sargs, (_, costs), _, _)) e g d ansub erwrtlkup = 
       let rm   = [((se, idx), lookup se erwrtlkup) | (se, idx) <- ansub]
           rm'  = [(se, m) | (se, Just m) <- rm]
           rm'' = concat [[(se, m') | m' <- m] | (se, m) <- rm']
           fappl = (\(rt, proofs) (rt2, solved, se, substexp) ->
                        if not (rt == OPrune)
                            then let state = (EEquiv (fst se) substexp, ENull, e)
                                 in (rt2, (makeProof state solved Rewrite) ++ proofs)
                            else (OPrune, []))
           rmnew = (parMap rdeepseq)
                    (\(se, substexp) -> 
                        let substedexp = subst substexp [se] e
                            goal = insertAssignMaps substedexp
                            state = (EEquiv (fst se) substexp, ENull, e)
                            (rt2, solved) = solve c goal (pathCost costs state g Rewrite sargs) d
                        in (rt2, solved, se, substexp)
                    ) rm''
       in foldl fappl (Passed, []) rmnew

track' :: TExpOccPair -> Exp -> Int -> Bool -> (Bool, Int)
track' (sub, idx) e i deep =
    if sub == e && deep
        then (idx == i, i + 1)
    else case e of        
             EOr ETrue a -> if sub == a
                                then (idx == i, i + 1)
                                else track' (sub, idx) a i True
             EOr a ETrue -> if sub == a
                                then (idx == i, i + 1)
                                else track' (sub, idx) a i True
             EAnd EFalse a -> if sub == a
                                  then (idx == i, i + 1)
                                  else track' (sub, idx) a i True
             EAnd a EFalse -> if sub == a
                                  then (idx == i, i + 1)
                                  else track' (sub, idx) a i True
             EImplies EFalse a -> if sub == a
                                      then (idx == i, i + 1)
                                      else track' (sub, idx) a i True
             EAnd a b             -> let (b1, i1) = track' (sub, idx) a i deep
                                         (b2, i2) = track' (sub, idx) b i1 deep
                                     in (b1 || b2, i2)
             EOr a b              -> let (b1, i1) = track' (sub, idx) a i deep
                                         (b2, i2) = track' (sub, idx) b i1 deep
                                     in (b1 || b2, i2)
             EImplies a b         -> let (b1, i1) = track' (sub, idx) a i deep
                                         (b2, i2) = track' (sub, idx) b i1 deep
                                in (b1 || b2, i2)
             EEquiv a b           -> let (b1, i1) = track' (sub, idx) a i deep
                                         (b2, i2) = track' (sub, idx) b i1 deep
                                     in (b1 || b2, i2)
             ENot a               -> track' (sub, idx) a i deep
             EForAll v dom a      -> track' (sub, idx) a i deep
             EExists v dom a      -> track' (sub, idx) a i deep
             ECtx a c             -> track' (sub, idx) a i deep
             _                    -> (False, i)

track :: TExpOccPair -> Exp -> Bool
track sub e = fst $ track' sub e 1 False

rewrites :: [TRule] -> Ctx -> WM -> TProofSize -> TProofSize -> TResult
rewrites usedrules c@(Ctx ((_, smp, _), (ts, nts, cs, ncs, rws, _), _, (sml, wml), _, _, _, _, _)) e g d = 
       let ansubs' = subformulas e False
           ansubs = [sub | sub <- ansubs', not $ track sub e] -- || (isDelta smp (fst sub) && expSize (expandedExp smp (fst sub)) <= sml) ] 
           allsubs = nub $ map fst ansubs                                
       in if elem Rewrite usedrules
              then let rewritelkup = [(e', nub (exprewrite e' rws)) | e' <- allsubs]
                   in makeRewriteProof usedrules c e g d ansubs rewritelkup
              else (Passed, [])

formulaInspect :: Ctx -> WM -> TProofSize -> TProofSize -> TResult
formulaInspect (Ctx (s@(svs, smp, (discovered, aux, occmp)), memsegs, ps, (sml, wml), pws, sargs, pm@(_, costs), model, coffs)) e g d =
    let fappl1 = (\ret (sensvar, idx) ->
                     let (rt, proofs) = ret
                     in if not (rt == OPrune)
                           then    let expsens = case lookup sensvar smp of
                                                     Just exp -> exp 
                                                     _        -> error "error in inspect"
                                       substedexps = [substForSensVars s es expsens | es <- getComprCombs expsens] ++ [(expsens, s)]
                                       iteratewms = [let substsubexp = subst e' [(ESensVar (Ident sensvar) 1, idx)] e
                                                         goal = insertAssignMaps substsubexp
                                                         newAuxTree = goal `par` substAuxTree (createAuxTree e') (sensvar, 1) aux
                                                         (svs', smp', (discovered', _, occmp')) = nsmap
                                                         nsmap' = (svs', smp', (discovered', newAuxTree, occmp'))
                                                     in (goal, (nsmap', e')) | (e', nsmap) <- substedexps, expSize e' <= sml - 2]
                                       prunewms = [(goals, nsmp) | (goals, ((_, nsmp, _), _)) <- iteratewms]
                                       fappl2 = 
                                           (\ret2 (preswm, (ns, sense')) ->
                                               let (rt2, proofs2) = ret2
                                               in  if not (rt2 == OPrune)
                                                       then    let (_, nsmap, _) = ns
                                                                   state = (ENull, EEqual (ESensVar (Ident sensvar) 1) sense', e)
                                                                   (rt3, solved) = solve (Ctx (ns, memsegs, ps, (sml, wml), pws ++ 
                                                                                      (delete (preswm, nsmap) prunewms), sargs, pm, model, coffs)) 
                                                                                      preswm 
                                                                                      (pathCost costs state g FormInspect sargs) d
                                                               in (rt3, (makeProof state solved FormInspect) ++ proofs2)
                                                       else (OPrune, []))
                                       (rt4, proofs3) = foldl fappl2  (Passed, []) iteratewms
                                   in (rt4, proofs ++ proofs3)
                           else (OPrune, []))
    in foldl fappl1 (Passed, []) (indexedList $ [v | v <- getVars' True e, case lookup v smp of
                                                                                        Just sexp -> (not $ isDelta smp sexp) || expSize sexp > sml
                                                                                        _ -> error "error in formula inspect"])

isConst :: Atom -> Bool
isConst (AConst _) = True
isConst _          = False

myAnd :: Maybe Bool -> Maybe Bool -> Maybe Bool
myAnd (Just a) (Just b) = Just (a && b)
myAnd _ _ = Nothing

myOr :: Maybe Bool -> Maybe Bool -> Maybe Bool
myOr (Just a) (Just b) = Just (a || b)
myOr Nothing (Just True) = Just True
myOr (Just True) Nothing = Just True
myOr _ _ = Nothing

myNot :: Maybe Bool -> Maybe Bool
myNot (Just a) = Just (not a)
myNot Nothing = Nothing

myEqual :: Maybe Bool -> Maybe Bool -> Maybe Bool
myEqual (Just a) (Just b) = Just (a == b)
myEqual _ _ = Nothing

myAll :: (Exp -> Maybe Bool) -> [Exp] -> Maybe Bool
myAll f exps = let answerlst = map f exps
               in if any (== Nothing) answerlst then Nothing else Just $ all (== Just True) $ answerlst

myAny :: (Exp -> Maybe Bool) -> [Exp] -> Maybe Bool
myAny f exps = let answerlst = map f exps
               in if any (== Nothing) answerlst && all (/= Just True) answerlst
                      then Nothing 
                      else Just $ any (== Just True) $ answerlst
        
eval' :: TModel -> Exp -> Maybe Bool
eval' model e =
    case e of
        EAnd a b             -> eval' model a `myAnd` eval' model b
        EOr a b              -> eval' model a `myOr` eval' model b
        EImplies a b         -> eval' model b `myOr` myNot (eval' model a)
        EEquiv a b           -> eval' model a `myEqual` eval' model b
        ENot a               -> myNot $ eval' model a
        EForAll (Var v) (OptDom domp) a -> 
            myAll (eval' model) $ nub [insertConst (ECtx a [Ctxt (Var v) i]) [] | i <- domp]
        EExists (Var v) (OptDom domp) a -> 
            myAny (eval' model) $ nub [insertConst (ECtx a [Ctxt (Var v) i]) [] | i <- domp]
        EPred pred           -> case pred of
                                    EPRed (AConst x) -> if checkNodeColor model (fromInteger x) "red" == ETrue
                                                            then Just True 
                                                            else Just False
                                    EPBlue (AConst x) -> if checkNodeColor model (fromInteger x) "blue" == ETrue
                                                             then Just True
                                                             else Just False
                                    EPYellow (AConst x) -> if checkNodeColor model (fromInteger x) "yellow" == ETrue
                                                               then Just True
                                                               else Just False
                                    EPEdge (AConst x) (AConst y) -> if checkEdge model (fromInteger x) (fromInteger y) == ETrue 
                                                                        then Just True
                                                                        else Just False
                                    _ -> Nothing
        ETrue                -> Just True
        EFalse               -> Just False
        _                    -> Nothing

eval :: TModel -> Exp -> Maybe Exp
eval model e =
    case eval' model e of
        Just True -> Just ETrue
        Just False -> Just EFalse
        _          -> Nothing
       
inspectDomain :: TModel -> Exp -> Exp
inspectDomain model e = 
    case e of
        EAnd a b             -> EAnd (inspectDomain model a) (inspectDomain model b)
        EOr a b              -> EOr (inspectDomain model a) (inspectDomain model b)
        EImplies a b         -> EImplies (inspectDomain model a) (inspectDomain model b)
        EEquiv a b           -> EEquiv (inspectDomain model a) (inspectDomain model b)
        ENot a               -> ENot (inspectDomain model a)
        EForAll v domp a     -> EForAll v (if domp == EmptyOptDom then (dom model) else domp) (inspectDomain model a)
        EExists v domp a     -> EExists v (if domp == EmptyOptDom then (dom model) else domp) (inspectDomain model a)
        a                    -> a

insertAssignMaps :: Exp -> WM
insertAssignMaps e = 
    insertConst e []

insertConst :: Exp -> [(String, Integer)] -> Exp
insertConst e varconsts =
    case e of
        ECtx a ctx -> insertConst a (sort $ [(v, i) | Ctxt (Var v) i <- ctx] ++ varconsts)
        EAnd a b         -> EAnd (insertConst a varconsts) (insertConst b varconsts)
        EOr a b          -> EOr (insertConst a varconsts) (insertConst b varconsts)
        EImplies a b     -> EImplies (insertConst a varconsts) (insertConst b varconsts)
        EEquiv a b       -> EEquiv (insertConst a varconsts) (insertConst b varconsts)
        ENot a           -> ENot (insertConst a varconsts)
        EForAll (Var qv) dom a -> EForAll (Var qv) dom (insertConst a varconsts)
        EExists (Var qv) dom a -> EExists (Var qv) dom (insertConst a varconsts)
        ESensVar v i     -> if null varconsts then ESensVar v i else ECtx (ESensVar v i) [Ctxt (Var v) i | (v, i) <- varconsts]
        EPred pred       -> case pred of
                                EPRed (AVar (Var qv)) -> 
                                    case lookup qv varconsts of
                                        Just val -> EPred $ EPRed (AConst val) 
                                        _        -> EPred pred
                                EPBlue (AVar (Var qv)) -> 
                                    case lookup qv varconsts of
                                        Just val -> EPred $ EPBlue (AConst val) 
                                        _        -> EPred pred
                                EPYellow (AVar (Var qv)) -> 
                                    case lookup qv varconsts of
                                        Just val -> EPred $ EPYellow (AConst val) 
                                        _        -> EPred pred
                                EPEdge (AVar (Var qv)) (AVar (Var qv2)) -> 
                                    case (lookup qv varconsts, lookup qv2 varconsts) of
                                        (Just val1, Just val2) -> EPred $ EPEdge (AConst val1) (AConst val2)
                                        (Nothing, Just val2)   -> EPred $ EPEdge (AVar (Var qv)) (AConst val2) 
                                        (Just val1, Nothing)   -> EPred $ EPEdge (AConst val1) (AVar (Var qv2))
                                        _                      -> EPred pred
                                EPEdge (AVar (Var qv)) y -> 
                                    case lookup qv varconsts of
                                        Just val -> EPred $ EPEdge (AConst val) y
                                        _        -> EPred pred
                                EPEdge x (AVar (Var qv)) -> 
                                    case lookup qv varconsts of
                                        Just val -> EPred $ EPEdge x (AConst val)
                                        _        -> EPred pred
                                _  -> EPred pred
        a                -> a

forallReduce :: Ctx -> WM -> TProofSize -> TProofSize -> TResult
forallReduce c@(Ctx (_, _, ps, _, _, sargs, (_, costs), model, _)) e g d =
       let sl = concat [[(ts, (EAnd (EForAll (Var v) (OptDom (dom \\ [i])) x) (ECtx x [Ctxt (Var v) i]))) | i <- dom] 
                                      | ts@((EForAll (Var v) (OptDom dom) x), idx) <- subformulas e False, not $ null dom || track ts e]
           substedlst = [(subst se [ts] e) | (ts, se) <- sl]
           fappl = (\(rt, proofs) sexp ->
                          if not (rt == OPrune)
                               then    let goal = insertAssignMaps sexp
                                           state = (ENull, ENull, e)
                                       
                                           (rt2, solved) = solve c goal (pathCost costs state g AReduce sargs) d
                                       in (rt2, (makeProof state solved AReduce) ++ proofs)
                               else (OPrune, []))
       in foldl fappl (Passed, []) substedlst

forallReducePlus :: Ctx -> WM -> TProofSize -> TProofSize -> TResult
forallReducePlus c@(Ctx ((sv:svs, smp, disc), msegs, ps, ms, pwm, sargs, (rules, costs), model, cutoff)) e g d =
       let sl = concat [if length dom > 1
                            then [(ts, (svs, (sv, (EForAll (Var v) (OptDom (dom \\ [i])) x)):smp, disc), EAnd (ESensVar (Ident sv) 1) (ECtx x [Ctxt (Var v) i])) | i <- dom]
                            else [(ts, (sv:svs, smp, disc), ECtx x [Ctxt (Var v) i]) | i <- dom]
                                      | ts@((EForAll (Var v) (OptDom dom) x), idx) <- subformulas e False, not $ null dom || track ts e]
           substedlst = [(subst se [ts] e, nsmp) | (ts, nsmp, se) <- sl]
           fappl = (\(rt, proofs) (sexp, nsmp) ->
                        if not (rt == OPrune)
                               then    let goal = insertAssignMaps sexp
                                           (sv':svs', smp', _) = nsmp
                                           state = (if sv' == sv
                                                        then ENull
                                                        else EEquiv (ESensVar (Ident (fst (head smp'))) 1) (snd (head smp')), ENull, e)
                                           newc = Ctx (nsmp, msegs, ps, ms, pwm, sargs, (rules, costs), model, cutoff)
                                           (rt2, solved) = solve newc goal (pathCost costs state g AReduceP sargs) d
                                       in (rt2, (makeProof state solved AReduceP) ++ proofs)
                               else (OPrune, [])  )
       in foldl fappl (Passed, []) substedlst

existsReduce :: Ctx -> WM -> TProofSize -> TProofSize -> TResult
existsReduce c@(Ctx (_, _, ps, _, _, sargs, (_, costs), model, _)) e g d =
       let sl = concat [[(ts, (EOr (EExists (Var v) (OptDom (dom \\ [i])) x) (ECtx x [Ctxt (Var v) i]))) | i <- dom] 
                                      | ts@((EExists (Var v) (OptDom dom) x), idx) <- subformulas e False, not $ null dom || track ts e]
           substedlst = [(subst se [ts] e) | (ts, se) <- sl]
           fappl = (\(rt, proofs) sexp ->
                        if not (rt == OPrune)
                               then    let goal = insertAssignMaps sexp
                                           state = (ENull, ENull, e)
                                           (rt2, solved) = solve c goal (pathCost costs state g EReduce sargs) d
                                       in (rt2, (makeProof state solved EReduce) ++ proofs)
                               else (OPrune, []))
       in foldl fappl (Passed, []) substedlst
       
existsReducePlus :: Ctx -> WM -> TProofSize -> TProofSize -> TResult
existsReducePlus c@(Ctx ((sv:svs, smp, disc), msegs, ps, ms, pwm, sargs, (rules, costs), model, cutoff)) e g d =
       let sl = concat [if length domp > 1
                            then [(ts, (svs, (sv, (EExists (Var v) (OptDom (domp \\ [i])) x)):smp, disc), EOr (ESensVar (Ident sv) 1) (ECtx x [Ctxt (Var v) i])) | i <- domp]
                            else [(ts, (sv:svs, smp, disc), ECtx x [Ctxt (Var v) i]) | i <- domp]
                                      | ts@((EExists (Var v) (OptDom domp) x), idx) <- subformulas e False, not $ null domp || track ts e]
           substedlst = [(subst se [ts] e, nsmp) | (ts, nsmp, se) <- sl]
           fappl = (\(rt, proofs) (sexp, nsmp) ->
                        if not (rt == OPrune)
                               then    let goal = insertAssignMaps sexp
                                           (sv':svs', smp', _) = nsmp
                                           state = (if sv' == sv
                                                        then ENull
                                                        else EEquiv (ESensVar (Ident (fst (head smp'))) 1) (snd (head smp')), ENull, e)
                                           newc = Ctx (nsmp, msegs, ps, ms, pwm, sargs, (rules, costs), model, cutoff)
                                           (rt2, solved) = solve newc goal (pathCost costs state g EReduceP sargs) d
                                       in (rt2, (makeProof state solved EReduceP) ++ proofs)
                               else (OPrune, [])  )
       in foldl fappl (Passed, []) substedlst

varPresent :: String -> Exp -> Bool
varPresent v e =
    case e of
        EAnd a b             -> varPresent v a || varPresent v b
        EOr a b              -> varPresent v a || varPresent v b
        EImplies a b         -> varPresent v a || varPresent v b
        EEquiv a b           -> varPresent v a || varPresent v b
        ENot a               -> varPresent v a
        EForAll qv domp a     -> varPresent v a
        EExists qv domp a     -> varPresent v a
        ECtx a _             -> varPresent v a
        EPred (EPBlue (AVar (Var pv))) -> v == pv
        EPred (EPRed (AVar (Var pv))) -> v == pv
        EPred (EPYellow (AVar (Var pv))) -> v == pv
        EPred (EPEdge (AVar (Var pv1)) (AVar (Var pv2))) -> v == pv1 || v == pv2
        EPred (EPEdge (AVar (Var pv1)) _) -> v == pv1
        EPred (EPEdge _ (AVar (Var pv2))) -> v == pv2
        ESensVar _ _ -> True
        _ -> False

forallDistribute :: Ctx -> WM -> TProofSize -> TProofSize -> TResult
forallDistribute c@(Ctx ((_, smap, _), _, ps, _, _, sargs, (_, costs), model, _)) e g d =
       let qes = [ts | ts@((EForAll _ (OptDom dom) _), idx) <- subformulas e False, not $ null dom || track ts e]
           dexps = concat [case qexp of
                               EForAll (Var v) d (EAnd a b) -> if not (varPresent v a || varPresent v b)
                                                                   then []
                                                                   else  [(ts, EAnd (if varPresent v a
                                                                                         then (EForAll (Var v) d a)
                                                                                         else a)
                                                                                    (if varPresent v b
                                                                                         then (EForAll (Var v) d b)
                                                                                         else b))]
                               EForAll (Var v) d (EOr a b) -> if not (varPresent v a || varPresent v b)
                                                                  then []
                                                                  else  [(ts, EOr (if varPresent v a
                                                                                       then (EForAll (Var v) d a)
                                                                                       else a)
                                                                                  (if varPresent v b
                                                                                       then (EForAll (Var v) d b)
                                                                                       else b))]
                               EForAll (Var v) d (EImplies a b) -> if not (varPresent v a || varPresent v b)
                                                                       then []
                                                                       else  [(ts, EOr (if varPresent v a
                                                                                            then (EForAll (Var v) d (ENot a))
                                                                                            else a)
                                                                                       (if varPresent v b
                                                                                            then (EForAll (Var v) d b)
                                                                                            else b))]
                               EForAll (Var v) d (ENot a) -> if not (varPresent v a)
                                                                 then []
                                                                 else  [(ts, ENot (EExists (Var v) d a))]
                               _ -> [] | ts@(qexp, idx) <- qes]
           substedlst' = [subst se [ts] e | (ts, se) <- dexps]
           substedlst = [sub | sub <- substedlst', case sub of (EAnd a b) -> True; _ -> not $ opposite smap model sub ps]
           fappl = (\(rt, proofs) (rt2, solved) -> (Passed, (makeProof (ENull, ENull, e) solved ADistribute) ++ proofs))
           substed = (parMap rdeepseq) -- rseq)
                        (\sexp ->
                            let goal = insertAssignMaps sexp
                                state = (ENull, ENull, e)
                            in (solve c goal (pathCost costs state g ADistribute sargs) d) ) substedlst
       in foldl fappl (Passed, []) substed
     
existsDistribute :: Ctx -> WM -> TProofSize -> TProofSize -> TResult
existsDistribute c@(Ctx ((_, smap, _), _, ps, _, _, sargs, (_, costs), model, _)) e g d =
       let qes = [ts | ts@((EExists _ (OptDom dom) _), idx) <- subformulas e False, not $ null dom || track ts e]
           dexps = concat [case qexp of
                               EExists (Var v) d (EAnd a b) -> if not (varPresent v a || varPresent v b)
                                                                   then []
                                                                   else [(ts, EAnd (if varPresent v a
                                                                                        then (EExists (Var v) d a)
                                                                                        else a)
                                                                                   (if varPresent v b
                                                                                        then (EExists (Var v) d b)
                                                                                        else b))]
                               EExists (Var v) d (EOr a b) -> if not (varPresent v a || varPresent v b)
                                                                  then []
                                                                  else [(ts, EOr (if varPresent v a
                                                                                      then (EExists (Var v) d a)
                                                                                      else a)
                                                                                  (if varPresent v b
                                                                                       then (EExists (Var v) d b)
                                                                                       else b))]
                               EExists (Var v) d (EImplies a b) -> if not (varPresent v a || varPresent v b)
                                                                       then []
                                                                       else [(ts, EOr (EExists (Var v) d (ENot a))
                                                                                      (if varPresent v b
                                                                                           then (EExists (Var v) d b)
                                                                                           else b))]
                               EExists (Var v) d (EEquiv a b) -> if varPresent v a && varPresent v b
                                                                     then []
                                                                     else [(ts, EEquiv (if varPresent v a
                                                                                            then (EExists (Var v) d a)
                                                                                            else a)
                                                                                       (if varPresent v b
                                                                                            then (EExists (Var v) d b)
                                                                                            else b))]
                               EExists (Var v) d (ENot a) -> if not $ varPresent v a
                                                                   then []
                                                                   else [(ts, ENot (EForAll (Var v) d a))]
                               _ -> [] | ts@(qexp, idx) <- qes]
           substedlst' = [subst se [ts] e | (ts, se) <- dexps]
           substedlst = [sub | sub <- substedlst', not $ opposite smap model sub ps]
           fappl = (\(rt, proofs) (rt2, solved) -> (Passed, (makeProof (ENull, ENull, e) solved EDistribute) ++ proofs))
           substed = (parMap rdeepseq) -- rseq)
                        (\sexp ->
                            let goal = insertAssignMaps sexp
                                state = (ENull, ENull, e)
                            in (solve c goal (pathCost costs state g EDistribute sargs) d) ) substedlst
       in foldl fappl (Passed, []) substed

isSigma :: [(String, Exp)] -> Exp -> Bool
isSigma smp e =
    case e of
        EExists v dom a      -> isDelta smp a
        a                    -> isDelta smp a

isGamma :: [(String, Exp)] -> Exp -> Bool
isGamma smp e =
    case e of
        EForAll v dom a      -> isDelta smp a
        a                    -> isDelta smp a

isDelta :: [(String, Exp)] -> Exp -> Bool
isDelta smp e =
    case e of
        EAnd a b             -> isDelta smp a && isDelta smp b
        EOr a b              -> isDelta smp a && isDelta smp b
        EImplies a b         -> isDelta smp a && isDelta smp b
        EEquiv a b           -> isDelta smp a && isDelta smp b
        ENot a               -> isDelta smp a
        ECtx a _             -> isDelta smp a
        ESensVar (Ident v) _ -> case lookup v smp of
                                    Just exp -> isDelta smp exp
                                    Nothing  -> error "error in isDelta"
        EForAll v dom a      -> False
        EExists v dom a      -> False
        _                    -> True

isFree :: Pred -> Bool
isFree p =
    case p of
        EPRed x -> isConst x
        EPBlue x -> isConst x
        EPYellow x -> isConst x
        EPEdge x y -> all isConst [x, y]

modelInspect :: Ctx -> WM -> TProofSize -> TProofSize -> TResult
modelInspect c@(Ctx (_, _, _, _, _, sargs, (_, costs), model, _)) e g d =
       let substedexps = [case p of
                              (EPred (EPRed (AConst x)), _) -> subst (checkNodeColor model (fromInteger x) "red") [p] e
                              (EPred (EPYellow (AConst x)), _) -> subst (checkNodeColor model (fromInteger x) "yellow") [p] e
                              (EPred (EPBlue (AConst x)), _) -> subst (checkNodeColor model (fromInteger x) "blue") [p] e
                              (EPred (EPEdge (AConst x) (AConst y)), _) -> subst (checkEdge model (fromInteger x) (fromInteger y)) [p] e
                              _ -> error "error in modelInspect" | p <- subformulas e True, case (fst p) of
                                                                                                EPred pred -> isFree pred
                                                                                                _          -> False]
           fappl = (\ret sexp ->
                        let (rt, proofs) = ret
                        in if not (rt == OPrune)
                               then    let goal = insertAssignMaps sexp
                                           state = (ENull, ENull, e)
                                           (rt2, solved) = solve c goal (pathCost costs state g MInspect sargs) d
                                       in (rt2, (makeProof state solved MInspect) ++ proofs)
                               else (OPrune, []))
       in foldl fappl (Passed, []) substedexps

truthInspect :: Ctx -> WM -> TProofSize -> TProofSize -> TResult
truthInspect c@(Ctx ((_, smp, _), _, ps, _, _, sargs, (_, costs), model, _)) e g d =
       let sl = [subst ETrue [(ts, i)] e | (ts, i) <- subformulas e True, isSigma smp ts &&
                                                                          eval model (insertAssignMaps ts) == Just ETrue &&
                                                                          (not $ track (ts, i) e)]
           fappl = (\(rt, proofs) (rt2, solved) ->
                      if not (rt == OPrune)
                            then (rt2, (makeProof (ENull, ENull, e) solved TInspect) ++ proofs)
                            else (OPrune, [])  )
           sl' = (parMap rdeepseq) -- rseq)
                   (\sexp -> let goal = insertAssignMaps sexp
                                 state = (ENull, ENull, e)
                             in solve c goal (pathCost costs state g TInspect sargs) d)  sl
       in foldl fappl (Passed, []) sl'

falsityInspect :: Ctx -> WM -> TProofSize -> TProofSize -> TResult
falsityInspect c@(Ctx ((_, smp, _), _, ps, _, _, sargs, (_, costs), model, _)) e g d =
       let sl = [subst EFalse [(ts, i)] e | (ts, i) <- subformulas e True, isGamma smp ts && 
                                                                           eval model (insertAssignMaps ts) == Just EFalse &&
                                                                           (not $ track (ts, i) e)]
           fappl = (\(rt, proofs) (rt2, solved) ->
                        if not (rt == OPrune)
                            then (rt2, (makeProof (ENull, ENull, e) solved FInspect) ++ proofs)
                            else (OPrune, []))
           sl' = (parMap rdeepseq) -- rseq)
                   (\sexp -> let goal = insertAssignMaps sexp
                                 state = (ENull, ENull, e)
                             in solve c goal (pathCost costs state g FInspect sargs) d) sl
       in foldl fappl (Passed, []) sl'

piInspect' :: Ctx -> WM -> TProofSize -> TProofSize -> TResult
piInspect' c@(Ctx ((_, smp, _), _, ps, (sml, wml), _, sargs, (_, costs), model, _)) e g d =
       let sl = [(ts, (EForAll (Var v) (OptDom (tail dom)) x))
                                    | ts@(tse@(EForAll (Var v) (OptDom dom) x), idx) <- subformulas e False, 
                                                                                            (not $ null dom || track ts e) && 
                                                                                            isDelta smp x && let expexp = expandedExp smp tse
                                                                                                                 expx = expandedExp smp x 
                                                                                                             in if expSize expx > sml
                                                                                                                    then False
                                                                                                                    else eval model expexp == Just ETrue]
           substedlst = [(subst se [ts] e) | (ts, se) <- sl]
           fappl = (\(rt, proofs) (rt2, solved) ->
                        if not (rt == OPrune)
                            then (rt2, (makeProof (ENull,ENull,e) solved PiInspectP) ++ proofs)
                            else (OPrune, []) )
           substedlst' = (parMap rdeepseq) (\sexp -> solve c (insertAssignMaps sexp) (pathCost costs (ENull,ENull,e) g PiInspectP sargs) d) substedlst
       in foldl fappl (Passed, []) substedlst'

deltaInspect :: Ctx -> WM -> TProofSize -> TProofSize -> TResult
deltaInspect c@(Ctx ((_, smp, _), _, ps, (sml, wml), _, sargs, (_, costs), model, _)) e g d =
       let sl = [(ts, case eval model (expandedExp smp tse) of Just tv -> [tv]; _ -> []) | 
                                            ts@(tse, idx) <- subformulas e True, isDelta smp tse && expSize (expandedExp smp tse) <= sml]
           substedlst = [(subst se [ts] e) | (ts, [se]) <- sl]
           fappl = (\(rt, proofs) (rt2, solved) ->
                        if not (rt == OPrune)
                            then  (rt2, (makeProof (ENull, ENull, e) solved DeltaInspect) ++ proofs)
                            else (OPrune, [])  )
           substedlst' = (parMap rdeepseq) (\sexp -> solve c (insertAssignMaps sexp) (pathCost costs (ENull, ENull, e) g DeltaInspect sargs) d) substedlst
       in foldl fappl (Passed, []) substedlst'

sigmaInspect' :: Ctx -> WM -> TProofSize -> TProofSize -> TResult
sigmaInspect' c@(Ctx ((_, smp, _), _, ps, (sml, wml), _, sargs, (_, costs), model, _)) e g d =
       let sl = [(ts, (EExists (Var v) (OptDom (tail dom)) x))
                                    | ts@(tse@(EExists (Var v) (OptDom dom) x), idx) <- subformulas e False, 
                                                                                            (not $ null dom || track ts e) && 
                                                                                            isDelta smp x && let expexp = expandedExp smp tse
                                                                                                                 expx = expandedExp smp x
                                                                                                             in if expSize expx > sml
                                                                                                                    then False
                                                                                                                    else eval model expexp == Just EFalse]
           substedlst = [(subst se [ts] e) | (ts, se) <- sl]
           fappl = (\(rt, proofs) sexp ->
                        if not (rt == OPrune)
                               then    let goal = insertAssignMaps sexp
                                           state = (ENull, ENull, e)
                                           (rt2, solved) = solve c goal (pathCost costs state g SigmaInspectP sargs) d
                                       in (rt2, (makeProof state solved SigmaInspectP) ++ proofs)
                               else (OPrune, [])  )
       in foldl fappl (Passed, []) substedlst

-- Compute domain in the given model for the given predicate
getDom :: [Integer] -> TModel -> Pred -> Exp -> [Integer]
getDom domp model pred tval =
    [i | i <- domp, case pred of
                        EPBlue _ -> eval model (EPred (EPBlue (AConst i))) == Just tval
                        EPRed _ -> eval model (EPred (EPRed (AConst i))) == Just tval
                        EPYellow _ -> eval model (EPred (EPYellow (AConst i))) == Just tval
                        EPEdge (AVar (Var x)) (AVar (Var y)) ->
                                if x == y then eval model (EPred (EPEdge (AConst i) (AConst i))) == Just tval
                                          else False
                        _ -> False]

-- check if the predicate is a color predicate
isColorPred :: Pred -> Bool
isColorPred pred =
    case pred of
        EPBlue _ -> True
        EPRed _ -> True
        EPYellow _ -> True
        _ -> False

-- Either color predicate or E(x,x), but not E(x,y)
isUnaryPred :: Pred -> Bool
isUnaryPred pred =
    case pred of
        EPBlue _ -> True
        EPRed _ -> True
        EPYellow _ -> True
        EPEdge (AVar (Var x)) (AVar (Var y)) ->
            if x == y then True else False
        EPEdge (AConst i) (AConst j) ->
            if i == j then True else False
        _ -> False


getPredVar :: Pred -> String
getPredVar pred =
    case pred of
        EPBlue (AVar (Var v)) -> v
        EPRed (AVar (Var v)) -> v
        EPYellow (AVar (Var v)) -> v
        EPEdge (AVar (Var x)) (AVar (Var y)) -> if x==y then x else ""
        _ -> ""

sigmaAndComprehension :: Ctx -> WM -> TProofSize -> TProofSize -> TResult
sigmaAndComprehension c@(Ctx (_, _, ps, _, _, sargs, (_, costs), model, _)) e g d =
       let sl1 = concat [let domp = getDom dom' model pred ETrue
                         in if null domp
                             then []
                             else [(ts, EExists (Var v) (OptDom domp) x)]
                                  | ts@(EExists (Var v) (OptDom dom') (EAnd (EPred pred) x), idx) <- subformulas e False, 
                                                                                                          isUnaryPred pred && (not $ null dom')]
           sl2 = concat [let domp = getDom dom' model pred ETrue
                         in if null domp
                             then []
                             else [(ts, EExists (Var v) (OptDom domp) x)] 
                                  | ts@(EExists (Var v) (OptDom dom') (EAnd x (EPred pred)), idx) <- subformulas e False, 
                                                                                                          isUnaryPred pred && (not $ null dom')] 
           sl3 = concat [let domp = getDom dom' model pred EFalse
                         in if null domp
                             then []
                             else [(ts, EExists (Var v) (OptDom domp) x)]
                                  | ts@(EExists (Var v) (OptDom dom') (EAnd (ENot (EPred pred)) x), idx) <- subformulas e False, 
                                                                                                          isUnaryPred pred && (not $ null dom')]
           sl4 = concat [let domp = getDom dom' model pred EFalse
                         in if null domp
                             then []
                             else [(ts, EExists (Var v) (OptDom domp) x)] 
                                  | ts@(EExists (Var v) (OptDom dom') (EAnd x (ENot (EPred pred))), idx) <- subformulas e False, 
                                                                                                          isUnaryPred pred && (not $ null dom')] 
           substedlst = [(subst se [ts] e) | (ts, se) <- sl1 ++ sl2 ++ sl3 ++ sl4]
           fappl = (\(rt, proofs) sexp ->
                        if not (rt == OPrune)
                               then    let goal = insertAssignMaps sexp
                                           state = (ENull, ENull, e)
                                           (rt2, solved) = solve c goal (pathCost costs state g EAndComp sargs) d
                                      in (rt2, (makeProof state solved EAndComp) ++ proofs)
                               else (OPrune, [])  )
       in foldl fappl (Passed, []) substedlst

sigmaOrComprehension :: Ctx -> WM -> TProofSize -> TProofSize -> TResult
sigmaOrComprehension c@(Ctx (_, _, ps, _, _, sargs, (_, costs), model, _)) e g d =
       let sl1 = concat [let domp = getDom dom' model pred ETrue
                         in if not $ null domp
                             then []
                             else [(ts, EExists (Var v) (OptDom dom') x)] 
                                  | ts@(EExists (Var v) (OptDom dom') (EOr (EPred pred) x), idx) <- subformulas e False,
                                                                                                        isUnaryPred pred && (not $ null dom')]
           sl2 = concat [let domp = getDom dom' model pred ETrue
                         in if not $ null domp
                             then []
                             else [(ts, EExists (Var v) (OptDom dom') x)] 
                                  | ts@(EExists (Var v) (OptDom dom') (EOr x (EPred pred)), idx) <- subformulas e False,
                                                                                                         isUnaryPred pred && (not $ null dom')]
           sl3 = concat [let domp = getDom dom' model pred EFalse
                         in if not $ null domp
                             then []
                             else [(ts, EExists (Var v) (OptDom dom') x)] 
                                  | ts@(EExists (Var v) (OptDom dom') (EOr (ENot (EPred pred)) x), idx) <- subformulas e False,
                                                                                                        isUnaryPred pred && (not $ null dom')]
           sl4 = concat [let domp = getDom dom' model pred EFalse
                         in if not $ null domp
                             then []
                             else [(ts, EExists (Var v) (OptDom dom') x)] 
                                  | ts@(EExists (Var v) (OptDom dom') (EOr x (ENot (EPred pred))), idx) <- subformulas e False,
                                                                                                         isUnaryPred pred && (not $ null dom')]

           substedlst = [(subst se [ts] e) | (ts, se) <- sl1 ++ sl2 ++ sl3 ++ sl4]
           fappl = (\(rt, proofs) sexp ->
                        if not (rt == OPrune)
                               then    let goal = insertAssignMaps sexp
                                           state = (ENull, ENull, e)
                                           (rt2, solved) = solve c goal (pathCost costs state g EOrComp sargs) d
                                       in (rt2, (makeProof state solved EOrComp) ++ proofs)
                               else (OPrune, []))
       in foldl fappl (Passed, []) substedlst

piAndComprehension :: Ctx -> WM -> TProofSize -> TProofSize -> TResult
piAndComprehension c@(Ctx (_, _, ps, _, _, sargs, (_, costs), model, _)) e g d =
       let sl1 = concat [let domp = getDom dom' model pred EFalse
                         in if not $ null domp
                             then []
                             else [(ts, EForAll (Var v) (OptDom dom') x)] 
                                  | ts@(EForAll (Var v) (OptDom dom') (EAnd (EPred pred) x), idx) <- subformulas e False,
                                                                                                          isUnaryPred pred && (not $ null dom')]
           sl2 = concat [let domp = getDom dom' model pred EFalse
                         in if not $ null domp
                             then []
                             else [(ts, EForAll (Var v) (OptDom dom') x)] 
                                  | ts@(EForAll (Var v) (OptDom dom') (EAnd x (EPred pred)), idx) <- subformulas e False,
                                                                                                          isUnaryPred pred && (not $ null dom')]
           sl3 = concat [let domp = getDom dom' model pred ETrue
                         in if not $ null domp
                             then []
                             else [(ts, EForAll (Var v) (OptDom dom') x)] 
                                  | ts@(EForAll (Var v) (OptDom dom') (EAnd (ENot (EPred pred)) x), idx) <- subformulas e False,
                                                                                                          isUnaryPred pred && (not $ null dom')]
           sl4 = concat [let domp = getDom dom' model pred ETrue
                         in if not $ null domp
                             then []
                             else [(ts, EForAll (Var v) (OptDom dom') x)] 
                                  | ts@(EForAll (Var v) (OptDom dom') (EAnd x (ENot (EPred pred))), idx) <- subformulas e False,
                                                                                                          isUnaryPred pred && (not $ null dom')]

           substedlst = [(subst se [ts] e) | (ts, se) <- sl1 ++ sl2 ++ sl3 ++ sl4]
           fappl = (\(rt, proofs) sexp ->
                        if not (rt == OPrune)
                               then    let goal = insertAssignMaps sexp
                                           state = (ENull, ENull, e)
                                           (rt2, solved) = solve c goal (pathCost costs state g AAndComp sargs) d
                                       in (rt2, (makeProof state solved AAndComp) ++ proofs)
                               else (OPrune, [])  )
       in foldl fappl (Passed, []) substedlst

piOrComprehension :: Ctx -> WM -> TProofSize -> TProofSize -> TResult
piOrComprehension c@(Ctx (_, _, ps, _, _, sargs, (_, costs), model, _)) e g d =
       let sl1 = concat [let domp = getDom dom' model pred EFalse
                         in if null domp
                             then []
                             else [(ts, EForAll (Var v) (OptDom domp) x)] 
                                  | ts@(EForAll (Var v) (OptDom dom') (EOr (EPred pred) x), idx) <- subformulas e False,
                                                                                                         isUnaryPred pred && (not $ null dom')]
           sl2 = concat [let domp = getDom dom' model pred EFalse
                         in if null domp
                             then []
                             else [(ts, EForAll (Var v) (OptDom domp) x)] 
                                  | ts@(EForAll (Var v) (OptDom dom') (EOr x (EPred pred)), idx) <- subformulas e False,
                                                                                                         isUnaryPred pred && (not $ null dom')]
           sl3 = concat [let domp = getDom dom' model pred ETrue
                         in if null domp
                             then []
                             else [(ts, EForAll (Var v) (OptDom domp) x)] 
                                  | ts@(EForAll (Var v) (OptDom dom') (EOr (ENot (EPred pred)) x), idx) <- subformulas e False,
                                                                                                         isUnaryPred pred && (not $ null dom')]
           sl4 = concat [let domp = getDom dom' model pred ETrue
                         in if null domp
                             then []
                             else [(ts, EForAll (Var v) (OptDom domp) x)] 
                                  | ts@(EForAll (Var v) (OptDom dom') (EOr x (ENot (EPred pred))), idx) <- subformulas e False,
                                                                                                         isUnaryPred pred && (not $ null dom')]

           substedlst = [(subst se [ts] e) | (ts, se) <- sl1 ++ sl2 ++ sl3 ++ sl4]
           fappl = (\(rt, proofs) sexp ->
                        if not (rt == OPrune)
                               then    let goal = insertAssignMaps sexp
                                           state = (ENull, ENull, e)
                                           (rt2, solved) = solve c goal (pathCost costs state g AOrComp sargs) d
                                       in (rt2, (makeProof state solved AOrComp) ++ proofs)
                               else (OPrune, [])  )
       in foldl fappl (Passed, []) substedlst

piArrowComprehension :: Ctx -> WM -> TProofSize -> TProofSize -> TResult
piArrowComprehension c@(Ctx (_, _, ps, _, _, sargs, (_, costs), model, _)) e g d =
       let s1 = concat [let domp = getDom dom' model pred ETrue
                        in if null domp
                            then []
                            else [(ts, EForAll (Var v) (OptDom domp) x)]
                                 | ts@(EForAll (Var v) (OptDom dom') (EImplies (EPred pred) x), idx) <- subformulas e False,
                                                                                                            isUnaryPred pred && (not $ null dom') && (getPredVar pred == v)]
           s2 = concat [let domp = getDom dom' model pred EFalse
                        in if null domp
                            then []
                            else [(ts, EForAll (Var v) (OptDom domp) x)]
                                 | ts@(EForAll (Var v) (OptDom dom') (EImplies (ENot (EPred pred)) x), idx) <- subformulas e False,
                                                                                                            isUnaryPred pred && (not $ null dom') && (getPredVar pred == v)]
           substedlst = [(subst se [ts] e) | (ts, se) <- s1 ++ s2]
           fappl = (\(rt, proofs) sexp ->
                        if not (rt == OPrune)
                               then    let goal = insertAssignMaps sexp
                                           state = (ENull, ENull, e)
                                           (rt2, solved) = solve c goal (pathCost costs state g AArrowComp sargs) d
                                       in (rt2, (makeProof state solved AArrowComp) ++ proofs)
                               else (OPrune, []))
       in foldl fappl ((Passed, [])) substedlst

{- discover :: Ctx -> WM -> TProofSize -> TProofSize -> IO TResult
discover c@(Ctx ((svs, smap, (discovered, aux, occmp)), a1, a2, a3, a4, sargs, (a5, costs), a6, a7)) wm@(e, mp) g d =
    do let svars = getSensVars e
       let svars' = map fst svars
       mConcat [mConcat [do let occ = case lookup s1 occmp of; Just x -> x; _ -> error "error in discover";
                            let newoccmp = (s1, occ + 1):delete (s1, occ) occmp
                            let newaux = updateAux (s1, occ + 1) (s2, i) aux
                            let goal = (subst (ESensVar (Ident s1) (occ + 1)) [(ESensVar (Ident s2) i, 0)] e, mp)
                            let state = (ENull, EEqual (ESensVar (Ident s1) 1) (ESensVar (Ident s2) 1), wm)
                            let newc = Ctx ((svs, smap, (discovered, newaux, newoccmp)), a1, a2, a3, a4, sargs, (a5, costs), a6, a7)
                            solved <- solve newc goal (pathCost costs state g FInspect sargs) d 
                            return $ makeProof state solved FInspect | (v, i) <- filter ((==s2).fst) svars]
                                                                     | (s1, s2) <- discovered, elem s1 svars' && elem s2 svars'] -}

----------------------------
-- End of inference rules --
----------------------------

expMatches :: TSensMap -> Exp -> (Exp, [(String, Exp)]) -> Bool
expMatches smap (EAnd a b) (EAnd a2 b2, e2mp) = expMatches smap a (a2, e2mp) && expMatches smap b (b2, e2mp) 
expMatches smap (EOr a b) (EOr a2 b2, e2mp) = expMatches smap a (a2, e2mp) && expMatches smap b (b2, e2mp)
expMatches smap (EImplies a b) (EImplies a2 b2, e2mp) = expMatches smap a (a2, e2mp) && expMatches smap b (b2, e2mp) 
expMatches smap (EEquiv a b) (EEquiv a2 b2, e2mp) = expMatches smap a (a2, e2mp) && expMatches smap b (b2, e2mp)
expMatches smap (ENot a) (ENot a2, e2mp) = expMatches smap a (a2, e2mp)
expMatches smap (EForAll v1 od1 a1) (EForAll v2 od2 a2, e2mp) = v1 == v2 && od1 == od2 && expMatches smap a1 (a2, e2mp)
expMatches smap (EExists v1 od1 a1) (EExists v2 od2 a2, e2mp) = v1 == v2 && od1 == od2 && expMatches smap a1 (a2, e2mp)
expMatches smap (ECtx a1 c1) (ECtx a2 c2, e2mp) = c1 == c2 && expMatches smap a1 (a2, e2mp)
expMatches (_, smap, (discovered, _, _)) (ESensVar (Ident v1) _) (ESensVar (Ident v2) _, e2mp) =
    let (e1, e2) = (lookup v1 smap, lookup v2 e2mp)
    in case (e1, e2) of
           (Just e1', Just e2') -> e1' == e2' && not (any (\(a, b) -> (a == v1 && b == v2) || (a == v2 && b == v1)) discovered)
           _                    -> error "error in expMatches"
expMatches smap a (b, e2mp) = a == b

wmMatches :: TSensMap -> WM -> (WM, [(String, Exp)]) -> Bool
wmMatches smap e (pwme, pwmsmap) = 
    expMatches smap e (pwme, pwmsmap)

matches :: TSensMap -> WM -> TPrevWM -> Bool
matches smap wm prevwms = or [wmMatches smap wm prevwm | prevwm <- prevwms]

ruleApplications :: Ctx -> WM -> TProofSize -> TProofSize -> TResult
ruleApplications c@(Ctx (_, _, _, _, _, _, (funcs, _), _, _)) wm g d =
       let fappl = (\ret func -> 
                         let (rt, proofs) = ret
                         in if (rt /= OPrune)
                                then let (rt2, proofs2) = func c wm g d
                                     in if (rt2 /= OPrune)
                                            then (rt2, proofs ++ proofs2)
                                            else (OPrune, [])
                             else (OPrune, [])   )
       in foldl fappl (Passed, []) funcs

ruleApplications2 :: Ctx -> WM -> TProofSize -> TProofSize -> TResult
ruleApplications2 c@(Ctx (_, _, _, _, _, _, (funcs, _), _, _)) wm g d =
       let funcs' = (parMap rdeepseq) (\f -> f c wm g d) funcs
           fappl = (\(rt, proofs) (rt2, proofs2) -> 
                        if (rt /= OPrune)
                          then if (rt2 /= OPrune)
                                  then (rt2, proofs ++ proofs2)
                                  else (OPrune, [])
                          else (OPrune, [])   )
       in foldl fappl (Passed, []) funcs'

expandedExp :: [(String, Exp)] -> WM -> WM
expandedExp smap e =
    let a = foldl (\e' v -> case lookup v smap of; Just exp -> subst exp [(ESensVar (Ident v) 1, 0)] e'; _ -> e';) e 
                  (map fst smap)
    in insertAssignMaps a

opposite :: [(String, Exp)] -> TModel -> Exp -> TPsystem -> Bool
opposite smap model e ps =
    eval model (expandedExp smap e) == (if ps == T then Just EFalse else Just ETrue)

prunePossible :: Ctx -> WM -> TResultType
prunePossible c@(Ctx ((_, smap, _), memsegs, ps, (_, wmlim), _, _, _, model, _)) e =
       if expSize e > wmlim
           then WMO
           else if ps == T
                    then    let (ts, nts, cs, ncs, _, _) = memsegs
                            in if not $ null [m | Ok m <- [getPatternMapping e p [] | p <- nts]]
                                   then OPrune
                                   else Passed
                    else    let (ts, nts, cs, ncs, _, _) = memsegs
                            in if not $ null [m | Ok m <- [getPatternMapping e p [] | p <- ts]]
                                   then OPrune
                                   else Passed

solve :: Ctx -> WM -> TProofSize -> TProofSize -> TResult
solve c@(Ctx (s@(_, smap, _), msegs, psystem, memlims, prevwm, sargs@(le, p, u), pm@(_, costs), model, coffs)) wm g d =
   let prunetype = prunePossible c wm
   in  if prunetype == WMO || prunetype == OPrune
           then (prunetype, [])
           else if g <= d
                    then if matches s wm prevwm
                                then (Passed, [])
                                else let newc = Ctx (s, msegs, psystem, memlims, (wm, smap):prevwm, sargs, pm, model, coffs)
                                         (rt, proofs) = ruleApplications2 newc wm g d
                                         smallestproofs = smallest costs proofs sargs
                                         valsols = validSolutions smallestproofs
                                     in if null valsols
                                                       then if null smallestproofs
                                                               then (rt, [])
                                                               else (rt, [head smallestproofs])
                                                      else (rt, valsols)
                    else (Passed, [CutOff ((expSize wm) * coffs)])    

isValidProof :: TProof -> Bool
isValidProof (CutOff _) = False
isValidProof FoundSol = True
isValidProof (Rule (_, _, proof)) = isValidProof proof  

validSolutions :: [TProof] -> [TProof]
validSolutions proofs = [proof | proof <- proofs, isValidProof proof]

-- iterativeSolve' func (x:xs) (y:ys) u costs sargs =
    -- do
        -- (rt, solutions) <- x
        -- if (null solutions)
           -- then do let unit = case u of
                                  -- SIZE_ -> "size: "
                                  -- WEIGHT_ -> "weight: "
                                  -- VOLUME_ -> "volume: "
                   -- putStrLn $ unit ++ "@"
                   -- putStrLn "length: @"
                   -- putStrLn $ (if null solutions then "No solution possible" else printProof $ head $ smallest costs solutions sargs)
                   -- error ""
           -- else if null (validSolutions solutions)
                    -- then do handle <- Control.Monad.Parallel.forkExec $ func y
                            -- iterativeSolve' func (xs ++ [handle]) ys u costs sargs
                    -- else return $ nub $ validSolutions solutions

-- iterativeSolve :: Ctx -> WM -> TProofSize -> TProofSize -> IO [TProof]
-- iterativeSolve c@(Ctx (_, _, _, _, _, sargs@(le, p, u), (_, costs), _, _)) wm d depth =
    -- do cpuCores <- Control.Concurrent.getNumCapabilities 
       -- let range = if le == LENGTH_SEARCH_ then [0..] else [0,5..]
       -- let (first,rem) = splitAt (cpuCores) range
       -- let func = return . (solve c wm 0)
       -- xs <- mapM (Control.Monad.Parallel.forkExec . func) $ first
       -- result <- iterativeSolve' func xs rem u costs sargs
       -- return result 

iterativeSolve :: Ctx -> WM -> TProofSize -> TProofSize -> IO [TProof]
iterativeSolve c@(Ctx (_, _, _, _, _, sargs@(le, p, u), (_, costs), _, _)) wm d depth =
    do let (rt, solutions) = solve c wm 0 d
       if (null solutions)  -- || mod depth 4 == 0
           then do let unit = case u of
                                  SIZE_ -> "size: "
                                  WEIGHT_ -> "weight: "
                                  VOLUME_ -> "volume: "
                   putStrLn $ unit ++ "@"
                   putStrLn "length: @"
                   putStrLn $ (if null solutions then "No solution possible" else printProof $ head $ smallest costs solutions sargs)
                   error ""
           else if null (validSolutions solutions)
                    then do let lim = if le == LENGTH_SEARCH_ then (depth + 1) else minimum $ map (proofsize costs sargs) solutions
                            -- putStrLn $ "lim is " ++ show lim
                            -- mapM (appendFile "temp.txt") $ map printProof solutions
                            -- putStrLn $ show rt
                            itersolv <- iterativeSolve c wm lim (depth + 1)
                            return $! itersolv
                    else return $ nub $ validSolutions solutions
       
printSolutions :: TRCostLkup -> [TProof] -> TSwitchTpl -> IO ()
printSolutions _ [] _ = return ()
printSolutions costs solutions sargs@(_, p, u) =
    do let lengths = map prooflength solutions
       let sizes = map (proofsize costs sargs) solutions
       let solutionlist = map (++"\n\n") (map (if p == TERMINAL_ then printProof else printProof) solutions)
       let unit = case u of
                      SIZE_ -> "size: "
                      WEIGHT_ -> "weight: "
                      VOLUME_ -> "volume: "
       mapM putStrLn [s ++ unit ++ (show size) ++ "\nlength: " ++ (show l) ++ "\n" | 
                     (s, (l, size)) <- zip solutionlist (zip lengths sizes)]
       return ()

typeCheckExp :: String -> Exp -> IO ()
typeCheckExp prgName e =
    do let vars = getVars False e
       let matches = [var | var <- vars, (length var > 1) && ((head var) == 'a' && all isDigit (tail var))]
       if not (null matches)
           then printErrMsg prgName
           else return ()

getStrengths :: [Exp] -> [(Exp, Exp)]
getStrengths strweaks =
    [(a, b) | (EImplies a b) <- strweaks, flength a < flength b && null (getPatternVars a \\ getPatternVars b)]
    
getWeaks :: [Exp] -> [(Exp, Exp)]
getWeaks strweaks =
    [(a, b) | (EImplies a b) <- strweaks, flength b < flength a && null (getPatternVars b \\ getPatternVars a)]

getMemSegs :: Ltm -> IO TMemSegs
getMemSegs (LTMC ltm)  =
    do let tauts' = nub $ concat [exps | Taut exps <- ltm]
       let revtauts = [EEquiv b a | EEquiv a b <- tauts']
       let contras' = concat [exps | Contr exps <- ltm]
       let revcontras = [EEquiv b a | EEquiv a b <- contras']
       let contras = contras' ++ revcontras
       let tauts'' = tauts' ++ revtauts
       let nontauts = nub $ concat [exps | Nontaut exps <- ltm] ++ contras
       let extcontras = concat [[{-EEquiv EFalse a,-} EEquiv a EFalse] | a <- contras]
       let tauts = nub $ (concat [[{-EEquiv ETrue a,-} EEquiv a ETrue] | a <- tauts'' ++ extcontras]) ++ tauts'' ++ extcontras
       let noncontras = nub $ concat [exps | Noncontr exps <- ltm] ++ tauts
       let rewritestpls = nub [f | f@(EEquiv a b) <- tauts, a /= b && flength a > flength b]
       let strweaks = [EImplies a b | EImplies a b <- tauts']
       let strns = getStrengths strweaks
       let weaks = getWeaks strweaks
       return (tauts, nontauts, contras, noncontras, rewritestpls, (strns, weaks))

updateSwitchTpl :: TSwitchTpl -> String -> TSwitchTpl
updateSwitchTpl (lengths, print, unit) argsw =
    case argsw of
        "--latex"      -> (lengths, LATEX_, unit)
        "--lx"         -> (lengths, LATEX_, unit)
        "--length"     -> (LENGTH_SEARCH_, print, unit)
        "--l"          -> (LENGTH_SEARCH_, print, unit)
        "--size"       -> (lengths, print, SIZE_)
        "--s"          -> (lengths, print, SIZE_)
        "--weight"     -> (lengths, print, WEIGHT_)
        "--w"          -> (lengths, print, WEIGHT_)
        "--volume"     -> (lengths, print, VOLUME_)
        "--v"          -> (lengths, print, VOLUME_)
        "--help"       -> (lengths, print, unit)
        "--print-all"  -> (lengths, print, unit)
        "--pa"         -> (lengths, print, unit)
        _              -> error $ "unrecognized argument switch: " ++ argsw ++ ", use --help for available options."
        
parseSwitchArgs :: [String] -> TSwitchTpl
parseSwitchArgs sargs =
    foldl updateSwitchTpl (NO_LENGTH_SEARCH_, TERMINAL_, SIZE_) sargs

parsePM :: String -> TPsystem -> ([TRuleFunc], [(TRule, Int)])
parsePM pm ps =
    let noCR = filter (\a -> a /= "" && (take 2 a) /= "//") (lines pm)
        nowhitespace = map (filter (\a -> (not.isSpace) a && a /= '-')) noCR
        noequalsign = map (map toLower) $ map (filter isAlphaNum) nowhitespace
        tpllist' = map (span isAlpha) noequalsign
        tpllist = nubBy (\(a, b) (c, d) -> a == c) tpllist' 
        cmbl = [((case rule of
                      "formulainspect"         -> FormInspect
                      "truthinspect"           -> TInspect
                      "falsityinspect"         -> FInspect
                      "piinspectprime"         -> PiInspectP
                      "sigmainspectprime"      -> SigmaInspectP
                      "deltainspect"           -> DeltaInspect
                      "modelinspect"           -> MInspect
                      "forallreduce"           -> AReduce
                      "existsreduce"           -> EReduce
                      "forallreduceplus"       -> AReduceP
                      "existsreduceplus"       -> EReduceP
                      "rewrite"                -> Rewrite
                      "tautologyrecall"        -> TRecall
                      "contradictionrecall"    -> CRecall
                      "strengthen"             -> Strengthen
                      "weaken"                 -> Weaken
                      "alldistribute"          -> ADistribute
                      "existsdistribute"       -> EDistribute
                      "sigmaandcomprehension"  -> EAndComp
                      "sigmaorcomprehension"   -> EOrComp
                      "piandcomprehension"     -> AAndComp
                      "piorcomprehension"      -> AOrComp
                      "piarrowcomprehension"   -> AArrowComp
                      a                        -> error $ "unrecognized rule " ++ a ++ " in PM"),
                                                                    if null weight then 1 else read weight) | (rule, weight) <- tpllist]
        allrules = [FormInspect, TInspect, FInspect, MInspect, Rewrite, TRecall, CRecall, Strengthen, DeltaInspect,
                    PiInspectP, SigmaInspectP, Weaken, AReduce, EReduce, AReduceP, EReduceP, ADistribute, EDistribute,
                    EAndComp, EOrComp, AAndComp, AOrComp, AArrowComp]
        trules = allrules \\ [CRecall, Weaken]
        ntrules = allrules \\ [TRecall, Strengthen]
        wlst = case ps of
                   T  -> filter (\(a, b) -> elem a trules) cmbl
                   NT -> filter (\(a, b) -> elem a ntrules) cmbl
                   _  -> error "This proof system is not implemented yet"
        usedrules = map fst wlst
        usedrewrites = intersect [Rewrite] usedrules
        usedrecalls = intersect [TRecall, CRecall] usedrules
        funcs' = concat [case rule of
                             FormInspect   -> [formulaInspect]
                             TInspect      -> [truthInspect]
                             FInspect      -> [falsityInspect] 
                             PiInspectP    -> [piInspect']
                             SigmaInspectP -> [sigmaInspect']
                             DeltaInspect  -> [deltaInspect]
                             MInspect      -> [modelInspect]
                             AReduce       -> [forallReduce]
                             EReduce       -> [existsReduce]
                             AReduceP      -> [forallReducePlus]
                             EReduceP      -> [existsReducePlus] 
                             Strengthen    -> [strengthen]
                             Weaken        -> [weaken]
                             ADistribute   -> [forallDistribute]
                             EDistribute   -> [existsDistribute]
                             EAndComp      -> [sigmaAndComprehension]
                             EOrComp       -> [sigmaOrComprehension]
                             AAndComp      -> [piAndComprehension]
                             AOrComp       -> [piOrComprehension]
                             AArrowComp    -> [piArrowComprehension]
                             _             -> [] | rule <- usedrules]
        funcs = funcs' ++ (if null usedrewrites then [] else [rewrites usedrewrites]) ++ 
                          (if null usedrecalls then [] else [recall usedrecalls])
    in (funcs, wlst) 

checkNodeColor :: TModel -> Int -> String -> Exp
checkNodeColor model node color =
    if null [True | (node', color', _) <- model, node' == node && color' == color]
        then EFalse
        else ETrue

checkEdge :: TModel -> Int -> Int -> Exp
checkEdge model node1 node2 =
    if null [True | (node, color, adjlst) <- model, node == node1 && elem node2 adjlst]
        then EFalse
        else ETrue

dom ::TModel -> ODom
dom model = OptDom (sort [toInteger node | (node, _, _) <- model])
    
extractModel :: String -> Int -> TModel
extractModel models mnr =
    let splitAtComma = [let (pt1, pt2) = span (/='\"') (filter (not.isSpace) line)
                            x = filter (/= ",") (groupBy (\a b -> a /= ',' && b /= ',') pt1)
                            y = filter (/= ",") (groupBy (\a b -> a /= ',' && b /= ',') (filter (/='\"') pt2))
                        in (x, y) | line <- lines models]
    in [(read node, map toLower color, map read adjlst)  | ([node, model, color], adjlst) <- splitAtComma, 
                                                               read model == mnr && elem (map toLower color) ["red", "yellow", "blue"]]


printErrMsg :: String -> IO ()
printErrMsg prgname =
    error $ "Usage:\n\n> " ++ prgname ++ " \"formula\" ltm-file pm-file model-file model (T | NT) SM-size WM-size ...\n" ++
            "  ... (--latex | --size | --weight | --length | --print-all)*\n" ++
            "\ne.g. type " ++ prgname ++ " \">x>y(Blue(x) -> E(x,y))\" ltm.txt pm.txt models.txt 3 T 5 8 --latex\n" ++
            "to check the T-proof of the given formula in model 3 from modelfile models.txt with a visual memory of\n" ++
            "size 5 and a working memory of size 8, using the long term- and procedural\nmemory specified in ltm.txt and pm.txt,\n" ++
            "compiling the proofs to latex notation.\nVariable names a0, a1, ..., are reserved for abstraction variables and\n" ++
            "are therefore not allowed as variable names in the logic formula you supply."
