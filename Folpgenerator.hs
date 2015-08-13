module Main where

import FolpLib
import Lexfolp
import Parfolp
import Absfolp
import ErrM
import Data.Char
import Data.List
import System.IO
import System.Environment (getArgs, getProgName)
import Test.QuickCheck
import Test.QuickCheck.Gen
import Text.Printf

data Color = Red | Blue | Yellow
    deriving (Show,Eq)

rColor :: Gen Color
rColor = elements [Red,Blue,Yellow]

nodes :: Gen Int   -- random number of nodes in a model
nodes = choose (2,10)

edges :: Int -> Gen Int -- random number of edges, sized by number of nodes
edges n = choose (0,n)

-- modelSize :: Gen (Int,Int) -- (Nodes,Edges)
-- modelSize = do
--         n <- nodes
--         e <- edges n
--         return (n,e)

type Node = (Int,Color)
type Edge = (Int,Int)
type FolpModel = ([Node],[Edge])

rModel :: Gen FolpModel
rModel = do 
    n <- nodes
    nodes <- mapM (\x -> do c <- rColor; return (x,c)) [1..n]
    let edges' = [(a,b) | a <- [1..n], b <- [1..n]]
    edges'' <- shuffle edges'
    numedges <- edges (length edges'')
    let edges''' = take numedges edges''
    let edgs = takeProper n edges'''
    return (nodes,edgs)

-- take n edges from the set that do not cross each other
takeProper :: Int -> [Edge] -> [Edge]
--takeProper n [] = []
--takeProper 0 _  = []
--takeProper n [e] = [e]
--takeProper 1 (e:es) = [e]
takeProper n es = take' n es []
    where take' 0 _ result = result
          take' n [] result = result
          take' n [e] result = if anyCross n e result then result else (e:result)
          take' n (e:es) result = if anyCross n e result then take' n es result else take' n es (e:result)

anyCross :: Int -> Edge -> [Edge] -> Bool
anyCross n e es = or $ map (edgeCross n e) es

edgeCross :: Int -> Edge -> Edge -> Bool
edgeCross n (a1',a2') (b1,b2) =
        let (a1,a2) = (min a1' a2', max a1' a2')
            (n1,n2) = ([(a1+1)..(a2-1)],[1..(a1-1)] ++ [(a2+1)..n])
        in ((elem b1 n1 && elem b2 n2) || (elem b1 n2 && elem b2 n1))

shuffle :: (Eq a) => [a] -> Gen [a]
shuffle [] = return []
shuffle xs = do x  <- oneof $ map return xs
                ys <- shuffle $ delete x xs
                return (x:ys)

instance Arbitrary Atom where
    arbitrary = rAtom

instance Arbitrary Exp where
    arbitrary = sized rExp

--instance Arbitrary FolpModel where
--    arbitrary = do undefined


vars = ["x","y","z"]

rAtom :: Gen Atom
rAtom = do v <- elements vars
           return (AVar (Var v))

-- Generates a random formula for the test
rExp :: Int -> Gen Exp
rExp s = rExp' s -- suchThat (rExp' s) (\e -> expSize e <= 12)
rExp' :: Int -> Gen Exp
rExp' s = do sub <- rSubExp s
             let var' = map (\v -> varPresent v sub) vars
             let var = map fst . filter (snd) $ zip vars var'
             let func v sub = do e <- sub
                                 e' <- rQuantifier e v
                                 return e'
             foldr func (return sub) var

-- Generates the sub expression given quantifiers
rSubExp :: Int -> Gen Exp
rSubExp s =
    do  pred1 <- rPred
        pred2 <- rPred
        frequency  [(1,return pred1),
                    (s,do e <- rBinary pred1 pred2
                          return e),
                    (s,do e1 <- rSubExp (s `div` 2)
                          e2 <- rSubExp (s `div` 2)
                          e <- rBinary e1 e2
                          return e)]

rexp =  do s <- sample' $ rExp 4
           mapM_ (putStrLn . printExp) s
rBinary :: Exp -> Exp -> Gen Exp
rBinary e1 e2 = do frequency [(10,return (EOr e1 e2)),
                              (10,return (EAnd e1 e2)),
                              (6,return (EImplies e1 e2)),
                              (3,return (EEquiv e1 e2))]

-- given variable names, generates a random predicate
-- | ENot Exp
rPred :: Gen Exp
rPred = 
    do  atom1 <- arbitrary
        atom2 <- arbitrary
        pred <- oneof [ return (EPBlue atom1),
                        return (EPRed atom1),
                        return (EPYellow atom1),
                        return (EPEdge atom1 atom2)]
        return (EPred pred)

-- generate a random quantifier, with the given variable
rQuantifier :: Exp -> String -> Gen Exp
rQuantifier e v = oneof [return (EForAll (Var v) EmptyOptDom e),
                         return (EExists (Var v) EmptyOptDom e)]


main :: IO ()
main = do
    args    <- getArgs
    prgName <- getProgName
    if length args >= 1
      then
        do let [num'] = take 1 args
           let num = (read num' :: Int)
           mapM (\x -> makeQuestion ("model" ++ printf "%03d" x ++ ".tex") ("model" ++ printf "%03d" x ++ ".txt") ) [1..num]
           writeBatchFile num
           putStrLn $ "Finished. Generated " ++ show num ++ " questions."
      else
        putStrLn $ "Correct use: " ++ prgName ++ " numquestions"

writeBatchFile :: Int -> IO ()
writeBatchFile num =
    do
        file <- openFile "compile-models.bat" WriteMode
        let text = concatMap (\x -> "pdflatex -shell-escape " ++ ("model" ++ printf "%03d" x ++ ".tex") ++ "\n") [1..num]
        hPutStrLn file text
        hClose file

-- make a random question, and save in the latex+text files
makeQuestion :: FilePath -> FilePath -> IO ()
makeQuestion latexfile textfile = 
    do
        models <- sample' rModel
        exps <- sample' $ rExp 4
        let exp = head exps
        let model = head models
        let text = questionLatex model exp
        latex <- openFile latexfile WriteMode
        hPutStrLn latex text
        hClose latex
        let qtext = questionText model exp
        textfile'  <- openFile textfile  WriteMode
        hPutStrLn textfile' qtext
        hClose textfile'

-- Generate latex code for a single question, with both model and sentence
questionLatex :: FolpModel -> Exp -> String
questionLatex model exp = 
        modelLatex model
        ++ "\n\\node[] (s) at (0,-3.5) {$" ++ folpLatex exp ++ "$};"
        ++ tikzEnd
        ++ docEnd

-- Generate text code for a single question, with both model and sentence
-- Text code is read by proof generator (Folp program)
questionText :: FolpModel -> Exp -> String
questionText model exp = -- expText exp ++ "\n" ++ 
                         modelText model

modelText :: FolpModel -> String
modelText ([],_) = ""
modelText (n:ns,edges) = modelText' n edges ++ "\n" ++ modelText (ns,edges)
    where
        modelText' (n,color) edges = show n ++ ",1," ++ show color ++ (getEdges n edges)
        getEdges n edges | null (getConnections n edges) = ""
        getEdges n edges = 
                    let cons = getConnections n edges
                        ed = concatMap (\x -> "," ++ show x) cons
                    in  "," ++ "\"" ++ tail ed ++ "\""

getConnections :: Int -> [Edge] -> [Int]
getConnections n edges =
        nub $ (map fst $ filter (\x -> snd x == n) edges)
                ++ (map snd $ filter (\x -> fst x == n) edges)

expText :: Exp -> String
expText (EFalse) = "F"
expText (ETrue)  = "T"
expText (EPred p) = predText p
expText (ENot e) = "~(" ++ expText e ++ ")"
expText (EEquiv   e1 e2) = "(" ++ expText e1 ++ ")<->(" ++ expText e2 ++ ")"
expText (EEqual   e1 e2) = "(" ++ expText e1 ++ ")=(" ++ expText e2 ++ ")"
expText (EImplies e1 e2) = "(" ++ expText e1 ++ ")->(" ++ expText e2 ++ ")"
expText (EOr      e1 e2) = "(" ++ expText e1 ++ ")|(" ++ expText e2 ++ ")"
expText (EAnd     e1 e2) = "(" ++ expText e1 ++ ")&(" ++ expText e2 ++ ")"
expText (EForAll (Var v) _ e) = ">" ++ v ++ "(" ++ expText e ++ ")"
expText (EExists (Var v) _ e) = "<" ++ v ++ "(" ++ expText e ++ ")"
expText _ = ""

predText :: Pred -> String
predText ((EPBlue a)) = "Blue(" ++ show a ++ ")"
predText ((EPRed a))  = "Red(" ++ show a ++ ")"
predText ((EPYellow a)) = "Yellow(" ++ show a ++ ")"
predText ((EPEdge a b)) = "E(" ++ show a ++ "," ++ show b ++ ")"


folpLatex' (EPred p) = predLatex p
folpLatex' e = "(" ++ folpLatex e ++ ")"

-- convert FOL sentence to latex code
folpLatex :: Exp -> String
folpLatex (EFalse) = "\\bot "
folpLatex (ETrue)  = "\\top "
folpLatex (EPred p) = predLatex p
folpLatex (ENot EFalse) = "\\urcorner \\bot "
folpLatex (ENot ETrue) = "\\urcorner \\top "
folpLatex (ENot e@(EPred p)) = "\\urcorner " ++ folpLatex e
folpLatex (ENot e@(ENot p)) = "\\urcorner " ++ folpLatex e
folpLatex (ENot e) = "\\urcorner (" ++ folpLatex e ++ ")"
folpLatex (EOr e1 e2) = folpLatex' e1 ++ "\\vee " ++ folpLatex' e2
folpLatex (EAnd e1 e2) = folpLatex' e1 ++ "\\wedge " ++ folpLatex' e2
folpLatex (EEquiv e1 e2) = folpLatex' e1 ++ "\\leftrightarrow " ++ folpLatex' e2
folpLatex (EEqual e1 e2) = folpLatex' e1 ++ " = " ++ folpLatex' e2
folpLatex (EImplies e1 e2) = folpLatex' e1 ++ "\\to " ++ folpLatex' e2
folpLatex (EForAll (Var v) _ e@(EForAll _ _ _)) = "\\forall " ++ v ++ folpLatex e
folpLatex (EForAll (Var v) _ e@(EExists _ _ _)) = "\\forall " ++ v ++ folpLatex e
folpLatex (EForAll (Var v) _ e@(EPred _)) = "\\forall " ++ v ++ folpLatex e
folpLatex (EForAll (Var v) _ e) = "\\forall " ++ v ++ " [" ++ folpLatex e ++ "]"

folpLatex (EExists (Var v) _ e@(EForAll _ _ _)) = "\\exists " ++ v ++ folpLatex e
folpLatex (EExists (Var v) _ e@(EExists _ _ _)) = "\\exists " ++ v ++ folpLatex e
folpLatex (EExists (Var v) _ e@(EPred _)) = "\\exists " ++ v ++ folpLatex e
folpLatex (EExists (Var v) _ e) = "\\exists " ++ v ++ " [" ++ folpLatex e ++ "]"
folpLatex _ = ""

predLatex :: Pred -> String
predLatex ((EPBlue a)) = "Blue(" ++ show a ++ ")"
predLatex ((EPRed a))  = "Red(" ++ show a ++ ")"
predLatex ((EPYellow a)) = "Yellow(" ++ show a ++ ")"
predLatex ((EPEdge a b)) = "Edge(" ++ show a ++ "," ++ show b ++ ")"

-- print latex code for generating the graph model
modelLatex :: FolpModel -> String
modelLatex ([node],edges) =
    latexBegin
    ++ nodeLatex (0.0,0.0,0)  node
    ++ concatMap (edgeLatex 1) edges
    
modelLatex (nodes,edges) =
        latexBegin
        ++ concatMap (\(n,c) -> nodeLatex (nodePos 2 (length nodes) n) (n,c)) nodes
        ++ concatMap (edgeLatex (length nodes)) edges

nodePos :: Int -> Int -> Int -> (Float,Float,Int)
nodePos radius totNodes node =
    let gap = 360 `div` totNodes
        start = 0
        degree = start + (node-1) * gap
        x = (fromIntegral radius) * cos ((fromIntegral degree) * pi / 180.0 )
        y = (fromIntegral radius) * sin ((fromIntegral degree) * pi / 180.0 )
    in  (x,y,degree)

edgeLatex :: Int -> Edge -> String
edgeLatex nodes (a,b)
    | a == b = let (_,_,deg) = nodePos 2 nodes a
                   in' = deg - 45
                   out = deg + 45
               in "\n\\path (" ++ show a ++ ") edge [out=" ++ show out ++ ",in=" ++ show in' ++ ",looseness=1.0,loop,distance=1cm,-,thick] node {} (" ++ show a ++ ");"
    | otherwise = "\n\\Edge[color=black,lw=1.0pt](" ++ show a ++ ")(" ++ show b ++ ")"


nodeLatex :: (Float,Float,Int) -> Node -> String
nodeLatex (x,y,pos) (n,color) =
    "\n\\node[shape=circle,draw,fill=" ++ show color ++ ",minimum size=0.5cm] (" ++ show n ++ ") at (" ++ (printf "%.3f" x) ++ "," ++ (printf "%.3f" y) ++ ") {" ++ show n ++ "};"

latexBegin = "\\documentclass[convert={density=300,outext=.png,convertexe={convert}}]{standalone}"
             -- ++ "\n\\usepackage[paperwidth=10cm,paperheight=10cm,hmargin=0cm,vmargin=0cm]{geometry}"
             ++ "\n\\usepackage[utf8]{inputenc}"
             ++ "\n\\usepackage[usenames,dvipsnames]{xcolor}"
             -- ++ "\n\\usepackage{fullpage}"
             -- ++ "\n\\usepackage[upright]{fourier}"
             ++ "\n\\usepackage{tkz-graph}"
             ++ "\n\\pagestyle {empty}"
             ++ "\n\\usetikzlibrary{arrows}"
             ++ "\n\\thispagestyle{empty}"
             ++ "\n\\begin{document}"
             ++ "\n\\begin{tikzpicture}"

tikzEnd =  "\n\\end{tikzpicture}"

docEnd = "\n\\end{document}"
