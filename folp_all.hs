module Main where

import Absfolp
import FolpLib
import System.Time
import System.Environment (getArgs, getProgName)
import Data.Char

type ModelNo = Int
type Sentence = String
type Problem = (Sentence,ModelNo,Bool)
-- response times, used to find correlation coefficient
rTimes :: [Float]
rTimes =
  [ 48.259, 60.311, 29.906, 41.0985, 52.733, 29.061, 22.203, 32.845, 26.2815,
    24.392, 29.803, 34.163, 37.116, 41.7495, 30.4015, 50.982, 27.217, 17.609,
    40.048, 38.133, 34.7575, 28.341, 38.284, 32.1715, 40.056, 41.024, 42.921,
    45.28, 19.7965, 40.672, 34.688, 36.198, 39.5945, 21.115, 27.0235, 34.297,
    36.364, 55.015, 51.8595, 52.575, 37.8355, 21.1245, 16.3445, 31.0935,
    49.759, 36.2255, 43.259, 11.3595, 35.14, 18.406 ]


sentences :: [String]
sentences = ["<x(Blue(x)&>y(Blue(y)->E(x,y)))",
            ">x(Red(x)-><y<z(E(x,y)&E(y,z)&Red(z)))",
            "<x(Yellow(x)&>y(~E(x,y)))",
            ">x>y(~E(x,y)->Yellow(y))",
            ">x(<y(Yellow(y)&E(x,y))<->Yellow(x))",
            ">x((E(x,x)|~Blue(x))->E(3,x))",
            "<x<y(Red(x)&Red(y)&E(x,y)&E(y,x))",
            "<x<y<z(E(x,y)&E(y,z)&~E(z,x))",
            ">x(~Red(x)->>y(E(x,y)))",
            ">x>y(E(x,y)&E(y,x))",
            ">x(~E(x,x)->~Red(x))",
            ">x(E(x,x)-><y(E(x,y)&Yellow(y)))",
            ">x(~E(x,x)-><y(Yellow(y)&E(y,x)))",
            "<x>y(~E(x,y))",
            ">x(~E(x,x)-><y(Blue(y)&E(x,y)))",
            "<x(~<y(Blue(y)&E(y,x)))",
            ">x(Red(x)-><y(E(x,y)&~Blue(y)))",
            ">x(Red(x)->E(x,x))",
            ">x(E(x,x)-><y(Blue(y)&E(x,y)))",
            "<x(~Yellow(x)&>y(~E(x,y)))",
            ">x<y(E(x,y)&Red(y))",
            "<x>y(E(x,y)&E(y,x))",
            "<x(~E(x,x)&<y(E(y,x)))",
            ">x>y(E(x,y)<->E(x,x))",
            ">x(Yellow(x)<-><y(Yellow(y)&E(x,y)))",
            ">x>y(Blue(x)&Yellow(y)->E(x,y))",
            ">x(~E(x,x)-><y(E(x,y)&~Blue(y)))",
            "<x(Yellow(x)&~<y(~Yellow(y)&E(x,y)))",
            "<x<y(E(x,x)&E(y,y)&E(x,y))",
            "<x(>y(E(x,y))->Blue(x))",
            ">x(Red(x)->~<y(~Red(y)&E(x,y)))",
            ">x(Yellow(x)->>y(E(x,y)))",
            ">x(E(x,2)|~<y(Blue(y)&E(y,x)))",
            ">x(E(x,3))",
            ">x(E(x,2)&E(x,3))",
            ">x(Yellow(x)-><y(Red(y)&E(x,y)))",
            "<x<y(~Yellow(x)&Yellow(y)&~E(x,y))",
            ">x(<y(E(x,y))->E(x,x))",
            ">x(~E(x,x)-><y(E(y,y)&E(x,y)))",
            ">x(>y(E(x,y))<->Blue(x))",
            "<x(Blue(x)&>y(Yellow(y)->E(x,y)|E(y,x)))",
            ">x(Red(x)-><y(E(x,y)))",
            ">x(Red(x)|Yellow(x))",
            ">x(Yellow(x)-><y(E(x,y)))",
            ">x>y(E(x,y)&E(y,x)->E(x,x)|E(y,y))",
            "<x>y(Blue(y)->~E(x,y))",
            ">x(Yellow(x)-><y(E(x,y)&(Blue(y)|Red(y))))",
            "<x(E(x,x))",
            "<x>y(~E(x,y))",
            "<x(~Blue(x)&E(x,x))"
            ]
-- truth of sentences in respective models
pTruth :: [Bool]
pTruth = 
   [False, False, True,  False, True,  False, True,  True,  False, False,
    True,  False, True,  True,  True,  False, True,  False, True,  True,
    False, True,  True,  False, False, False, False, True,  True,  True,
    False, False, False, False, False, False, True,  False, False, False,
    True,  True,  False, False, True,  True,  False, True,  True,  True ]


problems :: [(Sentence,ModelNo,Bool)] -- [Problem]
problems = zip3 sentences [1..(length sentences)] pTruth

main :: IO ()
main = do args    <- getArgs
          prgName <- getProgName
          -- writeFile "temp.txt" ""
          starttime <- getClockTime
          startCal <- toCalendarTime starttime
          -- writeFile "biskvi.txt" ""
          if any (== "--help") args then printErrMsg prgName else return ()

          if length args >= 8  -- if there are atleast 8 arguments
              then do let mandatargs = take 8 args   -- then separate into mandatory and switch args
                      let switchargs = drop 8 args
                      let [exp, ltmf, pmf, modf, mdl, p, sm, wm] = mandatargs -- expand mandatory arguments
                      e     <- parseExp exp    -- parse the expression
                      typeCheckExp prgName e   -- typecheck the expression
                      ltm   <- readFile ltmf   -- read ltm file into string
                      ltmc  <- parseLtm ltm    -- parse the ltm string
                      pmstr <- readFile pmf    -- read pm file into string
                      modstr <- readFile modf  -- read model file into string
                      let model = extractModel modstr (read mdl)  -- extract the required model from model file

					  -- for remaining arguments (more than 8)
                      let sargv = parseSwitchArgs switchargs
                      memsegs <- getMemSegs ltmc
                      let isSMWMdigit = all isDigit sm && all isDigit wm
					  
					  -- if something wrong
                      if not (elem (map toUpper p) ["T", "NT"]) || not isSMWMdigit || read sm < 0 || read wm < 0
                          then printErrMsg prgName
                          else return ()

                      let psystem = case (map toUpper p) of
                                        "T"  -> T
                                        "NT" -> NT
                      let pm = parsePM pmstr psystem
                      -- putStrLn $ "Model is: " ++ (show model)
                      -- putStrLn $ "Exp is: " ++ printExp (inspectDomain model e)
                      -- sequence [putStrLn $ (printExp sf) | (sf, i) <- subformulas (inspectDomain model e) True]
                      let sensmap = (map (("a"++).show) [1..], [("a0", inspectDomain model e)], ([], (Node ("a0", 1) TNull TNull), [("a0", 1)]))
                      let e' = ESensVar (Ident "a0") 1
                      let reclkup = case psystem of
                                        T -> TRecall
                                        NT -> CRecall
                      let coffs = case lookup reclkup (snd pm) of
                                      Just cost -> cost
                                      _         -> error $ "Rule " ++ show reclkup ++ "has to be present in PM"  
                      let (l, p, u) = sargv
                      putStrLn $ "\nQuestion " ++ mdl ++ ": " ++ (if p == TERMINAL_ then (printExp e ++ "\n") else printExp e ++ "\n")
                      putStrLn $ "SM=" ++ sm
                      putStrLn $ "WM=" ++ wm ++ "\n"
                      solutions <- iterativeSolve (Ctx (sensmap, memsegs, psystem, (read sm, read wm), [], sargv, pm, model, coffs)) e' 0 0
                      printSolutions (snd pm) (if any (=="--print-all") (map (map toLower) args) then solutions else [head solutions]) sargv
                      endtime <- getClockTime
                      endCal <- toCalendarTime endtime
                      putStrLn $ "Started:  " ++ calendarTimeToString  startCal
                      putStrLn $ "Finished: " ++ calendarTimeToString  endCal
                      let difftime = diffClockTimes endtime starttime
                      putStrLn $ "Duration: " ++ timeDiffToString difftime
              -- if arguments < 8
              else case args of
                       [exp, modf, mdl] ->
                           do e <- parseExp exp                          -- parse expression
                              modstr <- readFile modf                    -- read model file
                              let model = extractModel modstr (read mdl) -- extract required model
                              case eval model (inspectDomain model e) of -- print truth value
                                  Just ETrue -> putStrLn "TRUE"
                                  Just EFalse -> putStrLn "FALSE"
                                  _           -> error "hmm.. something is wrong"
                              return ()
                       _ ->  printErrMsg prgName


iterativeSolve' :: Ctx -> WM -> TProofSize -> TProofSize -> Maybe TProof
iterativeSolve' c@(Ctx (_, _, _, _, _, sargs@(le, p, u), (_, costs), _, _)) wm d depth =
    let (rt, solutions) = solve c wm 0 d
    in if (null solutions)
           then Nothing
           else let valid = validSolutions solutions
                in if null valid
                    then  let lim = if le == LENGTH_SEARCH_ then (depth + 1) else minimum $ map (proofsize costs sargs) solutions
                          in (iterativeSolve' c wm lim (depth + 1) )
                    else Just $ head valid
-- Ctx (TSensMap, TMemSegs, TPsystem, (TMemSize, TMemSize), TPrevWM, TSwitchTpl, TPMData, TModel, Int)
-- c@(Ctx (sensmap, memsegs, _, (sm,wm), _, sargs@(le, p, u), (_, costs), _, _))

-- solveOne :: Int -> (Exp, ModelNo, TPsystem) -> Maybe TProof
solveOne sensmap memsegs sm wm sargv pm coffs (exp,model,psystem) =
    let c = Ctx (sensmap, memsegs, psystem, (sm, wm), [], sargv, pm, model, coffs)
    in  iterativeSolve' c exp 0 0

-- solveAll :: ... -> [Problem] -> [(ModelNo,TProof)]
solveAll sensmap memsegs sm wm sargv pm coffs problems@[(exp,model,psystem)] =
    let p = map (solveOne sensmap memsegs sm wm sargv pm coffs) problems
    in (zip (map (\(_,m,_) -> m) problems) p)

-- find correlation coefficient
findCorrel :: [TProof] -> [Float] -> Float
findCorrel proofs rtimes = Math.Statistics.pearson (map (proofsize ) proofs) rtimes

