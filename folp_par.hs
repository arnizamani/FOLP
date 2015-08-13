module Main where

import Absfolp
import FolpLib
import System.Time
import System.Environment (getArgs, getProgName)
import Data.Char
import Text.Printf

{-
import Fol.FirstOrderLogic

type ExpS = Sentence

convertExp :: Exp -> ExpS
convertExp (EEquiv e1 e2) = Equiv
convertExp (EEqual e1 e2) = undefined
convertExp (EImplies e1 e2) = Imply
convertExp (EOr e1 e2) = Or
convertExp (EAnd e1 e2) = And
convertExp (ENot e) = Not (convertExp e)
convertExp (EFalse) = 
convertExp (ETrue) = 
convertExp (EPattern Pattern) = 
convertExp (EPred Pred) = 
convertExp (EForAll (Var v) _ e) = Quantifier ForAll [v] (convertExp e)
convertExp (EExists (Var v) _ e) = Quantifier Exists [v] (convertExp e)
convertExp (ECtx Exp [VCtx]) = 
convertExp (ESensVar Ident Int) = 
convertExp _ = error "Cannot convert Exp to ExpS"

ExpS ::
	      | Predicate Predicate [Term]
	      | Equal Term Term

Exp : 
   EEquiv Exp Exp
 | EEqual Exp Exp
 | EImplies Exp Exp
 | EOr Exp Exp
 | EAnd Exp Exp
 | ENot Exp
 | EFalse
 | ETrue
 | EPattern Pattern
 | EPred Pred
 | EForAll Var ODom Exp
 | EExists Var ODom Exp
 | ECtx Exp [VCtx]
 | ESensVar Ident Int
 | ENull
-}
 

fileMain :: FilePath -> [String] -> IO ()
fileMain question args =
    do 
        prgName <- getProgName
        starttime <- getClockTime
        startCal <- toCalendarTime starttime
        (exp,modstr) <- parseFile question
        if length args >= 5
           then do let mandatargs = take 5 args
                   let switchargs = drop 5 args
                   let [ltmf, pmf, p, sm, wm] = mandatargs
                   e <- parseExp exp
                   typeCheckExp prgName e
                   ltm <- readFile ltmf
                   ltmc <- parseLtm ltm
                   pmstr <- readFile pmf
                   let model = extractModel modstr 1
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
                   putStrLn $ "\nQuestion 1: " ++ (if p == TERMINAL_ then (printExp e ++ "\n") else printExp e ++ "\n")
                   putStrLn $ "SM=" ++ sm
                   putStrLn $ "WM=" ++ wm ++ "\n"
                   solutions <- iterativeSolve (Ctx (sensmap, memsegs, psystem, (read sm, read wm), [], sargv, pm, model, coffs)) e' 0 0
                   printSolutions (snd pm) (if any (=="--print-all") (map (map toLower) args) then solutions else [head solutions]) sargv
                   endtime <- getClockTime
                   endCal <- toCalendarTime endtime
                   putStrLn $ "Started:  " ++ calendarTimeToString  startCal
                   putStrLn $ "Finished: " ++ calendarTimeToString  endCal
                   let difftime = diffClockTimes endtime starttime

                   putStrLn $ "Duration: " ++ show (timeDiff endtime starttime) -- timeDiffToString difftime
					  
              -- if arguments < 8
           else -- if fewer arguments, print the truth value of given expression
                        do e <- parseExp exp                          -- parse expression
                           let model = extractModel modstr 1 -- extract required model
                           case eval model (inspectDomain model e) of -- print truth value
                               Just ETrue -> putStrLn "TRUE"
                               Just EFalse -> putStrLn "FALSE"
                               _           -> error "hmm.. something is wrong"
                           return ()

-- absolute time difference in seconds and picoseconds
timeDiff :: ClockTime -> ClockTime -> ClockTime
timeDiff (TOD a1 b1) (TOD a2 b2) = TOD (abs (a1 - a2)) (abs (b1 - b2))

showTime :: ClockTime -> String
showTime (TOD a b) = show a ++ "." ++ printf "%3d" (b `div` 1000000000)

parseFile :: FilePath -> IO (String,String)
parseFile filename =
    do  file <- readFile filename
        let lin = lines file
        let exp = if null lin then "" else head lin
        let model = if length lin > 1 then unlines (tail lin) else ""
        return (exp,model)

stringMain :: String -> [String] -> IO ()
stringMain exp args = 
    do  prgName <- getProgName
        starttime <- getClockTime
        startCal <- toCalendarTime starttime

        if length args >= 7  -- if there are atleast 8 arguments
              then do let mandatargs = take 7 args   -- then separate into mandatory and switch args
                      let switchargs = drop 7 args
                      let [ltmf, pmf, modf, mdl, p, sm, wm] = mandatargs -- expand mandatory arguments
                      e     <- parseExp exp    -- parse the expression
                      typeCheckExp prgName e   -- typecheck the expression
                      ltm   <- readFile ltmf   -- read ltm file into string
                      ltmc  <- parseLtm ltm    -- parse the ltm string
                      pmstr <- readFile pmf    -- read pm file into string
                      modstr <- readFile modf  -- read model file into string
                      let model = extractModel modstr (read mdl)  -- extract the required model from model file

                      -- writeFile "latexltm.txt" (compileLtm ltmc) 
                      {- tquests' <- readFile "pntauts.txt"
                      tquests <- sequence $ map parseExp $ lines $ tquests'
                      ttabf <- readFile "ntauttab.txt"
                      let ttab = map words $ lines ttabf
                      let ttab' = [[compileExp q] ++ tdata | (q, tdata) <- zip tquests ttab]
                      let table = makeTable ttab'
                      putStrLn table -}

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
                      putStrLn $ "Duration: " ++ showTime (timeDiff endtime starttime) -- timeDiffToString difftime
					  
              -- if arguments < 8
              else case args of

                       {- [fname] -> 
                           if (map toLower fname) == "--help"
                               then printErrMsg prgName
                               else do ltm <- readFile fname
                                       ltmc <- parseLtm ltm
                                       putStrLn $ compileLtm ltmc -}

                       -- if only 3 arguments, print the truth value of given expression
                       [modf, mdl] ->
                           do e <- parseExp exp                          -- parse expression
                              modstr <- readFile modf                    -- read model file
                              let model = extractModel modstr (read mdl) -- extract required model
                              case eval model (inspectDomain model e) of -- print truth value
                                  Just ETrue -> putStrLn "TRUE"
                                  Just EFalse -> putStrLn "FALSE"
                                  _           -> error "hmm.. something is wrong"
                              return ()
                       _ ->  printErrMsg prgName


main :: IO ()
main = do args    <- getArgs
          prgName <- getProgName
          -- writeFile "temp.txt" ""
          -- writeFile "biskvi.txt" ""
          if any (== "--help") args then printErrMsg prgName else return ()

          if length args >= 1
            then if reverse (take 4 (reverse (head args))) == ".txt"
                   then fileMain (head args) (tail args)
                   else stringMain (head args) (tail args)
            else printErrMsg prgName

          
