{--
Write a Haskell program that simulates a deterministic finite accepter (DFA), or a
nondeterministic finite acceptor (NDFA). The first has difficulty level 1, the second difficulty level
2. The program should read the specification of the (N)DFA from a file, in the format of your own
choosing, which will include the alphabet, the set of states, the initial and accept states, and the
transition function (or transition relation in the case of nondeterministic automata). It should then
prompt the user for the input string, simulate execution of the (N)DFA on that string, and output
whether the string was accepted or not. Then the program should prompt the user for another
string and simulate the same (N)DFA again until the process is killed or a special “quit”
instruction is given.
--}
import Data.Char
import Data.List
import Data.Maybe

type Var = String
type Inputs = Char 

data States = AcceptStartstate | Startstate | Acceptstate Var | Normalstate Var | Transition Inputs States States 
    deriving (Show,Eq)

data Token = StartK  | AcceptK String | NormalK String | DashK  | ArrowK | InputK Char | Err String | LPar | RPar | ST States | AStartK
    deriving (Eq,Show)

--example input 
ex1 :: String
ex1 = "(Start -0> N1) (Start -1> N2) (N1 -1> N1) (N1 -0> N2) (N2 -1> A1) (N2 -0> N2)"


--Basic lexer that turns strings into tokens

lexer :: String -> [Token]
lexer "" = []
lexer ('(' : xs) = LPar : lexer xs
lexer (')' : xs) = RPar : lexer xs
lexer s | isPrefixOf "Start" s = StartK : lexer (drop 5 s)
lexer s | isPrefixOf "AcceptStart" s = AStartK : lexer (drop 11 s)
lexer (x:xs) | (x == 'A') = let (hd, tl) = span (\x -> isAlphaNum x) xs
                            in AcceptK (x : hd) : lexer tl
lexer (x:xs) | (x == 'N') = let (hd, tl) = span (\x -> isAlphaNum x) xs
                            in NormalK (x : hd) : lexer tl 
lexer ('-':xs) = DashK : lexer xs
lexer (x : xs) | isLower x = InputK x : lexer xs
lexer (x: xs ) | isDigit x = InputK x : lexer xs
lexer ('>':xs) = ArrowK : lexer xs
--lexer s | isPrefixOf "->" s = TransitionK : lexer (drop 2 s)
lexer (x:xs) | isSpace x = lexer xs 
lexer s = [Err s]

ex6 :: [Token]
ex6 = [LPar,StartK,DashK,InputK '0',ArrowK,NormalK "N1",RPar,LPar,StartK,DashK,InputK '1',ArrowK,NormalK "N2",RPar,LPar,NormalK "N1",DashK,InputK '1',ArrowK,NormalK "N1",RPar,LPar,NormalK "N1",DashK,InputK '0',ArrowK,NormalK "N2",RPar,LPar,NormalK "N2",DashK,InputK '1',ArrowK,AcceptK "A1",RPar,LPar,NormalK "N2",DashK,InputK '0',ArrowK,NormalK "N2",RPar]


fullParsed :: [Token] -> [States]
fullParsed x = reverse(parser(sr [] x))

parser :: [Token] -> [States]
parser [] = []
parser (ST (Transition x t1 t2) : xs) = (Transition x t1 t2) : parser xs
parser _ = error $ "improper parsing"

ex3 :: [Token]
ex3 = [ST (Transition '0' (Normalstate "N2") (Normalstate "N2")),
       ST (Transition '1' (Normalstate "N2") (Acceptstate "A1")),
       ST (Transition '0' (Normalstate "N1") (Normalstate "N2")),
       ST (Transition '1' (Normalstate "N1") (Normalstate "N1")),
       ST (Transition '1' Startstate (Normalstate "N2")),
       ST (Transition '0' Startstate (Normalstate "N1"))]

sr :: [Token] -> [Token] -> [Token]
sr (RPar : ST t1 : LPar : s) i = sr (ST t1 : s) i  --Handles parapheses
sr (StartK : s) i = sr(ST (Startstate) : s) i  --Start State
sr (AStartK : s) i = sr(ST (AcceptStartstate) : s) i --Accept Start State
sr (NormalK x : s) i = sr(ST (Normalstate x) : s) i --Normal State
sr (AcceptK x : s) i = sr(ST (Acceptstate x) : s) i  -- Accept State
sr (ST t2 : ArrowK : InputK x : DashK : ST t1 : s) i = sr (ST (Transition x t1 t2) : s) i --parses into transition state
sr s (i : is ) = sr (i : s) is
sr s [] = s

ex2 :: [Token]
ex2 = [LPar,StartK,DashK,InputK '0',ArrowK,NormalK "N1",RPar,LPar,StartK,DashK,InputK '1',ArrowK,NormalK "N2",RPar,LPar,NormalK "N1",DashK,InputK '1',ArrowK,NormalK "N1",RPar,LPar,NormalK "N1",DashK,InputK '0',ArrowK,NormalK "N2",RPar,LPar,NormalK "N2",DashK,InputK '1',ArrowK,AcceptK "A1",RPar,LPar,NormalK "N2",DashK,InputK '0',ArrowK,NormalK "N2",RPar]

type Env = [((States,Inputs) , States)]

loadEnv :: [States] -> Env -> Env
loadEnv [] env = env  
loadEnv ((Transition x t1 t2) : xs) env = ((t1, x), t2) : loadEnv xs env
loadEnv _ env = error $ "Not properly parsed"

checkDFAHelper2 :: ((States,Inputs),States) -> Env -> Bool
checkDFAHelper2 x [] = True
checkDFAHelper2 ((x,y),z) (((x1,y1),z1) : xs) | x == x1 && y == y1 = False
checkDFAHelper2 ((x,y),z) (((x1,y1),z1) : xs) = checkDFAHelper2 ((x,y),z) xs

checkDFA :: Env -> Bool
checkDFA [] = True
checkDFA env = if checkDFAHelper2 (head env) (tail env) then checkDFA (tail env) else False

ex4 :: [States]
ex4 = [Transition '1' (Normalstate "N2") (Normalstate "N2"),
       Transition '0' (Normalstate "N2") (Acceptstate "A1"),
       Transition '0' (Normalstate "N1") (Normalstate "N2"),
       Transition '1' (Normalstate "N1") (Normalstate "N1"),
       Transition '1' Startstate (Normalstate "N2"),
       Transition '0' Startstate (Normalstate "N1")]

ex5 :: Env 
ex5 = [((Normalstate "N2",'1'),Normalstate "N2"),
       ((Normalstate "N2",'0'),Acceptstate "A1"),
       ((Normalstate "N1",'0'),Normalstate "N2"),
       ((Normalstate "N1",'1'),Normalstate "N1"),
       ((Startstate,'1'),Normalstate "N2"),
       ((Startstate,'0'),Normalstate "N1")]

transition :: States -> Char -> Env -> States
transition x y [] = x
transition x y (((x1,y1), s) : xs) | x == x1 && y == y1 = s
transition x y (((x1,y1), s) : xs) = transition x y xs

checkTransition :: States -> Char -> Env -> Bool
checkTransition x y [] = False
checkTransition x y (((x1,y1), s) : xs) | x == x1 && y == y1 = True
checkTransition x y (((x1,y1), s) : xs) = checkTransition x y xs

transitions :: States -> String -> Env -> States
transitions st1 "" env = st1
transitions st1 (x:xs) env = if (checkTransition st1 x env) then transitions (transition st1 x env) xs env else st1 


solve :: States -> String
solve (Normalstate t1) = "Grammer is not apart of DFA, Accept state: " ++ show t1 
solve (Startstate ) = "Grammer is apart of DFA, Accept state: Startstate"
solve (AcceptStartstate) = "Grammer is apart of DFA: Startstate"
solve (Acceptstate t1) = "Grammer is apart of DFA, Accept state: " ++ show t1 
solve (Transition x t1 t2) = error $ "not properly parsed"


isAccept :: Env -> Bool
isAccept [] = False
isAccept (((x,y),z) : xs) | x == (AcceptStartstate) || z == (AcceptStartstate) = True
isAccept (((x,y),z) : xs) | x == (AcceptStartstate) && z == (Startstate) = error $ "Invalid DFA"
isAccept (((x,y),z) : xs) | x == (Startstate) && z == (AcceptStartstate) = error $ "Invalid DFA"
isAccept (((x,y),z) : xs) = isAccept xs 



main :: IO ()
main = do
    putStrLn "Please enter a file name: "
    file <- getLine 
    code <- readFile file  
    let parsed = fullParsed (lexer code)
    let dfa = loadEnv parsed []
    if checkDFA dfa then do 
        putStrLn "DFA is valid and fully parsed"
        if isAccept dfa then do 
           loop (AcceptStartstate) (dfa)
        else do
           loop (Startstate) (dfa)
    else do 
       putStrLn $ "Not a valid DFA"

loop :: States -> Env -> IO ()
loop state env = do 
  putStrLn "Please enter a grammar or the word .Quit: " 
  a <- getLine
  case words a of 
    [".Quit"] -> do
       putStrLn "Bye! have a great day"
       return ()
    _ -> do
      let tren = transitions (state) a (env)
      let answer = solve (tren)
      putStrLn $ "Answer: " ++ answer
      loop (state) (env)
    
  

