module REPL where
import Prelude hiding (print, (<>))
import Control.Monad.Except
import Data.List
import Data.Char
import Text.PrettyPrint.HughesPJ hiding (parens)
import qualified Text.PrettyPrint.HughesPJ as PP
import Text.ParserCombinators.Parsec hiding (parse, State)
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language
import System.IO hiding (print)
import System.IO.Error
import Common

{-The REPL module provides functionality for a Read-Eval-Print Loop (REPL) 
for the lambda calculus interpreter. It includes parsing and handling interactive 
commands, evaluating expressions, handling let bindings, type checking, and more.-}

-- Represents different commands that can be executed in the interpreter.
data Command = TypeOf String
             | Compile CompileForm
             | Browse
             | Quit
             | Help
             | Noop

-- Represents the form to compile.
data CompileForm = CompileInteractive  String
                 | CompileFile         String

-- is a data type that represents interactive commands in the interpreter
data InteractiveCommand = Cmd [String] String (String -> Command) String

-- represents the context in the interpreter where names are associated with information of type inf. This context is crucial for type checking, evaluating expressions, and maintaining the state of the interpreter.
type Ctx inf = [(Name, inf)]

-- represents the state of the interpreter during its execution. 
type State v inf = (String, NameEnv v, Ctx inf)

-- data type encapsulates all the essential components required for interpreting the specific programming language
data Interpreter i c v t tinf inf =
  I { iname :: String,
      iprompt :: String,
      iitype :: NameEnv v -> Ctx inf -> i -> Result t,
      iquote :: v -> c,
      ieval  :: NameEnv v -> i -> v,
      ihastype :: t -> inf,
      icprint :: c -> Doc,
      itprint :: t -> Doc,
      iiparse :: CharParser () i,
      isparse :: CharParser () (Stmt i tinf),
      iassume :: State v inf -> (String, tinf) -> IO (State v inf) }

-- function generates a help message for the interactive environment based on a list of interactive commands (InteractiveCommand)
helpTxt :: [InteractiveCommand] -> String
helpTxt cs
  =  "List of commands:  Any command may be abbreviated to :c where\n" ++
     "c is the first character in the full name.\n\n" ++
     "<expr>                  evaluate expression\n" ++
     "let <var> = <expr>      define variable\n" ++
     "assume <var> :: <expr>  assume variable\n\n"
     ++
     unlines (map (\ (Cmd cs a _ d) -> let  ct = concat (intersperse ", " (map (++ if null a then "" else " " ++ a) cs))
                                       in   ct ++ replicate ((24 - length ct) `max` 2) ' ' ++ d) cs)

-- contains instances of the InteractiveCommand data type. Each instance represents a specific interactive command that the user can execute in the REPL                                      
commands :: [InteractiveCommand]
commands
  =  [ Cmd [":type"]        "<expr>"  TypeOf         "print type of expression",
       Cmd [":browse"]      ""        (const Browse) "browse names in scope",
       Cmd [":load"]        "<file>"  (Compile . CompileFile)
                                                     "load program from file",
       Cmd [":quit"]        ""        (const Quit)   "exit interpreter",
       Cmd [":help",":?"]   ""        (const Help)   "display this list of commands" ]

dummy = makeTokenParser (haskellStyle { identStart = letter <|> P.char '_',
                                              reservedNames = [] })

-- provides a way to parse a string (x) using a specified parser (p), handling any parse errors and returning the parsed result wrapped in a Maybe
parseIO :: String -> CharParser () a -> String -> IO (Maybe a)
parseIO f p x = case P.parse (whiteSpace dummy >> p >>= \ x -> eof >> return x) f x of
                  Left e  -> putStrLn (show e) >> return Nothing
                  Right r -> return (Just r)

-- sets up an interactive environment where the user can input commands, and the interpreter processes these commands and provides outputs. The loop continues until the user decides to exit the interpreter
readevalprint :: Interpreter i c v t tinf inf -> State v inf -> IO ()
readevalprint int state@(out, ve, te) =
  let rec int state =
        do
          putStr (iprompt int)
          hFlush stdout
          x <- catchIOError (fmap Just getLine) (\_ -> return Nothing)
          case x of
            Nothing   ->  return ()
            Just ""   ->
              rec int state
            Just x    ->
              do
                c  <- interpretCommand x
                state' <- handleCommand int state c
                maybe (return ()) (rec int) state'
  in
    do
      --  welcome
      putStrLn ("Interpreter for " ++ iname int ++ ".\n" ++
                             "Type :? for help.")
      --  enter loop
      rec int state

-- This function essentially interprets user input strings and translates them into commands that the interpreter can understand and execute
interpretCommand :: String -> IO Command
interpretCommand x
  =  if isPrefixOf ":" x then
       do  let  (cmd,t')  =  break isSpace x
                t         =  dropWhile isSpace t'
           --  find matching commands
           let  matching  =  filter (\ (Cmd cs _ _ _) -> any (isPrefixOf cmd) cs) commands
           case matching of
             []  ->  do  putStrLn ("Unknown command `" ++ cmd ++ "'. Type :? for help.")
                         return Noop
             [Cmd _ _ f _]
                 ->  do  return (f t)
             x   ->  do  putStrLn ("Ambiguous command, could be " ++ concat (intersperse ", " [ head cs | Cmd cs _ _ _ <- matching ]) ++ ".")
                         return Noop
     else
       return (Compile (CompileInteractive x))

-- interprets different commands and performs corresponding actions, updating the state of the interpreter accordingly
handleCommand :: Interpreter i c v t tinf inf -> State v inf -> Command -> IO (Maybe (State v inf))
handleCommand int state@(out, ve, te) cmd
  =  case cmd of
       Quit   ->  (putStrLn "!@#$^&*") >> return Nothing
       Noop   ->  return (Just state)
       Help   ->  putStr (helpTxt commands) >> return (Just state)
       TypeOf x ->
                  do  x <- parseIO "<interactive>" (iiparse int) x
                      t <- maybe (return Nothing) (iinfer int ve te) x
                      maybe (return ()) (\u -> putStrLn (render (itprint int u))) t
                      return (Just state)
       Browse ->  do  putStr (unlines [ s | Global s <- reverse (nub (map fst te)) ])
                      return (Just state)
       Compile c ->
                  do  state <- case c of
                                 CompileInteractive s -> compilePhrase int state s
                                 CompileFile f        -> compileFile int state f
                      return (Just state)

-- responsible for reading, parsing, and evaluating the content of a file, updating the interpreter state based on the statements in the file
compileFile :: Interpreter i c v t tinf inf -> State v inf -> String -> IO (State v inf)
compileFile int state@(out, ve, te) f =
  do
    x <- readFile f
    stmts <- parseIO f (many (isparse int)) x
    maybe (return state) (foldM (handleStmt int) state) stmts

-- function in the interpreter takes an input string (x), parses it into a single statement using the isparse parser, and then evaluates the parsed statement using the handleStmt function.
compilePhrase :: Interpreter i c v t tinf inf -> State v inf -> String -> IO (State v inf)
compilePhrase int state@(out, ve, te) x =
  do
    x <- parseIO "<interactive>" (isparse int) x
    maybe (return state) (handleStmt int state) x

-- responsible for inferring the type of an input term t given the variable environment d and type environment g.
iinfer int d g t =
  case iitype int d g t of
    Left e -> putStrLn e >> return Nothing
    Right v -> return (Just v)

-- This function handles different types of statements, ensuring that the interpreter can process assumptions, assignments, evaluations, and output operations correctly, updating the state as necessary
handleStmt :: Interpreter i c v t tinf inf
              -> State v inf -> Stmt i tinf -> IO (State v inf)
handleStmt int state@(out, ve, te) stmt =
  do
    case stmt of
        Assume ass -> foldM (iassume int) state ass
        Let x e    -> checkEval x e
        Eval e     -> checkEval it e
        PutStrLn x -> putStrLn x >> return state
        Out f      -> return (f, ve, te)
  where 
    --  checkEval :: String -> i -> IO (State v inf)
    checkEval i t =
      check int state i t
        (\ (y, v) -> do
                       let outtext = if i == it then render (icprint int (iquote int v) <> text " :: " <> itprint int y)
                                                else render (text i <> text " :: " <> itprint int y)
                       putStrLn outtext
                       unless (null out) (writeFile out (process outtext)))
        (\ (y, v) -> ("", (Global i, v) : ve, (Global i, ihastype int y) : te))

-- This function is essentially handling the process of type-checking an input expression, evaluating it, and then processing the result and updating the state accordingly.
check :: Interpreter i c v t tinf inf -> State v inf -> String -> i
         -> ((t, v) -> IO ()) -> ((t, v) -> State v inf) -> IO (State v inf)
check int state@(out, ve, te) i t kp k =
                do
                  -- i: String, t: Type
                  --  typecheck and evaluate
                  x <- iinfer int ve te t
                  case x of
                    Nothing  ->
                      do
                        --  putStrLn "type error"
                        return state
                    Just y   ->
                      do
                        let v = ieval int ve t
                        kp (y, v)
                        return (k (y, v))

--  takes a string, splits it into lines, adds "< " in front of each line, and then concatenates these modified lines back into a single string separated by newline characters.
it = "it"
process :: String -> String
process = unlines . map (\ x -> "< " ++ x) . lines