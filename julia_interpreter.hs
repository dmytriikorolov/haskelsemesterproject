import System.Environment
import System.IO
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr


-- as i described before my program will only work with bools and ints,
-- because i need to have both x = 10 and y = true, i decided to keep it this way
data Value =
      IntValue Int
    | BoolValue Bool
    deriving (Eq, Show)


-- this part are for my expressions
-- examples: 5, true, x, x+1, !x , max(1,2  ), let SMTH SMTH end .
data Expr =
      Number Int
    | Boolean Bool
    | Variable String
    | Binary String Expr Expr
    | Unary String Expr
    | Function String [Expr]
    | LetExpr [(String, Expr)] Expr
    deriving Show


-- this is for statements, examples: x = 10, println(x), if expr ..., while ..., let ... [(string, expr)]
-- is for something like this: let x = 10, y = 10 end
data Stmt =
      Assign String Expr
    | Println Expr
    | If Expr [Stmt] [Stmt]
    | While Expr [Stmt]
    | For String Expr Expr [Stmt]
    | Let [(String, Expr)] [Stmt]
    | ExprStmt Expr
    deriving Show


-- the last exprstmt is not very needed but i decided to keep for expressions like x + 1

-- this is environment, it stores [("x", IntValue 10), ("ok", BoolValue True)] which represents x =10, ok = true
type Env = [(String, Value)]


-- these are the words to which we cant assign anything, like if = 10.
wordsUsed :: [String]
wordsUsed =
    ["if", "else", "end", "while", "for", "let", "println",
     "true", "false", "div", "mod"]


-- run parser and ignore spaces in beginning and in the end
lexeme :: Parser a -> Parser a
lexeme p =
    try (spaces *> p <* spaces)


-- here we pass just one piece of text while ignoring whitespaces
symbol :: String -> Parser String
symbol s =
    lexeme (string s)


-- here we parse reserved words
-- algorithm: notFollowedBy checks that the word is not only beginning
-- of longer variable name, for example "ifx" etc
word :: String -> Parser String
word s =
    try (spaces *> string s <* notFollowedBy (alphaNum <|> char '_') <* spaces)


-- here we parse variable name, trim spaces, then take first letter (it only accepts first symbols as letter)
-- then we take as many nums, char or _, glue everything in one word and check if its not a reserved word
name :: Parser String
name = try $ do
    spaces
    first <- letter
    rest <- many (alphaNum <|> char '_')
    spaces
    let s = first : rest
    if elem s wordsUsed
        then unexpected "reserved word"
        else return s


-- parse Julia integer into Number
number :: Parser Expr
number = do
    digits <- spaces *> many1 digit <* spaces
    return (Number (read digits))


-- same idea but for bool
boolean :: Parser Expr
boolean =
        (word "true" *> return (Boolean True))
    <|> (word "false" *> return (Boolean False))


-- here we parse a variable, e.g x
variable :: Parser Expr
variable = do
    s <- name
    return (Variable s)


-- parses a function call like max(3, 7)
-- try because we can have max as variable name
functionCall :: Parser Expr
functionCall = try $ do
    s <- name
    args <- symbol "(" *> sepBy expr (symbol ",") <* symbol ")"
    return (Function s args)


-- here we parse expression inside ()
parens :: Parser Expr
parens =
    symbol "(" *> expr <* symbol ")"


-- parses one assignment inside let
-- returns the variable name and expression
oneLetAssign :: Parser (String, Expr)
oneLetAssign = try $ do
    s <- name
    e <- symbol "=" *> expr
    return (s, e)


-- parses let used as an expression
 -- it has local assignments and one final result expression
letExpression :: Parser Expr
letExpression = do
    assigns <- word "let" *> many oneLetAssign
    result <- expr <* word "end"
    return (LetExpr assigns result)


-- this is first building block i would say
-- this parses a very simple expression like (1+2) or let x = 1 x + 1 end
term :: Parser Expr
term =
        parens
    <|> letExpression
    <|> boolean
    <|> number
    <|> functionCall
    <|> variable


-- here we create parser for a binary symbol operator like + or *
-- assoc tells parsec how repeated operators are grouped
binarySymbol op assoc =
    Infix parser assoc
        where
            parser =
                symbol op *> return (Binary op)


-- same idea as before but its for div and mod
binaryWord op assoc =
    Infix parser assoc
        where
            parser =
                word op *> return (Binary op)


-- this is for unary ops like ! or -
unarySymbol op =
    Prefix parser
        where
            parser =
                symbol op *> return (Unary op)


operators =
    [
        [ unarySymbol "!", unarySymbol "-" ],
        [ binarySymbol "*" AssocLeft,
        binarySymbol "/" AssocLeft,
        binaryWord "div" AssocLeft,
        binaryWord "mod" AssocLeft],
        [ binarySymbol "+" AssocLeft,
        binarySymbol "-" AssocLeft],
        [ binarySymbol "<=" AssocNone,
        binarySymbol ">=" AssocNone,
        binarySymbol "==" AssocNone,
        binarySymbol "!=" AssocNone,
        binarySymbol "<" AssocNone,
        binarySymbol ">" AssocNone],
        [ binarySymbol "&&" AssocLeft ],
        [ binarySymbol "||" AssocLeft ]
    ]


-- builds the full expression parser from simple terms and operators
expr :: Parser Expr
expr =
    buildExpressionParser operators term


-- parses assignment like x = 10
assignment :: Parser Stmt
assignment = try $ do
    s <- name
    e <- symbol "=" *> expr
    return (Assign s e)


-- here i parse println with one expression inside parentheses
printlnStmt :: Parser Stmt
printlnStmt = do
    e <- word "println" *> symbol "(" *> expr <* symbol ")"
    return (Println e)


-- parse if with optional else part
-- parse until we see else or end
ifStmt :: Parser Stmt
ifStmt = do
    cond <- word "if" *> expr
    yes <- manyTill stmt (lookAhead (word "else" <|> word "end"))
    w <- word "else" <|> word "end"
    if w == "else"
        then do
            no <- manyTill stmt (word "end")
            return (If cond yes no)
        else
            return (If cond yes [])


-- here we parse while loop with condition and body
-- body is read until end
whileStmt :: Parser Stmt
whileStmt = do
    cond <- word "while" *> expr
    body <- manyTill stmt (word "end")
    return (While cond body)


-- this parses for loop like for i = 1:10
forStmt :: Parser Stmt
forStmt = do
    s <- word "for" *> name
    first <- symbol "=" *> expr
    last <- symbol ":" *> expr
    body <- manyTill stmt (word "end")
    return (For s first last body)


-- let which is used as statement block
letStmt :: Parser Stmt
letStmt = do
    assigns <- word "let" *> many oneLetAssign
    body <- manyTill stmt (word "end")
    return (Let assigns body)


-- this allows to accepts lines like x + 1
exprStmt :: Parser Stmt
exprStmt = do
    e <- expr
    return (ExprStmt e)


-- this chooses what kind of statement is next
stmt :: Parser Stmt
stmt =
        ifStmt
    <|> whileStmt
    <|> forStmt
    <|> letStmt
    <|> printlnStmt
    <|> assignment
    <|> exprStmt


-- this parses the whole program as a list of statements
program :: Parser [Stmt]
program =
    spaces *> many stmt <* eof


-- searches for a variable in the environment
-- returns runtime error if the name is not found
findVar :: String -> Env -> Either String Value
findVar s [] =
    Left "Runtime error"
findVar s ((name, value) : rest)
    | s == name = Right value
    | otherwise = findVar s rest


-- changes var value in env
-- if the value does not exist we just add it
changeVar :: String -> Value -> Env -> Env
changeVar s value [] =
    [(s, value)]
changeVar s value ((name, old) : rest)
    | s == name = (name, value) : rest
    | otherwise = (name, old) : changeVar s value rest


-- this is a short helper for any runtime errors
bad :: Either String a
bad =
    Left "Runtime error"


-- beginning of evaluations process


-- eval a number
evalExpr :: Env -> Expr -> Either String Value
evalExpr env (Number n) =
    Right (IntValue n)


-- eval a bool
evalExpr env (Boolean b) =
    Right (BoolValue b)


-- find var value in the env
evalExpr env (Variable s) =
    findVar s env


-- eval with "-" in front
evalExpr env (Unary "-" e) = do
    value <- evalExpr env e
    case value of
        IntValue n -> Right (IntValue (-n))
        _ -> bad


-- eval negation
evalExpr env (Unary "!" e) = do
    value <- evalExpr env e
    case value of
        BoolValue b -> Right (BoolValue (not b))
        _ -> bad


-- here we evaluate binary ops, i used the idea we used on tutorial somewhere in the beginning
-- is we have false && anything we can just return false, same idea with true ||
evalExpr env (Binary "&&" a b) = do
    left <- evalExpr env a
    case left of
        BoolValue False -> Right (BoolValue False)
        BoolValue True -> evalExpr env b
        _ -> bad


evalExpr env (Binary "||" a b) = do
    left <- evalExpr env a
    case left of
        BoolValue True -> Right (BoolValue True)
        BoolValue False -> evalExpr env b
        _ -> bad


-- this is for normal binary ops
evalExpr env (Binary op a b) = do
    left <- evalExpr env a
    right <- evalExpr env b
    evalBinary op left right


-- first we evaluate function args and then we evaluate the whole function
evalExpr env (Function s args) = do
    values <- evalExprs env args
    evalFunction s values


-- evaluation for let
evalExpr env (LetExpr assigns result) = do
    newEnv <- evalAssigns env assigns
    evalExpr newEnv result


-- evaluate list of exprs
evalExprs :: Env -> [Expr] -> Either String [Value]
evalExprs env [] =
    Right []
evalExprs env (e : rest) = do
    value <- evalExpr env e
    values <- evalExprs env rest
    return (value : values)


-- the whole block below is rather tedious, i just go through all possible binary ops
evalBinary :: String -> Value -> Value -> Either String Value
evalBinary "+" (IntValue a) (IntValue b) =
    Right (IntValue (a + b))


evalBinary "-" (IntValue a) (IntValue b) =
    Right (IntValue (a - b))


evalBinary "*" (IntValue a) (IntValue b) =
    Right (IntValue (a * b))


evalBinary "/" (IntValue a) (IntValue b)
    | b == 0 = bad
    | otherwise = Right (IntValue (a `div` b))


evalBinary "div" (IntValue a) (IntValue b)
    | b == 0 = bad
    | otherwise = Right (IntValue (a `div` b))


evalBinary "mod" (IntValue a) (IntValue b)
    | b == 0 = bad
    | otherwise = Right (IntValue (a `mod` b))


evalBinary "<" (IntValue a) (IntValue b) =
    Right (BoolValue (a < b))


evalBinary "<=" (IntValue a) (IntValue b) =
    Right (BoolValue (a <= b))


evalBinary ">" (IntValue a) (IntValue b) =
    Right (BoolValue (a > b))


evalBinary ">=" (IntValue a) (IntValue b) =
    Right (BoolValue (a >= b))


evalBinary "==" a b =
    Right (BoolValue (a == b))


evalBinary "!=" a b =
    Right (BoolValue (a /= b))


evalBinary _ _ _ =
    bad


-- same here, all functions
evalFunction :: String -> [Value] -> Either String Value
evalFunction "abs" [IntValue a] =
    Right (IntValue (abs a))


evalFunction "min" [IntValue a, IntValue b] =
    Right (IntValue (min a b))


evalFunction "max" [IntValue a, IntValue b] =
    Right (IntValue (max a b))


evalFunction _ _ =
    bad


-- eval assignment and changes env
evalStmt :: Env -> Stmt -> Either String (Env, [String])
evalStmt env (Assign s e) = do
    value <- evalExpr env e
    return (changeVar s value env, [])


-- everything below is also self explanatory
evalStmt env (Println e) = do
    value <- evalExpr env e
    return (env, [showValue value])


evalStmt env (ExprStmt e) = do
    value <- evalExpr env e
    return (env, [])


evalStmt env (If cond yes no) = do
    value <- evalExpr env cond
    case value of
        BoolValue b -> evalBlock env (if b then yes else no)
        _ -> bad


evalStmt env (While cond body) =
    evalWhile env cond body


evalStmt env (For s first last body) = do
    a <- evalExpr env first
    b <- evalExpr env last
    case (a, b) of
        (IntValue start, IntValue stop) ->
            evalFor env s start stop body
        _ ->
            bad


evalStmt env (Let assigns body) = do
    newEnv <- evalAssigns env assigns
    (after, output) <- evalBlock newEnv body
    return (env, output)


-- evaluate list of statements
evalBlock :: Env -> [Stmt] -> Either String (Env, [String])
evalBlock env [] =
    Right (env, [])
evalBlock env (s : rest) = do
    (env2, out1) <- evalStmt env s
    (env3, out2) <- evalBlock env2 rest
    return (env3, out1 ++ out2)


-- evaluate while with this logic:
-- eval cond -> if true run once then we call evalwhile again
-- if false then stop
evalWhile :: Env -> Expr -> [Stmt] -> Either String (Env, [String])
evalWhile env cond body = do
    value <- evalExpr env cond
    case value of
        BoolValue True -> do
            (env2, out1) <- evalBlock env body
            (env3, out2) <- evalWhile env2 cond body
            return (env3, out1 ++ out2)
        BoolValue False ->
            Right (env, [])
        _ -> bad


-- same recursive idea as while, but with int counter
evalFor :: Env -> String -> Int -> Int -> [Stmt] -> Either String (Env, [String])
evalFor env s i stop body
    | i > stop = Right (env, [])
    | otherwise = do
        let env2 = changeVar s (IntValue i) env
        (env3, out1) <- evalBlock env2 body
        (env4, out2) <- evalFor env3 s (i + 1) stop body
        return (env4, out1 ++ out2)


-- eval LOCAL assignment for let
evalAssigns :: Env -> [(String, Expr)] -> Either String Env
evalAssigns env [] =
    Right env
evalAssigns env ((s, e) : rest) = do
    value <- evalExpr env e
    evalAssigns (changeVar s value env) rest


-- this block is for showing ints and bools in julia style
showValue :: Value -> String
showValue (IntValue n) =
    show n


showValue (BoolValue True) =
    "true"


showValue (BoolValue False) =
    "false"


-- here we parse text first and then run the parsed statements
runCode :: Env -> String -> Either String (Env, [String])
runCode env input =
    case parse program "" input of
        Left err ->
            Left "Parse error"
        Right stmts ->
            evalBlock env stmts


-- just printing list of strings
printLines :: [String] -> IO ()
printLines [] =
    return ()
printLines (x : xs) = do
    putStrLn x
    printLines xs


-- printing program output
printResult :: Either String (Env, [String]) -> IO ()
printResult (Left err) =
    putStrLn err
printResult (Right (env, output)) =
    printLines output


-- this is for running program from a file
runFile :: String -> IO ()
runFile file = do
    text <- readFile file
    printResult (runCode [] text)


repl :: Env -> IO ()
repl env = do
    putStr "julia> "
    hFlush stdout
    line <- getLine
    if line == "quit"
        then return ()
        else
            case runCode env line of
                Left err -> do
                    putStrLn err
                    repl env
                Right (newEnv, output) -> do
                    printLines output
                    repl newEnv


main :: IO ()
main = do
    args <- getArgs
    case args of
        [file] -> runFile file
        [] -> repl []
        _ -> putStrLn "Usage: runhaskell julia_interpreter.hs file.jl"
