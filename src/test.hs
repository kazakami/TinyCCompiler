module Main where
import Control.Monad
import System.Environment
import Control.Monad.Error
import Control.Applicative hiding ((<|>), many)
import Data.IORef
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.ParserCombinators.Parsec.Language
import GHC.IO.Handle
import System.IO
import Data.Maybe
import Data.List
import Data.Char (ord)

tinyCStyle = emptyDef {
commentStart = "/*"
           , commentEnd = "*/"
           , commentLine = "//"
           , nestedComments = True
           , identStart = letter <|> char '_'
           , identLetter = alphaNum <|> oneOf "_"
           , opStart = opLetter emptyDef
           , opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
           , reservedOpNames= []
           , reservedNames = ["if", "else", "while", "return", "int", "void"
                              , "break", "continue"]
           , caseSensitive = True
           }

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser tinyCStyle

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer
lexeme :: Parser a -> Parser a
lexeme = Token.lexeme lexer
symbol :: String -> Parser String
symbol = Token.symbol lexer
natural :: Parser Integer
natural = Token.natural lexer
identifier :: Parser String
identifier = Token.identifier lexer
reserved :: String -> Parser ()
reserved = Token.reserved lexer
operator :: Parser String
operator = Token.operator lexer
reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer
parens :: Parser a -> Parser a
parens = Token.parens lexer
braces :: Parser a -> Parser a
braces = Token.braces lexer
squares :: Parser a -> Parser a
squares = Token.squares lexer
semi :: Parser String
semi = Token.semi lexer
comma :: Parser String
comma = Token.comma lexer
spaces1 :: Parser ()
spaces1 = skipMany1 space

prs ::(Show a) => Parser a -> String -> String
prs parser str = case parse parser "TinyC" str of
    Left err -> show err
    Right val -> show val

putPrs :: (Show a) => Parser a -> String -> IO ()
putPrs parser str = putStr $ prs parser str



main :: IO ()
main = do
   input <- getArgs
   fileData <- readFile $ head input
   prsProgram fileData

data ObjKind = Fresh
             | Var
             | Global
             | Func
             | Param
             | PrmNumNonMathedFunc
             | UnDefFun 
             | Err deriving (Show, Eq)

data Obj = Obj { name::String,
                 lev::Integer,
                 kind::ObjKind,
                 offset::Integer } deriving Show

--引数2の演算
data Op2 = Non2
         | Mul
         | Div
         | Mod
         | Add
         | Sub
         | GrTh
         | GrEq
         | LsTh
         | LsEq
         | ShL
         | ShR
         | Eq
         | NE
         | And
         | Or deriving (Show, Eq)

--Op2と演算子の対応
op2OperationTable =
    [(Mul, (*)),
     (Div, div),
     (Add, (+)),
     (Sub, (-)),
     (Mod, (mod)),
     (Eq, equal),
     (NE, neq),
     (Or, (+)),
     (And, (*)),
     (GrTh, grTh),
     (GrEq, grEq),
     (LsTh, lsTh),
     (LsEq, lsEq)]
    where b2i :: Bool -> Integer
          b2i b = if b then 1 else 0
          equal :: Integer -> Integer -> Integer
          equal x y | x == y = 1
                    | otherwise = 0
          neq :: Integer -> Integer -> Integer
          neq x y | x /= y = 1
                  | otherwise = 0
          grTh :: Integer -> Integer -> Integer
          grTh x y | x > y = 1
                   | otherwise = 0
          grEq :: Integer -> Integer -> Integer
          grEq x y | x >= y = 1
                   | otherwise = 0
          lsTh :: Integer -> Integer -> Integer
          lsTh x y | x < y = 1
                   | otherwise = 0
          lsEq :: Integer -> Integer -> Integer
          lsEq x y | x <= y = 1
                   | otherwise = 0

searchOp :: Op2 -> Maybe (Integer -> Integer -> Integer)
searchOp = search op2OperationTable
    where search :: [(Op2, (Integer -> Integer -> Integer))] -> Op2
                     -> Maybe (Integer -> Integer -> Integer)
          search [] _ = Nothing
          search (car:cdr) op | fst car == op = Just $ snd car
                              | otherwise     = search cdr op

data CmpndAsgn = CAAdd
               | CASub
               | CAMul
               | CADiv
               | CAMod deriving Show

data Expr = Ident String
          | Declarator String
          | DeclaratorList [Expr]
          | Declaration Expr
          | DeclarationList [Expr]
          | ParamDclr Expr
          | ParamDclrList [Expr]
          | Number Integer
          | Minus Expr
          | TwoOp Op2 Expr Expr
          | Ternary Expr Expr Expr
          | Asgn Expr Expr
          | CompoundAsgn CmpndAsgn Expr Expr
          | Parenthesis Expr
          | ArguExprList [Expr]
          | CallFunc Expr Expr
          | ExprList [Expr]
          | Object Obj
          | Exprssn [Expr]
          | UnDefVar String

data Statement = Non
               | Solo Expr
               | If Expr Statement
               | TagedIf [Integer] Expr Statement
               | IfElse Expr Statement Statement
               | TagedIfElse [Integer] Expr Statement Statement
               | While Expr Statement
               | TagedWhile [Integer] Expr Statement
               | Break
               | TagedBreak [Integer]
               | Continue
               | TagedContinue [Integer]
               | Return Expr
               | SttList [Statement]
               | CompoundStt Expr Statement

data FuncDef = FuncDefinition Expr Expr Statement
data ExternDclr = ExternDeclaration Expr
                | ExternFuncDec FuncDef
                | NLFunc FuncDef Integer
                | TagedFunc FuncDef Integer [([Integer], Integer)]
                | Program [ExternDclr]

showVal :: Expr -> String
showVal (Number n) = show n -- "(Num " ++ show n ++ ")"
showVal (Ident s) = s --"(Ident " ++ s ++ ")"
showVal (Minus n) = "(- " ++ showVal n ++ ")"
showVal (TwoOp op n1 n2) =
    "(" ++ show op ++ " " ++ showVal n1 ++ " " ++ showVal n2 ++ ")"
showVal (Ternary e1 e2 e3) =
    "(" ++ showVal e1 ++ " ? " ++ showVal e2 ++ " : " ++ showVal e3 ++ ")"
showVal (Parenthesis n) = showVal n
showVal (Declarator s) = s
showVal (DeclaratorList []) = ""--"(DeclaratorList nil)"
showVal (DeclaratorList l) = "(DeclaratorList " ++  (unwords $ map showVal l) ++ ")"
showVal (Declaration n) = "(dclrt " ++ showVal n ++ ")"
showVal (DeclarationList []) = ""--"(DeclarationList nil)"
showVal (DeclarationList l) = " (DeclarationList " ++  (unwords $ map showVal l) ++ ")"
showVal (ParamDclr n) = "(int " ++ showVal n ++ ")"
showVal (ParamDclrList []) = "(ParamDclrList nil)"
showVal (ParamDclrList l) = "" ++  (unwords $ map showVal l) ++ ""
showVal (ArguExprList []) = "(ArguExprList nil)"
showVal (ArguExprList l) = "(ArguExprList " ++  (unwords $ map showVal l) ++ ")"
showVal (CallFunc n1 n2) = "(CallFunc " ++ showVal n1 ++ " " ++
                                showVal n2 ++ ")"
showVal (ExprList []) = "(List nil)"
showVal (ExprList l) = "(List " ++  (unwords $ map showVal l) ++ ")"
showVal (Exprssn l) = "(Exprssn " ++ (unwords $ map showVal l) ++ ")"
showVal (Object o) =
    case kind o of
        Param -> "(" ++ name o ++ ":" ++ (show $ lev o) ++ ":" ++ 
                  (show $ 8 + 4 *  offset o) ++ ")"
        Var -> "(" ++ name o ++ ":" ++ (show $ lev o) ++ ":" ++ 
                (show $ - 4 *  offset o) ++ ")"
        _ -> "(" ++ name o ++ ":" ++ (show $ lev o) ++ ":" ++ 
              (show $ kind o) ++ ":" ++ (show $ offset o) ++ ")"
showVal (UnDefVar s) = "(UnDefinedVar " ++ s ++ ")"
showVal (Asgn e1 e2) = "(setq " ++ showVal e1 ++ " " ++ showVal e2 ++ ")"
showVal (CompoundAsgn op e1 e2) =
    "(" ++ show op ++ " = " ++ showVal e1 ++ " " ++ showVal e2 ++ ")"

showStatement :: Statement -> String
showStatement (Non) = "()"
showStatement (Solo e) = "(Stt " ++ showVal e ++ ")"
showStatement (If e s) = "(if " ++ showVal e ++ " " ++ showStatement s ++ ")"
showStatement (IfElse e s1 s2) = "(if " ++ showVal e ++ " " ++
                                 showStatement s1 ++ " " ++
                                 showStatement s2 ++ ")"
showStatement (While e s) = "(while (" ++ showVal e ++ ")" ++
                            showStatement s ++ ")"
showStatement (TagedIf i e s) =
    "(tif " ++ show i ++ " " ++ showVal e ++ " " ++ showStatement s ++ ")"
showStatement (TagedIfElse i e s1 s2) =
    "(tif " ++ show i ++ " "  ++ showVal e ++ " " ++
     showStatement s1 ++ " " ++ showStatement s2 ++ ")"
showStatement (TagedWhile i e s) =
    "(twhile " ++ show i ++ "(" ++ showVal e ++ ")" ++
     showStatement s ++ ")"
showStatement (Break) = "(break)"
showStatement (TagedBreak i) = "(tBreak " ++ show i ++ ")"
showStatement (Continue) = "(continue)"
showStatement (TagedContinue i) = "(tContinue " ++ show i ++ ")"
showStatement (Return e) = "(return " ++ showVal e ++ ")"
showStatement (SttList []) = "(SttList nil)"
showStatement (SttList l) = "(SttList " ++ (unwords $ map showStatement l) ++ ")"
showStatement (CompoundStt e s) = "(CompoundStt " ++ showVal e ++ "" ++
                                  showStatement s ++ ")"

showFuncDef :: FuncDef -> String
showFuncDef (FuncDefinition e1 e2 s) = "(FuncDef (int " ++ showVal e1 ++ ") (" ++
                                       showVal e2 ++ ")\n    " ++ showStatement s ++ ")"

showExternDclr :: ExternDclr -> String
showExternDclr (ExternDeclaration e) = showVal e
showExternDclr (ExternFuncDec f) = showFuncDef f
showExternDclr (NLFunc f nl) = showFuncDef f ++ "\n  " ++ show nl 
showExternDclr (TagedFunc f nl i) = showFuncDef f ++ "\n  " ++ show nl ++
                                     "\n  " ++ show i ++ "\n" 
showExternDclr (Program []) = "(Program nil)"
showExternDclr (Program l) =
    "(Program\n " ++ (unwords $ map ((++ "\n") . showExternDclr) l) ++ ")\n"


rightExpr :: Parser Expr
rightExpr = buildExpressionParser operatorTable unaryExpr

operatorTable = [[op "*" (TwoOp Mul) AssocLeft, op "/" (TwoOp Div) AssocLeft
                , op "%" (TwoOp Mod) AssocLeft]
                ,[op "+" (TwoOp Add) AssocLeft, op "-" (TwoOp Sub) AssocLeft]
                ,[op ">" (TwoOp GrTh) AssocLeft, op ">=" (TwoOp GrEq) AssocLeft
                , op "<" (TwoOp LsTh) AssocLeft, op "<=" (TwoOp LsEq) AssocLeft]
                ,[op "==" (TwoOp Eq) AssocLeft, op "!=" (TwoOp NE) AssocLeft]
                ,[op "&&" (TwoOp And) AssocLeft]
                ,[op "||" (TwoOp Or) AssocLeft]]
         where
           op s f assoc
              = Infix (do{reservedOp s; return f}) assoc

alphabetList = ['a'..'z'] ++ ['A'..'Z']
digitList = ['0'..'9']

instance Show Expr where show = showVal
instance Show Statement where show = showStatement
instance Show FuncDef where show = showFuncDef
instance Show ExternDclr where show = showExternDclr


program :: Parser ExternDclr
program = do p <- sepBy externDeclaration spaces
             return $ Program p

externDeclaration :: Parser ExternDclr
externDeclaration = try (do spaces
                            p <- declaration
                            return (ExternDeclaration p))
                     <|> try (do spaces
                                 p <- funcDef
                                 return (ExternFuncDec p))

blank :: Parser ()
blank = try (do char ' '
                return ())
         <|> try ( do char '\t'
                      return ())
         <|> try ( do char '\n'
                      return ())

blanks :: Parser ()
blanks = do many blank
            return ()

blanks1 :: Parser ()
blanks1 = do many1 blank
             return ()

funcDef :: Parser FuncDef
funcDef = do string "int"
             spaces1
             p <- declarator
             spaces
             char '('
             spaces
             q <- paramDeclarationList
             spaces
             char ')'
             spaces
             s <- compoundStatement
             spaces
             return $ FuncDefinition p q s

paramDeclaration :: Parser Expr
paramDeclaration = do string "int"
                      spaces1
                      p <- declarator
                      spaces
                      return $ ParamDclr p

paramDeclarationList :: Parser Expr
paramDeclarationList = do p <- sepBy paramDeclaration $ spaces >> char ',' >> spaces
                          return $  ParamDclrList p

declarationList :: Parser Expr
declarationList = do p <- sepBy declaration spaces
                     return $  DeclarationList p

declaration :: Parser Expr
declaration = do reserved "int"
                 p <- declaratorList
                 spaces
                 _ <- string ";"
                 spaces
                 return $ Declaration p

declarator :: Parser Expr
declarator = do p <- identifier
                return $ Declarator p

declaratorList :: Parser Expr
declaratorList = do p <- sepBy declarator $ spaces >> char ',' >> spaces
                    return $ DeclaratorList p

statementList :: Parser Statement
statementList = do p <- sepBy statement spaces
                   return $ SttList p

statement :: Parser Statement
statement =
    try (do char ';'
            spaces
            return (Non))
     <|> try (do reserved "break"
                 spaces
                 char ';'
                 spaces
                 return (Break))
     <|> try (do reserved "continue"
                 spaces
                 char ';'
                 spaces
                 return (Continue))
     <|> try (do p <- expression
                 spaces
                 _ <- char ';'
                 spaces
                 return (Solo p))
     <|> try (do string "if"
                 spaces
                 char '('
                 spaces
                 e <- expression
                 spaces
                 char ')'
                 spaces
                 s1 <- statement
                 spaces
                 string "else"
                 spaces
                 s2 <- statement
                 spaces
                 return (IfElse e s1 s2))
     <|> try (do string "if"
                 spaces
                 char '('
                 spaces
                 e <- expression
                 spaces
                 char ')'
                 spaces
                 s <- statement
                 spaces
                 return (If e s))
     <|> try (do string "while"
                 spaces
                 char '('
                 spaces
                 e <- expression
                 spaces
                 char ')'
                 spaces
                 s <- statement
                 spaces
                 return (While e s))
     <|> try (do string "return"
                 spaces
                 e <- expression
                 spaces
                 char ';'
                 spaces
                 return (Return e))
     <|> try (do p <- compoundStatement
                 spaces
                 return p)

expression :: Parser Expr
expression = do p <- sepBy ternaryExpr $ spaces >> char ',' >> spaces
                return $ Exprssn p

ternaryExpr :: Parser Expr
ternaryExpr = try (do e1 <- assignExpr
                      spaces
                      char '?'
                      spaces
                      e2 <- assignExpr
                      spaces
                      char ':'
                      spaces
                      e3 <- assignExpr
                      spaces
                      return (Ternary e1 e2 e3))
               <|> assignExpr

assignExpr :: Parser Expr
assignExpr = try (do p <- ident
                     spaces
                     _ <- char '='
                     spaces
                     q <- assignExpr
                     spaces
                     return (Asgn p q))
--              <|> (
              <|> rightExpr

primaryExpr :: Parser Expr
primaryExpr =
    try (do char '('
            spaces 
            p <- expression
            char ')'
            spaces
            return p) --Parenthesis
     <|> number
     <|> ident

ident :: Parser Expr
ident = do p <- identifier
           spaces
           return $ Ident p

number :: Parser Expr
number = liftM (Number . read) $ many1 digit

num :: Parser Expr
num = do c <- oneOf digitList
         spaces
         return . Number $ charToInt c
      where
         charToInt c = toInteger (ord c - ord '0')

postfixExpr :: Parser Expr
postfixExpr = try (do p <- ident
                      spaces
                      char '('
                      spaces
                      q <- arguExprList
                      spaces
                      char ')'
                      spaces
                      return (CallFunc p q))
                <|>  try (do p <- primaryExpr
                             spaces
                             return p)

unaryExpr :: Parser Expr
unaryExpr = postfixExpr
             <|> do _ <- char '-'
                    spaces
                    p <- unaryExpr
                    spaces
                    return $ Minus p

arguExprList :: Parser Expr
arguExprList = do p <- sepBy assignExpr $ spaces >> char ',' >> spaces
                  return $ ArguExprList p

compoundStatement :: Parser Statement
compoundStatement = try (do char '{'
                            spaces
                            p <- declarationList
                            spaces
                            q <- statementList
                            spaces
                            char '}'
                            spaces
                            return (CompoundStt p q))

prsProgram :: String -> IO ()
prsProgram str = case parse program "TinyC" str of
    Left err -> putStrLn $ show err
    Right val -> case checkTreeDuplication val of
        Just a -> putStrLn . (++) "Multiple Declaration : " . unwords $ a
        Nothing -> 
            let semanticAnalysised = semanticAnalysis (constEval val, [])
            in case extractErr . snd $ semanticAnalysised of
                errs@_:_ -> putStrLn . (++) "Err : " . show $ errs
                [] -> putStrLn . unlines . codeGenerate (snd semanticAnalysised)
                       . labelTagging . fst $ semanticAnalysised

taggedProgram :: String -> IO ()
taggedProgram str = case parse program "TinyC" str of
    Left err -> putStrLn $ show err
    Right val -> case checkTreeDuplication val of
        Just a -> putStrLn . (++) "Multiple Declaration : " . unwords $ a
        Nothing -> 
            let semanticAnalysised = semanticAnalysis (constEval val, [])
            in case extractErr . snd $ semanticAnalysised of
                errs@_:_ -> putStrLn . (++) "Err : " . show $ errs
                [] -> putStrLn . show . labelTagging . fst $ semanticAnalysised


constEvalProgram :: String -> IO ()
constEvalProgram str = case parse program "TinyC" str of
    Left err -> putStrLn $ show err
    Right val -> case checkTreeDuplication val of
        Just a -> putStrLn . (++) "Multiple Declaration : " . unwords $ a
        Nothing ->
            let semanticAnalysised = semanticAnalysis (constEval val, [])
            in putStrLn . show  $ fst semanticAnalysised

showTree :: String -> IO ()
showTree str = case parse program "TinyC" str of
    Left err -> putStrLn $ show err
    Right val -> putStrLn $ show val

codeGenerate :: [Obj] -> ExternDclr -> [String]
codeGenerate o (Program l) =
    (map ((\ nam -> "\tEXTERN\t" ++ nam) . name) . extractUnDefFunc $ o)
    ++ (foldr (++) [] $ map codeGenerateEx l)
codeGenerateEx :: ExternDclr -> [String]
codeGenerateEx (ExternDeclaration (Declaration (DeclaratorList l))) =
    map ((\ nam -> "\tCOMMON\t" ++ nam ++ " 4") . showVal) l
codeGenerateEx (TagedFunc (FuncDefinition (Declarator name) (ParamDclrList l) s)
                          nl i) =
    case stackOffset of {--
        0 ->
            ["\tGLOBAL\t" ++ name,
             name ++ ":"]
            ++ fst generatedS ++
            ["L" ++ name ++ "Ret:\t",
            "\tret"] --}
        otherwise ->
            ["\tGLOBAL\t" ++ name,
             name ++ ":\tpush\tebp",
             "\tmov\tebp, esp",
             "\tsub\tesp, " ++ (show stackOffset)]
            ++ fst generatedS ++
            ["L" ++ name ++ "Ret:",
             "\tmov\tesp, ebp",
             "\tpop\tebp",
             "\tret",
             ""]
    where generatedS = codeGenerateS nl name [] i s
          stackOffset | snd generatedS == [] = 0
                      | otherwise            = maximum $ snd generatedS

--第二引数は関数の名前
--第三引数はラベル識別に使う値
codeGenerateS :: Integer -> String -> [Integer] -> [([Integer], Integer)] -> Statement
                  -> ([String], [Integer])
codeGenerateS nl fnam idLst i (SttList l) =
    tupleListFold generatedSl
    where generatedSl =
              map (\ stt -> codeGenerateS nl fnam ((snd stt):idLst) i . fst $ stt)
                  labeled
          labeled = indexing l 1
codeGenerateS nl fnam idLst i (CompoundStt e s) =
    codeGenerateS nl fnam idLst i s
codeGenerateS nl fnam idLst i (Return e) =
    (fst gen
     ++ ["\tjmp\tL" ++ fnam ++ "Ret"]
     , snd gen)
    where gen = codeGenerateSoloExpr nl fnam idLst e
codeGenerateS nl fnam idLst i (Solo e) =
    codeGenerateSoloExpr nl fnam idLst e
codeGenerateS nl fnam idLst i (Non) = ([],[])
codeGenerateS nl fnam idLst i (TagedIfElse tag e s1 s2) =
    (fst cond
     ++["\tcmp\teax, 0", "\tje\t" ++ "L" ++ fnam ++ show (labelHead+1)] ++ fst gen1
     ++ ["\tjmp\t" ++ "L" ++ fnam ++ show (labelHead+2),
         "L" ++ fnam ++ show (labelHead+1) ++ ":"]
     ++ fst gen2 ++ ["L" ++ fnam ++ show (labelHead+2) ++ ":"]
     , snd gen1 ++ snd gen2 ++ snd cond)
    where gen1 = codeGenerateS nl fnam (1:idLst) i s1
          gen2 = codeGenerateS nl fnam (2:idLst) i s2
          cond = codeGenerateSoloExpr nl fnam (0:idLst) e
          labelHead = case tagSearch i tag of
                          Just a -> a
                          Nothing -> error "in generating \"if\""
codeGenerateS nl fnam idLst i (TagedWhile tag e s) =
    (["L" ++ fnam ++ show (labelHead+1) ++ ":"] ++ fst cond
     ++ ["\tcmp\teax, 0", "\tje\t" ++ "L" ++ fnam ++ show (labelHead+2)] ++ fst gen
     ++ ["\tjmp\t" ++ "L" ++ fnam ++ show (labelHead+1)
         , "L" ++ fnam ++ show (labelHead+2) ++ ":"]
     , snd gen ++ snd cond)
    where gen = codeGenerateS nl fnam (1:idLst) i s
          cond = codeGenerateSoloExpr nl fnam (0:idLst) e
          labelHead = case tagSearch i tag of
                          Just a -> a
                          Nothing -> error "in generating \"while\""
codeGenerateS nl fnam idLst i (TagedBreak tag) =
    (["\tjmp\tL" ++ fnam ++ show (labelHead+2) ++ ":"], [])
    where labelHead = case tagSearch i tag of
                          Just a -> a
                          Nothing -> error "in generating \"break\""
codeGenerateS nl fnam idLst i (TagedContinue tag) =
    (["\tjmp\tL" ++ fnam ++ show (labelHead+1) ++ ":"], [])
    where labelHead = case tagSearch i tag of
                          Just a -> a
                          Nothing -> error "in generating \"break\""
codeGenerateS _ _ _ _ s = error $ showStatement s


codeGenerateSoloExpr :: Integer -> String -> [Integer] -> Expr -> ([String], [Integer])
--代入式のコード生成
codeGenerateSoloExpr i fnam idLst (Asgn (Object o1) (Object o2)) =
    (["\tmov\t" ++ objLoc o1 ++ ", " ++ objLoc o2 ++ "\t;SoloAsgn"]
    , [i])
codeGenerateSoloExpr i fnam idLst (Asgn (Object o1) (Number n)) =
    (["\tmov\tdword\t" ++ objLoc o1 ++ ", " ++ show n ++ "\t;SoloAsgn"]
    , [i])
codeGenerateSoloExpr i fnam idLst (Asgn (Object o) e) =
    (fst gen ++ ["\tmov\t" ++ objLoc o ++ ", eax\t;SoloAsgn"]
    ,snd gen)
    where gen = codeGenerateE i fnam idLst e
codeGenerateSoloExpr i fnam idLst e = codeGenerateE i fnam idLst e

--第一引数は一時変数のスタックの深さを意味する。
--第三引数はラベル識別に使う値
codeGenerateE :: Integer -> String -> [Integer] -> Expr -> ([String], [Integer])
codeGenerateE i fnam idLst (Number n) =
    ([(emit OpMOV "" eax $ show n) ++ "\t\t;Number"]
     , [i])
codeGenerateE i fnam idLst (Object o) =
    ([(emit OpMOV "" eax $ objLoc o)
       ++ "\t;LoadObject"]
     , [i])
--代入式のコード生成
codeGenerateE i fnam idLst (Asgn (Object o1) (Object o2)) =
    (["\tmov\t" ++ objLoc o1 ++ ", " ++ objLoc o2
     , "\tmov\teax, " ++ objLoc o1 ++ "\t;Asgn"]
    , [i])
codeGenerateE i fnam idLst (Asgn (Object o1) (Number n)) =
    (["\tmov\t" ++ objLoc o1 ++ ", " ++ show n, "\tmov\teax, " ++ show n ++ "\t;Asgn"]
    , [i])
codeGenerateE i fnam idLst (Asgn (Object o) e) =
    (fst gen ++ ["\tmov\t" ++ objLoc o ++ ", eax\t;Asgn"]
    ,snd gen)
    where gen = codeGenerateE i fnam idLst e
--比較演算子のコード生成
codeGenerateE i fnam idLst (TwoOp GrTh e1 e2) = codeGenerateCmp "g" fnam idLst i e1 e2
codeGenerateE i fnam idLst (TwoOp GrEq e1 e2) = codeGenerateCmp "ge" fnam idLst i e1 e2
codeGenerateE i fnam idLst (TwoOp LsTh e1 e2) = codeGenerateCmp "l" fnam idLst i e1 e2
codeGenerateE i fnam idLst (TwoOp LsEq e1 e2) = codeGenerateCmp "le" fnam idLst i e1 e2
codeGenerateE i fnam idLst (TwoOp Eq e1 e2) = codeGenerateCmp "e" fnam idLst i e1 e2
codeGenerateE i fnam idLst (TwoOp NE e1 e2) = codeGenerateCmp "ne" fnam idLst i e1 e2
--論理演算子のコード生成
codeGenerateE i fnam idLst (TwoOp And e1 e2) =
    (["\tmov dword\t" ++ (tmpVar i) ++ ", 0"]
     ++ fst gen1 ++ ["\tcmp\teax, 0", "\tje\tLlgc" ++ showIdentList idLst]
     ++ fst gen2 ++ ["\tcmp\teax, 0", "\tje\tLlgc" ++ showIdentList idLst]
     ++ ["\tmov dword\t" ++ (tmpVar i) ++ ", 1"
         , "Llgc" ++ showIdentList idLst ++ ":"
         , "\tmov\teax, " ++ (tmpVar i)]
     , snd gen1 ++ snd gen2)
    where gen1 = codeGenerateE (i+4) fnam (1:idLst) e1
          gen2 = codeGenerateE (i+4) fnam (2:idLst) e2
codeGenerateE i fnam idLst (TwoOp Or e1 e2) =
    (["\tmov dword\t" ++ (tmpVar i) ++ ", 1"]
     ++ fst gen1 ++ ["\tcmp\teax, 0", "\tjne\tLlgc" ++ showIdentList idLst]
     ++ fst gen2 ++ ["\tcmp\teax, 0", "\tjne\tLlgc" ++ showIdentList idLst]
     ++ ["\tmov dword\t" ++ (tmpVar i) ++ ", 0"
         , "Llgc" ++ showIdentList idLst ++ ":"
         , "\tmov\teax, " ++ (tmpVar i)]
     , snd gen1 ++ snd gen2)
    where gen1 = codeGenerateE (i+4) fnam (1:idLst) e1
          gen2 = codeGenerateE (i+4) fnam (2:idLst) e2
--三項演算子のコード生成
codeGenerateE i fnam idLst (Ternary e1 e2 e3) =
    (fst gen1 ++ ["\tcmp\teax, 0", "\tje\tLtnr1_" ++ showIdentList idLst] ++ fst gen2
     ++ ["\tjmp\tLtnr2_" ++ showIdentList idLst, "Ltnr1_" ++ showIdentList idLst ++ ":"]
     ++ fst gen3 ++ ["Ltnr2_" ++ showIdentList idLst ++ ":"]
    , snd gen1 ++ snd gen2 ++ snd gen3)
    where gen1 = codeGenerateE i fnam (1:idLst) e1
          gen2 = codeGenerateE i fnam (2:idLst) e2
          gen3 = codeGenerateE i fnam (3:idLst) e3
--割り算のコード生成
codeGenerateE i fnam idLst (TwoOp Div e1 (Number n)) =
    (fst gen ++ ["\tcdq", "\tmov\tdword " ++ tmpVar i ++ ", " ++ show n
                , "\tidiv\tdword " ++ tmpVar i]
    , snd gen)
    where gen = codeGenerateE (i+4)fnam (1:idLst) e1
codeGenerateE i fnam idLst (TwoOp Div e1 (Object o)) =
    (fst gen ++ ["\tcdq", "\tidiv\tdword " ++ objLoc o]
    , snd gen)
    where gen = codeGenerateE i fnam (1:idLst) e1
codeGenerateE i fnam idLst (TwoOp Div e1 e2) =
    (fst gen2 ++ ["\tmov\t" ++ (tmpVar i) ++ ", eax"] ++ fst gen1
     ++ ["\tcdq", "\tidiv\tdword " ++ (tmpVar i)]
    , snd gen1 ++ snd gen2)
    where gen1 = codeGenerateE (i+4) fnam (1:idLst) e1
          gen2 = codeGenerateE (i+4) fnam (2:idLst) e2
--剰余演算子のコード生成
codeGenerateE i fnam idLst (TwoOp Mod e1 (Number n)) =
    (fst gen ++ ["\tcdq", "\tmov\tdword " ++ tmpVar i ++ ", " ++ show n
                , "\tidiv\tdword " ++ tmpVar i, "\tmov\teax, edx"]
    , snd gen)
    where gen = codeGenerateE (i+4)fnam (1:idLst) e1
codeGenerateE i fnam idLst (TwoOp Mod e1 (Object o)) =
    (fst gen ++ ["\tcdq", "\tidiv\tdword " ++ objLoc o, "\tmov\teax, edx"]
    , snd gen)
    where gen = codeGenerateE i fnam (1:idLst) e1
codeGenerateE i fnam idLst (TwoOp Mod e1 e2) =
    (fst gen2 ++ ["\tmov\t" ++ (tmpVar i) ++ ", eax"] ++ fst gen1
     ++ ["\tcdq", "\tidiv\tdword " ++ (tmpVar i), "\tmov\teax, edx"]
    , snd gen1 ++ snd gen2)
    where gen1 = codeGenerateE (i+4) fnam (1:idLst) e1
          gen2 = codeGenerateE (i+4) fnam (2:idLst) e2
--一般的な二項演算子のコード生成
codeGenerateE i fnam idLst (TwoOp op e1 (Number n)) =
    (fst gen1 ++ [emitOp2 op "" eax $ show n]
    , snd gen1)
    where gen1 = codeGenerateE i fnam (1:idLst) e1
codeGenerateE i fnam idLst (TwoOp op e1 (Object o)) =
    (fst gen1 ++ [emitOp2 op "" eax $ objLoc o]
    , snd gen1)
    where gen1 = codeGenerateE i fnam (1:idLst) e1
codeGenerateE i fnam idLst (TwoOp op e1 e2) =
    (fst gen2
     ++ [emit OpMOV "" (tmpVar i) eax]
     ++ fst gen1
     ++ [emitOp2 op "" eax $ tmpVar i]
     , snd gen1 ++ snd gen2)
    where gen1 = codeGenerateE (i+4) fnam (1:idLst) e1
          gen2 = codeGenerateE (i+4) fnam (2:idLst) e2
codeGenerateE i fnam idLst (CallFunc (Object o) (ArguExprList l)) =
    ((foldr (++) [] . map fst . reverse $ codeL)
      ++ ["\tcall\t" ++ name o]
      ++ ["\tadd\tesp, " ++ show (length l * 4)]
    ,foldr (++) [] . map snd $ codeL)
    where codeL = map codeGen labeled
          codeGen :: (Expr, Integer) -> ([String], [Integer])
          codeGen ((Number n), _) = (["\tpush\t" ++ show n], [i])
          codeGen ((Object o), _) = (["\tpush\t" ++ objLoc o], [i])
          codeGen e = let tuple = codeGenerateE i fnam ((snd e):idLst) . fst $ e
                      in (fst tuple ++ ["\tpush\teax"], snd tuple)
          labeled = indexing l 1
codeGenerateE i fnam idLst (Exprssn l) = 
    (foldr (++) [] . map fst . reverse $ codeL
     ,foldr (++) [] . map snd $ codeL)
    where codeL = map (\ expr -> codeGenerateSoloExpr i fnam ((snd expr):idLst) . fst $ expr)
                      labeled
          labeled = indexing l 1
codeGenerateE _ _ _ e = error $ showVal e ++ " in code Gen"

objLoc :: Obj -> String
objLoc (Obj _ _ Var off) =
    "[ebp-" ++ (show . (*) 4 . (+) 1 $ off) ++ "]"
objLoc (Obj _ _ Param off) =
    "[ebp+" ++ (show . (+) 8 . (*) 4 $ off) ++ "]"
objLoc (Obj nam _ Global _) =
    "[" ++ nam ++ "]"

showIdentList :: [Integer] -> String
showIdentList [] = []
showIdentList (car:cdr) = show car ++ showIdentList cdr

--第三引数はラベル識別に使う値
codeGenerateCmp :: String -> String -> [Integer] -> Integer -> Expr -> Expr
                    -> ([String], [Integer])
codeGenerateCmp opr fnam idLst i (Object o) (Number n) =
    (["\tcmp\tdword\t" ++ (objLoc o) ++ ", " ++ show n]
     ++ ["\tset" ++ opr ++ "\tal"]
     ++ ["\tmovzx\teax, al"]
    , [])
codeGenerateCmp opr fnam idLst i (Object o1) (Object o2) =
    (["\tcmp\tdword\t" ++ (objLoc o1) ++ ", " ++ objLoc o2]
     ++ ["\tset" ++ opr ++ "\tal"]
     ++ ["\tmovzx\teax, al"]
    , [])
codeGenerateCmp opr fnam idLst i e1 (Number n) =
    (fst gen
     ++ [emit OpCMP "" eax $ show n]
     ++ ["\tset" ++ opr ++ "\tal"]
     ++ ["\tmovzx\teax, al"]
    , snd gen)
    where gen = codeGenerateE i fnam (1:idLst) e1
codeGenerateCmp opr fnam idLst i e1 e2 =
    (fst gen2
     ++ [emit OpMOV "" (tmpVar i) eax]
     ++ fst gen1
     ++ [emit OpCMP "" eax $ tmpVar i]
     ++ ["\tset" ++ opr ++ "\tal"]
     ++ ["\tmovzx\teax, al"]
    , snd gen1 ++ snd gen2)
    where gen1 = codeGenerateE (i+4) fnam (1:idLst) e1
          gen2 = codeGenerateE (i+4) fnam (2:idLst) e2

data AsmOper = OpMOV
             | OpADD
             | OpSUB
             | OpMUL 
             | OpCMP
 deriving (Show, Eq)

asmOpTable = [(Add, OpADD, "add"),
              (Sub, OpSUB, "sub"),
              (Mul, OpMUL, "imul"),
              (Non2, OpMOV, "mov"),
              (Non2, OpCMP, "cmp")]


asmToOp2 :: Op2 -> Maybe AsmOper
asmToOp2 = search asmOpTable
    where search :: [(Op2, AsmOper, String)] -> Op2 -> Maybe AsmOper
          search [] op = Nothing
          search (car:cdr) op | (fstIn3 car) == op = Just $ sndIn3 car
                              | otherwise          = search cdr op

asmToStr :: AsmOper -> Maybe String
asmToStr = search asmOpTable
    where search :: [(Op2, AsmOper, String)] -> AsmOper -> Maybe String
          search [] asm = Nothing
          search (car:cdr) asm | (sndIn3 car) == asm = Just $ thdIn3 car
                               | otherwise           = search cdr asm

eax = "eax"

tmpVar :: Integer -> String
tmpVar i = "[ebp-" ++ show (i+4) ++ "]"

--ラベル、第一オペランド、第二オペランド
emit :: AsmOper -> String -> String -> String -> String
emit op l e1 e2 = case asmToStr op of
                      Just a -> l ++ "\t" ++ a ++ "\t" ++ e1 ++ ", " ++ e2
                      Nothing -> ";No implementation error : " ++ show op

emitOp2 :: Op2 -> String -> String -> String -> String
emitOp2 op label e1 e2 = case asmToOp2 op of
                             Just a -> emit a label e1 e2
                             Nothing -> ";No implementation error in op2 : " ++ show op


labelSort :: [([Integer], Integer)] -> [([Integer], Integer)]
labelSort a = sort (\ (x,_) (y,_) -> labelCmp x y) a
    where labelCmp :: [Integer] -> [Integer] -> Bool
          labelCmp [] [] = True
          labelCmp [] _ = True
          labelCmp _ [] = False
          labelCmp a b | last a < last b = True
                       | last a == last b = labelCmp (init a) (init b)
                       | last a > last b = False
          sort :: (a -> a -> Bool) -> [a] -> [a]
          sort cmp (pivot:rest) = filter (\ x -> cmp x pivot) (sort cmp rest)
                                   ++ [pivot]
                                   ++ filter (\ x -> not $ cmp x pivot) (sort cmp rest)
          sort _ l = l

labelModify :: Integer -> [([Integer], Integer)] -> [([Integer], Integer)]
labelModify a ((l,off):cdr) = (l,a) : (labelModify (a+off) cdr)
labelModify _ [] = []

labelTagging :: ExternDclr -> ExternDclr
labelTagging (Program l) =
    Program $ map labelTagging l
labelTagging (NLFunc (FuncDefinition e1 e2 s) nl) =
    TagedFunc (FuncDefinition e1 e2 . labelTaggingBC [] . fst $ labeled) nl .
     labelModify 0 . labelSort . snd $ labeled
    where labeled = labelTaggingS s []
labelTagging e = e

--Break Continue
labelTaggingBC :: [Integer] -> Statement -> Statement
labelTaggingBC iLst (CompoundStt e s) =
    (CompoundStt e $ labelTaggingBC iLst s)
labelTaggingBC iLst (TagedIf i e s) =
    (TagedIf i e $ labelTaggingBC iLst s)
labelTaggingBC iLst (TagedIfElse i e s1 s2) =
    (TagedIfElse i e (labelTaggingBC iLst  s1) $ labelTaggingBC iLst s2)
labelTaggingBC _ (TagedWhile i e s) =
    (TagedWhile i e $ labelTaggingBC i s)
labelTaggingBC iLst (SttList l) =
    (SttList $ map (labelTaggingBC iLst) l)
labelTaggingBC [] (Break) =
    error "Break out of While"
labelTaggingBC iLst (Break) =
    (TagedBreak iLst)
labelTaggingBC [] (Continue) =
    error "Continue out of While"
labelTaggingBC iLst (Continue) =
    (TagedContinue iLst)
labelTaggingBC _  s = s

labelTaggingS :: Statement -> [Integer] -> (Statement, [([Integer], Integer)])
labelTaggingS (CompoundStt e s) i =
    (CompoundStt e $ fst tag, snd tag)
    where tag = labelTaggingS s i
labelTaggingS (If e s) i =
    (TagedIf i e . fst $ tag, (i,1):(snd tag))
    where tag = labelTaggingS s $ 1:i
labelTaggingS (IfElse e s1 s2) i =
    (TagedIfElse i e (fst tag1) (fst tag2), (i,2):(snd tag1 ++ snd tag2))
    where tag1 = labelTaggingS s1 $ 1:i
          tag2 = labelTaggingS s2 $ 2:i
labelTaggingS (While e s) i =
    (TagedWhile i e (fst tag), (i,2):(snd tag))
    where tag = labelTaggingS s $ 2:i
labelTaggingS (SttList l) i =
    (SttList $ map fst labeled, foldr (++) [] $ map snd labeled)
    where labeling :: (Statement, Integer) -> (Statement, [([Integer], Integer)])
          labeling (s, num) = labelTaggingS s $ num:i
          labeled = map labeling $ indexing l 1
labelTaggingS statement i = (statement, [])

--labelTaggingE :: Expr -> [Integer] -> (Expr, [([Integer], Integer)])
--labelTaggingE = undefined

tagSearch :: [([Integer], Integer)] -> [Integer] -> Maybe Integer
tagSearch [] _ = Nothing
tagSearch (car:cdr) tag | fst car == tag = Just $ snd car
                        | otherwise      = tagSearch cdr tag

semanticAnalysis :: (ExternDclr, [Obj]) -> (ExternDclr, [Obj])
semanticAnalysis (prog@(Program l) ,st) =
    (Program $ map semanticAnlysEx l, stack ++ (foldr (++) [] $ map semanticAnlysSt l))
    where levZeroVarObj :: String -> Obj
          levZeroVarObj s = Obj s 0 Global 0
          levZeroFuncObj :: (String, Integer) -> Obj
          levZeroFuncObj (s, i) = Obj s 0 Func i
          semanticAnlysEx :: ExternDclr -> ExternDclr
          semanticAnlysEx ex = fst $ semanticAnalysis (ex, stack)
          semanticAnlysSt :: ExternDclr -> [Obj]
          semanticAnlysSt ex = snd $ semanticAnalysis (ex, stack)
          stack =
           (reverse . snd . pushList st . map levZeroVarObj . extractDclrName $ prog)
           ++ 
           (reverse . snd . pushList st . map levZeroFuncObj . extractFuncName $ prog)
semanticAnalysis ((ExternFuncDec (FuncDefinition name p@(ParamDclrList l) s)), st) =
    (NLFunc (FuncDefinition name p $ fst statement) nl, snd statement)
    where stack =
           snd $ pushList
            (reverse . (++) [Obj "" 1 Param 0] . map makeLevOneObj
             $ indexing (map getPrmName l) 0) st
          statement = makeSemanticTreeS (s, stack)
          makeLevOneObj :: (String, Integer) -> Obj
          makeLevOneObj (nam, off) = Obj nam 1 Param off
          getPrmName :: Expr -> String
          getPrmName (ParamDclr (Declarator s)) = s
          vars = filter (\ o -> kind o == Var) $ snd statement
          nl | length vars == 0 = 0
             | otherwise = (*) 4 .(+) 1 . maximum . map offset $ vars
semanticAnalysis (extern, st) = (extern, st)

makeSemanticTreeS :: (Statement, [Obj]) -> (Statement, [Obj])
makeSemanticTreeS (statement@(CompoundStt e s), st) =
    (CompoundStt (fst expr) (fst statement),
     st ++ (snd expr) ++ (snd statement))
    where expr = makeSemanticTreeE (e, st)
          statement = makeSemanticTreeS (s, snd expr)
makeSemanticTreeS (statement@(If e s), st) =
    (If (fst expr) . fst $ statement, st ++ (snd expr) ++ (snd statement))
    where expr = makeSemanticTreeE (e, st)
          statement = makeSemanticTreeS (s, st)
makeSemanticTreeS (statement@(IfElse e s1 s2), st) =
    (IfElse (fst expr)
            (fst statement1) . fst $ statement2,
     st ++ (snd expr) ++ (snd statement1) ++ (snd statement2))
    where expr = makeSemanticTreeE (e, st)
          statement1 = makeSemanticTreeS (s1, st)
          statement2 = makeSemanticTreeS (s2, st)
makeSemanticTreeS (statement@(While e s), st) =
    (While (fst expr) . fst $ statement, st ++ (snd expr) ++ (snd statement))
    where expr = makeSemanticTreeE (e, st)
          statement = makeSemanticTreeS (s, st)
makeSemanticTreeS (statement@(Solo e), st) =
    (Solo . fst $ expr, st ++ (snd expr))
    where expr = makeSemanticTreeE (e, st)
makeSemanticTreeS (statement@(Return e), st) =
    (Return . fst $ expr, st ++ snd expr)
    where expr = makeSemanticTreeE (e, st)
makeSemanticTreeS (statement@(SttList l), st) =
    (SttList (map makeTree l), st ++ (foldr (++) [] $ map makeTreeSt l))
    where makeTree :: Statement -> Statement
          makeTree s = fst $ makeSemanticTreeS (s, st)
          makeTreeSt :: Statement -> [Obj]
          makeTreeSt s = snd $ makeSemanticTreeS (s, st)
makeSemanticTreeS (statement, st) = (statement, st)

makeSemanticTreeE :: (Expr, [Obj]) -> (Expr, [Obj])
makeSemanticTreeE (expr@(DeclarationList l), st) =
    (expr,
     snd $ pushList (reverse . map makeObjFromCouple $ indexing
      (foldr (++) [] $ map showList l) stackOff) st)
    where --これからスタックに積むオブジェクトのlevel
          stackLev | length st == 0 = 2
                   | kind (head st) == Param = 2
                   | otherwise = (+) 1 . lev . head $ st
          --これからスタックに積むオブジェクトのoffsetの始点
          stackOff | length st == 0 = 0
                   | kind (head st) == Param = 0
                   | otherwise = (+) 1 . offset . head $ st
          makeObjFromCouple :: (String, Integer) -> Obj
          makeObjFromCouple (nam, off) = Obj nam stackLev Var off
          showList :: Expr -> [String]
          showList (Declaration (DeclaratorList l)) = map showStr l
          showList _ = []
          showStr :: Expr -> String
          showStr (Declarator s) = s
          showStr _ = ""
makeSemanticTreeE (Ident s, st) =
    case searchStack st s of
        Just obj -> (Object obj, st)
        Nothing -> (UnDefVar s, st ++ [Obj s 0 Err 0])
makeSemanticTreeE (Minus e, st) =
    makeSemanticTreeE_1op Minus e st

makeSemanticTreeE ((TwoOp op e1 e2), st) =
    makeSemanticTreeE_2op (TwoOp op) e1 e2 st

makeSemanticTreeE ((Asgn e1 e2), st) =
    makeSemanticTreeE_2op Asgn e1 e2 st

makeSemanticTreeE ((CompoundAsgn op e1 e2), st) =
    makeSemanticTreeE_2op (CompoundAsgn op) e1 e2 st

makeSemanticTreeE ((CallFunc e1@(Ident s) e2@(ArguExprList l)), st) =
    case foundObj of
        Nothing -> makeCallFunc UnDefFun
        Just o | offset o == numOfArgu -> makeCallFunc Func
               | otherwise             -> makeCallFunc PrmNumNonMathedFunc
    where numOfArgu = fromIntegral $ length l
          foundObj = searchStack st s
          analysisedArgList = fst $ makeSemanticTreeE (e2, st)
          analysisedArgListSt = snd $ makeSemanticTreeE (e2, st)
          makeCallFunc :: ObjKind -> (Expr, [Obj])
          makeCallFunc UnDefFun =
           (CallFunc (Object $ Obj s 0 UnDefFun numOfArgu) analysisedArgList,
            st ++  [Obj s 0 UnDefFun 0] ++ analysisedArgListSt)
          makeCallFunc k =
           (CallFunc (Object $ Obj s 0 k numOfArgu) analysisedArgList,
            st ++ analysisedArgListSt)

makeSemanticTreeE (ArguExprList l, st) =
    (ArguExprList $ map makeTree l,st ++ (foldr (++) [] $ map makeTreeSt l))
    where makeTree :: Expr -> Expr
          makeTree e = fst $ makeSemanticTreeE (e, st)
          makeTreeSt :: Expr -> [Obj]
          makeTreeSt e = snd $ makeSemanticTreeE (e, st)

makeSemanticTreeE (Exprssn l, st) =
    (Exprssn $ map makeTree l,st ++ (foldr (++) [] $ map makeTreeSt l))
    where makeTree :: Expr -> Expr
          makeTree e = fst $ makeSemanticTreeE (e, st)
          makeTreeSt :: Expr -> [Obj]
          makeTreeSt e = snd $ makeSemanticTreeE (e, st)

makeSemanticTreeE (Ternary e1 e2 e3, st) =
    (Ternary (fst expr1) (fst expr2) (fst expr3)
    , st ++ snd expr1 ++ snd expr2 ++ snd expr3)
    where expr1 = makeSemanticTreeE (e1, st)
          expr2 = makeSemanticTreeE (e2, st)
          expr3 = makeSemanticTreeE (e3, st)


makeSemanticTreeE (expr, st) = (expr, st)

makeSemanticTreeE_1op :: (Expr -> Expr) -> Expr -> [Obj]
                          -> (Expr, [Obj])
makeSemanticTreeE_1op constructor e st =
    (constructor (fst expr), st ++ (snd expr))
    where expr = makeSemanticTreeE (e, st)

makeSemanticTreeE_2op :: (Expr -> Expr -> Expr) -> Expr -> Expr -> [Obj]
                          -> (Expr, [Obj])
makeSemanticTreeE_2op constructor e1 e2 st =
    (constructor (fst expr1) (fst expr2),
     st ++ (snd expr1) ++ (snd expr2))
    where expr1 = makeSemanticTreeE (e1, st)
          expr2 = makeSemanticTreeE (e2, st)

makeSemanticTreeE_lst :: ([Expr] -> Expr) -> [Expr] -> [Obj]
                          -> (Expr, [Obj])
makeSemanticTreeE_lst constructor l st =
    ((constructor $ map makeTree l), st ++ (foldr (++) [] $ map makeTreeSt l))
    where makeTree :: Expr -> Expr
          makeTree e = fst $ makeSemanticTreeE (e, st)
          makeTreeSt :: Expr -> [Obj]
          makeTreeSt e = snd $ makeSemanticTreeE (e, st)


varObj :: String -> Integer -> Integer -> Obj
varObj nam lev off = Obj nam lev Var off
          
searchStack :: [Obj] -> String -> Maybe Obj
searchStack [] _ = Nothing
searchStack (car:cdr) ident | name car == ident = Just car
                            | otherwise         = searchStack cdr ident

extractKindObj :: ObjKind -> [Obj] -> [Obj]
extractKindObj objKind st = filter isKnd st
    where isKnd :: Obj -> Bool
          isKnd (Obj _ _ knd _) | knd == objKind = True
                                | otherwise      = False

extractErr = extractKindObj Err
extractUnDefFunc = extractKindObj UnDefFun

checkTreeDuplication :: ExternDclr -> Maybe [String]
checkTreeDuplication (Program l) =
    case duplication . extractName $ Program l of
        Just a -> Just a
        Nothing -> foldr maybeCouple Nothing $ map checkTreeDuplication l
checkTreeDuplication (ExternFuncDec (FuncDefinition e1 e2 s)) =
    case duplication . extractAllDclrFromFunc $ FuncDefinition e1 e2 s of
        Just a -> Just a
        Nothing -> checkSttDuplication s
checkTreeDuplication _ = Nothing


checkSttDuplication :: Statement -> Maybe [String]
checkSttDuplication (CompoundStt e s) =
    maybeCouple (duplication . extractDclrFromStt $ CompoundStt e s)
                 $ checkSttDuplication s
checkSttDuplication (If e s) = checkSttDuplication s
checkSttDuplication (IfElse e s1 s2) =
    maybeCouple (duplication $ extractDclrFromStt s1)
    . duplication . extractDclrFromStt $ s2
checkSttDuplication (While e s) = checkSttDuplication s
checkSttDuplication (SttList l) =
    foldr maybeCouple Nothing $ map checkSttDuplication l
checkSttDuplication _ = Nothing


extractParamDclrFromFunc :: FuncDef -> [String]
extractParamDclrFromFunc (FuncDefinition e1 (ParamDclrList l) s) =
    map extractNameFromExpr l ++ extractDclrFromStt s
extractParamDclrFromFunc _ = []



extractAllDclrFromFunc :: FuncDef -> [String]
extractAllDclrFromFunc (FuncDefinition e1 e2 s) =
    extractParamDclrFromFunc $ FuncDefinition e1 e2 s


extractDclrFromStt :: Statement -> [String]
extractDclrFromStt (CompoundStt (DeclarationList l) s) =
    foldr (++) [] $ map showList l
    where showList :: Expr -> [String]
          showList (Declaration (DeclaratorList l)) = map showStr l
          showList _ = []
          showStr :: Expr -> String
          showStr (Declarator s) = s
          showStr _ = ""
extractDclrFromStt _ = []

extractName :: ExternDclr -> [String]
extractName (ExternDeclaration (Declaration (DeclaratorList l))) =
    map showVal l
extractName (ExternFuncDec (FuncDefinition e1 e2 e3)) = [showVal e1]
extractName (Program l) = foldr (++) [] $ map extractName l
extractName _ = []

extractDclrName :: ExternDclr -> [String]
extractDclrName (ExternDeclaration (Declaration (DeclaratorList l))) =
    map showVal l
extractDclrName (Program l) = foldr (++) [] $ map extractDclrName l
extractDclrName _ = []

extractFuncName :: ExternDclr -> [(String, Integer)]
extractFuncName (ExternFuncDec (FuncDefinition e1 (ParamDclrList l) e3)) =
    [(showVal e1, fromIntegral $ length l)]
extractFuncName (Program l) = foldr (++) [] $ map extractFuncName l
extractFuncName _ = []

extractNameFromExpr :: Expr -> String
extractNameFromExpr (ParamDclr e) = showVal e
extractNameFromExpr _ = []
    
extractNameFromCompound _ = []


isConst :: Expr -> Bool
isConst (Number _) = True
isConst _ = False

constEval :: ExternDclr -> ExternDclr
constEval (Program l) = Program $ map constEval l
constEval (ExternFuncDec (FuncDefinition e1 e2 s)) =
    ExternFuncDec . FuncDefinition e1 e2 $ constEvalS s
constEval e = e

constEvalS :: Statement -> Statement
constEvalS (SttList l) =
    SttList . filter isNotNon $ map constEvalS l
    where isNotNon (Non) = False
          isNotNon _ = True
constEvalS (CompoundStt e s) =
    CompoundStt e $ constEvalS s
constEvalS (If e s) =
    constIf . If evaledE $ constEvalS s
    where evaledE = constEvalE e
          constIf :: Statement -> Statement
          constIf (If (Exprssn []) _) = error "condition of if is empty"
          constIf (If (Number 0) s) = Non
          constIf (If (Number _) s) = s
          constIf s@(If _ _) = s
constEvalS (IfElse e s1 s2) =
    constIfElse . IfElse evaledE (constEvalS s1) $ constEvalS s2
    where evaledE = constEvalE e
          constIfElse :: Statement -> Statement
          constIfElse (IfElse (Exprssn []) _ _) = error "condition of if is empty"
          constIfElse (IfElse (Number 0) s1 s2) = s2
          constIfElse (IfElse (Number _) s1 s2) = s1
          constIfElse s@(IfElse _ _ _) = s
constEvalS (While e s) =
    constWhile . While evaledE $ constEvalS s
    where evaledE = constEvalE e
          constWhile :: Statement -> Statement
          constWhile (While (Exprssn []) _) = error "condition of if is empty"
          constWhile (While (Number 0) _) = Non
          constWhile s@(While _ _) = s
constEvalS (Return e) =
    Return $ constEvalE e
constEvalS (Solo e) = Solo $ constEvalE e
{--
constEvalS (Solo e) = removeNonExec . Solo . constEvalE $ e
    where removeNonExec s@(Solo (Asgn _ _)) = s
          removeNonExec s@(Solo (CallFunc _ _)) = s
          removeNonExec s = Non
--}
constEvalS s = s

constEvalE :: Expr -> Expr
constEvalE (Ternary e1 e2 e3) =
    ternaryEval $ Ternary (constEvalE e1) (constEvalE e2) (constEvalE e3)
    where ternaryEval :: Expr -> Expr
          ternaryEval (Ternary (Number 0) _ e) = e
          ternaryEval (Ternary (Number _) e _) = e
          ternaryEval e = e
constEvalE (TwoOp opr e1 e2) =
    numberEval $ TwoOp opr c1 c2
    where c1 = constEvalE e1
          c2 = constEvalE e2
          numberEval :: Expr -> Expr
          numberEval e@(TwoOp opr e1@(Number n1) e2@(Number n2)) =
              case searchOp opr of
                  Just a -> Number $ a n1 n2
                  Nothing -> e
          numberEval e@(TwoOp Eq e1@(Ident i1) e2@(Ident i2))
              | i1 == i2  = Number 1
              | otherwise = e
          numberEval e@(TwoOp NE e1@(Ident i1) e2@(Ident i2))
              | i1 == i2  = Number 0
              | otherwise = e
          numberEval e@(TwoOp Sub e1@(Ident i1) e2@(Ident i2))
              | i1 == i2  = Number 0
              | otherwise = e
          numberEval e@(TwoOp Div e1@(Ident i1) e2@(Ident i2))
              | i1 == i2  = Number 1
              | otherwise = e
          numberEval e@(TwoOp opr e1@(Ident _) e2@(Ident _)) = e
          numberEval (TwoOp And e1@(Number _) e2) = numberEval (TwoOp And e2 e1)
          numberEval (TwoOp And e1 (Number 0)) = Number 0
          numberEval (TwoOp And e1 (Number _)) = numberEval e1
          numberEval (TwoOp Or e1@(Number _) e2) = numberEval (TwoOp Or e2 e1)
          numberEval (TwoOp Or e1 (Number 0)) = numberEval e1
          numberEval (TwoOp Or e1 (Number _)) = Number 1
          numberEval (TwoOp Add e1 (Number 0)) = numberEval e1
          numberEval (TwoOp Add (Number 0) e2) = numberEval e2
          numberEval (TwoOp Sub e1 (Number 0)) = numberEval e1
          numberEval (TwoOp Sub (Number 0) e2) = numberEval $ Minus e2
          numberEval (TwoOp Mul e1 (Number 0)) = Number 0
          numberEval (TwoOp Mul (Number 0) e2) = Number 0
          numberEval (TwoOp Mul e1 (Number 1)) = Number 1
          numberEval (TwoOp Mul (Number 1) e2) = Number 1
          numberEval (TwoOp Div e1 (Number 0)) = error "Zero Division"
          numberEval (TwoOp Div (Number 0) e2) = Number 0
          numberEval (TwoOp opr e1@(Ident _) e2) =
              TwoOp opr e1 $ constEvalE e2
          numberEval (TwoOp opr e1 e2@(Ident _)) = 
              TwoOp opr (constEvalE e1) e2
          numberEval e = e
constEvalE (Minus e) =
    constMinus . Minus $ constEvalE e
    where constMinus :: Expr -> Expr
          constMinus (Minus (Number n)) = Number $ (-1) * n
          constMinus e = e
constEvalE e@(Exprssn []) = e
constEvalE (Exprssn l) =
    case length filtered of
        0 -> evaledLast
        _ -> Exprssn $ filtered ++ [evaledLast]
    where isNotConst :: Expr -> Bool
          isNotConst (Number _) = False
          isNotConst _ = True
          filtered = filter isNotConst $ init l
          evaledLast = constEvalE $ last l
constEvalE (CallFunc e (ArguExprList l)) =
    (CallFunc e (ArguExprList $ map constEvalE l))
constEvalE (Asgn e1 e2) =
    Asgn e1 $ constEvalE e2
constEvalE e = e



maybeCouple :: Maybe [a] -> Maybe [a] -> Maybe [a]
maybeCouple x y = if length l == 0 then Nothing else Just l
    where p = fromMaybe [] x
          q = fromMaybe [] y
          l = p ++ q

push :: a -> [a] -> (a, [a])
push c cs = (c, c:cs)

pop :: [a] -> (a, [a])
pop (c:cs) = (c, cs)

pushList :: [a] -> [a] -> ([a], [a])
pushList c cs = (c, c ++ cs)

duplication :: Eq a => [a] -> Maybe [a]
duplication [] = Nothing
duplication l = case inList (head l) $ tail l of
    Just a -> Just [a]
    Nothing -> duplication $ tail l

inList :: Eq a => a -> [a] -> Maybe a
inList a [] = Nothing
inList a l = if a == head l then Just a else inList a $ tail l

indexing :: [a] -> Integer -> [(a, Integer)]
indexing l start = iter (l, start)
    where iter :: ([a], Integer) -> [(a, Integer)]
          iter ([], m) = []
          iter ([atom], m) = [(atom, m)]
          iter (car:cdr, m) = (car, m) : iter (cdr, m + 1)

reverseIndexing :: [a] -> Integer -> [(a, Integer)]
reverseIndexing l start = indexReverse $ indexing l start
    where indexReverse :: [(a, Integer)] -> [(a, Integer)]
          indexReverse l =
           map (\ (dat, index) -> (dat, fromIntegral (length l)-index-1+start*2)) l

tupleListFold :: [([a], [b])] -> ([a], [b])
tupleListFold l = (foldr (++) [] . map fst $ l, foldr (++) [] . map snd $ l)

fstIn3 (a,_,_) = a
sndIn3 (_,b,_) = b
thdIn3 (_,_,c) = c


