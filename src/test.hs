module Main where
import Control.Monad
import System.Environment
import Control.Monad.Error
import Control.Applicative hiding ((<|>))
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
           , reservedNames = ["if", "else", "while", "return", "int", "void"]
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
   input <- getLine
   putStrLn $ prs program input

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
         | Add
         | Sub
         | GrTh
         | GrEq
         | LsTh
         | LsEq
         | Eq
         | NE
         | And
         | Or
         | Asgn deriving (Show, Eq)

--Op2と演算子の対応
op2OperationTable =
    [(Mul, (*)),
     (Div, div),
     (Add, (+)),
     (Sub, (-))]

searchOp :: Op2 -> Maybe (Integer -> Integer -> Integer)
searchOp = search op2OperationTable
    where search :: [(Op2, (Integer -> Integer -> Integer))] -> Op2
                     -> Maybe (Integer -> Integer -> Integer)
          search [] _ = Nothing
          search (car:cdr) op | fst car == op = Just $ snd car
                              | otherwise     = search cdr op

--引数1の演算

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

operatorTable = [[op "*" (TwoOp Mul) AssocLeft, op "/" (TwoOp Div) AssocLeft]
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

funcDef :: Parser FuncDef
funcDef = do spaces
             string "int"
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
             return $ FuncDefinition p q s

paramDeclaration :: Parser Expr
paramDeclaration = do spaces
                      string "int"
                      spaces1
                      p <- declarator
                      return $ ParamDclr p

paramDeclarationList :: Parser Expr
paramDeclarationList = do p <- sepBy paramDeclaration $ spaces >> char ',' >> spaces
                          return $  ParamDclrList p

declarationList :: Parser Expr
declarationList = do p <- sepBy declaration spaces
                     return $  DeclarationList p

declaration :: Parser Expr
declaration = do spaces
                 reserved "int"
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
    try (do spaces
            char ';'
            spaces
            return (Non))
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
     <|> try (do spaces
                 p <- compoundStatement
                 spaces
                 return p)

expression :: Parser Expr
expression = do p <- sepBy assignExpr $ spaces >> char ',' >> spaces
                return $ Exprssn p

assignExpr :: Parser Expr
assignExpr = try (do spaces
                     p <- ident
                     spaces
                     _ <- char '='
                     spaces
                     q <- assignExpr
                     return (TwoOp Asgn p q))
              <|> rightExpr

primaryExpr :: Parser Expr
primaryExpr =
    try (do spaces
            char '('
            spaces 
            p <- expression
            char ')'
            spaces
            return p) --Parenthesis
     <|> number
     <|> ident

ident :: Parser Expr
ident = do spaces
           p <- identifier
           return $ Ident p

number :: Parser Expr
number = liftM (Number . read) $ many1 digit

num :: Parser Expr
num = do spaces
         c <- oneOf digitList
         spaces
         return . Number $ charToInt c
      where
         charToInt c = toInteger (ord c - ord '0')

postfixExpr :: Parser Expr
postfixExpr = try (do spaces
                      p <- ident
                      spaces
                      char '('
                      spaces
                      q <- arguExprList
                      spaces
                      char ')'
                      return (CallFunc p q))
                <|>  try (do spaces
                             p <- primaryExpr
                             return p)

unaryExpr :: Parser Expr
unaryExpr = postfixExpr
             <|> do  spaces
                     _ <- char '-'
                     spaces
                     p <- unaryExpr
                     return $ Minus p

arguExprList :: Parser Expr
arguExprList = do p <- sepBy assignExpr $ spaces >> char ',' >> spaces
                  return $ ArguExprList p

compoundStatement :: Parser Statement
compoundStatement = try (do spaces
                            char '{'
                            spaces
                            p <- declarationList
                            spaces
                            q <- statementList
                            spaces
                            char '}'
                            return (CompoundStt p q))

prsProgram :: String -> IO ()
prsProgram str = case parse program "TinyC" str of
    Left err -> putStrLn $ show err
    Right val -> case checkTreeDuplication val of
        Just a -> putStrLn . (++) "Multiple Declaration : " . unwords $ a
        Nothing -> 
            let semanticAnalysised = semanticAnalysis (val, [])                
            in case extractErr . snd $ semanticAnalysised of
                errs@_:_ -> putStrLn . (++) "Err : " . show $ errs
                [] -> putStrLn . unlines . codeGenerate (snd semanticAnalysised)
                       . labelTagging . fst $ semanticAnalysised

codeGen :: String -> IO ()
codeGen str = case parse program "TinyC" str of
    Left err -> putStrLn $ show err
    Right val -> case checkTreeDuplication val of
        Just a -> putStrLn . (++) "Multiple Declaration : " . unwords $ a
        Nothing ->
            let semanticAnalysised = semanticAnalysis (constEval val, [])
            in putStrLn . show  $ fst semanticAnalysised

codeGenerate :: [Obj] -> ExternDclr -> [String]
codeGenerate o (Program l) =
    (map ((\ nam -> "\tEXTERN\t" ++ nam) . name) . extractUnDefFunc $ o)
    ++ (foldr (++) [] $ map codeGenerateEx l)
codeGenerateEx :: ExternDclr -> [String]
codeGenerateEx (ExternDeclaration (Declaration (DeclaratorList l))) =
    map ((\ nam -> "\tCOMMON\t" ++ nam ++ " 4") . showVal) l
codeGenerateEx (TagedFunc (FuncDefinition (Declarator name) (ParamDclrList l) s)
                          nl i) =
    case stackOffset of
        0 ->
            ["\tGLOBAL\t" ++ name,
             name ++ ":"]
            ++ fst generatedS ++
            ["L" ++ name ++ "Ret:\tret"]
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
    (fst generatedE
     ++ ["\tjmp\tL" ++ fnam ++ "Ret"]
     , snd generatedE)
    where generatedE = codeGenerateE nl fnam idLst e
codeGenerateS nl fnam idLst i (Solo e) =
    codeGenerateE nl fnam idLst e
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
          cond = codeGenerateE nl fnam (0:idLst) e
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
          cond = codeGenerateE nl fnam (0:idLst) e
          labelHead = case tagSearch i tag of
                          Just a -> a
                          Nothing -> error "in generating \"while\""
codeGenerateS _ _ _ _ s = error $ showStatement s

--第一引数は一時変数のスタックの深さを意味する。
--第三引数はラベル識別に使う値
codeGenerateE :: Integer -> String -> [Integer] -> Expr -> ([String], [Integer])
codeGenerateE i fnam idLst (Number n) =
    ([(emit OpMOV "" eax $ show n) ++ "\t\t;Number"]
     , [i])
codeGenerateE i fnam idLst (Object (Obj nam lev Var off)) =
    ([(emit OpMOV "" eax $ "[ebp-" ++ (show . (*) 4 . (+) 1 $ off) ++ "]")
       ++ "\t;Var"]
     , [i])
codeGenerateE i fnam idLst (Object (Obj nam lev Param off)) =
    ([(emit OpMOV "" eax $ "[ebp+" ++ (show . (+) 8 . (*) 4 $ off) ++ "]")
       ++ "\t;Param"]
     , [i])
codeGenerateE i fnam idLst (Object (Obj nam lev Global off)) =
    ([(emit OpMOV "" eax $ "[" ++ nam ++ "]") ++ "\t;Global"]
     , [i])
codeGenerateE i fnam idLst (TwoOp Asgn (Object (Obj nam lev knd off)) e) =
    case knd of
        Var ->
            (fst gen
             ++ ["\tmov\t[ebp-" ++ (show . (*) 4 . (+) 1 $ off) ++ "], eax\t;AsgnVar"]
             , snd gen)
        Param -> 
            (fst gen
             ++ ["\tmov\t[ebp+" ++ (show . (+) 8 . (*) 4 $ off) ++ "], eax\t;AsgnPrm"]
             , snd gen)
        Global -> 
            (fst gen ++ ["\tmov\t[" ++ nam ++ "], eax\t;Global"]
             , snd gen)
    where gen = codeGenerateE i fnam idLst e
codeGenerateE i fnam idLst (TwoOp GrTh e1 e2) = codeGenerateCmp "g" fnam idLst i e1 e2
codeGenerateE i fnam idLst (TwoOp GrEq e1 e2) = codeGenerateCmp "ge" fnam idLst i e1 e2
codeGenerateE i fnam idLst (TwoOp LsTh e1 e2) = codeGenerateCmp "l" fnam idLst i e1 e2
codeGenerateE i fnam idLst (TwoOp LsEq e1 e2) = codeGenerateCmp "le" fnam idLst i e1 e2
codeGenerateE i fnam idLst (TwoOp Eq e1 e2) = codeGenerateCmp "e" fnam idLst i e1 e2
codeGenerateE i fnam idLst (TwoOp NE e1 e2) = codeGenerateCmp "ne" fnam idLst i e1 e2
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
codeGenerateE i fnam idLst (TwoOp op e1 e2) =
    (fst gen2
     ++ [emit OpMOV "" (tmpVar i) eax]
     ++ fst gen1
     ++ [emitOp2 op "" eax $ tmpVar i]
     , snd gen1 ++ snd gen2)
    where gen1 = codeGenerateE (i+4) fnam (1:idLst) e1
          gen2 = codeGenerateE (i+4) fnam (2:idLst) e2
codeGenerateE i fnam idLst (CallFunc (Object o) (ArguExprList l)) =
    ((foldr (++) [] . map (genPush . fst) . reverse $ codeL)
     ++ ["\tcall\t" ++ name o]
     ,foldr (++) [] . map snd $ codeL)
    where codeL = map (\ expr -> codeGenerateE i fnam ((snd expr):idLst) . fst $ expr) labeled
          labeled = indexing l 1
          genPush :: [String] -> [String]
          genPush s = s ++ ["\tpush\teax"]
codeGenerateE i fnam idLst (Exprssn l) = 
    (foldr (++) [] . map fst . reverse $ codeL
     ,foldr (++) [] . map snd $ codeL)
    where codeL = map (\ expr -> codeGenerateE i fnam ((snd expr):idLst) . fst $ expr) labeled
          labeled = indexing l 1
codeGenerateE _ _ _ e = error $ showVal e

showIdentList :: [Integer] -> String
showIdentList [] = []
showIdentList (car:cdr) = show car ++ showIdentList cdr

--第三引数はラベル識別に使う値
codeGenerateCmp :: String -> String -> [Integer] -> Integer -> Expr -> Expr
                    -> ([String], [Integer])
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
    TagedFunc (FuncDefinition e1 e2 . fst $ labeled) nl .
     labelModify 0 . labelSort . snd $ labeled
    where labeled = labelTaggingS s []
labelTagging e = e

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
    SttList $ map constEvalS l
constEvalS (CompoundStt e s) =
    CompoundStt e $ constEvalS s
constEvalS (If e s) =
    constIf . If evaledE $ constEvalS s
    where evaledE = constEvalE e
          constIf :: Statement -> Statement
          constIf (If (Number 0) s) = Non
          constIf (If (Number _) s) = s
          constIf e@(If _ s) = e
constEvalS (IfElse e s1 s2) =
    constIfElse . IfElse evaledE (constEvalS s1) $ constEvalS s2
    where evaledE = constEvalE e
          constIfElse :: Statement -> Statement
          constIfElse (IfElse (Number 0) s1 s2) = s2
          constIfElse (IfElse (Number _) s1 s2) = s1
          constIfElse e@(IfElse _ s1 s2) = e
constEvalS (Return e) =
    Return $ constEvalE e
constEvalS s = s

constEvalE :: Expr -> Expr
constEvalE e@(TwoOp opr e1@(Number n1) e2@(Number n2)) =
    case searchOp opr of
        Just a -> Number $ a n1 n2
        Nothing -> e
constEvalE (TwoOp opr e1@(Ident _) e2@(Ident _)) =
    TwoOp opr (constEvalE e1) (constEvalE e2)
constEvalE (TwoOp opr e1@(Ident _) e2) =
    TwoOp opr e1 $ constEvalE e2
constEvalE (TwoOp opr e1 e2@(Ident _)) =
    TwoOp opr (constEvalE e1) e2
constEvalE (TwoOp opr e1 e2) =
    constEvalE $ TwoOp opr c1 c2
    where c1 = constEvalE e1
          c2 = constEvalE e2
constEvalE (Minus e) =
    constMinus . Minus $ constEvalE e
    where constMinus :: Expr -> Expr
          constMinus (Minus (Number n)) = Number $ (-1) * n
          constMinus e = e
constEvalE e@(Exprssn []) = e
--constEvalE (Exprssn l) =
--    where 

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


