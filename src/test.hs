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
data Op2 = Mul
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
         | Asgn
         | Exprssn deriving (Show, Eq)

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
          | PostfixExpr Expr Expr
          | ExprList [Expr]
          | Object Obj
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
showVal (PostfixExpr n1 n2) = "(CallFunc " ++ showVal n1 ++ " " ++
                                showVal n2 ++ ")"
showVal (ExprList []) = "(List nil)"
showVal (ExprList l) = "(List " ++  (unwords $ map showVal l) ++ ")"
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
expression = try (do spaces
                     p <- assignExpr
                     spaces
                     _ <- char ','
                     spaces
                     q <- expression
                     return (TwoOp Exprssn p q))
               <|> assignExpr

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
                      return (PostfixExpr p q))
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
{--                     <|> (do spaces
                             char '{'
                             spaces
                             p <- statementList
                             spaces
                             char '}'
                             return (CompoundStt (DeclarationList []) p))
--}


prsProgram :: String -> IO ()
prsProgram str = case parse program "TinyC" str of
    Left err -> putStrLn $ show err
    Right val -> case checkTreeDuplication val of
        Just a -> putStrLn . (++) "Multiple Declaration : " . unwords $ a
        Nothing -> 
            let semanticAnalysised = semanticAnalysis (val, [])
            in case extractErr . snd $ semanticAnalysised of
                errs@_:_ -> putStrLn . (++) "Err : " . show $ errs
                [] -> putStrLn . unlines . codeGenerate . labelTagging . fst $ semanticAnalysised


codeGenerate :: ExternDclr -> [String]
codeGenerate (Program l) =
    foldr (++) [] $ map codeGenerate l
codeGenerate (TagedFunc (FuncDefinition (Declarator name) (ParamDclrList l) s)
                        nl i) =
    ["\tGLOBAL\t" ++ name,
     name ++ "\tpush\tebp",
     "\tmov\tebp, esp",
     "\tsub\tesp, " ++ show nl]
    ++ codeGenerateS nl s ++
    ["Lret\tmov\tesp, ebp",
     "\tpop\tebp",
     "\tret"]

codeGenerateS :: Integer ->Statement -> [String]
codeGenerateS nl (SttList l) =
    foldr (++) [] $ map (codeGenerateS nl) l
codeGenerateS nl (CompoundStt e s) =
    codeGenerateS nl s
codeGenerateS nl (Return e) =
    codeGenerateE nl e
    ++ ["\tjmp\tLret"]

codeGenerateE :: Integer -> Expr -> [String]
codeGenerateE _ (Number n) =
    ["\tmov\teax, " ++ show n]
codeGenerateE i (TwoOp op e1 e2) =
    codeGenerateE (i+4) e1
    ++ [emit OpMOV "" (genLoc i) eax]
    ++ codeGenerateE (i+4) e2
    ++ [emitOp2 op "" eax $ genLoc i]
    

data AsmOper = OpMOV
             | OpADD
             | OpMUL

showOp OpMOV = "mov"
showOp OpADD = "add"
showOp OpMUL = "imul"


instance Show AsmOper where show = showOp

asmOpTable = [(Add, OpADD),
              (Mul, OpMUL)]


searchAsmOp :: Op2 -> Maybe AsmOper
searchAsmOp = search asmOpTable
    where search :: [(Op2, AsmOper)] -> Op2 -> Maybe AsmOper
          search [] op = Nothing
          search (car:cdr) op | (fst car) == op = Just $ snd car
                              | otherwise       = search cdr op

eax = "eax"

genLoc :: Integer -> String
genLoc i = "loc(" ++ show i ++ ")"

--ラベル、第一オペランド、第二オペランド
emit :: AsmOper -> String -> String -> String -> String
emit op l e1 e2 = l ++ "\t" ++ show op ++ "\t" ++ e1 ++ ", " ++ e2

emitOp2 :: Op2 -> String -> String -> String -> String
emitOp2 op label e1 e2 = case searchAsmOp op of
                             Just a -> emit a label e1 e2
                             Nothing -> ";No implementation error"


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



semanticAnalysis :: (ExternDclr, [Obj]) -> (ExternDclr, [Obj])
semanticAnalysis (prog@(Program l) ,st) =
    (Program $ map semanticAnlysEx l, stack ++ (foldr (++) [] $ map semanticAnlysSt l))
    where levZeroVarObj :: String -> Obj
          levZeroVarObj s = Obj s 0 Var 0
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
{--
makeSemanticTreeE ((Mul e1 e2), st) =
    makeSemanticTreeE_2op Mul e1 e2 st
makeSemanticTreeE ((Div e1 e2), st) =
    makeSemanticTreeE_2op Div e1 e2 st
makeSemanticTreeE ((Add e1 e2), st) =
    makeSemanticTreeE_2op Add e1 e2 st
makeSemanticTreeE ((Sub e1 e2), st) =
    makeSemanticTreeE_2op Sub e1 e2 st
makeSemanticTreeE ((GrTh e1 e2), st) =
    makeSemanticTreeE_2op GrTh e1 e2 st
makeSemanticTreeE ((GrEq e1 e2), st) =
    makeSemanticTreeE_2op GrEq e1 e2 st
makeSemanticTreeE ((LsTh e1 e2), st) =
    makeSemanticTreeE_2op LsTh e1 e2 st
makeSemanticTreeE ((LsEq e1 e2), st) =
    makeSemanticTreeE_2op LsEq e1 e2 st
makeSemanticTreeE ((Eq e1 e2), st) =
    makeSemanticTreeE_2op Eq e1 e2 st
makeSemanticTreeE ((NE e1 e2), st) =
    makeSemanticTreeE_2op NE e1 e2 st
makeSemanticTreeE ((And e1 e2), st) =
    makeSemanticTreeE_2op And e1 e2 st
makeSemanticTreeE ((Or e1 e2), st) =
    makeSemanticTreeE_2op Or e1 e2 st
makeSemanticTreeE ((Asgn e1 e2), st) =
    makeSemanticTreeE_2op Asgn e1 e2 st
makeSemanticTreeE ((Exprssn e1 e2), st) =
    makeSemanticTreeE_2op Exprssn e1 e2 st
--}
makeSemanticTreeE ((PostfixExpr e1@(Ident s) e2@(ArguExprList l)), st) =
    case foundObj of
        Nothing -> makePostfixExpr UnDefFun
        Just o | offset o == numOfArgu -> makePostfixExpr Func
               | otherwise             -> makePostfixExpr PrmNumNonMathedFunc
    where numOfArgu = fromIntegral $ length l
          foundObj = searchStack st s
          analysisedArgList = fst $ makeSemanticTreeE (e2, st)
          analysisedArgListSt = snd $ makeSemanticTreeE (e2, st)
          makePostfixExpr :: ObjKind -> (Expr, [Obj])
          makePostfixExpr UnDefFun =
           (PostfixExpr (Object $ Obj s 0 UnDefFun numOfArgu) analysisedArgList,
            st ++  [Obj s 0 Err 0] ++ analysisedArgListSt)
          makePostfixExpr k =
           (PostfixExpr (Object $ Obj s 0 k numOfArgu) analysisedArgList,
            st ++ analysisedArgListSt)

makeSemanticTreeE (ArguExprList l, st) =
    (ArguExprList $ map makeTree l,st ++ (foldr (++) [] $ map makeTreeSt l))
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

extractErr :: [Obj] -> [Obj]
extractErr st = filter isErr st
    where isErr :: Obj -> Bool
          isErr (Obj _ _ knd _) | knd == Err = True
                                | otherwise  = False

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

