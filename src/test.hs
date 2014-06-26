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
             | UndefFun deriving Show

data Obj = Obj { name::String,
                 lev::Integer,
                 kind::ObjKind,
                 offsef::Integer } deriving Show

data Expr = Ident String
          | Declarator String
          | DeclaratorList [Expr]
          | Declaration Expr
          | DeclarationList [Expr]
          | ParamDclr Expr
          | ParamDclrList [Expr]
          | Number Integer
          | Minus Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Add Expr Expr
          | Sub Expr Expr
          | Pair Expr Expr
          | GrTh Expr Expr
          | GrEq Expr Expr
          | LsTh Expr Expr
          | LsEq Expr Expr
          | Eq Expr Expr
          | NE Expr Expr
          | And Expr Expr
          | Or Expr Expr
          | Asgn Expr Expr
          | Exprssn Expr Expr
          | Parenthesis Expr
          | ArguExprList [Expr]
          | PostfixExpr Expr Expr
          | ExprList [Expr]

data Statement = Non
               | Solo Expr
               | If Expr Statement
               | IfElse Expr Statement Statement
               | While Expr Statement
               | Return Expr
               | SttList [Statement]
               | CompoundStt Expr Statement

data FuncDef = FuncDefinition Expr Expr Statement
data ExternDclr = ExternDeclaration Expr
                | ExternFuncDec FuncDef
                | Program [ExternDclr]

showVal :: Expr -> String
showVal (Number n) = show n -- "(Num " ++ show n ++ ")"
showVal (Ident s) = s --"(Ident " ++ s ++ ")"
showVal (Minus n) = "(- " ++ showVal n ++ ")"
showVal (Mul n1 n2) = "(* " ++ showVal n1 ++ " " ++ showVal n2 ++ ")"
showVal (Div n1 n2) = "(/ " ++ showVal n1 ++ " " ++ showVal n2 ++ ")"
showVal (Add n1 n2) = "(+ " ++ showVal n1 ++ " " ++ showVal n2 ++ ")"
showVal (Sub n1 n2) = "(- " ++ showVal n1 ++ " " ++ showVal n2 ++ ")"
showVal (GrTh n1 n2) = "(> " ++ showVal n1 ++ " " ++ showVal n2 ++ ")"
showVal (GrEq n1 n2) = "(>= " ++ showVal n1 ++ " " ++ showVal n2 ++ ")"
showVal (LsTh n1 n2) = "(< " ++ showVal n1 ++ " " ++ showVal n2 ++ ")"
showVal (LsEq n1 n2) = "(<= " ++ showVal n1 ++ " " ++ showVal n2 ++ ")"
showVal (Eq n1 n2) = "(== " ++ showVal n1 ++ " " ++ showVal n2 ++ ")"
showVal (NE n1 n2) = "(/= " ++ showVal n1 ++ " " ++ showVal n2 ++ ")"
showVal (And n1 n2) = "(and " ++ showVal n1 ++ " " ++ showVal n2 ++ ")"
showVal (Or n1 n2) = "(or " ++ showVal n1 ++ " " ++ showVal n2 ++ ")"
showVal (Pair n1 n2) = "(Pair " ++ showVal n1 ++ " " ++ showVal n2 ++ ")"
showVal (Asgn n1 n2) = "(set! " ++ showVal n1 ++ " " ++ showVal n2 ++ ")"
showVal (Exprssn n1 n2) = "(expr " ++ showVal n1 ++ " " ++ showVal n2 ++ ")"
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

showStatement :: Statement -> String
showStatement (Non) = "()"
showStatement (Solo e) = "(Stt " ++ showVal e ++ ")"
showStatement (If e s) = "(if " ++ showVal e ++ " " ++ showStatement s ++ ")"
showStatement (IfElse e s1 s2) = "(if " ++ showVal e ++ " " ++
                                 showStatement s1 ++ " " ++
                                 showStatement s2 ++ ")"
showStatement (While e s) = "(while (" ++ showVal e ++ ")" ++
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
showExternDclr (Program []) = "(Program nil)"
showExternDclr (Program l) = "(Program\n  " ++ (unwords $  map ((++ "\n") . showExternDclr) l) ++ ")\n"


rightExpr :: Parser Expr
rightExpr = buildExpressionParser operatorTable unaryExpr

operatorTable = [[op "*" Mul AssocLeft, op "/" Div AssocLeft]
                ,[op "+" Add AssocLeft, op "-" Sub AssocLeft]
                ,[op ">" GrTh AssocLeft, op ">=" GrEq AssocLeft
                , op "<" LsTh AssocLeft, op "<=" LsEq AssocLeft]
                ,[op "==" Eq AssocLeft, op "!=" NE AssocLeft]
                ,[op "&&" And AssocLeft]
                ,[op "||" Or AssocLeft]]
         where
           op s f assoc
              = Infix (do{reservedOp s; return f}) assoc

alphabetList = ['a'..'z'] ++ ['A'..'Z']
digitList = ['0'..'9']

instance Show Expr where show = showVal
instance Show Statement where show = showStatement
instance Show FuncDef where show = showFuncDef
instance Show ExternDclr where show = showExternDclr


pairExpr :: Parser Expr
pairExpr = do n1 <- unaryExpr
              _ <- char '*'
              n2 <- unaryExpr
              return $ Pair n1 n2

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
                 string "int"
                 spaces1
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
                    return $  DeclaratorList p

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
                     return (Exprssn p q))
               <|> assignExpr

assignExpr :: Parser Expr
assignExpr = try (do spaces
                     p <- ident
                     spaces
                     _ <- char '='
                     spaces
                     q <- assignExpr
                     return (Asgn p q))
              <|> rightExpr

primaryExpr :: Parser Expr
primaryExpr =
    try (do spaces
            char '('
            spaces 
            p <- expression
            char ')'
            spaces
            return (Parenthesis p))
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
                     <|> (do spaces
                             char '{'
                             spaces
                             p <- statementList
                             spaces
                             char '}'
                             return (CompoundStt (DeclarationList []) p))



prsProgram :: String -> IO ()
prsProgram str = case parse program "TinyC" str of
    Left err -> putStrLn $ show err
    Right val -> case checkTreeDuplication val of
        Just a -> putStrLn . (++) "Multiple Declaration : " . unwords $ a
        Nothing -> putStrLn . unwords . map show . snd . semanticAnalysis $ (val, []::[Obj])
--        Nothing -> putStrLn $ show val --putStrLn . unwords . extractName $ val




semanticAnalysis :: (ExternDclr, [Obj]) -> (ExternDclr, [Obj])
semanticAnalysis ((Program l) ,st) =
    (prog,
    (reverse . snd . pushList st . map levZeroVarObj . extractDclrName $ prog)
    ++ (reverse . snd . pushList st . map levZeroFuncObj . extractFuncName $ prog))
    where levZeroVarObj :: String -> Obj
          levZeroVarObj s = Obj s 0 Var 0
          levZeroFuncObj :: (String, Integer) -> Obj
          levZeroFuncObj (s, i) = Obj s 0 Func i
          prog = Program l
semanticAnalysis (ExternFuncDec (FuncDefinition e1 e2 s)) =
    


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
    maybeCouple (duplication . extractDclrFromStt $ CompoundStt e s) $ checkSttDuplication s
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
extractName (Program l)  = foldr (++) [] $ map extractName l
extractName _ = []

extractDclrName :: ExternDclr -> [String]
extractDclrName (ExternDeclaration (Declaration (DeclaratorList l))) =
    map showVal l
extractDclrName (Program l)  = foldr (++) [] $ map extractDclrName l
extractDclrName _ = []

extractFuncName :: ExternDclr -> [(String, Integer)]
extractFuncName (ExternFuncDec (FuncDefinition e1 (ParamDclrList l) e3)) =
    [(showVal e1, fromIntegral $ length l)]
extractFuncName (Program l)  = foldr (++) [] $ map extractFuncName l
extractFuncName _ = []

extractNameFromExpr :: Expr -> String
extractNameFromExpr (ParamDclr e) = showVal e
extractNameFromExpr _ = []
    
extractNameFromCompound _ = []

maybeCouple ::Eq a => Maybe [a] -> Maybe [a] -> Maybe [a]
maybeCouple x y = if l == [] then Nothing else Just l
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



