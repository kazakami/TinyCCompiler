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





{--
parseFactor :: Parser CVal
parseFactor = do
    whiteSpace
    f <- parens parseExpr
         <|> parseVar
    whiteSpace
    return f

parseStatement :: Parser CVal
parseStatement = do
    exp <- parseExpr
    whiteSpace >> semi
    return exp

parseVar :: Parser CVal
parseVar = do
    whiteSpace
    n <- parseNumber
         <|> parseDeclarator
    whiteSpace
    return n


parseParameterDeclaration :: Parser CVal
parseParameterDeclaration = do
    whiteSpace
    char 'i' >> char 'n' >> char 't' >> spaces1
    p <- parseDeclarator
    return p

parseDeclarationList :: Parser [CVal]
parseDeclarationList = do
    d <- parseDeclaration
    ((++) d <$> (whiteSpace *> parseDeclarationList)) <|> pure d
    
parseDeclaration :: Parser [CVal]
parseDeclaration = do
    char 'i' >> char 'n' >> char 't'
    spaces1
    pl <- parseDeclaratorList
    whiteSpace >> semi
    return pl
    
parseDeclarator :: Parser CVal
parseDeclarator = do
    v <- identifier
    return $ Variation v

parseDeclaratorList :: Parser [CVal]
parseDeclaratorList = do
    p <- parseDeclarator
    ((:) p <$> (whiteSpace *> comma *> whiteSpace *> parseDeclaratorList)) <|> pure (p : [])

parseNumber :: Parser CVal
parseNumber = liftM (Number . read) $ many1 digit


parseProgram :: Parser [CVal]
parseProgram = do
    p <- parseExternalDeclaration
    ((++) p <$> (whiteSpace *> parseProgram)) <|> pure p

parseFunctionDefinition :: Parser [CVal]
parseFunctionDefinition = do
    p <- parseNumber
    ((:) p <$> (whiteSpace *> comma *> whiteSpace *> parseFunctionDefinition)) <|> pure (p : [])

parseParamTypeList :: Parser [CVal]
parseParamTypeList = do
    p <- parseParamDeclaration
    ((:) p <$> (whiteSpace *> comma *> whiteSpace *> parseParamTypeList)) <|> pure (p : [])

parseParamDeclaration :: Parser CVal
parseParamDeclaration = do
    char 'i' >> char 'n' >> char 't' >> spaces1
    p <- parseDeclarator
    return p    

parseExternalDeclaration :: Parser [CVal]
parseExternalDeclaration = do
    whiteSpace
    p <- parseDeclaration
      <|> parseFunctionDefinition
    whiteSpace
    return p
--}


prs ::(Show a) => Parser a -> String -> IO ()
prs parser str = print $ case parse parser "TinyC" str of
    Left err -> show err
    Right val -> show val



main :: IO ()
main = do
   input <- getLine
   print $ case parse unaryExpr "TinyC" input of
      Left err -> show err
      Right val -> show val


data Expr = Ident String
          | Declarator String
          | DeclaratorList Expr Expr
          | Declaration Expr
          | DeclarationList Expr Expr
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

data Statement = Non
               | Solo Expr
               | If Expr Statement
               | IfElse Expr Statement Statement
               | While Expr Statement
               | Return Expr
               | SttList Statement Statement

showVal :: Expr -> String
showVal (Number n) = show n -- "(Num " ++ show n ++ ")"
showVal (Ident s) = s -- "(Ident " ++ s ++ ")"
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
showVal (DeclaratorList n1 n2) = "(DcLst " ++ showVal n1 ++ " " ++
                                 showVal n2 ++ ")"
showVal (Declaration n) = "(dclrt " ++ showVal n ++ ")"
showVal (DeclarationList n1 n2) = "(DecList " ++ showVal n1 ++ " " ++
                                  showVal n2 ++ ")"

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
showStatement (SttList s1 s2) = "(SttList " ++ showStatement s1 ++
                                " " ++ showStatement s2 ++ ")"

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


pairExpr :: Parser Expr
pairExpr = do n1 <- unaryExpr
              _ <- char '*'
              n2 <- unaryExpr
              return $ Pair n1 n2

declarationList :: Parser Expr
declarationList = try (do spaces
                          p <- declaration
                          spaces
                          q <- declarationList
                          return (DeclarationList p q))
                   <|> declaration

declaration :: Parser Expr
declaration = do string "int"
                 _ <- string " "
                 spaces
                 p <- declaratorList
                 spaces
                 _ <- string ";"
                 return $ Declaration p

declarator :: Parser Expr
declarator = do p <- identifier
                return $ Declarator p

declaratorList :: Parser Expr
declaratorList = try (do spaces
                         p <- declarator
                         spaces
                         _ <- char ','
                         spaces
                         q <- declaratorList
                         return (DeclaratorList p q))
                 <|> declarator
                         

statementList :: Parser Statement
statementList = try (do spaces
                        p <- statement
                        spaces
                        q <- statementList
                        return (SttList p q))
                 <|> statement


statement :: Parser Statement
statement =
    try (do spaces
            char ';'
            return (Non))
     <|> try (do p <- expression
                 spaces
                 _ <- char ';'
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
                 return (While e s))
     <|> try (do string "return"
                 spaces
                 e <- expression
                 spaces
                 char ';'
                 return (Return e))

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
postfixExpr = do 
    p <- primaryExpr
    return p

unaryExpr :: Parser Expr
unaryExpr = postfixExpr
             <|> do  spaces
                     _ <- char '-'
                     spaces
                     p <- unaryExpr
                     return $ Minus p





