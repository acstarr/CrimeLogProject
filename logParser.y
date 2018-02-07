-- Adam Starr
-- Feb. 6 2018

{
module LogParser where
import Data.Char
import qualified Data.Map as Map
import System.Environment
import System.IO
import System.Exit
}


%name logParser
%tokentype { Token }


%token 
 "Incident Type:\nLocation:\nDate/Time Reported: Case #:\nCLAREMONT COLLEGES"    { TokenHeader}
 loc     { TokenLoc $$ }
 date    { TokenDate $$ }
 time    { TokenTime $$ } 
 "AM"    { TokenAM }
 "PM"    { TokenPM }
 desc    { TokenDes $$ }
 col     { TokenCol $$ }
 "PENAL CODE :"      { TokenPC }
 "P.C."  { TokenPCEnd }
 "VEHICLE CODE :"      { TokenVC }
  "V.C."  { TokenVCEnd }
 ':'     { TokenCol }
 '/'     { TokenSlash }
 '.'     { TokenPd }
 '-'     { TokenDash }
 "Page"  { TokenPage }
 "of"    { TokenOf }
 num     { TokenNum $$ }
 "Incident Occurred Between:"  { TokenOccur}
 "and"    { TokenAnd }
 "Int. Ref. #: Disposition:" {TokenDispoHead}
 dispo    { TokenDispo }



header =  "Incident Type:\nLocation:\nDate/Time Reported: Case #:\nCLAREMONT COLLEGES :"

%%


Log :: { [Incident] }
Log : Incident Log          { $1 :: $2 }
  | Incident PageNum Log    { $1 :: $3 } 
  | Incident                { [$1] } 
  | PageNum                 { []   }



Incident : header Violation '-' Desc col ':' Location Date Time  "Incident Occurred Between:" Date Time "and" Date Time CaseNum Dispo {}



--Do we have all of the cases?
Violation :  "PENAL CODE :" num '(' char ')'  "P.C." {}
  | "PENAL CODE :" num '(' char ')'  "P.C." {}
  | "PENAL CODE :" num '/' num  "P.C." {}
  | "VEHICLE CODE :" num  "V.C."{}


Desc words { $1 }
  | words '-' words { $1 ++ " - " ++ $3 }


Location words { ($1, "") }
  | words ':' Address { ($1, $3) }

Address -- combination of numbers and words and periods take all as a string

Date num '/' num '/' num { ($1, $3, $5) }


 -- AM is True PM is False
Time num ':' num "AM" { ($1,$3,True) }
  |  num ':' num "PM" { ($1,$3,False) }



CaseNum 

Dispo "Int. Ref. #: Disposition:" words { $2 }
Dispo "Int. Ref. #: Disposition:" words '/' words { $2 ++"/"++ $4 }


PageNum "Page" num "of" num { ($2, $4) }


{

happyError :: [Token] -> a
happyError xs =  error ("Parse error " ++ show (take 10 xs))


data LC  = App LC LC 
       | Var String 
       | Lambda Arg LC 
       | If LC LC LC 
       | Typed LC Type 
       | Num Int 
       | Truth Bool
       | Tuple LC LC 
       | Plus LC LC 
       | Minus LC LC 
       | Times LC LC 
       | Div LC LC 
       | And LC LC 
       | Or LC LC 
       | DubEq LC LC 
       | Less LC LC
       | Neg LC 
       | Not LC 
       | Fst LC 
       | Snd LC 
       | LetRec Arg LC LC deriving Eq

data Type = TInt | TBool | TAr Type Type | TTuple Type Type deriving Eq
type Arg = (String, Type)

type Vars = [String] 


data Token
 = TokenLam
 | TokenLet
 | TokenIn
 | TokenVar String
 | TokenNum Int
 | TokenInt
 | TokenPd
 | TokenEq 
 | TokenOB
 | TokenCB
 | TokenPlus
 | TokenMinus
 | TokenTimes
 | TokenDiv
 | TokenAnd
 | TokenOr
 | TokenDubEq
 | TokenCol
 | TokenIf
 | TokenThen
 | TokenElse
 | TokenNot
 | TokenFst
 | TokenSnd
 | TokenTrue
 | TokenFalse
 | TokenRec
 | TokenCom
 | TokenAr
 | TokenBool
 | TokenLess
 | TokenGreater
 | TokenNeq
 | TokenGte
 | TokenLte deriving Show 


lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) 
 | isSpace c = lexer cs
 | isAlpha c = lexVar (c:cs)
 | isDigit c = lexNum (c:cs)
lexer ('=':'=':cs) = TokenDubEq : lexer cs
lexer ('!':'=':cs) = TokenNeq : lexer cs
lexer ('<':'=':cs) = TokenLte : lexer cs
lexer ('>':'=':cs) = TokenGte : lexer cs
lexer ('-':'>':cs) = TokenAr : lexer cs
lexer ('=':cs) = TokenEq : lexer cs
lexer ('.':cs) = TokenPd : lexer cs
lexer ('(':cs) = TokenOB : lexer cs
lexer (')':cs) = TokenCB : lexer cs
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('-':cs) = TokenMinus : lexer cs
lexer ('*':cs) = TokenTimes : lexer cs
lexer ('/':cs) = TokenDiv : lexer cs
lexer ('<':cs) = TokenLess : lexer cs
lexer ('>':cs) = TokenGreater : lexer cs
lexer (',':cs) = TokenCom : lexer cs
lexer (':':cs) = TokenCol : lexer cs


lexVar cs = 
  case span ((||) <$> isAlphaNum <*> (== '\'')) cs of
 ("lambda",rest) -> TokenLam : lexer rest
 ("let",rest) -> TokenLet : lexer rest
 ("in",rest)  -> TokenIn : lexer rest
 ("if",rest)  -> TokenIf : lexer rest
 ("then",rest)  -> TokenThen : lexer rest
 ("else",rest)  -> TokenElse : lexer rest
 ("true",rest)  -> TokenTrue : lexer rest
 ("false",rest)  -> TokenFalse : lexer rest
 ("not",rest)  -> TokenNot : lexer rest
 ("fst",rest)  -> TokenFst : lexer rest
 ("snd",rest)  -> TokenSnd : lexer rest
 ("and",rest)  -> TokenAnd : lexer rest
 ("or",rest)  -> TokenOr : lexer rest
 ("rec",rest)  -> TokenRec : lexer rest
 ("int",rest)  -> TokenInt : lexer rest
 ("bool",rest)  -> TokenBool : lexer rest
 (var,rest)   -> TokenVar var : lexer rest

lexNum cs = TokenNum (read num) : lexer rest
  where (num,rest) = span isDigit cs

runLambdaCalc :: String -> LC
runLambdaCalc = typedLambdaCalc . lexer
