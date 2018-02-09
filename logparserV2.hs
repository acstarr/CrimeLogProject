-- Adam Starr
-- Feb. 8 2018

-- Assumes Claremont College Daily Crime logs converted from PFD using the pdftotext command from poppler 

module LogParser where
import Data.Char
import qualified Data.Map as Map
import System.Environment
import System.IO
import System.Exit
import Data.List
import Text.ParserCombinators.Parsec
import Control.Monad (join)

data LogEntry = LogEntry { code :: String
                         , crime :: String
                         , description :: Maybe String
                         , college :: String
                         , location :: Maybe String
                         , address :: Maybe String
                         , dateReported :: Maybe (Int,Int,Int)
                         , timeReported :: Maybe (Int,Int,Bool)
                         , startDate :: Maybe (Int,Int,Int)
                         , startTime :: Maybe (Int,Int,Bool)
                         , endDate :: Maybe (Int,Int,Int)
                         , endTime :: Maybe (Int,Int,Bool)
                         , caseNumber :: String
                         , disposition :: String
                         } deriving (Show) 

log :: GenParser Char st [LogEntry]
log =  do pages <- many page
          eof
          return $ join pages

page :: GenParser Char st [LogEntry]
page = do entries <- many incident
          eof
          return entries

           
pCode :: GenParser Char st String
pCode = many $ satisfy (\c-> isAlphaNum c || (c== '.') || (c== '(') || (c== ')') || (c== '/') || (c== ' '))

pCrime :: GenParser Char st String
pCrime = many $ satisfy (\c-> isAlphaNum c || (c== ' ')|| (c== '/'))

pDescription :: GenParser Char st (Maybe String)
pDescription =(Just <$> try (char '-' *> many (satisfy (\c-> (c/='\n') && isAlphaNum c || isSeparator c || isSymbol c || isPunctuation c))))<|> pure Nothing 

pCollege :: GenParser Char st String
pCollege = choice $ map (try.string) $ ["POM", "SCR", "PTZ", "KGI", "CMC", "HMC", "CGU", "CUC", "OFF CAMPUS"]

pLocation :: GenParser Char st (Maybe String)
pLocation = (Just <$> try (char ':' *> spaces *> many (satisfy (\c-> isAlphaNum c || isSeparator c || isSymbol c || isPunctuation c)))) <|> pure Nothing 



pAddress :: GenParser Char st (Maybe String)
pAddress = (Just <$> try (char ':' *> spaces *> many (satisfy (/= '\n')))) <|> pure Nothing 

dateTime :: GenParser Char st (Maybe (Int,Int,Int), Maybe (Int,Int,Bool)) 
dateTime = (try $ string "UNKNOWN" *> pure (Nothing,Nothing)) <|> do theDate <- date 
                                                                     theTime <- time
                                                                     return (theDate,theTime)


date :: GenParser Char st (Maybe (Int,Int,Int))
date = do month <- many digit
          char '/'
          day <- many digit
          char '/'
          year <- many digit
          return $ Just (read month, read day, read year)

time :: GenParser Char st (Maybe (Int,Int,Bool))
time = do spaces
          h <- many digit
          char ':'
          m <- many digit
          spaces
          isAM <- (try $ string "AM" *> (pure True)) <|> (string "PM" *> (pure False))
          return $ Just (read h,read m,isAM)

pDispositionLine :: GenParser Char st String
pDispositionLine = (many1 (satisfy (\c-> isAlphaNum c || (c == '/') || (c== ' ')|| (c== '-') || (c== ',')|| (c== '(') || (c== ')')))) <* char '\n'  


pDisposition :: GenParser Char st String
pDisposition = join <$> many pDispositionLine



pageNum :: GenParser Char st ()
pageNum = do spaces 
             string "Page"
             spaces
             many digit
             spaces
             string "of"
             spaces
             many digit
             return ()


pCaseNumber :: GenParser Char st String
pCaseNumber = many $ satisfy (\c -> isAlphaNum c || c == '-') 

incident :: GenParser Char st LogEntry
incident =  do spaces
               string "Incident Type:"
               spaces
               optional $ try (string "CLAREMONT COLLEGES :")
               spaces
               theCode <- pCode
               spaces
               char ':'
               spaces
               theCrime <- pCode
               spaces
               theDescription <- pDescription
               spaces
               string "Location:"
               spaces
               theCollege <- pCollege
               spaces
               theLoc <- pLocation
               spaces
               theAddress <- pAddress
               spaces
               string "Date/Time Reported:" 
               spaces
               (reportDate, reportTime) <- dateTime
               spaces 
               string "Incident Occurred Between:"
               spaces
               (startDate, startTime) <- dateTime
               spaces 
               string "and"
               spaces
               (endDate, endTime) <- dateTime
               spaces
               string "Case #:"
               spaces
               theCaseNum <- pCaseNumber 
               spaces
               string "Int. Ref. #:"
               spaces
               string "Disposition:"
               theDispo <- pDisposition
               optional $ try pageNum                      -- end of line
               return LogEntry { code = theCode
                                 , crime = theCrime
                                 , description = theDescription
                                 , college = theCollege
                                 , location = theLoc
                                 , address = theAddress
                                 , dateReported = reportDate
                                 , timeReported =reportTime
                                 , startDate = startDate
                                 , startTime = startTime
                                 , endDate = endDate
                                 , endTime = endTime
                                 , caseNumber = theCaseNum
                                 , disposition = theDispo 
                                 }

-- The end of line character is \n
eol :: GenParser Char st Char
eol = char '\n'

parseLog :: String -> Either ParseError [LogEntry]
parseLog input = parse LogParser.page "error" input


main :: IO ()
main = do 
  args <- getArgs
  theLog <- if last args == "-"
    then getContents 
    else readFile $ last args
  print $ length <$> parseLog theLog
  exitSuccess