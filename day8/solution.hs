import Data.Either (rights)
import Data.Map.Strict (Map, fromList, (!), adjust, elems)
import Data.List (sort)
import Control.Applicative ((<|>), many, liftA)
import Text.Parsec (ParseError, runParser, many1, option, try)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (lower, string, char, digit, space, endOfLine)

type State = Map RegName Int
newtype RegName = RegName String
instance Show RegName where
  show (RegName a) = "RegName " ++ show a
instance Eq RegName where
  (RegName a) == (RegName b) = a == b
instance Ord RegName where
  (RegName a) `compare` (RegName b) = compare a b

data Op = Inc | Dec deriving (Show, Read)
data CondOp = Sup | SupEq | Inf | InfEq | Eq | NEq deriving Show
data Cond = Cond RegName CondOp Int deriving Show
data Inst = Inst { reg :: RegName,
                   op :: Op,
                   amount :: Int,
                   cond :: Cond } deriving Show

regParser :: Parser RegName
regParser = RegName <$> many1 lower <* space

opParser :: Parser Op
opParser =  ((pure Inc <* string "inc") <|> (pure Dec <* string "dec")) <* space

amountParser :: Parser Int
amountParser = read <$> ((++) <$> (option "" (string "-")) <*> many1 digit) <* (space <|> endOfLine)

condOpParser :: Parser CondOp
condOpParser = (try (string ">=") *> pure SupEq
                <|> try (string "<=") *> pure InfEq
                <|> try (string "!=") *> pure NEq
                <|> try (string "==") *> pure Eq
                <|> char '>' *> pure Sup
                <|> char '<' *> pure Inf) <* space

condParser :: Parser Cond
condParser = string "if " *> pure Cond <*> regParser <*> condOpParser <*> amountParser

instParser :: Parser Inst
instParser = Inst <$> regParser <*> opParser <*> amountParser <*> condParser

registers :: [Inst] -> State
registers xs = fromList $ foldl (\acc inst -> (reg inst, 0):acc) [] xs

verify :: Cond -> State -> Bool
verify (Cond n op o) s = case op of
                           Sup -> r > o
                           SupEq -> r >= o
                           Inf -> r < o
                           InfEq -> r <= o
                           Eq -> r == o
                           NEq -> r /= o
  where r = s!n

update :: Inst -> State -> State
update i s = case op i of
               Inc -> adjust (\e -> e + (amount i)) (reg i) s
               Dec -> adjust (\e -> e - (amount i)) (reg i) s

execute :: Inst -> State -> State
execute i s =  if verify (cond i) s then
                 update i s
               else
                 s

runPart1 :: [Inst] -> State -> State
runPart1 is s = foldl (\s' i -> execute i s') s is

runPart2 :: [Inst] -> State -> Int
runPart2 is s = head $ reverse $ sort $ map (head . reverse . sort . elems) $ foldl (\acc i -> (execute i (head acc)):acc) [s] is

main = do
  raw <- readFile "input.txt"
  let parsed = rights $ map (runParser instParser () "input.txt") (map (++"\n") $ lines raw)
      regs = registers parsed
      results = runPart1 parsed regs
  print $ sort (elems results)
  print $ runPart2 parsed regs

