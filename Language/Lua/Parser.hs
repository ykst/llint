module Language.Lua.Parser where
import Text.Parsec
import Text.Parsec.Combinator (manyTill)
import Debug.Trace
import Control.Applicative hiding ((<|>),many,optional)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad (liftM, foldM)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Language.Lua.AST
import Language.Lua.Symbol (Intrinsic(..), binops, unops, stringOps, reservedWords)
-- the type of parser
type TheParser =  ParsecT String () Identity
-- sets for keyword lookup
reservedSet = Set.fromList reservedWords
stringOpSet = Set.fromList stringOps
-- utilities
comment   = try $ skipMany $ string "--" >> choice
     [try $ string "[[" >> manyTill anyChar (try (string "]]")) >> pure (),
      skipMany (noneOf "\r\n")] >> skipMany blank
blank     = oneOf " 　\t\r\n"
symbol s  = lx (string s) <?> "SYMBOL \"" ++ s ++ "\""
keyword s = try (lx $ string s <* notFollowedBy (alphaNum <|> char '_')) 
              <?> "KEYWORD \"" ++ s ++ "\""
parens    = between (symbol "(") (symbol ")")
braces    = between (symbol "{") (symbol "}")
angles    = between (symbol "[") (symbol "]")

dotn n = lx (string (replicate n '.') <* notFollowedBy (string "."))

ignored = skipMany blank >> optional comment
lx :: TheParser a -> TheParser a
lx p = p <* ignored

-- identifier
idHeadP   = letter <|> char '_' 
idP       = lx ((:) <$> idHeadP <*> many (alphaNum <|> char '_')) 
              <?> "IDENTIFIER"

nameP :: TheParser Name
nameP = (,) <$> getPosition <*> (idP >>= \x -> if Set.member x reservedSet then parserZero else pure x)
-- program
programP :: FilePath -> TheParser LuaProgram
programP path = LuaProgram path <$> (ignored >> chunkP) <* eof
-- block
chunkP :: TheParser Block
chunkP = Block <$> many (statP <* optional (symbol ";")) <*> optionMaybe (laststatP <* optional (symbol ";"))

laststatP :: TheParser LastStatement
laststatP = choice
    [Return <$> getPosition <*> (keyword "return" >> sepBy expP (symbol ",")),
     Break <$> getPosition <* keyword "break"]

blockP = chunkP
-- statements 
statP :: TheParser Statement
statP = choice [
    try assignP, 
    statementCallP, 
    doP,
    whileP, 
    repeatP, 
    ifP,
    try forP, 
    foreachP, 
    try localassignP,
    functiondefP,
    emptyP]

assignP :: TheParser Statement 
assignP = Assign <$> getPosition <*> varlistP <*> (symbol "=" >> explistP)

statementCallP :: TheParser Statement
statementCallP = StatementCall <$> functioncallP

localassignP :: TheParser Statement
localassignP = LocalDef <$> getPosition <*> (keyword "local" >> namelistP) <*> 
    choice [symbol "=" >> explistP, pure []]

doP :: TheParser Statement
doP = Do <$> getPosition <*> (keyword "do" >>  blockP) <* keyword "end"

forP :: TheParser Statement 
forP = For <$> getPosition <*> (keyword "for" >> nameP) <*> 
               (symbol "=" >> expP) <*> 
               (symbol "," >> expP) <*> 
               optionMaybe (symbol "," >> expP) <*> (keyword "do" >> blockP) <* keyword "end"

foreachP :: TheParser Statement
foreachP = ForEach <$> getPosition <*> (keyword "for" >> namelistP) <*> 
                       (keyword "in" >> explistP) <*> 
                       (keyword "do" >> blockP) <* keyword "end"

functiondefP :: TheParser Statement
functiondefP = choice [localFunctiondefP, nonlocalFunctiondefP]

localFunctiondefP :: TheParser Statement
localFunctiondefP = 
    LocalFunctionDef <$> getPosition <*> (keyword "local" >> keyword "function" >> nameP) <*> funcbodyP 

nonlocalFunctiondefP :: TheParser Statement
nonlocalFunctiondefP = getPosition >>= \pos -> 
    keyword "function" >> dotlistP >>= \dots -> choice
        [FunctionDefSelf pos dots <$> (symbol ":" >> nameP) <*> funcbodyP,
         FunctionDef pos dots <$> funcbodyP]

whileP :: TheParser Statement
whileP = While <$> getPosition <*> (keyword "while" >> expP) <* keyword "do" <*> (blockP <* keyword "end")

repeatP :: TheParser Statement
repeatP = Repeat <$> getPosition <*> (keyword "repeat" >> blockP) <*> (keyword "until" >> expP)

ifP = If <$> getPosition <*> (keyword "if"  >> expP) <*> (keyword "then" >> blockP) <*> elseifP
    where 
    elseifP :: TheParser (Maybe Statement)
    elseifP = choice 
            [Nothing <$ keyword "end",
             pure . Just =<< Do <$> getPosition <*> (keyword "else" >> blockP) <* keyword "end",
             pure . Just =<< If <$> getPosition <*> (keyword "elseif" >> expP ) <*> (keyword "then" >> blockP) <*> elseifP] 

emptyP :: TheParser Statement
emptyP = EmptyStatement <$> getPosition <* symbol ";"
-- function body
funcbodyP :: TheParser Function
funcbodyP = 
    getPosition >>= \pos ->
    parens (sepBy (nameP <|> ((,) <$> getPosition <*> dotn 3)) (symbol ",")) >>= \args ->
    if check args 
        then Function pos <$> pure args <*> blockP <* keyword "end" <*> getPosition
        else parserZero
    where 
    check (a1:a2:ls)
      | snd a1 == "..." = False 
      | otherwise = check (a2:ls)
    check _ = True
-- function call and object access
argsP :: TheParser [Exp] 
argsP = getPosition >>= \pos -> choice 
    [parens (sepBy expP (symbol ",")),
     pure <$> Literal pos <$> tableValueP,
     pure <$> Literal pos <$> stringValueP] 

prefixexpP :: TheParser Exp
prefixexpP = accessP >>= either pure (pure . ExpCall)
    
functioncallP :: TheParser FunctionCall
functioncallP = accessP >>= either (const parserZero) pure

varP :: TheParser Exp
varP = accessP >>= either check (const parserZero)
    where
    check (Parens _) = parserZero
    check exp = pure exp

accessP :: TheParser (Either Exp FunctionCall)
accessP = try $ phi =<< choice [pure . Left =<< Parens <$> parens expP, pure . Left =<< Fetch <$> nameP]
     where 
     phi :: (Either Exp FunctionCall) -> TheParser (Either Exp FunctionCall)
     phi exp = 
        getPosition >>= \pos -> 
        let rawExp = either id ExpCall exp in choice 
            [phi =<< choice [try $ pure . Left =<< Dot pos rawExp <$> (dotn 1 >> nameP),
                          pure . Left =<< Index pos rawExp <$> angles expP,
                          pure . Right =<< Call pos rawExp <$> argsP,
                          pure . Right =<< CallSelf pos rawExp <$> (symbol ":" >> nameP) <*> argsP],
             pure exp]
-- listed
dotlistP :: TheParser Exp
dotlistP = 
    getPosition >>= \pos -> 
    sepBy1 nameP (dotn 1) >>= \(first:rest) -> 
    foldM (\ctx next -> Dot <$> getPosition <*> pure ctx <*> pure next) (Fetch first) rest

namelistP :: TheParser [Name]
namelistP = sepBy1 nameP (symbol ",")

explistP :: TheParser [Exp]
explistP = sepBy1 expP (symbol ",")

varlistP :: TheParser [Exp]
varlistP = sepBy1 varP (symbol ",")
-- literals
literalP :: TheParser Exp
literalP = Literal <$> getPosition <*> typedvalueP 

literalFunctionP :: TheParser Exp
literalFunctionP = Literal <$> getPosition <*> (TypedFunction <$> (keyword "function" >> funcbodyP))

typedvalueP :: TheParser TypedValue
typedvalueP = lx $ choice 
    [tableValueP, stringValueP, try intValueP, 
     try varargP, realValueP, try boolValueP, try nilP]

varargP      = TypedVararg <$ lx (string "..." <* notFollowedBy (string "."))
intValueP :: TheParser TypedValue
intValueP = TypedInt . read  <$> choice [
                        try $ (++) <$> (string "0x") <*> many1 alphaNum,
                        many1 digit] <* notFollowedBy (oneOf ".eE")
realValueP :: TheParser TypedValue
realValueP = TypedReal . (read . ((:) '0')) <$> 
                 ((:) <$> oneOf ('.':['0'..'9']) <*> many1 (oneOf $ ".eE" ++ ['0'..'9']))
nilP         = TypedNil <$ keyword "nil"
boolValueP   = choice [TypedBool True <$ keyword "true", TypedBool False <$ keyword "false"]
tableValueP  = TypedTable <$> braces (sepEndBy tablecontents (symbol "," <|> symbol ";"))
    where 
    tablecontents = getPosition >>= \pos -> choice
        [TableItemValueValue pos <$> angles expP <* symbol "=" <*> expP,
         try $ TableItemKeyValue pos  <$> idP <* symbol "=" <*> expP,
         TableItemValue pos <$> expP]
stringValueP = 
    choice [TypedString DoubleQuoted <$> doubleQuoted escapedChars, 
            TypedString SingleQuoted <$> singleQuoted unescapedChars, 
            TypedString MultiLined   <$> multilined escapedChars]
    where
        
    doubleQuoted = between (string "\"") (symbol "\"")
    singleQuoted = between (string "'") (symbol "'")
    multilined p = string "[[" >> manyTill anyChar (try (string "]]")) 
    unescapedChars = 
        many  (try (noneOf "\\\'" <|> escapeBS <|> escapeSQ) <|> try (char '\\' <* notFollowedBy (char '\'')))
    escapedChars   = 
        many  (try (noneOf "\\\"" <|>  escapeBS <|> escapeDQ) <|> try (char '\\' <* notFollowedBy (char '"')))
    escapeBS = try (string "\\\\" >> pure '\\')
    escapeDQ = try (string "\\\"" >> pure '"')
    escapeSQ = try (string "\\\'" >> pure '\'')
-- variable
fetchP :: TheParser Exp
fetchP = Fetch <$> nameP
-- expression
expP :: TheParser Exp
expP = foldr breakBinopP breakUnopP binops <?> "EXPRESSION"
    where 
    breakBinopP (Left sym) p  = 
        foldl (\lhs (pos,rhs) -> Binop pos sym lhs rhs) <$> p <*> (many $ binopP sym p)
    breakBinopP (Right sym) p = 
        p >>= \lhs -> 
        (\(pos,rhs) -> Binop pos sym lhs rhs) <$> binopP sym (breakBinopP (Right sym) p) <|> pure lhs
    binopP (Intrinsic intr) p = (,) <$> getPosition <*> try (intrP intr >> p) 
    breakUnopP  = choice (map unopP unops ++ [terminalP])
    unopP intr@(Intrinsic str)  = getPosition >>= \pos -> 
        (Unop pos intr <$> (intrP str >> unopP intr)) <|> terminalP
    intrP intr 
      | Set.member intr stringOpSet = keyword intr
      | intr == ".."                = dotn 2
      | otherwise                   = symbol intr
    terminalP = choice [try literalP, try literalFunctionP, try prefixexpP] 
-- interface
fromFile :: FilePath -> IO (Either String LuaProgram)
fromFile path = readFile path >>= \src ->
    pure (either (error . show) (Right) (fromString path src))

fromString :: FilePath -> String -> Either ParseError LuaProgram
fromString path = parse (programP path) path
