module PacketParser where

import Bits
import Control.Monad (ap, liftM)
import Packets

newtype Parser e r = Parser ([e] -> [(r, [e])])

parse :: Parser e r -> [e] -> [(r, [e])]
parse (Parser parseFunction) = parseFunction

instance Functor (Parser e) where
  fmap = liftM

instance Applicative (Parser e) where
  pure = return
  (<*>) = ap

instance Monad (Parser e) where
  return r = Parser (\cs -> [(r, cs)])
  p >>= k = Parser (\cs -> concat [parse (k a) cs' | (a, cs') <- parse p cs])

-- | a parser that consume the first element or fails
next :: Parser e e
next = Parser f
  where
    f [] = []
    f (c : cs) = [(c, cs)]

-- | a parser that consume the first n elements or fails
takes :: Int -> Parser e [e]
takes n = Parser f
  where
    f xs
      | length xs >= n = [splitAt n xs]
      | otherwise = []

-- | a parser that checks if there are at least n elements or fails
peeks :: Int -> Parser e [e]
peeks n = Parser f
  where
    f xs
      | length xs >= n = [(take n xs, xs)]
      | otherwise = []

-- | a parser that skips the first n elements or fails
skip :: Int -> Parser e ()
skip n = do _ <- takes n; return ()

-- | a parser that always fails
empty :: Parser e r
empty = Parser (const [])

-- | combine results of two parsers
(+++) :: Parser e r -> Parser e r -> Parser e r
pl +++ pr = Parser (\cs -> parse pl cs ++ parse pr cs)

-- | take first successful parser
(<|>) :: Parser e r -> Parser e r -> Parser e r
pl <|> pr = Parser (take 1 . parse (pl +++ pr))

-- | parse a element satisfying a predicate
satisfy :: (e -> Bool) -> Parser e e
satisfy predicate = do e <- next; if predicate e then return e else empty

-- | parse a specific element
exact :: (Eq e) => e -> Parser e e
exact c = satisfy (== c)

-- | parse a specific element sequence
exacts :: (Eq e) => [e] -> Parser e [e]
exacts [] = return []
exacts (c : cs) = do _ <- exact c; _ <- exacts cs; return (c : cs)

-- | repeat a parser one or more times
many1 :: Parser e r -> Parser e [r]
many1 p = do a <- p; as <- many p; return (a : as)

-- | repeat a parser zero or more times
many :: Parser e r -> Parser e [r]
many p = many1 p <|> return []

-- | continue if the condition is true, otherwise an empty parser is returned
check :: Bool -> Parser e ()
check True = Parser (\cs -> [((), cs)])
check False = empty

-- | continue if the Maybe is defined, otherwise an empty parser is returned
get :: Maybe a -> Parser e a
get (Just a) = Parser (\cs -> [(a, cs)])
get Nothing = empty


packetType :: Int -> Parser Bit Int
packetType n = do bits <- takes 4; check (n == bitsToInt bits); return n

connectReturnCode :: Parser Bit ConnectReturnCode
connectReturnCode = do bits <- takes 8; get (toConnectReturnCode $ bitsToInt bits)

variableLength :: Parser Bit Int
variableLength = do
  c1 <- next
  b1 <- takes 7
  if c1
    then do
      c2 <- next
      b2 <- takes 7
      if c2
        then do
          c3 <- next
          b3 <- takes 7
          if c3
            then do
              c4 <- next
              b4 <- takes 7
              if c4
                then do empty
                else return $ bitsToInt (b4 ++ b3 ++ b2 ++ b1)
            else return $ bitsToInt (b3 ++ b2 ++ b1)
        else return $ bitsToInt (b2 ++ b1)
    else return $ bitsToInt b1
    
checkVariableLength :: Parser Bit Int
checkVariableLength = do l <- variableLength; _ <- peeks l; return l

connackParser :: Parser Bit Packet
connackParser = do
  _ <- packetType 2
  _ <- exacts [zero, zero, zero, zero]
  _ <- checkVariableLength
  _ <- exacts (zeros 7)
  session <- next
  code <- connectReturnCode
  return (Connack session code)
