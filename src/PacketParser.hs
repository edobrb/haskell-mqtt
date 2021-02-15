{-# LANGUAGE LambdaCase #-}

module PacketParser where

import Control.Monad (ap, liftM)

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

test :: Parser Char String
test = do a <- exacts "Hello"; b <- many1 (exact '!' <|> exact '?'); return (a ++ b)