module MonadicParser where

import Control.Monad (ap, liftM)
import Data.Either

newtype Parser e r = Parser ([e] -> [Either (r, [e]) String])

parse :: Parser e r -> [e] -> [Either (r, [e]) String]
parse (Parser parseFunction) = parseFunction

instance Functor (Parser e) where
  fmap = liftM

instance Applicative (Parser e) where
  pure = return
  (<*>) = ap

instance Monad (Parser e) where
  return r = Parser (\cs -> [Left (r, cs)])
  p >>= k = Parser parser
    where
      parser cs
        | all isRight res = res
        | otherwise = filter isLeft res
        where
          res = concatMap f (parse p cs)
          f (Left (a, cs')) = parse (k a) cs'
          f (Right err) = [Right err]

-- | a parser that consume the first element or fails
next :: Parser e e
next = Parser f
  where
    f [] = [Right "EOF"]
    f (c : cs) = [Left (c, cs)]

-- | a parser that consume the first n elements or fails
takes :: Int -> Parser e [e]
takes n = Parser f
  where
    f xs
      | length xs >= n = [Left (splitAt n xs)]
      | otherwise = [Right "EOF"]

-- | a parser that consume all the input and returns it
takeAll :: Parser e [e]
takeAll = Parser (\cs -> [Left (cs, [])])

-- | a parser that checks if there are at least n elements or fails
peeks :: Int -> Parser e [e]
peeks n = Parser f
  where
    f xs
      | length xs >= n = [Left (take n xs, xs)]
      | otherwise = [Right "EOF"]

-- | a parser that skips the first n elements or fails
skip :: Int -> Parser e ()
skip n = do _ <- takes n; return ()

-- | a parser that always fails
empty :: Parser e r
empty = Parser (const [Right "Failure"])

-- | a parser which is always successful and consume no input
continue :: Parser e ()
continue = Parser (\cs -> [Left ((), cs)])

-- | a parser which is always successful and consume no input
continueWith :: r -> Parser e r
continueWith result = Parser (\cs -> [Left (result, cs)])

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
check True = continue
check False = empty

-- | continue if the Maybe is defined, otherwise an empty parser is returned
get :: Maybe a -> Parser e a
get (Just a) = Parser (\cs -> [Left (a, cs)])
get Nothing = empty

-- | check if the input is empty
isAtEOF :: Parser e ()
isAtEOF = Parser f
  where
    f [] = [Left ((), [])]
    f _ = [Right "expected EOF"]

-- | The result of the first parser is given as input to the second. Only the first parser consume the original input.
(>->) :: Parser e [e] -> Parser e r -> Parser e r
(>->) first second = Parser f
  where
    f bits
      | null results = []
      | otherwise = map Left $ zip (map fst $ concatMap (lefts . parse second . fst) results) (map snd results)
      where
        results = lefts $ parse first bits
        

