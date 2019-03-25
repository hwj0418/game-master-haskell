module GameMaster where

import Control.Monad (ap, liftM)

import GameMasterDef

-- Question 1.

-- The game master for the guessing game.  The parameter is the secret number
-- the player has to guess.
guessingGame :: MonadGameMaster m => (Integer -> m Ending)
guessingGame secret
    | secret < 1 || secret > 100 = error "invalid game"
    | otherwise = guessingHint secret 1 100

guessingHint :: MonadGameMaster m => (Integer -> Integer -> Integer -> m Ending)
guessingHint secret lb ub = do
    req <- gmAction lb ub
    case req of
        Surrender -> return (Lose secret) 
        Guess i 
            | (i == secret) -> return Win
            | (i < lb || i > ub) -> guessingHint secret lb ub
            | otherwise -> case i < secret of
                True -> guessingHint secret (i + 1) ub
                False -> guessingHint secret lb (i - 1)


-- Question 2.

-- data FreeGameMaster a
--     = Pure a
--     | GMAction Integer Integer (PlayerMsg -> FreeGameMaster a)

instance Functor FreeGameMaster where
    -- fmap :: (a -> b) -> FreeGameMaster a -> FreeGameMaster b
    -- fmap f fgm = do
    --     a <- fgm
    --     pure (f a) 
    -- If you are confident with your Monad instance, you can just write
    fmap = liftM

instance Applicative FreeGameMaster where
    -- pure :: a -> FreeGameMaster a
    -- pure a = Pure a
    -- If you are confident with your Monad instance, you can just write
    pure = return

    -- (<*>) :: FreeGameMaster (a -> b) -> FreeGameMaster a -> FreeGameMaster b
    -- func <*> fgm = do
    --     f <- func
    --     a <- fgm 
    --     pure (f a)
    -- If you are confident with your Monad instance, you can just write
    (<*>) = ap

instance Monad FreeGameMaster where
    -- return :: a -> FreeGameMaster a
    return a = Pure a
    -- (>>=) :: FreeGameMaster a -> (a -> FreeGameMaster b) -> (FreeGameMaster b)
    Pure a >>= f = f a
    GMAction lb ub req >>= f = 
        GMAction lb ub (\msg -> do
            a <- req msg
            f a)

instance MonadGameMaster FreeGameMaster where
    -- gmAction :: Integer -> Integer -> FreeGameMaster PlayerMsg
    gmAction lb ub = GMAction lb ub (\req -> return req)

-- Question 3.

testPureWin :: Maybe (FreeGameMaster Ending) -> Maybe Ending
testPureWin action =
    case action of
        Just (Pure Win) -> Just (Win)
        otherwise -> Nothing

testPureLose :: Maybe (FreeGameMaster Ending) -> Integer -> Maybe Ending
testPureLose action secret =
    case action of
        Just (Pure (Lose n)) -> if (n == secret) then Just (Lose n) else Nothing
        otherwise -> Nothing

testGMAction :: Maybe (FreeGameMaster Ending) -> Integer -> Integer -> Maybe (PlayerMsg -> FreeGameMaster Ending)
testGMAction action lo hi =
    case action of
        Just (GMAction lo1 hi1 nextState) -> if (lo == lo1 && hi == hi1) then Just (nextState) else Nothing
        otherwise -> Nothing
