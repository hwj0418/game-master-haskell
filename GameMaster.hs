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

testGame :: (Integer -> FreeGameMaster Ending) -> Bool
testGame = error "TODO"

testPureWin :: (PlayMsg a -> FreeGameMaster Ending) -> Integer -> Maybe Ending
testPureWin game secret = case game (Guess secret) of
    Pure Win -> Just Win
    otherwise -> Nothing

testPureLose :: (PlayMsg a -> FreeGameMaster Ending) -> Integer -> Maybe Ending
testPureLose game secret = case game (Surrender) of
    Pure (Lose n) -> if n == secret then (Just (Lose n)) else Nothing
    otherwise -> Nothing

gmActionChecker input lo hi =
    case input of
        GMAction lo1 hi1 nextState -> if (lo == lo1 && hi1 == hi) then Just (nextState) else Nothing
        otherwise Nothing
