{-| Taken from http://stackoverflow.com/a/2233551 |-}

module Conc (forkJoin) where

import Control.Concurrent

fork1 :: (a -> IO b) -> a -> IO (MVar b)
fork1 f x =
  do
    cell <- newEmptyMVar
    _    <- forkIO (do { result <- f x; putMVar cell result })
    return cell

fork :: (a -> IO b) -> [a] -> IO [MVar b]
fork f = mapM (fork1 f)

join :: [MVar b] -> IO [b]
join = mapM takeMVar

forkJoin :: (a -> IO b) -> [a] -> IO [b]
forkJoin f xs = (fork f xs) >>= join
