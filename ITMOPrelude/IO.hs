{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.IO where

import ITMOPrelude.Primitive
import ITMOPrelude.List
import ITMOPrelude.Categories

import Prelude (Show,Read)

data RealWorld = RealWorld
    { stdIn :: List Nat
    , stdOut :: List Nat
    , exitCode :: Nat } deriving (Show)

type IO a = State RealWorld a

getNat :: IO Nat
getNat = State $ \st -> case st of
  RealWorld (Cons x input) output code ->
    (RealWorld input output code, x)
  RealWorld Nil output code ->
    undefined

putNat :: Nat -> IO ()
putNat x = State $ \st -> case st of
  RealWorld input output code ->
    (RealWorld (Cons x input) output code, ())

setExitCode :: Nat -> IO ()
setExitCode nc = State $ \st -> case st of
  RealWorld input output _ ->
    (RealWorld input output nc, ())


