#!/usr/bin/env runhaskell

module Main where
import System.IO( putStrLn )
import Development.Shake
import Development.Shake.Tools.C

main = shake shakeOptions $ do
    want ["a.out"]
    "hello" *> \out -> do
        let cs = [out++".c"]
        let os = map (++".o") cs
        need os
        mapM (\c -> compile options c x) cs
        link
    "*.c.o" *> \out -> do
        need [dropExtension out]
        
