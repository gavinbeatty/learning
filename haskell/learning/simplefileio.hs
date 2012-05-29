#!/usr/bin/env runhaskell

{-
/***************************************************************************
 *   Copyright (C) 2007 by Gavin Beatty                                    *
 *   gavinbeatty@gmail.com                                                 *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
-}


import IO

doAskLoop :: IO ()
doAskLoop = do putStrLn "Would you like to [read] a file, [write] a file or [quit]?"
               command <- getLine
               case command of
                    "read"  -> do putStrLn "What filename would you like to read?"
                                  filename <- getLine
                                  doRead filename
                                  doAskLoop
                    "write" -> do putStrLn "What filename would you like to write to?"
                                  filename <- getLine
                                  putStrLn "Enter text and terminate with a '.' on its own line."
                                  bracket (openFile filename WriteMode) hClose doWrite
                                  doAskLoop
                    "quit"  -> return ()
                    _       -> do putStrLn "Try enter the command again."
                                  doAskLoop

doRead :: FilePath -> IO ()
doRead filename = bracket (openFile filename ReadMode) hClose (\h -> do contents <- hGetContents h
                                                                        putStrLn contents)
doWrite :: Handle -> IO ()
doWrite handle = do line <- getLine
                    case line of
                         "." -> putStrLn "Finished writing to file."
                         _   -> do hPutStrLn handle line
                                   doWrite handle

main :: IO ()
main = do doAskLoop

