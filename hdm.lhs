GPLV3.0 or later copyright brmlab.cz contact timothyhobbs@seznam.cz

Also copyright cheater http://cheater.posterous.com/haskell-curses

Copyright 2012.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

HDM is a simple lightweight display manager.  I created it, since I need to be able to start up not just xmonad but other window managers and DE's sometimes.  I didn't want to use a standard display manager, because they are

A: Slow, they start up X once, and then close X and start it up all over again. What a waste!

B: GDM at least, doesn't have good support for multiple X sessions.

This program is as simple as they come. To use, you create a directory:

~/.xinitrc.d

and place various different .xinit files there.  I have 3 files there:

xmonad
gnome
xfce4

The program then creates a very basic .xinitrc file:

 #!/bin/bash
 #This is an hdm xinit file.
 sh ~/.xinitrc.d/current

It then shows a terminal based(Vty) menu with the different files in the ~/.xinitrc.d directory.

When you sellect one of them, it symlinks that file to ~/.xinitrc.d/current, and runs startx.

>module Main where

>import System.Exit
>import System.Environment
>import System.Directory
>import System.Process
>import System.Posix.User
>import Menu

>help :: String
>help =
> "hdm is a ncurses based(ok, I lied! It's vty based!) display manager which uses startx.\n\n\
> \To use it, create a directory: ~/.xinitrc.d/\
> \ and place any number of bash scripts there.  These will\
> \ now work as easilly swapable .xinitrc files.\
> \ To run hdm, either invoke hdm with the name of one of these\
> \\
> \ bash scripts:\n\n\
> \\
> \ $ hdm xmonad\n\n\
> \\
> \ or simply call\n\n\
> \\
> \ $ hdm\n\n\
> \\
> \ for an ncurses based menu.\n\n\
> \ please note! You cannot run hdm if you already have a\
> \ .xinitrc file!\
> \ You must place all files you want to use in the place\
> \ of xinitrc in your ~/.xinitrc.d/ directory!\n\n\
> \ In order to use hdm you must have startx installed."

>xinitrcd :: IO String
>xinitrcd = do 
> home <- getEnv "HOME"
> return $ home ++ "/.xinitrc.d/"

>xinitrc :: IO String
>xinitrc = do 
> home <- getEnv "HOME"
> return $ home ++ "/.xinitrc"

>standardHDMXinit :: String
>standardHDMXinit = "#!/bin/bash\n#This is an hdm xinit file.\nsh ~/.xinitrc.d/current\n"  

>initializeXinitrcFile :: IO ()
>initializeXinitrcFile = do
> xinitrcFile <- xinitrc
> xinitrcExists <- doesFileExist xinitrcFile
> if xinitrcExists
> then do
> xinitrcContents <- readFile xinitrcFile
> if xinitrcContents == standardHDMXinit
> then return ()
> else do
>   putStrLn "You seem to already have your own .xinitrc file."
>   putStrLn "Please move this file to ~/.xinitrc.d and run hdm again."
>   putStrLn help
>   System.Exit.exitWith $ System.Exit.ExitFailure 1
> else do
>  putStrLn "Creating you a ~/.xinitrc file for the first time."
>  writeFile xinitrcFile standardHDMXinit

>main :: IO()
>main = do
> initializeXinitrcFile
> args <- System.Environment.getArgs
> case args of

If the user passes an argument, we check if the argument is a valid session.  If it is, we load it.  Otherwise we screem bloody murder, and then die by printing a help message.

>  (file:_) -> do
>   xinitrcdir <- xinitrcd
>   sessionExists <- doesFileExist (xinitrcdir++file) 
>   case sessionExists of
>    True -> loadSession file
>    False -> do
>     putStrLn $"Bloody murder! The session "++file++" does not exist!!!\n"
>     putStrLn help

If the user doesn't pass any argument,

>  [] -> do

we check if hdm is set up.

>   xinitrcdir <- xinitrcd
>   sessionsDirectoryExists <- doesDirectoryExist xinitrcdir
>   case sessionsDirectoryExists of

If it is, we load the ncurses based session selector.

>    True -> startSessionSellector

Otherwise, we screem bloody murder, and print out a help message.

>    False -> do
>     putStrLn "Bloody murder!  hdm is not set up!!!\n"
>     putStrLn help

>loadSession :: FilePath -> IO()
>loadSession session = do
> xinitrcDir <- xinitrcd
> currentLinkExists <- doesFileExist $ xinitrcDir ++ "current"
> if currentLinkExists
> then removeFile $ xinitrcDir ++ "current"
> else return ()
> lnProc<-runProcess "ln" ["-s",xinitrcDir++session,xinitrcDir++"current"] Nothing Nothing Nothing Nothing Nothing
> waitForProcess lnProc
> putStrLn "Starting x"
> runProcess "startx" [] Nothing Nothing Nothing Nothing Nothing
> return ()

>startSessionSellector :: IO()
>startSessionSellector = do
> xinitrcdir <- xinitrcd
> sessions' <- getDirectoryContents xinitrcdir
> sessions <- return $ filter (\x -> not (((x == ".") || (x == "..")) || (x == "current"))) sessions'
> maybeSession <- displayMenu sessions
> case maybeSession of 
>  Just session -> loadSession session
>  Nothing -> do
>   putStrLn "Goodbye."
