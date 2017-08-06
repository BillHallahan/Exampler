> import Data.List
> import Data.Maybe
> import System.IO
> import System.IO.Temp
> import System.FilePath
> import System.Directory
> import System.Environment
> import System.Process
> import System.Exit
> import System.Console.GetOpt
> import Control.Exception
> import System.INotify

Flags for the Exampler program.

> data Flag = Recycle deriving (Eq, Show)
> options :: [OptDescr Flag]
> options = [ Option ['r'] ["recycle"] (NoArg Recycle) "Recycles examples previously generated" ]

Watches a file and generates examples for all branches whenever the file changes.

> main :: IO ()
> main = do
>   cliArgs <- getArgs
>   let (opts, args, errs) = getOpt RequireOrder options cliArgs
>   let header = "Usage: Exampler [OPTIONS] file"
>   if null errs
>     then return ()
>     else ioError $ userError $ concat errs ++ usageInfo header options
>   if null args
>     then ioError $ userError $ usageInfo header options
>     else return ()
>   let recycle = elem Recycle opts
>   let file = head args
>   path <- canonicalizePath file
>   inotify <- initINotify
>   wd <- addWatch inotify [Modify, MoveSelf, DeleteSelf] path (handler path recycle)
>   putStrLn "Type \"bye\" to exit..."
>   exampler path recycle
>   waitForBye

Exit when the user types "bye".

> waitForBye :: IO ()
> waitForBye = do
>   input <- getLine
>   if input == "bye" then return () else waitForBye

Responds to file changes by generating new examples.

> handler :: FilePath -> Bool -> Event -> IO ()
> handler path recycle (Modified _ _) = do
>   putStrLn "*** FILE UPDATED ***"
>   exampler path recycle
> handler _ _ (MovedSelf _)     = do
>   error "File deleted"
> handler _ _ DeletedSelf       = do
>   error "File moved"

Represents an HPC report.

> data Report = Report { coveredExpr :: Int
>                      , totalExpr :: Int
>                      , coveredAlt :: Int
>                      , totalAlt :: Int
>                      } deriving (Show)

Generates examples that fully cover the function in the file corresponding to FilePath.
It prints out examples that increase coverage until coverage is complete.
When recycle is true, examples that were generated previously used will be reused as long as the signature of the function is the same other than the return type.
The recycle option does not work since the program doesn't know how to call another Haskell program with the arguments (i.e., if an argument is a function, then no way to write it to a string then read it back in another program).

> exampler :: FilePath -> Bool -> IO ()
> exampler path recycle = do
>   temp <- createTemp path recycle
>   let original = takeBaseName path
>   let program = takeBaseName temp
>   let executable = joinPath [".", program]
> 
>   (e, out, err) <- readProcessWithExitCode "ghc" ["-fhpc", temp] ""
>   case e of
>     ExitSuccess -> return ()
>     ExitFailure _ -> do
>       error err
> 
>   putStrLn "Finding examples"
>   let report = Report 0 1 0 1
>   findExamples executable report 1
>   putStrLn "Coverage complete"

Creates the executable that will generate examples for the function in the file corresponding to FilePath.

> createTemp :: FilePath -> Bool -> IO String
> createTemp path recycle = do
>   contents <- readFile path
>   -- remove byte order mark
>   let func = dropWhile (\c -> c == '\65279') contents
>   let funcName = takeWhile (\c -> c /= ' ') func
>   (temp, h) <- openTempFile dir fileName
>   hPutStrLn h   "import System.Environment"
>   hPutStrLn h   "import Ives.ExampleGen.Gen"
>   hPutStrLn h   ""
>   hPutStrLn h   "main :: IO ()"
>   hPutStrLn h   "main = do"
>   hPutStrLn h   "  (_:size:_) <- getArgs"
>   hPutStrLn h $ "  example <- genExample " ++ funcName ++ " (read size)"
>   hPutStrLn h   "  print example"
>   hPutStrLn h   ""
>   hPutStrLn h   func
>   hClose h
>   return temp
>   where
>     dir = takeDirectory path
>     fileName = takeFileName path

Generate an HPC report for a program.

> genReport :: FilePath -> IO Report
> genReport executable = do
>   report <- readProcess "hpc" ["report", "--include=Main", executable] ""
>   let reportLines = lines report
>   let expressions = reportLines!!0
>   let alternatives = reportLines!!5
>   let coveredExpr = read $ takeWhile (/= '/') $ drop 1 $ dropWhile (/= '(') expressions
>   let totalExpr = read $ takeWhile (/= ')') $ drop 1 $ dropWhile (/= '/') expressions
>   let coveredAlt = read $ takeWhile (/= '/') $ drop 1 $ dropWhile (/= '(') alternatives
>   let totalAlt = read $ takeWhile (/= ')') $ drop 1 $ dropWhile (/= '/') alternatives
>   return $ Report coveredExpr totalExpr coveredAlt totalAlt

Checks if a report has full coverage.
Might make sense to consider a certain percent coverage as being "covered" or to let a few unexplored branches to slide if it's taking too long.

> isCovered :: Report -> Bool
> isCovered (Report ce te ca ta) = ce == te && ca == ta

Checks if the second report is an improvement over the first.

> hasImproved :: Report -> Report -> Bool
> hasImproved (Report cePrev _ caPrev _) (Report ce _ ca _) = ce > cePrev || ca > caPrev

Checks if the provided example improved the report.
If it did, it's printed to standard out.
It's possible that an example that passed this check prior to another example that covers all branches that the earlier example covered as well. In that case, the earlier example is not needed as a representative example, but this case is not accounted for currently.

> checkExample :: FilePath -> Report -> String -> IO Report
> checkExample executable prevReport example = do
>   report <- genReport executable
>   if hasImproved prevReport report
>     then do
>     putStr example
>     else return ()
>   return report

Keeps generating random examples and printing representative ones until the function is fully covered.

> findExamples :: FilePath -> Report -> Int -> IO ()
> findExamples executable prevReport count
>   | isCovered prevReport = do
>       putStrLn $ "Tried " ++ show count ++ " examples total"
>   | otherwise = do
>       example <- readProcess executable ["generate", "10"] ""
>       report <- checkExample executable prevReport example
>       let newCount = count + 1
>       if newCount `mod` 1000 == 0
>         then putStrLn $ "Tried " ++ show newCount ++ " examples so far"
>         else return ()
>       findExamples executable report newCount

Remove all temporary files that are created.

> cleanup :: FilePath -> IO ()
> cleanup program = do
>   removeFile program
>   removeFile $ addExtension program "hs"
>   removeFile $ addExtension program "hi"
>   removeFile $ addExtension program "o"
>   removeFile $ addExtension program "tix"
>   removeDirectoryRecursive ".hpc"