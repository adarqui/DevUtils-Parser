module System.DevUtils.Misc (
 getLineLoop
) where

getLineLoop :: IO ()
getLineLoop = getLine >> getLineLoop
