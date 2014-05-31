module System.DevUtils.Launch (
 Launch(..),
 defaultLaunch
) where

data Launch = Launch {
 _argv :: [String],
 _usage :: String,
 _run :: [String] -> IO ()
}

defaultArgv = []
defaultUsage = "No usage."
defaultRun argv = do return ()

defaultLaunch :: Launch
defaultLaunch = Launch {
 _argv = defaultArgv,
 _usage = defaultUsage,
 _run = defaultRun
 }
