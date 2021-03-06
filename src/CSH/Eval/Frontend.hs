{-|
Module      : CSH.Eval.Frontend
Description : The top level routing of calls to the web viewer
Copyright   : Stephen Demos, Matt Gambogi, Travis Whitaker, Computer Science House 2015
License     : MIT
Maintainer  : pvals@csh.rit.edu
Stability   : Provisional
Portability : POSIX

Defines the web application layer of Evals.
-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ViewPatterns      #-}

module CSH.Eval.Frontend (
    evalFrontend
  ) where

import CSH.Eval.Config
import CSH.Eval.Cacheable.Prim
import CSH.Eval.Frontend.Data
import CSH.Eval.Frontend.Attendance
import CSH.Eval.Frontend.Members
import CSH.Eval.Frontend.ProfilePhoto
import CSH.Eval.Frontend.Projects
import CSH.Eval.Frontend.Evals
import CSH.Eval.Frontend.Home
import Yesod
import Yesod.Static
import System.Log.Logger
import System.Log.Handler.Simple
import System.Log.Formatter (simpleLogFormatter)

mkYesodDispatch "EvalFrontend" resourcesEvalFrontend

-- | Defines the WAI Application for the eval Yesod app.
evalFrontend :: ServerCmd -> IO Application
evalFrontend config = do
    s <- static "frontend/static"
    updateGlobalLogger rootLoggerName ( setLevel DEBUG
                                      . addHandler stdout
                                      . removeHandler
                                      )
    cacheLogger <- getLogger "csh-eval-cache"
    frontendLogger <-getLogger "csh-eval-frontend"
    cache <- initCacheFromConfig cacheLogger
    toWaiApp $ EvalFrontend s config cache frontendLogger
  where
    stdout = GenericHandler
               DEBUG
               (simpleLogFormatter "$time [$prio#$loggername] $msg")
               ()
               (const putStrLn)
               ((const . pure) ())
