{-|
Module      : CSH.Eval.DB.TemplateQ
Description : QuasiQuoter for Hasql Query Templates
Copyright   : Stephen Demos, Matt Gambogi, Travis Whitaker, Computer Science House 2015
License     : MIT
Maintainer  : pvals@csh.rit.edu
Stability   : Provisional
Portability : POSIX

A QuasiQuoter for multi-line SQL query templates. Right now this is stupid
simple. We might later implement some additional checks.
-}

{-# LANGUAGE TemplateHaskell #-}

module CSH.Eval.DB.TemplateQ (tempq) where

import GHC.Exts (IsString(..))

import Language.Haskell.TH
import Language.Haskell.TH.Quote

-- | QuasiQuoter for multi-line SQL query templates. Right now this simply
--   produces something of @IsString a => a@. We might add more checks later.
tempq :: QuasiQuoter
tempq = QuasiQuoter ((\a -> [|fromString a|] . filter (/= '\r'))
                    (error "QQ 'tempq' not defined in pattern context.")
                    (error "QQ 'tempq' not defined in type context.")
                    (error "QQ 'tempq' not defined in declaration context.")
