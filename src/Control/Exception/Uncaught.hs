-- SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Customize uncaught exception handler.

module Control.Exception.Uncaught
  ( f
  ) where

f :: IO ()
f = putStrLn "Hello world!"
