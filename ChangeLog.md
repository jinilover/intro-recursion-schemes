Recursion/LICENSE                                                                                   000644  000765  000024  00000002771 13215647152 014675  0                                                                                                    ustar 00amywong                         staff                           000000  000000                                                                                                                                                                         Copyright Author name here (c) 2017

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Author name here nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
       Recursion/README.md                                                                                 000644  000765  000024  00000000014 13215647152 015133  0                                                                                                    ustar 00amywong                         staff                           000000  000000                                                                                                                                                                         # Recursion
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    Recursion/Recursion.cabal                                                                           000644  000765  000024  00000002633 13223671631 016620  0                                                                                                    ustar 00amywong                         staff                           000000  000000                                                                                                                                                                         -- This file has been generated from package.yaml by hpack version 0.17.0.
--
-- see: https://github.com/sol/hpack

name:           Recursion
version:        0.1.0.0
synopsis:       Short description of your package
description:    Please see the README on Github at <https://github.com/githubuser/Recursion#readme>
homepage:       https://github.com/githubuser/Recursion#readme
bug-reports:    https://github.com/githubuser/Recursion/issues
author:         Author name here
maintainer:     example@example.com
copyright:      2017 Author name here
cabal-version:  >= 1.10
build-type:     Simple
license:        BSD3
license-file:   LICENSE

extra-source-files:
    ChangeLog.md
    README.md

source-repository head
  type: git
  location: https://github.com/githubuser/Recursion

library
  hs-source-dirs:
      src
  default-language: Haskell2010
  build-depends:
      base >= 4.7 && < 5
    , pretty
  exposed-modules:
      Intro2.Expr
      Lib

executable Recursion-exe
  main-is: Main.hs
  hs-source-dirs:
      app
  ghc-options: -threaded -rtsopts -with-rtsopts=-N
  build-depends:
      base >= 4.7 && < 5
    , pretty
    , Recursion
  default-language: Haskell2010

test-suite Recursion-test
  type: exitcode-stdio-1.0
  main-is: Spec.hs
  hs-source-dirs:
      test
  ghc-options: -threaded -rtsopts -with-rtsopts=-N
  build-depends:
      base >= 4.7 && < 5
    , pretty
    , Recursion
  default-language: Haskell2010
                                                                                                     Recursion/Setup.hs                                                                                  000644  000765  000024  00000000056 13215647152 015316  0                                                                                                    ustar 00amywong                         staff                           000000  000000                                                                                                                                                                         import Distribution.Simple
main = defaultMain
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  Recursion/app/                                                                                      000755  000765  000024  00000000000 13215647152 014441  5                                                                                                    ustar 00amywong                         staff                           000000  000000                                                                                                                                                                         Recursion/app/Main.hs                                                                               000644  000765  000024  00000002613 13224404114 015650  0                                                                                                    ustar 00amywong                         staff                           000000  000000                                                                                                                                                                         -- {-# LANGUAGE DeriveFunctor #-}
--
-- module Main where
--
-- import Lib
-- import Control.Arrow
--
-- main :: IO ()
-- main = someFunc
--
-- data Lit
--   = StrLit String
--   | IntLit Int
--   | Ident String
--   deriving (Show, Eq)
--
-- data Expr a
--   = Index a a
--   | Call [a]
--   | Unary String a
--   | Binary a String a
--   | Paren a
--   | Literal Lit
--   deriving (Show, Eq, Functor)
--
-- newtype Term f = In (f (Term f))
--
-- out :: Term f -> f (Term f)
-- out (In t) = t
--
-- bottomUp :: Functor a => (Term a -> Term a) -> Term a -> Term a
-- bottomUp fn =
--   out                    -- 1) unpack
--   >>> fmap (bottomUp fn) -- 2) recurse
--   >>> In                 -- 3) repack
--   >>> fn                 -- 4) apply
--
-- topDown'' :: Functor a => (Term a -> Term a) -> Term a -> Term a
-- topDown'' fn =
--   In
--   <<< fmap (topDown fn)
--   <<< out
--   <<< fn
--
-- topDown :: Functor a => (Term a -> Term a) -> Term a -> Term a
-- topDown fn =
--   fn                    -- 1) apply
--   >>> out               -- 2) unpack
--   >>> fmap (topDown fn) -- 3) recurse
--   >>> In                -- 4) repack
--
-- topDown' :: Functor a => (Term a -> Term a) -> Term a -> Term a
-- topDown' fn =
--   In . fmap (topDown fn) . out . fn
--
-- fn :: Term a -> Term a
-- fn = undefined
--
-- flattenTerm :: Term Expr -> Term Expr
-- flattenTerm (In (Paren a)) = a
-- flattenTerm other = other
                                                                                                                     Recursion/package.yaml                                                                              000644  000765  000024  00000002247 13223671621 016142  0                                                                                                    ustar 00amywong                         staff                           000000  000000                                                                                                                                                                         name:                Recursion
version:             0.1.0.0
github:              "githubuser/Recursion"
license:             BSD3
author:              "Author name here"
maintainer:          "example@example.com"
copyright:           "2017 Author name here"

extra-source-files:
- README.md
- ChangeLog.md

# Metadata used when publishing your package
synopsis:            Short description of your package
# category:            Web

# To avoid duplicated efforts in documentation and dealing with the
# complications of embedding Haddock markup inside cabal files, it is
# common to point users to the README.md file.
description:         Please see the README on Github at <https://github.com/githubuser/Recursion#readme>

dependencies:
- base >= 4.7 && < 5
- pretty

library:
  source-dirs: src

executables:
  Recursion-exe:
    main:                Main.hs
    source-dirs:         app
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - Recursion

tests:
  Recursion-test:
    main:                Spec.hs
    source-dirs:         test
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - Recursion
                                                                                                                                                                                                                                                                                                                                                         Recursion/src/                                                                                      000755  000765  000024  00000000000 13221443773 014450  5                                                                                                    ustar 00amywong                         staff                           000000  000000                                                                                                                                                                         Recursion/src/Intro2/                                                                               000755  000765  000024  00000000000 13221444063 015616  5                                                                                                    ustar 00amywong                         staff                           000000  000000                                                                                                                                                                         Recursion/src/Lib.hs                                                                                000644  000765  000024  00000000130 13215647152 015504  0                                                                                                    ustar 00amywong                         staff                           000000  000000                                                                                                                                                                         module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"
                                                                                                                                                                                                                                                                                                                                                                                                                                        Recursion/src/Intro2/Expr.hs                                                                        000644  000765  000024  00000005755 13225712130 017101  0                                                                                                    ustar 00amywong                         staff                           000000  000000                                                                                                                                                                         {-# LANGUAGE DeriveFunctor #-}

module Intro2.Expr where

import Control.Arrow
import Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as P
import Data.Monoid
import Data.Function

data Expr a
    = Literal { intVal :: Int }
    | Ident   { name :: String  }
    | Index   { target :: a, idx :: a }
    | Unary   { op :: String, target :: a }
    | Binary  { lhs :: a, op :: String, rhs :: a }
    | Call    { func :: a, args :: [a] }
    | Paren   { target :: a }
    deriving (Show, Eq, Functor)

newtype Term f = In' { out' :: f (Term f) }

-- instance Functor Expr where
--   fmap _ (Literal i) = Literal i
--   fmap _ (Ident n) = Ident n
--   fmap f (Index t i) = Index (f t) (f i)
--   fmap f (Unary o t) = Unary o (f t)
--   fmap f (Binary l o r) = Binary (f l) o (f r)
--   fmap f (Call func args) = Call (f func) (map f args)
--   fmap f (Paren t) = Paren (f t)

bottomUp' :: Functor f => (f c -> c) -> Term f -> c
bottomUp' fn' =
  out'                      -- 1) unpack a `Term a` into an `a (Term a)`
  >>> fmap (bottomUp' fn')  -- 2) recurse, with fn, into the subterms
  >>> fn'                   -- 4) finally, apply fn to the packed `Term a`

bottomUp :: Functor f => (Term f -> Term f) -> Term f -> Term f
bottomUp fn =
  out'                    -- 1) unpack
  >>> fmap (bottomUp fn) -- 2) recurse
  >>> In'                 -- 3) repack
  >>> fn                 -- 4) apply

type Algebra f a = f a -> a

cata :: Functor f => Algebra f a -> Term f -> a
cata fn = out' >>> fmap (cata fn) >>> fn

type Coalgebra f a = a -> f a

ana :: Functor f => Coalgebra f a -> a -> Term f
ana fn = In' <<< fmap (ana fn) <<< fn

type RAlgebra f a = f (Term f, a) -> a

para :: Functor f => RAlgebra f a -> Term f -> a
para fn = out' >>> fmap (id &&& para fn) >>> fn

type RAlgebra' f a = Term f -> f a -> a

para'' :: Functor f => RAlgebra' f a -> Term f -> a
para'' fn term = out' term & fmap (para'' fn) & fn term

cata' :: Functor f => Algebra f a -> Term f -> a
cata' fn = para'' (const fn)

type RCoalgebra f a = a -> f (Either (Term f) a)

apo :: Functor f => RCoalgebra f a -> a -> Term f
apo fn = In' <<< fmap (id ||| apo fn) <<< fn

-- para'' :: (Functor f) => RAlgebra' f a -> Term f -> a
-- para'' fn = out' >>> fmap undefined >>> fn

ten  = In' Literal { intVal = 10 }
add  = In' Ident { name = "add" }
call = In' Call { func = add, args = [ten, ten] }

countNodes :: Expr Int -> Int
countNodes Literal {} = 1
countNodes Ident {} = 1
countNodes (Index target idx) = target + idx + 1
countNodes (Unary _ target) = target + 1
countNodes (Binary lhs _ rhs) = lhs + 1 + rhs
countNodes (Call func args) = func + sum args + 1
countNodes (Paren target) = target + 1

prettyPrint :: Algebra Expr Doc
prettyPrint (Literal i) = P.int i
prettyPrint (Ident s) = P.text s
prettyPrint (Call f as)     = f <> P.parens (P.cat $ P.punctuate (P.text ", ") as)
prettyPrint (Index it idx)  = it <> P.brackets idx
prettyPrint (Unary op it)   = P.text op <> it
prettyPrint (Binary l op r) = l <> P.text op <> r
prettyPrint (Paren exp)     = P.parens exp
                   Recursion/stack.yaml                                                                                000644  000765  000024  00000004175 13215650152 015653  0                                                                                                    ustar 00amywong                         staff                           000000  000000                                                                                                                                                                         # This file was automatically generated by 'stack init'
#
# Some commonly used options have been documented as comments in this file.
# For advanced use and comprehensive documentation of the format, please see:
# http://docs.haskellstack.org/en/stable/yaml_configuration/

# Resolver to choose a 'specific' stackage snapshot or a compiler version.
# A snapshot resolver dictates the compiler version and the set of packages
# to be used for project dependencies. For example:
#
# resolver: lts-3.5
# resolver: nightly-2015-09-21
# resolver: ghc-7.10.2
# resolver: ghcjs-0.1.0_ghc-7.10.2
# resolver:
#  name: custom-snapshot
#  location: "./custom-snapshot.yaml"
resolver: lts-9.3

# User packages to be built.
# Various formats can be used as shown in the example below.
#
# packages:
# - some-directory
# - https://example.com/foo/bar/baz-0.0.2.tar.gz
# - location:
#    git: https://github.com/commercialhaskell/stack.git
#    commit: e7b331f14bcffb8367cd58fbfc8b40ec7642100a
# - location: https://github.com/commercialhaskell/stack/commit/e7b331f14bcffb8367cd58fbfc8b40ec7642100a
#   extra-dep: true
#  subdirs:
#  - auto-update
#  - wai
#
# A package marked 'extra-dep: true' will only be built if demanded by a
# non-dependency (i.e. a user package), and its test suites and benchmarks
# will not be run. This is useful for tweaking upstream packages.
packages:
- '.'
# Dependency packages to be pulled from upstream that are not in the resolver
# (e.g., acme-missiles-0.3)
extra-deps: []

# Override default flag values for local packages and extra-deps
flags: {}

# Extra package databases containing global packages
extra-package-dbs: []

# Control whether we use the GHC we find on the path
# system-ghc: true
#
# Require a specific version of stack, using version ranges
# require-stack-version: -any # Default
# require-stack-version: ">=1.4"
#
# Override the architecture used by stack, especially useful on Windows
# arch: i386
# arch: x86_64
#
# Extra directories used by stack for building
# extra-include-dirs: [/path/to/dir]
# extra-lib-dirs: [/path/to/dir]
#
# Allow a newer minor version of GHC than the snapshot specifies
# compiler-check: newer-minor
                                                                                                                                                                                                                                                                                                                                                                                                   Recursion/test/                                                                                     000755  000765  000024  00000000000 13215647152 014640  5                                                                                                    ustar 00amywong                         staff                           000000  000000                                                                                                                                                                         Recursion/test/Spec.hs                                                                              000644  000765  000024  00000000077 13215647152 016072  0                                                                                                    ustar 00amywong                         staff                           000000  000000                                                                                                                                                                         main :: IO ()
main = putStrLn "Test suite not yet implemented"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 