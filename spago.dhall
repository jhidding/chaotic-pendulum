{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "chaos-pendulum"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "flare"
  , "math"
  , "numbers"
  , "psci-support"
  , "record"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
