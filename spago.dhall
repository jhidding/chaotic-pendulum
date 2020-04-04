{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "chaos-pendulum"
, dependencies =
  [ "arrays", "console", "effect", "math", "psci-support", "spork" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
