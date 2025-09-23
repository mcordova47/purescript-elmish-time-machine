{ name = "my-project"
, dependencies =
  [ "arrays"
  , "console"
  , "debug"
  , "effect"
  , "elmish"
  , "elmish-html"
  , "foldable-traversable"
  , "functions"
  , "lists"
  , "maybe"
  , "ordered-collections"
  , "prelude"
  , "record"
  , "tuples"
  , "web-dom"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
