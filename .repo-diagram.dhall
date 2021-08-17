let concatSep =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/v20.2.0/Prelude/Text/concatSep.dhall

let output_file = {- target to write diagram to -} ".repo-diagram.svg"

let excluded_paths = {- files to ignore -} concatSep "," [ "out", ".github" ]

let RepoVisualizerOptions
    : Type
    = { excluded_paths : Text, output_file : Text }

in  { name = "Create diagram"
    , on = { push.branches = [ "master" ], workflow_dispatch = {=} }
    , jobs.get_data
      =
      { runs-on = "ubuntu-latest"
      , steps =
            [ { name = "Checkout code"
              , uses = "actions/checkout@master"
              , `with` = None RepoVisualizerOptions
              }
            , { name = "Update diagram"
              , uses = "githubocto/repo-visualizer@0.5.0"
              , `with` = Some { excluded_paths, output_file }
              }
            ]
          : List
              { name : Text
              , uses : Text
              , `with` : Optional RepoVisualizerOptions
              }
      }
    }
