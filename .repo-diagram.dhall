let concatSep =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/v20.2.0/Prelude/Text/concatSep.dhall

let map =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/v20.2.0/Prelude/List/map.dhall

let output_file = {- target to write diagram to -} ".repo-diagram.svg"

let exampleOutDirs =
      map
        Text
        Text
        (\(exampleName : Text) -> "examples/${exampleName}/out")
        [ "prevalence-ci-calibration"
        , "simulation-study"
        , "simulation-study-aggregated-observations"
        , "simulation-study-time-dependent-rates"
        , "simulation-study-time-series"
        , "timing-evaluation"
        ]

let extraPDFs =
      map
        Text
        Text
        (\(filename : Text) -> "examples/prevalence-ci-calibration/${filename}")
        [ "timtam-figure-1.pdf"
        , "timtam-figure-s7a.pdf"
        , "timtam-figure-s7b.pdf"
        , "timtam-figure-s8a.pdf"
        , "timtam-figure-s8b.pdf"
        , "timtam-report-template.html"
        , "timtam-report.html"
        ]

let excluded_paths =
    {- files to ignore -}   concatSep
                              ","
                              (   [ "out"
                                  , ".github"
                                  , ".repo-diagram.dhall"
                                  , ".repo-diagram.svg"
                                  , "diagram.svg"
                                  , ".stack-work"
                                  , "Setup.hs"
                                  , "shell.nix"
                                  ]
                                # exampleOutDirs
                                # extraPDFs
                              )

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
