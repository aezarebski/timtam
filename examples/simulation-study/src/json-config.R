library(jsonlite)

OUTPUT_FILE <- "out/config.json"

config <- list(
  outputEventsFile = "out/simulated-events.txt",
  outputObservationsFile = "out/simulated-events-observed.txt",
  outputLlhdFile = "out/simulation-study-llhds.csv",
  simDuration = 6.5,
  simLambda = 1.5,
  simMu = 0.3,
  simPsi = 0.3,
  simRho = 0.15,
  simRhoTime = 3.0,
  simOmega = 0.3,
  simNu = 0.15,
  simNuTime = 4.0
)

write_json(
  x = config,
  path = OUTPUT_FILE,
  auto_unbox = TRUE,
  pretty = TRUE
)
