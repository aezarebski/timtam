library(jsonlite)

OUTPUT_FILE <- "out/config.json"

config <- list(
  outputEventsFile = "out/simulated-events.txt",
  outputEventsCsv = "out/simulated-events.csv",
  outputObservationsFile = "out/simulated-events-observed.txt",
  outputLlhdFile = "out/simulation-study-llhds.csv",
  simDuration = 7.0,
  simLambda = 1.5,
  simMu = 0.3,
  simPsi = 0.3,
  simRho = 0.2,
  simRhoTimes = c(3.5,4.5,5.5),
  simOmega = 0.3,
  simNu = 0.2,
  simNuTime = 6.0
)

write_json(
  x = config,
  path = OUTPUT_FILE,
  auto_unbox = TRUE,
  pretty = TRUE
)
