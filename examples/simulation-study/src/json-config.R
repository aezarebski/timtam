library(jsonlite)

OUTPUT_FILE <- "out/config.json"

config <- list(
  outputEventsFile = "out/simulated-events.txt",
  outputEventsCsv = "out/simulated-events.csv",
  outputObservationsFile = "out/simulated-events-observed.txt",
  outputLlhdFile = "out/simulation-study-llhds.csv",
  simDuration = 16.0,
  simLambda = 1.5,
  simMu = 0.3,
  simPsi = 0.3,
  simRho = 0.25,
  simRhoTimes = seq(from = 3.5, to = 14.5, by = 1),
  simOmega = 0.3,
  simNu = 0.25,
  simNuTimes = seq(from = 3.0, to = 14.0, by = 1)
)

write_json(
  x = config,
  path = OUTPUT_FILE,
  auto_unbox = TRUE,
  pretty = TRUE
)
