
suppressPackageStartupMessages(library(argparse))

parser <- ArgumentParser()

parser$add_argument(
         "-v",
         "--verbose",
         action = "store_true",
         default = FALSE,
         help = "Verbose output"
       )
parser$add_argument(
         "-s",
         "--seed",
         type = "integer",
         default = 1,
         help = "PRNG seed"
       )
parser$add_argument(
         "-i",
         "--input",
         type = "character",
         help = "Filepath for input data"
       )
parser$add_argument(
         "-o",
         "--output",
         type = "character",
         help = "Filepath for output data"
       )
parser$add_argument(
         "--unscheduled-data",
         action = "store_true",
         default = FALSE,
         help = "Do not modify the unscheduled data"
       )
parser$add_argument(
         "--aggregate-sequenced-from-to-by",
         type = "double",
         help = "Aggregate sequenced samples using these three values to construct a mesh"
       )
parser$add_argument(
         "--aggregate-unsequenced-from-to-by",
         type = "double",
         help = "Aggregate unsequenced samples using these three values to construct a mesh"
       )

observation <- function(d, e) {
  list(
    d,
    list(
      tag = switch(
        e,
        birth = "OBirth",
        occurrence = "OOccurrence",
        sampling = "ObsUnscheduledSequenced"
      )
    )
  )
}


main <- function(args) {
  if (args$verbose) {
    cat("reading parameters from", args$parameters, "\n")
  }
  set.seed(args$seed)
  if (file.exists(args$input)) {
    ## the simulation events are saved in order of type rather than time so we
    ## need to re-order them.
    sim_events <- read.csv(args$input)
    sim_events <- sim_events[order(sim_events$time), ]
    stopifnot(all(sim_events$events != "rho"))

    delays <- diff(sim_events$time)
    events <- tail(sim_events$event, -1)
  } else {
    stop("Could not find given input file: ", args$input)
  }

  true_params <- jsonlite::read_json("../example-parameters.json")

  observations_list <- purrr::map2(delays, events, observation)

  mcmc_init <- c(0.228, 0.048, 0.026)
  mcmc_parameterisation <- "identity-muKnown-lambda-psi-noRho-omega-noNu"

  mcmc_input <- list(
    mcmcObservations = observations_list,
    mcmcNumSamples = 2e5,
    mcmcSampleCSV= paste0(c(dirname(args$output), "posterior-samples.csv"), collapse = "/"),
    mcmcRecordFinalPrevalence = TRUE,
    mcmcStepSD  = 1e-3,
    mcmcInit   = mcmc_init,
    mcmcSeed  = c(1, 2),
    mcmcParameterisation = mcmc_parameterisation,
    mcmcKnownMu = true_params$deathRate,
    mcmcPrior = "foobar"
  )

  jsonlite::write_json(
              x = mcmc_input,
              path = args$output,
              auto_unbox = T,
              digits = 16,
              pretty = TRUE,
              )
}

if (!interactive()) {
  args <- parser$parse_args()
  main(args)
}
