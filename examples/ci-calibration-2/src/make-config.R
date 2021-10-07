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
         "--parameterisation",
         type = "character",
         help = "Parameterisation of the model"
       )
parser$add_argument(
         "--num-mcmc-samples",
         type = "integer",
         help = "The number of posterior samples to generate via MCMC."
       )
parser$add_argument(
         "--burn",
         type = "integer",
         help = "The number of MCMC samples to discard from the start of the chain."
       )
parser$add_argument(
         "--thin",
         type = "integer",
         help = "The factor to thin the MCMC samples by."
       )

fix_from_to_by <- function(from_to_by_string) {
  if (from_to_by_string != "") {
    tmp <- as.numeric(unlist(strsplit(from_to_by_string, split = " ")))
    return(seq(from = tmp[1], to = tmp[2], by = tmp[3]))
  } else {
    return(NULL)
  }
}

observation <- function(d, e, s) {
  switch(
    e,
    birth = list(d, list(tag = "OBirth")),
    occurrence = list(d, list(tag = "OOccurrence")),
    sampling = list(d, list(tag = "ObsUnscheduledSequenced")),
    rho = list(d, list(tag = "OCatastrophe", contents = s)),
    nu = list(d, list(tag = "ODisaster", contents = s))
  )
}

## Extension of purrr::map2 to handle three arguments.
map3 <- function(xs, ys, zs, f) {
  lapply(seq_along(xs), function(ix) f(xs[ix], ys[ix], zs[ix]))
}

main <- function(args) {
  set.seed(args$seed)
  if (file.exists(args$input)) {
    is_aggregated <- grepl(pattern = "aggregated", x = args$input)
    ## the simulation events are saved in order of type rather than time so we
    ## need to re-order them.
    sim_events <- read.csv(args$input)
    sim_events <- sim_events[order(sim_events$time), ]
    ## Because the MCMC works on the sums of delays we need to adjust for the
    ## TMRCA offset when we compute the rho and nu times from the simulation
    ## events.
    tmrca_offset <- sim_events[sim_events$event == "origin", ]$time
    rho_times <- sim_events[sim_events$event == "rho", ]$time - tmrca_offset
    nu_times <- sim_events[sim_events$event == "nu", ]$time - tmrca_offset
    delays <- diff(sim_events$time)
    events <- tail(sim_events$event, -1)
    if (is.element("size", names(sim_events))) {
      sizes <- tail(sim_events$size, -1)
    } else {
      sizes <- rep(NA, length(events))
    }
  } else {
    stop("Could not find given input file: ", args$input)
  }

  true_params <- jsonlite::read_json("../example-parameters.json")

  observations_list <- map3(delays, events, sizes, observation)

  ## TODO Set this up so that it generates a random initial condition sensibly...
  mcmc_init <- c(0.228, 0.048, 0.026)

  mcmc_input <- list(
    mcmcObservations = observations_list,
    mcmcNumSamples = args$num_mcmc_samples,
    mcmcBurn = args$burn,
    mcmcThinFactor = args$thin,
    mcmcSampleCSV= paste0(
      c(dirname(args$output),
        ifelse(is_aggregated, 
               "aggregated-posterior-samples.csv",
               "posterior-samples.csv")
        ),
      collapse = "/"
    ),
    mcmcRecordFinalPrevalence = TRUE,
    mcmcStepSD  = 1e-3,
    mcmcInit   = mcmc_init,
    mcmcSeed  = c(1, 2),
    mcmcParameterisation = args$parameterisation,
    mcmcKnownMu = true_params$deathRate,
    mcmcRhoTimes = rho_times,
    mcmcNuTimes = nu_times,
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
} else {
  args <- list(
    verbose = TRUE,
    seed = 1,
    input = "out/ape-sim-aggregated-event-times.csv",
    output = "out/aggregated-data/mcmc-app-config.json",
    parameterisation = "identity-muKnown-lambda-psi-noRho-omega-noNu",
    num_mcmc_samples = 10100,
    burn = 100,
    thin = 10
  )
  main(args)
}
