
params <- jsonlite::read_json("../example-parameters.json")

input_csv <- "out/ape-sim-event-times.csv"

sim_events <- read.csv(input_csv)
sim_events <- sim_events[order(sim_events$time), ]

if (is.element("rho", sim_events$event)) {
  maybe_sim_dur <- diff(range(sim_events$time))
} else {
  maybe_sim_dur <- NULL
}

num_rho_sampled <- sum(sim_events$event == "rho")
## we only support a single rho event at the moment
rho_times <- unique(sim_events[sim_events$event == "rho", "time"])
stopifnot(length(rho_times) <= 1)
sim_events <- sim_events[sim_events$event != "rho", ]

last_non_rho_time <- max(sim_events$time)
delays <- diff(sim_events$time)
events <- tail(sim_events$event, -1)

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

observations_list <- purrr::map2(delays, events, observation)

if (length(rho_times) == 1) {
  rho_sample <- list(rho_times[1] - last_non_rho_time,
                     list(tag = "OCatastrophe",
                          contents = num_rho_sampled))
  observations_list <- c(observations_list, list(rho_sample))
}

mcmc_input <- list(
  mcmcObservations = observations_list,
  mcmcNumSamples = 1e6,
  mcmcSampleCSV= "out/mcmc-samples.csv",
  mcmcStepSD  = 1e-3,
  ## mcmcInit   = c(0.228, 0.048, 0.5, 0.026),
  mcmcInit   = c(0.228, 0.048, 0.026),
  mcmcSeed  = c(1, 2),
  ## mcmcParameterisation = "identity-muKnown-lambda-psi-rhoAtDuration-omega-noNu",
  mcmcParameterisation = "identity-muKnown-lambda-psi-noRho-omega-noNu",
  mcmcKnownMu = params$deathRate,
  ## mcmcSimDuration = maybe_sim_dur,
  mcmcPrior= "foobar"
)


jsonlite::write_json(
            x = mcmc_input,
            path = "out/simulation-data.json",
            auto_unbox = T
          )

