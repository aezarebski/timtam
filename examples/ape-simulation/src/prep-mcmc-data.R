input_csv <- "out/ape-sim-event-times.csv"

sim_events <- read.csv(input_csv)
sim_events <- sim_events[order(sim_events$time), ]

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

mcmc_input <- list(
  mcmcObservations = observations_list,
  mcmcNumSamples = 1e3,
  mcmcSampleCSV= "foo.csv",
  mcmcStepSD  = 1e-3,
  mcmcInit   = c(0.228, 0.048, 0.026),
  mcmcSeed  = c(1,2),
  mcmcParameterisation = "identity-mu1-lambda-psi-noRho-omega-noNu",
  mcmcPrior= "foobar"
)


jsonlite::write_json(
            x = mcmc_input,
            path = "out/simulation-data.json",
            auto_unbox = T
          )

