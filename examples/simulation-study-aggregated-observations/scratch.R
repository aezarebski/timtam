

#' 1st March: 72 confirmed cases cumulative
#' 23rd March: 939 deaths cumulative
#' 1st April: 43387 confirmed cases cumulative
#' 23rd April: 24246 deaths cumulative
#' IFR: 100
#' one-in-ten confirmed cases get sequenced
#' average duration of infectiousness at 7 days
#'

total_infections <- (24246 - 939) * 100
total_confirmed_cases <- 43387 - 72

p_obs <- total_confirmed_cases / total_infections

mu <- 1 / 7
omega <- (10/11) * p_obs * mu / (1 - p_obs)

psi <- omega / 10

r_0 <- 3

lambda <- r_0 * (mu + omega + psi)

