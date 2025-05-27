#' Lotka-Volterra Model with Carrying Capacity and Hunting
#'
#' Function computes the rate of change of prey and predator populations in a predator-prey interaction,
#' including logistic growth with carrying capacity and prey hunting.
#'
#' @param t Time (days)
#' @param pop Numeric vector with initial conditions: \code{prey} = number of prey, \code{pred} = number of predators
#' @param pars datatype list  coefficient in Lotka-Voltera pars$rprey,pars$K, pars$alpha, pars$eff, par$pmort
#' pars$hunt_rate, pars$min_prey
#' \describe{
#'   \emph{rprey}{Growth rate of prey population}
#'   \emph{K}{Carrying capacity of the prey population}
#'   \emph{alpha}{Interaction coefficient (higher values mean stronger interaction)}
#'   \emph{eff}{Efficiency or rate of ingestion of prey by predators}
#'   \emph{pmort}{Mortality rate of predator population}
#'   \emph{hunt_rate}{Rate at which prey are hunted (harvested)}
#'   \emph{min_prey}{Minimum prey population threshold before hunting is allowed}
#' }
#'
#' @examples
#' pars <- list(rprey = 0.5, K = 100, alpha = 0.3, eff = 0.2, pmort = 0.2, hunt_rate = 0.1, min_prey = 5)
#' currpop <- c(prey = 10, pred = 1)
#' days <- seq(1, 20)
#' res <- ode(func = lotvmodK_hunt, y = currpop, times = days, parms = pars)
#'
#' @return A list containing:
#' \describe{
#'   \item{dprey}{Rate of change of prey population}
#'   \item{dpred}{Rate of change of predator population}
#' }



# Define the Lotka-Volterra model with carrying capacity and hunting
lotvmodK_hunt <- function(t, pop, pars) {
  with(as.list(c(pars, pop)), {
    # Time-dependent hunting: active for first 25 days of every 50-day cycle
    seasonal_hunt_rate <- ifelse((t %% 50) < 25, hunt_rate, 0)
    
    # Only hunt if prey population is above the threshold
    hunting_rate <- ifelse(prey > min_prey, seasonal_hunt_rate, 0)
    
    # Hunting amount is proportionate to prey but cannot exceed prey population
    hunt_amount <- min(hunting_rate * prey, prey)
    
    dprey <- rprey * (1 - prey / K) * prey - alpha * prey * pred - hunt_amount
    dpred <- eff * alpha * prey * pred - pmort * pred
    
    return(list(c(dprey, dpred)))
  })
}
