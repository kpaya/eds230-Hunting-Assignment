#' Lotka-Volterra Model with Carrying Capacity and Hunting
#'
#' Function computes the rate of change of populations in a predator-prey interaction
#' with carrying capacity and seasonal hunting
#' 
#' @param t  time (days)
#' @param pop datatype list initial conditions; list with two values prey=number of prey and pred=number of predators
#' @param pars datatype list  coefficients in Lotka-Volterra model plus hunting parameters
#'  \emph{rprey} is growth rate of prey population
#'  \emph{alpha} is predation rate coefficient
#'  \emph{eff} is conversion efficiency of prey to predators
#'  \emph{pmort} is mortality rate of predator population
#'  \emph{K} is carrying capacity for prey
#'  \emph{hunt_rate} is hunting rate when hunting is active
#'  \emph{min_prey} is minimum prey population required before hunting is allowed
#' @examples
#' pars <- c(rprey = 0.95, alpha = 0.01, eff = 0.6, pmort = 0.4, K = 2000, hunt_rate = 0.1, min_prey = 400)
#' currpop <- c(prey = 500, pred = 50)
#' days <- seq(from=1, to=365)
#' res <- ode(func = lotvmodK_hunt_v2, y = currpop, times = days, parms = pars)
#'
#' @return  lotvmodK_hunt_v2 returns a list containing the following components
#' \describe{
#' \item{dprey}{rate of change of prey population}
#' \item{dpred}{rate of change of predator population}
#' }
#' 
  
# Lotka-Volterra Model with Carrying Capacity and Hunting
lotvmodK_hunt_v2 <- function(t, pop, pars) {
  with(as.list(c(pars, pop)), {
    
    # Basic predator-prey dynamics
    dprey <- rprey * (1 - prey / K) * prey - alpha * prey * pred
    dpred <- eff * alpha * prey * pred - pmort * pred
    
    # allow hunting if conditions are met
    if (prey > min_prey && hunt_rate > 0) {
      # only hunt prey above minimum threshold
      available_prey <- prey - min_prey
      # don't hunt more than available
      hunting_mort <- min(hunt_rate * available_prey, available_prey)
      dprey <- dprey - hunting_mort
    }
    
    return(list(c(dprey, dpred)))
  })
}