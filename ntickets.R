#' @title ntickets
#' @name ntickets
#' @param N the number of seats in the flight
#' @param gamma the probability that a plane will be truly overbooked
#' @param p the probability of a "show"
#'
#' @returns the number of tickets to be sold using the discrete distribution "nd", the number of tickets to be sold using the normal approximation "nc", N, p, and gamma
#' @export
#'
#' @examples \dontrun{ntickets(N=400,gamma = 0.02, p = 0.95)}

ntickets <- function(N, gamma, p) {

  # Define the objective function using the discrete binomial distribution
  objectiveFunctionDiscrete <- function(n) {
    return(1 - gamma - pbinom(N, n, p))
  }

  # Define the objective function using the normal approximation
  objectiveFunctionNormal <- function(n) {
    meanApprox <- n * p
    sdApprox <- sqrt(n * p * (1 - p))
    return(1 - gamma - pnorm(N, meanApprox, sdApprox))
  }

  # Define range of ticket values to evaluate
  nVals <- seq(1, 1000, by = 1)

  # Compute objective function values
  objectiveValsDiscrete <- sapply(nVals, objectiveFunctionDiscrete)
  objectiveValsNormal <- sapply(nVals, objectiveFunctionNormal)

  # Find the value of n that minimizes the absolute objective function (closest to zero)
  indDiscrete <- which.min(abs(objectiveValsDiscrete))
  nd <- nVals[indDiscrete]

  indNormal <- which.min(abs(objectiveValsNormal))
  nc <- nVals[indNormal]

  # Store results in a named list
  namedlist <- list(nd = nd, nc = nc, N = N, p = p, gamma = gamma)

  # Plot the objective function for the discrete case
  plot(nVals, objectiveValsDiscrete, type = 'l', col = 'darkblue',
       main = "Objective Function vs n (Discrete Distribution)",
       xlab = "Number of Tickets Sold (n)",
       ylab = "Objective Function")
  abline(h = 0, lty = 2, col = "red")  # Horizontal line at y = 0
  points(nVals[indDiscrete], objectiveValsDiscrete[indDiscrete],
         pch = 21, bg = "red", cex = 2)  # Mark minimum point

  # Plot the objective function for the normal approximation case
  plot(nVals, objectiveValsNormal, type = 'l', col = "darkgreen",
       main = "Objective Function vs n (Normal Approximation)",
       xlab = "Number of Tickets Sold (n)",
       ylab = "Objective Function")
  abline(h = 0, lty = 2, col = "red")  # Horizontal line at y = 0
  points(nVals[indNormal], objectiveValsNormal[indNormal],
         pch = 21, bg = "red", cex = 2)  # Mark minimum point

  return(namedlist)
}

