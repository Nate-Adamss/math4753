#' @title myncurve
#'
#' @param mu mean
#' @param sigma standard deviation
#' @param a x limit
#'
#' @returns plot, mu, sigma, probability
#' @export
#'
#' @examples myncurve(mu=5, sigma=5, a=15)
myncurve = function(mu, sigma, a) {
  # Plot the normal distribution curve
  curve(dnorm(x, mean = mu, sd = sigma),
        xlim = c(mu - 3 * sigma, mu + 3 * sigma),
        main = paste("Normal Distribution (mu =", mu, ", sigma =", sigma, ")"),
        ylab = "Density", xlab = "X",
        col = "darkblue", lwd = 2)

  # Shade the area under the curve from -∞ to a
  x_fill <- seq(mu - 3 * sigma, a, length.out = 1000)
  y_fill <- dnorm(x_fill, mean = mu, sd = sigma)
  polygon(c(x_fill, a), c(y_fill, 0), col = "darkblue", border = NA)

  # Calculate the cumulative probability P(X <= a)
  prob <- pnorm(a, mean = mu, sd = sigma)

  # Print the probability and return a list
  cat("The probability P(X <= ", a, ") is:", prob, "\n")

  return(list(mu = mu, sigma = sigma, probability = prob))
}







