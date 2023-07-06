library(devtools)
use_git()







# Benchmarking the methods to compare execution times and performance
benched <- microbenchmark(
  method1 = poissonSimulation(lambda = 1, start = 0, end = 20, method = 1),
  method2 = poissonSimulation(lambda = 1, start = 0, end = 20, method = 2),
  times = 1000
)
print(benched)




















### ?????????


# Simulating T ~ exp(gamma) to return a vector of poisson process of rate Beta
#### poissonSimulation(lambda = beta,
###                  start = stormStart,
###                  end = stormEnd,
###                  method = 1)

# Simulating storm arrivals based on Poisson process
simulateStormArrivals <- function(lambda, beta, gamma, start, end) {
  # Generating storm arrival times using Poisson process
  stormArrivals <- poissonSimulation(lambda = beta,
                                     start = stormStart,
                                     end = stormEnd,
                                     method = 1)

  # Simulating storm intervals based on the generated storm arrival times
  stormList <- list()
  for (i in 1:length(stormArrivals)) {
    # Setting the start time for the current storm
    stormStart <- stormArrivals[i]
    # Determining the end time of the current storm based on exponential distribution
    stormEnd <- stormStart + stats::rexp(rate = beta)
    # Generating storm events within the current storm duration
    stormList[i] <- poissonSimulation(lambda = lambda, start = stormStart, end = stormEnd, method = 1)
  }

  # Assigning names to the elements of stormList based on the length of stormArrivals
  names(stormList) <- paste0("storm", 1:length(stormArrivals))

  # Filtering the stormList
  ### filteredStormList <- filterStormArrivals(stormList, "arrival times <= end")

  # Returning the list of simulated storms
  return(stormList)

}

























### SEPERATE HELP FILE

#' @examples
#' durations_exp <- cellDurations(n = 100, distn = "exp", eta = 0.1)
#' durations_gamma <- cellDurations(n = 100, distn = "gamma", eta = 0.1, shape = 2)
#' durations_weibull <- cellDurations(n = 100, distn = "weibull", shape = 2, scale = 0.5)




# Cell Duration:

# Option 1: Generating cell duration taking 2 arguments: n (no. of cells) and eta (parameter of the exp dist)
cellDurations <- function(n, eta) {
  # Generates n random numbers from an exponential distribution with rate beta
  durations <- rexp(n, rate = eta)
  return(durations)
}

# Option 2: Generating cell duration by creating a set of options
cellDurations <- function(n, distn = c("exp", "gamma", "weibull"), pars = list(eta = 1, shape = 1, rate = 1, scale = 1)) {
  distn <- match.arg(distn)

  if (distn == "exp") {
    durations <- rexp(n, rate = pars$eta)
  } else if (distn == "gamma") {
    durations <- rgamma(n, shape = pars$shape, rate = pars$eta)
  } else if (distn == "weibull") {
    durations <- rweibull(n, shape = pars$shape, scale = pars$scale)
  }

  return(durations)
}

# Option 3: Generating cell durations by allowing full freedom of the functions
cellDurations <- function(n, distn = NULL, ...) {
  if (!is.null(distn)) {
    durations <- distn(n, ...)
  } else {
    stop("No probability distribution function provided.")
  }

  return(durations)
}










# Cell Intensities:

# Option 1: Generating cell intensity taking 2 arguments: n (no. of cells) and beta (parameter of the exp dist)
cellIntensities <- function(n, beta) {
  intensities <- rexp(n, rate = beta)
  return(intensities)
}

# Option 2: Generating cell intensity by creating a set of options
cellIntensities <- function(n, distn = c("exp", "gamma", "weibull"),
                            pars = list(eta = 1, shape = 1, rate = 1, scale = 1)) {
  distn <- match.arg(distn)

  if (distn == "exp") {
    intensities <- rexp(n, rate = pars$eta)
  } else if (distn == "gamma") {
    intensities <- rgamma(n, shape = pars$shape, rate = pars$eta)
  } else if (distn == "weibull") {
    intensities <- rweibull(n, shape = pars$shape, scale = pars$scale)
  }

  return(intensities)
}

# Option 3: Generating cell intensity by allowing full freedom of the functions
cellIntensities <- function(n, distn = NULL, ...) {
  if (!is.null(distn)) {
    intensities <- distn(n, ...)
  } else {
    stop("No probability distribution function provided.")
  }

  return(intensities)
}











### OLD PLOT:
### @export
### Creating a function to generate plots of the poissonSimulation
### plot.pp <- function(x, which = 1, ...) {
###   if (which == 1) {
###     # Plotting the initial line graph to the plot
###     graphics::plot(x, ..., type = "l", xlab = "Index", ylab = "Event Time")
###   } else if (which == 2) {
###     # Plotting the cumulative sum graph of event times
###     graphics::plot(cumsum(x), ..., type = "l", xlab = "Index", ylab = "Cumulative Sum")
###   } else {
###     stop("Invalid value for 'which' parameter")
###   }
###   return(invisible())
### }



