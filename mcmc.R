# generate our data
N <- 100
mu.true <- 3
sigma.true <- .5
y <- rnorm(n = N, mean = mu.true, sd = sigma.true)

# establish the prior probability distribution of mu and sigma, and the likelihood function of our data given both
muPriorProbability <- function(mu, mean = 0, sigma = 10) { dnorm(x = mu, mean = mean, sd = sigma) }
sigmaPriorProbability <- function(sigma, min = 0, max = 5) { dunif(x = sigma, min = min, max = max) }
dataLikelihoodGivenMuAndSigma <- function(mu, observed.data.vector = y, sigma = sigma) { dnorm(x = observed.data.vector, mean = mu, sd = sigma)}

# combine the above functions to compute log posterior probability of mu given our data
logPosteriorProbabilityOfDataGivenMu <- function(mu, sigma) {
  mu.probability <- muPriorProbability(mu = mu)
  data.likelihood <- dataLikelihoodGivenMuAndSigma(mu = mu, sigma = sigma)
  log.mu.probability <- log(mu.probability + 1)
  log.data.likelihood <- sum( log(data.likelihood + 1) )
  return( log.mu.probability + log.data.likelihood )
}

logPosteriorProbabilityOfDataGivenSigma <- function(mu, sigma) {
  sigma.probability <- sigmaPriorProbability(sigma = sigma)
  data.likelihood <- dataLikelihoodGivenMuAndSigma(mu = mu, sigma = sigma)
  log.sigma.probability <- log(sigma.probability + 1)
  log.data.likelihood <- sum( log(data.likelihood + 1) )
  return( log.sigma.probability + log.data.likelihood )
}

# create a metropolis sampler
metropolisSampler <- function(mu.initial = 0, sigma.initial = 0, sampler.sigma = .1) {

  mu.current <- mu.initial
  sigma.current <- sigma.initial
  mu.samples <- vector()
  sigma.samples <- vector()

  moveProbability <- function(log.prob.proposal, log.prob.current) {
    return( exp( log.prob.proposal - log.prob.current ))
  }

  singleStep <- function() {
    mu.samples <<- append(mu.samples, mu.current)
    sigma.samples <<- append(sigma.samples, sigma.current)

    mu.proposal <- rnorm(n = 1, mean = mu.current, sd = sampler.sigma)
    mu.move.probability <- moveProbability(
      log.prob.current = logPosteriorProbabilityOfDataGivenMu(mu = mu.current, sigma = sigma.current),
      log.prob.proposal = logPosteriorProbabilityOfDataGivenMu(mu = mu.proposal, sigma = sigma.current)
    )
    mu.current <<- ifelse(test = runif(1) < mu.move.probability, yes = mu.proposal, no = mu.current)

    sigma.proposal <- rnorm(n = 1, mean = sigma.current, sd = sampler.sigma)
    if (sigma.proposal < 1) sigma.proposal <- abs(sigma.proposal)
    sigma.move.probability <- moveProbability(
      log.prob.current = log.posterior.probability.of.data.given.sigma(mu = mu.current, sigma = sigma.current),
      log.prob.proposal = log.posterior.probability.of.data.given.sigma(mu = mu.current, sigma = sigma.proposal)
    )
    sigma.current <<- ifelse(test = runif(1) < sigma.move.probability, yes = sigma.proposal, no = sigma.current)
  }

  step <- function(n.steps = 1) {
    for (i in 1:n.steps) singleStep()
  }

  get <- function(samples.vector = c("mu.samples", "sigma.samples")) {
    samples.vector <- match.arg(samples.vector)
    switch(samples.vector, mu.samples = mu.samples, sigma.samples = sigma.samples)
  }

  return(list(get = get, step = step))
}

# create a simulated annealing sampler
simulatedAnnealingSampler <- function(mu.initial = 0, sigma.initial = 0, sampler.sigma = .1) {

  mu.current <- mu.initial
  sigma.current <- sigma.initial
  temperature.current <- 1
  alpha <- .9999
  mu.samples <- vector()
  sigma.samples <- vector()

  moveProbability <- function(temperature, log.prob.current, log.prob.proposal) {
    exponent <- min( 0, (log.prob.proposal - log.prob.current) / temperature )
    return( exp(exponent) )
  }

  singleStep <- function() {
    mu.samples <<- append(mu.samples, mu.current)
    sigma.samples <<- append(sigma.samples, sigma.current)

    mu.proposal <- rnorm(n = 1, mean = mu.current, sd = sampler.sigma)
    mu.move.probability <- moveProbability(
      temperature = temperature.current,
      log.prob.current = logPosteriorProbabilityOfDataGivenMu(mu = mu.current, sigma = sigma.current),
      log.prob.proposal = logPosteriorProbabilityOfDataGivenMu(mu = mu.proposal, sigma = sigma.current)
    )
    mu.current <<- ifelse(test = runif(1) < mu.move.probability, yes = mu.proposal, no = mu.current)

    sigma.proposal <- rnorm(n = 1, mean = sigma.current, sd = sampler.sigma)
    if (sigma.proposal < 1) sigma.proposal <- abs(sigma.proposal)
    sigma.move.probability <- moveProbability(
      temperature = temperature.current,
      log.prob.current = log.posterior.probability.of.data.given.sigma(mu = mu.current, sigma = sigma.current),
      log.prob.proposal = log.posterior.probability.of.data.given.sigma(mu = mu.current, sigma = sigma.proposal)
    )
    sigma.current <<- ifelse(test = runif(1) < sigma.move.probability, yes = sigma.proposal, no = sigma.current)

    temperature.current <<- temperature.current * alpha
  }

  step <- function(n.steps = 1) {
    for (i in 1:n.steps) singleStep()
  }

  get <- function(samples.vector = c("mu.samples", "sigma.samples")) {
    samples.vector <- match.arg(samples.vector)
    switch(samples.vector, mu.samples = mu.samples, sigma.samples = sigma.samples)
  }

  return(list(get = get, step = step))
}
