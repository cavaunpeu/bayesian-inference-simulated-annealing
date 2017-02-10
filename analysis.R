# simulated super simple normal distribution data
N <- 100
mu.true <- 3
sigma.true <- .5
y <- rnorm(n = N, mean = mu.true, sd = sigma.true)

# establish the prior probability distribution of mu and sigma, and the likelihood function of our data given both
mu.prior.probability <- function(mu, mu.prior.min = -1, mu.prior.max = 6) { dunif(x = mu, min = mu.prior.min, max = mu.prior.max) }
sigma.prior.probability <- function(sigma, location = 0, scale = 5) { dcauchy(x = sigma, location = location, scale = scale) }
data.likelihood.given.mu.and.sigma <- function(mu, observed.data.vector = y, sigma = sigma) { dnorm(x = observed.data.vector, mean = mu, sd = sigma)}

# combine the above functions to compute log posterior probability of mu given our data
log.posterior.probability.of.data.given.mu <- function(mu, sigma) {
  mu.probability <- mu.prior.probability(mu = mu)
  data.likelihood <- data.likelihood.given.mu.and.sigma(mu = mu, sigma = sigma)
  log.mu.probability <- log(mu.probability)
  log.data.likelihood <- sum( log(data.likelihood) )
  return( log.mu.probability + log.data.likelihood )
}

log.posterior.probability.of.data.given.sigma <- function(mu, sigma) {
  mu.probability <- mu.prior.probability(mu = mu)
  data.likelihood <- data.likelihood.given.mu.and.sigma(mu = mu, sigma = sigma)
  log.mu.probability <- log(mu.probability)
  log.data.likelihood <- sum( log(data.likelihood) )
  return( log.mu.probability + log.data.likelihood )
}

# build an MCMC engine
n.steps <- 1e4
mu.samples <- rep(x = NA, n.steps)
mu.current <- mu.initial <- 1
sampler.sigma <- .1

# define move probability functions
move.probability.via.simulated.annealing <- function(temperature, log.prob.mu.current, log.prob.mu.proposal) {
  exponent <- min( 0, (log.prob.mu.proposal - log.prob.mu.current) / temperature )
  return( exp(exponent) )
}

move.probability.via.metropolis <- function(log.prob.mu.proposal, log.prob.mu.current) {
  return( exp( log.prob.mu.proposal - log.prob.mu.current ))
}

# define simulated annealing parameters
temperature <- 1
alpha <- .9999

for (step in 1:n.steps) {
  mu.samples[step] <- mu.current
  mu.proposal <- rnorm(n = 1, mean = mu.current, sd = sampler.sigma)
  move.probability <- move.probability.via.simulated.annealing(
    temperature = temperature,
    log.prob.mu.current = log.posterior.probability.of.data.given.mu(mu = mu.current, sigma = sigma.true),
    log.prob.mu.proposal = log.posterior.probability.of.data.given.mu(mu = mu.proposal, sigma = sigma.true)
  )
  mu.current <- ifelse(test = runif(1) < move.probability, yes = mu.proposal, no = mu.current)
  temperature <- temperature * alpha
}

# plot resulting samples
hist(mu.samples, breaks = 500, xlim = c(2.7, 3.3))
plot(1:n.steps, mu.samples, type = "l")
plot(200:n.steps, mu.samples[200:n.steps], type = "l")
