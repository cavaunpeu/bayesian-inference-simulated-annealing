N <- 100
mu.true <- 3
sigma.true <- .5
y <- rnorm(n = N, mean = mu.true, sd = sigma.true)

# establish the prior probability distribution of mu and sigma, and the likelihood function of our data given both
mu.prior.probability <- function(mu, mean = 0, sigma = 10) { dnorm(x = mu, mean = mean, sd = sigma) }
sigma.prior.probability <- function(sigma, min = 0, max = 5) { dunif(x = sigma, min = min, max = max) }
data.likelihood.given.mu.and.sigma <- function(mu, observed.data.vector = y, sigma = sigma) { dnorm(x = observed.data.vector, mean = mu, sd = sigma)}

# combine the above functions to compute log posterior probability of mu given our data
log.posterior.probability.of.data.given.mu <- function(mu, sigma) {
  mu.probability <- mu.prior.probability(mu = mu)
  data.likelihood <- data.likelihood.given.mu.and.sigma(mu = mu, sigma = sigma)
  log.mu.probability <- log(mu.probability + 1)
  log.data.likelihood <- sum( log(data.likelihood + 1) )
  return( log.mu.probability + log.data.likelihood )
}

log.posterior.probability.of.data.given.sigma <- function(mu, sigma) {
  sigma.probability <- sigma.prior.probability(sigma = sigma)
  data.likelihood <- data.likelihood.given.mu.and.sigma(mu = mu, sigma = sigma)
  log.sigma.probability <- log(sigma.probability + 1)
  log.data.likelihood <- sum( log(data.likelihood + 1) )
  return( log.sigma.probability + log.data.likelihood )
}

# build an MCMC engine
n.steps <- 1e4
mu.samples <- rep(x = NA, n.steps)
sigma.samples <- rep(x = NA, n.steps)
mu.current <- 1
sigma.current <- 1
sampler.sigma <- .1

# define move probability functions
move.probability.via.simulated.annealing <- function(temperature, log.prob.current, log.prob.proposal) {
  exponent <- min( 0, (log.prob.proposal - log.prob.current) / temperature )
  return( exp(exponent) )
}

move.probability.via.metropolis <- function(log.prob.proposal, log.prob.current, temperature) {
  return( exp( log.prob.proposal - log.prob.current ))
}

# define simulated annealing parameters
temperature <- 1
alpha <- .9999

for (step in 1:n.steps) {
  mu.samples[step] <- mu.current
  sigma.samples[step] <- sigma.current

  mu.proposal <- rnorm(n = 1, mean = mu.current, sd = sampler.sigma)
  mu.move.probability <- move.probability.via.metropolis(
    temperature = temperature,
    log.prob.current = log.posterior.probability.of.data.given.mu(mu = mu.current, sigma = sigma.current),
    log.prob.proposal = log.posterior.probability.of.data.given.mu(mu = mu.proposal, sigma = sigma.current)
  )
  mu.current <- ifelse(test = runif(1) < mu.move.probability, yes = mu.proposal, no = mu.current)


  sigma.proposal <- rnorm(n = 1, mean = sigma.current, sd = sampler.sigma)
  if (sigma.proposal < 1) sigma.proposal <- abs(sigma.proposal)
  sigma.move.probability <- move.probability.via.metropolis(
    temperature = temperature,
    log.prob.current = log.posterior.probability.of.data.given.sigma(mu = mu.current, sigma = sigma.current),
    log.prob.proposal = log.posterior.probability.of.data.given.sigma(mu = mu.current, sigma = sigma.proposal)
  )
  sigma.current <- ifelse(test = runif(1) < sigma.move.probability, yes = sigma.proposal, no = sigma.current)

  temperature <- temperature * alpha
}

# plot resulting samples
hist(mu.samples, breaks = 500, xlim = c(2.7, 3.3))
plot(1:n.steps, mu.samples, type = "l")
plot(1e3:n.steps, mu.samples[1e3:n.steps], type = "l")

hist(sigma.samples, breaks = 500, xlim = c(.1, .6))
plot(1:n.steps, sigma.samples, type = "l")
plot(1e3:n.steps, sigma.samples[1e3:n.steps], type = "l")

plot(x = mu.samples, y = sigma.samples, type = "l")
