# simulated multiple linear regression data
N <- 100
x1 <- runif(n = N, min = -6, max = -2)
x2 <- rnorm(n = N, mean = 3, sd = 1)
x1 <- x1 - mean(x1)
x2 <- x2 - mean(x2)

alpha <- 1
beta1 <- 2
beta2 <- 8
sigma.true <- .5
mu <- alpha + beta1*x1 + beta2*x2
y <- rnorm(n = N, mean = mu, sd = sigma.true)

# establish the prior probability distribution of each parameter and the likelihood function of our data given that parameter
alpha.prior <- function(alpha, mean = 0, sigma = 10) { dnorm(x = alpha, mean = mean, sd = sigma) }
beta1.prior <- function(beta, mean = 0, sigma = 10) { dnorm(x = beta, mean = mean, sd = sigma) }
beta2.prior <- function(beta, mean = 0, sigma = 10) { dnorm(x = beta, mean = mean, sd = sigma) }
sigma.prior <- function(sigma, location = 0, scale = 5) { dcauchy(x = sigma, location = location, scale = scale)}
data.likelihood.given.mu <- function(mu, observed.data.vector = y, sigma = sigma) { dnorm(x = observed.data.vector, mean = mu, sd = sigma)}

# combine the above functions to compute log posterior probability of a given parameter given our data
log.posterior.probability.of.data.given.parameter <- function(mu, sigma, parameter, parameter.prior) {
  parameter.probability <- parameter.prior(parameter)
  data.likelihood <- data.likelihood.given.mu(mu = mu, sigma = sigma)
  log.parameter.probability <- log(parameter.probability + 1)
  log.data.likelihood <- sum( log(data.likelihood + 1) )
  return( log.parameter.probability + log.data.likelihood )
}

# build an MCMC engine
n.steps <- 1e3
sampler.sigma <- .1

alpha.current <- rnorm(n = 1, mean = 0, sd = 10)
beta1.current <- rnorm(n = 1, mean = 0, sd = 10)
beta2.current <- rnorm(n = 1, mean = 0, sd = 10)
sigma.current <- abs(rcauchy(n = 1, location = 0, scale = 5))
mu.current <- alpha.current + beta1.current*x1 + beta2.current*x2

alpha.samples <- rep(x = NA, n.steps)
beta1.samples <- rep(x = NA, n.steps)
beta2.samples <- rep(x = NA, n.steps)
sigma.samples <- rep(x = NA, n.steps)

# define move probability functions
move.probability.via.simulated.annealing <- function(temperature, log.prob.mu.current, log.prob.mu.proposal) {
  exponent <- min( 0, (log.prob.mu.proposal - log.prob.mu.current) / temperature )
  return( exp(exponent) )
}

move.probability.via.metropolis <- function(temperature, log.prob.mu.proposal, log.prob.mu.current) {
  return( exp( log.prob.mu.proposal - log.prob.mu.current ))
}

# define simulated annealing parameters
temperature <- 1
alpha <- .9999

for (step in 1:n.steps) {

  alpha.samples[step] <- alpha.current
  beta1.samples[step] <- beta1.current
  beta2.samples[step] <- beta2.current
  sigma.samples[step] <- sigma.current

  alpha.proposal <- rnorm(n = 1, mean = alpha.current, sd = sampler.sigma)
  beta1.proposal <- rnorm(n = 1, mean = beta1.current, sd = sampler.sigma)
  beta2.proposal <- rnorm(n = 1, mean = beta2.current, sd = sampler.sigma)

  mu.given.alpha.proposal <- alpha.proposal + beta1.current*x1 + beta2.current*x2
  mu.given.beta1.proposal <- alpha.current + beta1.proposal*x1 + beta2.current*x2
  mu.given.beta2.proposal <- alpha.current + beta1.current*x1 + beta2.proposal*x2

  move.probability.alpha <- move.probability.via.metropolis(
    temperature = temperature,
    log.prob.mu.current = log.posterior.probability.of.data.given.parameter(mu = mu.current, sigma = sigma.current, parameter = alpha.current, parameter.prior = alpha.prior),
    log.prob.mu.proposal = log.posterior.probability.of.data.given.parameter(mu = mu.given.alpha.proposal, sigma = sigma.current, parameter = alpha.current, parameter.prior = alpha.prior)
  )
  move.probability.beta1 <- move.probability.via.metropolis(
    temperature = temperature,
    log.prob.mu.current = log.posterior.probability.of.data.given.parameter(mu = mu.current, sigma = sigma.current, parameter = beta1.current, parameter.prior = beta1.prior),
    log.prob.mu.proposal = log.posterior.probability.of.data.given.parameter(mu = mu.given.beta1.proposal, sigma = sigma.current, parameter = beta1.current, parameter.prior = beta1.prior)
  )
  move.probability.beta2 <- move.probability.via.metropolis(
    temperature = temperature,
    log.prob.mu.current = log.posterior.probability.of.data.given.parameter(mu = mu.current, sigma = sigma.current, parameter = beta2.current, parameter.prior = beta2.prior),
    log.prob.mu.proposal = log.posterior.probability.of.data.given.parameter(mu = mu.given.beta2.proposal, sigma = sigma.current, parameter = beta2.current, parameter.prior = beta2.prior)
  )

  if (is.na(move.probability.alpha)) browser()

  alpha.current <- ifelse(test = runif(1) < move.probability.alpha, yes = alpha.proposal, no = alpha.current)
  beta1.current <- ifelse(test = runif(1) < move.probability.beta1, yes = beta1.proposal, no = beta1.current)
  beta2.current <- ifelse(test = runif(1) < move.probability.beta2, yes = beta2.proposal, no = beta2.current)

  mu.current <- alpha.current + beta1.current*x1 + beta2.current*x2

  sigma.proposal <- rnorm(n = 1, mean = sigma.current, sd = sampler.sigma)
  if (sigma.proposal < 0) sigma.proposal <- abs(sigma.proposal)
  move.probability.sigma <- move.probability.via.metropolis(
    temperature = temperature,
    log.prob.mu.current = log.posterior.probability.of.data.given.parameter(mu = mu.current, sigma = sigma.current, parameter = sigma.current, parameter.prior = sigma.prior),
    log.prob.mu.proposal = log.posterior.probability.of.data.given.parameter(mu = mu.current, sigma = sigma.proposal, parameter = sigma.proposal, parameter.prior = sigma.prior)
  )
  sigma.current <- ifelse(test = runif(1) < move.probability.sigma, yes = sigma.proposal, no = sigma.current)

  temperature <- temperature * alpha
}

# plot results
hist(alpha.samples, breaks = 50)
hist(beta1.samples, breaks = 50)
hist(beta2.samples, breaks = 50)
hist(sigma.samples, breaks = 50)
plot(1:n.steps, alpha.samples, type = "l")
plot(1:n.steps, beta1.samples, type = "l")
plot(1:n.steps, beta2.samples, type = "l")
plot(1:n.steps, sigma.samples, type = "l")
