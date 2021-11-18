# Data exploration
library(dslabs)
library(tidyverse)
library(car)

# Load dataset
stars <- dslabs::stars

# Research Q: are hotter stars more luminous?
ggplot(stars, aes(x = temp, y = magnitude, colour = type)) +
  geom_point()

# Is the correlation significantly nonzero?
starMod <- lm(magnitude ~ temp, data = stars)
summary(starMod)

# Randomisation test
set.seed(11)
# Initialise list to store simulated coefficients
simCoefs <- list()
# Number of iterations
nRepeat <- 1000

# Loop over simulations to get coefficient estimates
for (i in 1:nRepeat) {
  
  reshuffled <- stars
  
  # Randomise magnitude values
  reshuffled$magnitude <- sample(reshuffled$magnitude,
                                 size = nrow(reshuffled), replace = FALSE)
  
  # Fit model with randomised values
  simMod <- lm(magnitude ~ temp, data = reshuffled)
  
  # Obtain coefficient estimate for simulated model
  simCoefs[i] <- simMod$coefficients["temp"]
  
}

simCoefs <- unlist(simCoefs)

hist(simCoefs, xlim = c(-7.5e-04, 5e-04))
abline(v = starMod$coefficients["temp"], col = "firebrick", lwd = 3)

# Randomisation test p-value
absObsCoef <- abs(starMod$coefficients["temp"])
pVal <- sum(abs(simCoefs) >= absObsCoef) / nRepeat

# Interpretation: our observed coefficient estimate falls way outside the
# /randomly generated distribution of coefficient estimates, given that H0
# /was true, strongly suggesting that we should reject H0. The observed
# /difference is extremely rare if the groups were randomly assigned.


# Simulation scenarios #########################################################

# FUNCTION: sim
sim <- function(sims = 100, data, n = nrow(data), formula, effect) {
  
  # Extract response variable from passed-in formula
  response <- subset(data, select = as.character(formula[[2]]))
  # Extract predictive variable from passed-in formula
  predictor <- subset(data, select = as.character(formula[[3]]))
  
  # Fit model using original dataset
  mod <- lm(formula, data = data)
  # Extract observed estimates from model
  intercept <- mod$coefficients[1]
  beta1 <- mod$coefficients[2]
  
  # Simulate dataset
  simList <- replicate(sims,
                       data.frame(response = rnorm(n, mean(response[[1]]), sd(response[[1]])),
                                  predictor = rnorm(n, mean(predictor[[1]]), sd(predictor[[1]]))),
                       simplify = FALSE)
  
  return(simList)
  
}

# Generate simulated data sets

## Size of simulated data tests ################################################

# FUNCTION: size
size <- function(data) {
  
  pVals <- c()
  
  for (i in 1:nrow(data[[1]])) {
    
    mod <- lm(response ~ predictor, data = data[[i]])
    # Extract p-value from each model summary
    pVals[i] <- summary(mod)[[4]][[8]]
    
  }
  
  hist(pVals, breaks = 20)
  abline(v = 0.05, col = "firebrick", lwd = 3)
  
  size <- length(pVals[pVals <= 0.05]) / length(pVals)
  
  return(size)
  
}

# Simulate data with effects of temperature on magnitude
set.seed(11)
tempEffect <- sim(data = stars,
                  formula = magnitude ~ temp,
                  effect = -1e-10)
tempEffect[[1]]
size(tempEffect)

# Simulate data with no effect of temperature on magnitude
set.seed(11)
noEffect <- sim(data = stars,
                formula = magnitude ~ temp,
                effect = 0)
noEffect[[1]]
size(noEffect)


## Power of simulated data tests ###############################################

