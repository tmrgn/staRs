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
