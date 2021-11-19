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

# Loop over simulations to get coefficient estimates
# INPUTS:
# data - list containing a unique data frame per list element
# nRepeat - number of randomisation iterations
randTest <- function(data, nRepeat = 100) {
  
  # Initialise list to store simulated coefficients
  simCoefs <- list()
  
  # Loop over each simulated data set
  for (i in 1:length(data)) {
    
    reshuffled <- data[[i]]
    
    # Loop over simulations to get coefficient estimates
    for (j in 1:nRepeat) {
      
      # Randomise magnitude values
      reshuffled$response <- sample(reshuffled$response,
                                    size = nrow(reshuffled), replace = FALSE)
      
      # Fit model with randomised values
      simMod <- lm(response ~ predictor, data = reshuffled)
      
      # Obtain coefficient estimate for simulated model
      simCoefs[[i]] <- simMod$coefficients["predictor"]
      
    }
  
  }
  
  simCoefs <- unlist(simCoefs)
  return(simCoefs)
  
}

obsRand <- randTest(stars, nRepeat = 1000)
hist(simCoefs, xlim = c(-7.5e-04, 5e-04))
abline(v = starMod$coefficients["temp"], col = "firebrick", lwd = 3)
stars[[1]]

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
  
  # Simulate dataset
  simList <- replicate(sims,
                       data.frame(predictor = rnorm(n, mean(predictor[[1]]), sd(predictor[[1]])),
                                  response = rnorm(predictor[[1]] + sample(c(-1, 1), size = nrow(data), replace = TRUE, prob = c(0.36, 0.64)) * (effect * predictor[[1]]), sd(response[[1]]))),
                       simplify = FALSE)
  
  return(simList)
  
}

# FUNCTION: pVal - calculate p-values for covariate in simple linear regression
# INPUTS:
# data - list object containing data sets with one dependent, one predictor variable
pValues <- function(data) {
  
  # Initialise empty vector to store p-values
  pVals <- c()
  
  # Loop over each data set in the list passed
  for (i in 1:length(data)) {
    
    # Fit the model
    mod <- lm(response ~ predictor, data = data[[i]])
    # Extract p-value from each model summary
    pVals[i] <- summary(mod)[[4]][[8]]
     
    }
  
  return(pVals)
  
}

## Size of simulated data tests ################################################

# FUNCTION: size
size <- function(values, threshold) {
  
  size <- length(values[values < threshold]) / length(values)

  return(size)
  
}


## Power of simulated data tests ###############################################

# FUNCTION: power
power <- function(values, threshold) {
  
  power <- length(values[values <= threshold]) / length(values)
  
  return(power)
  
}
