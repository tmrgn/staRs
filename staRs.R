## MT4113 - Assignment 3, Stars Simulation
library(dslabs)
library(tidyverse)
library(car)
library(truncnorm)
library(plyr)
library(gridExtra)

# Load dataset
stars <- dslabs::stars

# Fit model on the observed data
starMod <- lm(magnitude ~ temp, data = stars)
# View estimated regression coefficient for temperature
summary(starMod)
# Test normality assumption
shapiro.test(starMod$residuals)

sim <- function(sims = 100, data, n = nrow(data), formula, effect) {
# Purpose: generate simulated data sets
# Inputs:
# sims - number of simulated data sets to produce
# data - data set from which the simulations will be based on
# n - the number of observations in each simulated data set
# formula - formula for which simple linear regression follows
# effect - the effect size between the predictor and response
# Outputs:
# simsList - list containing a simulated data set per list index
  
  # Extract response variable from passed-in formula
  response <- subset(data, select = as.character(formula[[2]]))
  # Extract predictive variable from passed-in formula
  predictor <- subset(data, select = as.character(formula[[3]]))
  
  # Split predictor variable according to observed mixture distribution
  predLow <- predictor[[1]][predictor[[1]] < 8000]
  predHigh <- predictor[[1]][predictor[[1]] >= 8000]
  
  # Fit model using original dataset
  mod <- lm(formula, data = data)
  # Extract observed intercept from model
  intercept <- mod$coefficients[1]
  
  # Initialise list to store simulated data sets
  simsList <- list()
  
  # Produce number of data sets as specified in 'sims' argument
  for (i in 1:sims) {
    
    # Simulate predictor variable by capturing mixture distribution
    pred <- c(rtruncnorm(round(0.61 * n), a = min(predLow), b = max(predLow),
                         mean = mean(predLow), sd = sd(predLow)),
              rnorm(round(0.39 * n), mean(predHigh), sd = sd(predHigh)))
    
    # Simulate response variable according to relationship with predictor
    resp <- rnorm(n, mean = intercept + effect * pred, sd = sd(response[[1]]))
    
    # Create dataset combining simulated predictor with corresponding response
    simsList[[i]] <- data.frame(predictor = pred, response = resp)
    
  }
  
  # Return list of simulated data sets
  return(simsList)
  
}


parametricTest <- function(data, formula, effects, sampSizes) {
# Purpose: parametricTest - takes in parameters to then simulate data with,
#   \then calculates the p-value of the relationship for the formula passed in
# Inputs:
# data - data set to simulate from
# formula - formula for which simple linear regression will be fit
# effects - effect sizes to simulate values using
# sampSizes - sample size of data set to simulate for
# Output:
# results - list containing parametric test p-values
  
  # Initialise empty list to store each sample size result for each effect size
  results <- list()
  
  # Loop over effect sizes of predictor on response passed in
  for (effect in effects) {
    
    # Initialise vector to hold parametric test results for each effect size
    paraTest <- c()
    
    # Loop over sample sizes to simulate
    for (sampSize in sampSizes) {
      
      # Generate simulated data set
      simulation <- sim(data = data,
                        formula = formula,
                        effect = effect,
                        n = sampSize)
      
      # Extract p-values from simulated model fits
      pValsSimulation <- pValues(simulation)
      
      # Evaluate if no effect size, expecting null hypothesis is true (size)
      if (effect == 0) {
        
        # Compute size, given the null hypothesis is true; 5% significance
        paraTest <- c(paraTest, size(pValsSimulation, 0.05))
        
      } else {
        
        # Compute power, given the alt hypothesis is true; 5% significance
        paraTest <- c(paraTest, power(pValsSimulation, 0.05))
        
      }
      
    }
    
    # Append parametric test result to results list
    results[[length(results) + 1]] <- paraTest
    
  }
  
  # Return list of results
  return(results)
  
}


pValues <- function(data) {
# Purpose: pVal - for each data set in list, fit simple linear regression model
#   \and extract p-value from the predictor variable's coefficient estimate
# Inputs:
# data - list object containing data sets with one response, one predictor variable
# Output:
# pVals - vector of p-values from the fitted model for each simulated data set
  
  # Initialise empty vector to store p-values
  pVals <- c()
  
  # Loop over each data set in the list passed
  for (i in 1:length(data)) {
    
    # Fit the model
    mod <- lm(response ~ predictor, data = data[[i]])
    # Extract p-value from each model summary using summary index
    pVals[i] <- summary(mod)[[4]][[8]]
    
  }
  
  return(pVals)
  
}


nonParametricTest <- function(data, formula, effect, sampSize = nrow(data), nRepeat = 1000) {
# Purpose: nonParametricTest - takes in a data set, simulates a single data set
#   \with the given effect and sample size, performs a randomisation test for
#   \the regression coefficient for the predictor variable
# Inputs:
# data - data set to simulate using
# formula - linear regression formula for the relationship to test
# effect - effect size used to simulate data
# sampSize - sample size used to simulate data
# nRepeat - number of randomisations
# Outputs:
# randCoefs - randomised regression coefficients for the predictor
# simCoef - regression coefficient for model fit on original simulated data set
# pValue - pValue of randomisation test
  
  
  # Simulate data under given scenario
  simData <- sim(sims = 1,
                 data = data,
                 n = sampSize,
                 formula = formula,
                 effect = effect)
  
  # Initialise vector to store regression coefficient estimates
  randCoefs <- c()
  
  # Fit model to original simulated data set
  simModel <- lm(response ~ predictor, data = simData[[1]])
  
  # Store regression coefficient of predictor variable
  simCoef <- simModel$coefficients["predictor"]
  randCoefs <- c(randCoefs, simCoef)
  
  # Loop over number of desired randomisations
  for (i in 1:nRepeat) {
    
    # Create randomised data set of response values
    simData[[1]]$response <- sample(simData[[1]]$response, 
                                    size = nrow(simData[[1]]), replace = FALSE)
    
    # Fit model with randomised values
    randResult <- lm(response ~ predictor, data = simData[[1]])
    
    # Store coefficient estimate for simulated model
    randCoefs <- c(randCoefs, randResult$coefficients["predictor"])
    
  }
  
  # Calculate the p-value (power if effect != 0, size is effect = 0)
  if (effect == 0) {
    
    # If effect size is 0, calculate proportion that random coefficient is incorrectly
    #   \less-than coefficient from data set used for randomisation test
    pValue <- sum(randCoefs <= simCoef) / length(randCoefs)
    
  } else {
    
    pValue <- sum(randCoefs >= simCoef) / length(randCoefs)
    
  }
  
  # Return list containing coefficients and randomisation test p-value
  return(list(randCoefs = randCoefs,
              simCoef = simCoef,
              pValue = pValue))
  
}


simMmError <- function(data, variable, nameVariable, roundedNumber) {
# Purpose: simMmError - Takes the column of the variable passed and rounds it
#   \to a given number, before overwriting the original column with the new, rounded values
# Inputs:
# data - original data set containing observed values
# variable - the variable within the data set to round
# nameVariable - the name of the variable within the data set to round
# roundedNumber - the number for which the values will be rounded to
# Output:
# data - the updated data set with rounded values for the given column
  
  # Round each value of given variable to given number
  variable <- round_any(variable, roundedNumber)
  
  # Mutate data frame such that new rounded column takes the place of the old
  data[[nameVariable]] <- variable
  
  # Return data frame with updated values with given measurement error
  return(data)
  
}


size <- function(values, threshold) {
# Purpose: size - calculate size of statistical test
# Inputs:
# values - the test statistics to compare against a significance level
# threshold - the significance level to test against
# Output:
# size - the computed size value
  
  size <- sum(values < threshold) / length(values)

  return(size)
  
}


power <- function(values, threshold) {
# Purpose: power - calculate power of statistical test
# Inputs:
# values - the test statistics to compare against a significance level
# threshold - the significance level to test against
# Output:
# power - the computed power value
  
  power <- sum(values <= threshold) / length(values)
  
  return(power)
  
}


## Simulation scenarios

set.seed(11)
# Generate results for parametric test
simData <- parametricTest(data = stars, formula = magnitude ~ temp,
                          effects = c(-3e-3, -1e-3, -6e-4, -3e-4, -1e-4, -6e-5, -3e-5, -1e-5, 0),
                          sampSizes = c(10, 30, 50, 96, 150, 200, 250, 400, 500))
# View results for simulation scenarios
simData

set.seed(11)
# Perform randomisation test with specified input (change accordingly)
simNonPara <- nonParametricTest(data = stars, formula = magnitude ~ temp,
                                sampSize = 96, effect = -6e-4)
# View p-value of randomisation test
simNonPara$pValue

# Assign rounded data set to object to pass to simulation functions
mmErrorData <- simMmError(stars, stars$magnitude, "magnitude", 10)
# Perform simulations for various effect sizes and sample sizes for new data
set.seed(11)
simDataMmError <- parametricTest(data = mmErrorData, formula = magnitude ~ temp,
                                 effects = c(-3e-3, -1e-3, -6e-4, -3e-4, -1e-4, -6e-5, -3e-5, -1e-5, 0),
                                 sampSizes = c(10, 30, 50, 96, 150, 200, 250, 400, 500))
simDataMmError
