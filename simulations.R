## Simulation scenarios
source("staRs.R")

# FUNCTION: simScenarios
# INPUTS:
# data - data set to simulate from
# effects - effect sizes to simulate values using
# n - sample size of data set to simulate
# PROCESS
# 
# OUTPUT:
# results - list containing parametric test p-values
simScenarios <- function(data, formula, effects, sampSizes) {
  
  # Initialise empty vector to store non-parametric test results
  nonParaTest <- c()
  
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
      
      # Evaluate if no effect size, meaning the null hypothesis is true
      if (effect == 0) {
        
        # Compute size, given the null hypothesis is true; 5% significance
        paraTest <- c(paraTest, size(pValsSimulation, 0.05))
        
      } else {
        
        # Compute power, given the alt hypothesis is true; 5% significance
        paraTest <- c(paraTest, power(pValsSimulation, 0.05))
        
      }
      
      
    }
    
    results[[length(results) + 1]] <- paraTest
    
  }
  
  return(results)
  
}

set.seed(11)
test <- simScenarios(data = stars, formula = magnitude ~ temp,
                     effects = c(-6e-4, -3e-4, -1e-4, -6e-5, -3e-5, -1e-5),
                     sampSizes = c(10, 25, 50, 96, 150, 200, 250, 400, 500))
test

sampSizes = c(10, 25, 50, 96, 150, 200, 250, 400, 500)
plot(sampSizes, test[[4]], ylim = c(0, 1), main = "Sample Size p-Values for Effect Size -6e-5")

## Effect sizes ################################################################

## Simulate data with different effect sizes of temperature on magnitude
# Report power for each different effect size
# NOTE! Takes ~ 1 minute
effects <- c(-1e-4, -5e-4, -9e-4, -1e-5, -5e-5, -9e-5,
             -1e-6, -5e-6, -9e-6, -1e-7, -5e-7, -9e-7)

para <- c()
nonPara <- c()

set.seed(11)
for (effect in effects) {

  # Perform parametric (t-test) test
  tempEffect <- sim(data = stars,
                    formula = magnitude ~ temp,
                    effect = effect)
  
  pValsTempEffect <- pValues(tempEffect)
  #hist(pValsTempEffect, breaks = 20)
  # Significance level 5%
  para <- c(para, power(pValsTempEffect, 0.05))
  
  # Perform non-parametric (randomisation) test
  #set.seed(11)
  #tempEffectRand <- randTest(tempEffect)
  #hist(tempEffectRand, breaks = 20)
  #abline(v = i, col = "firebrick", lwd = 3)
  #nonPara <- c(nonPara, power(tempEffectRand, threshold = effect))
  
}

plot(effects[1:12], para[1:12])
plot(effects[1:12], nonPara[1:12])

## Simulate data with no effect of temperature on magnitude
set.seed(11)
noEffect <- sim(data = stars,
                formula = magnitude ~ temp,
                effect = 0)

pValsNoEffect <- pValues(noEffect)
hist(pValsNoEffect, breaks = 20)
abline(v = 0.05, col= "firebrick", lwd = 3)
size(pValsNoEffect, threshold = 0.05)

# Randomisation test
set.seed(11)
noEffectRand <- randTest(noEffect)
hist(noEffectRand, breaks = 20)
abline(v = 0, col = "firebrick", lwd = 3)
# Threshold equal to effect size
size(noEffectRand, threshold = 0)


## Sample sizes ################################################################
# Sample sizes to simulate
sampSizes <- c(10, 20, 30, 50, 60, 75, 96, 125, 150, 200, 250, 400, 500)
paraSampSizes <- c()

# Loop over sample size vector to pass it into simulation parameters
set.seed(11)
for (sampSize in sampSizes) {
  
  simSampSizes <- sim(sims = 100,
                      data = stars,
                      formula = magnitude ~ temp,
                      effect = 0,
                      n = sampSize)
  
  pValsSampSizes <- pValues(simSampSizes)
  # Significance level 5%
  paraSampSizes <- c(paraSampSizes, size(pValsSampSizes, 0.05))
  
}

paraSampSizes
plot(x = sampSizes, y = paraSampSizes)

simSampSizes[[1]]
pValsSampSizes <- pValues(simSampSizes)
pValsSampSizes
hist(pValsSampSizes, breaks = 20)
abline(v = 0.05, col = "firebrick", lwd = 3)



set.seed(11)
simS <- sim(data = stars,
            formula = magnitude ~ temp,
            effect = -4e-05,
            n = 96)

simS[[2]]

pV <- pValues(simS)
pV
power(pV, 0.05)


## Measurement error ###########################################################
# Account for measurement error, rounding magnitudes to nearest 1, 5, and 10

# FUNCTION: simMmError
# INPUTS:
# data - original data set containing observed values
# variable - the variable within the data set to round
# nameVariable - the name of the variable within the data set to round
# roundedNumber - the number for which the values will be rounded to
# PROCESS:
# Takes the column of the variable passed and rounds it to a given number,
#   \before overwriting the original column with the new, rounded values
# OUTPUT:
# data - the updated data set with rounded values for the given column
simMmError <- function(data, variable, nameVariable, roundedNumber) {
  
  # Round each value of given variable to given number
  variable <- round_any(variable, 1)

  # Mutate data frame such that new rounded column takes the place of the old
  data[[nameVariable]] <- variable
  
  # Return data frame with updated values with given measurement error
  return(data)
  
}

# Assign rounded data set to object to pass to simulation functions
mmErrorData <- simMmError(stars, stars$magnitude, "magnitude", 10)



