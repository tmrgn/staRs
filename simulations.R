## Simulation scenarios
source("staRs.R")

## Effect sizes ################################################################

## Simulate data with effects of temperature on magnitude
set.seed(11)
tempEffect <- sim(data = stars,
                  formula = magnitude ~ temp,
                  effect = -6.026e-04)

pValsTempEffect <- pValues(tempEffect)
hist(pValsTempEffect, breaks = 20)
# Significance level 5%
size(pValsTempEffect, 0.05)
power(pValsTempEffect, 0.05)

# Perform randomisation test
set.seed(11)
tempEffectRand <- randTest(tempEffect)
hist(tempEffectRand, breaks = 20)
abline(v = -6.026e-04, col = "firebrick", lwd = 3)
size(tempEffectRand, threshold = -6.026e-04)
power(tempEffectRand, threshold = -6.026e-04)


## Simulate data with no effect of temperature on magnitude
set.seed(11)
noEffect <- sim(data = stars,
                formula = magnitude ~ temp,
                effect = 0.0005)

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
