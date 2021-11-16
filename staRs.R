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