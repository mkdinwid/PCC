library(tidyverse)
library(MASS)

# N = 30

# # Parameters
# n <- 30  # number of data points
# mu_z <- -1
# sigma_z <- 1
# beta0 <- 0.7; beta1 <- 2
# gamma0 <- -2; gamma1 <- 1.9
# sigma_x <- 0.3
# sigma_y <- 0.8
# 
# # Set seed for reproducibility
# set.seed(43)

# N = 100

# Parameters
n <- 100  # number of data points
mu_z <- 1
sigma_z <- 0.75
beta0 <- 0.3; beta1 <- 1.7
gamma0 <- -1.5; gamma1 <- 1.3
sigma_x <- 0.4
sigma_y <- 0.7

# Set seed for reproducibility
set.seed(44)

# Generate Z
Z <- rnorm(n, mu_z, sigma_z)

# Generate X from Z
X <- beta0 + beta1 * Z + rnorm(n, 0, sigma_x)

# Generate Y from Z
Y <- gamma0 + gamma1 * Z + rnorm(n, 0, sigma_y)

# Create data frame
data <- data.frame(X, Y, Z)

# Check correlation before adjusting
print('PRE-ADJUSTMENT')
cor.test(data$X, data$Y)

# Adjust for Z using linear models
model_x <- lm(X ~ Z, data=data)
model_y <- lm(Y ~ Z, data=data)

resid_x <- residuals(model_x)
resid_y <- residuals(model_y)

# Check correlation after adjusting
print('POST-ADJUSTMENT')
cor.test(resid_x, resid_y)

# save to csv
write.csv(data, 'n_100.csv', row.names = FALSE)

#############################################################

# for another data set, generate a true correlation between X and Y, but generate a Z that is
# independent of X and Y

# try with a t-distribution, wider tails, try to change df and non-centrality param
# try with an F-S distribution, skewed, d1 = 5, d2 = 2

# https://en.wikipedia.org/wiki/Student%27s_t-distribution
# https://en.wikipedia.org/wiki/F-distribution

# Parameters
n <- 100  # number of data points
trial.seed = randint()
set.seed(trial.seed) # create vector 1:10000, seed = i from vector

mu_z <- runif(1, -1, 1)
sigma_z <- runif(1, 0, 1)

beta0 <- runif(1, -1, 1)
beta1 <- runif(1, -1, 1)

gamma0 <- runif(1, -1, 1)
gamma1 <- runif(1, -1, 1)

sigma_x <- runif(1, 0, 1)
sigma_y <- runif(1, 0, 1)

# Generate new X
X <- rnorm(n, mu_z, sigma_z)

# Generate Y from X
Y <- gamma0 + gamma1 * X + rnorm(n, 0, sigma_y)

Z <- rnorm(n, mu_z, sigma_z)

