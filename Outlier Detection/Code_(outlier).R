library(fda)
library(stats)

set.seed(1)

n_samples <- 100
t <- seq(from = 0, to = 1, length = 100)
prop <- 0.1 # Proportion of outliers

X_sample_1 <- matrix(data = 0, nrow = length(t), ncol = n_samples)
X_sample_2 <- matrix(data = 0, nrow = length(t), ncol = n_samples)
y_outlier_1 <- numeric(length = n_samples)
y_outlier_2 <- numeric(length = n_samples)

# Function generator
generate_fn <- function(t, max = 0.5)
{
  c <- runif(n = 3, min = 0, max = max)
  t1 <- c[1]*sin(10*c[1]*(t))
  t2 <- c[2]*sin(10*c[2]*(t))
  t3 <- c[3]*sin(10*c[3]*t)
  X <- 0.5*(t1 + t2 + t3) + exp(t) + rnorm(1, mean = 0, sd = max)
  return(X)
}

##### Generating sample functions with offset type 2 #####

# Outlier generator #1

for(i in 1:(n_samples*(1-prop)))
{
  c <- runif(1, min = 0.5, max = 0.8)
  X_sample_2[ ,i] <- generate_fn(t = t, max = 0.1) + c*sin(10*t + runif(1)) + rnorm(length(t), sd = 0.1)
} 
for(i in (n_samples*(1-prop) + 1):n_samples)
{
  c <- runif(1, min = 1, max = 2)
  X_sample_2[ ,i] <- generate_fn(t = t, max = 0.1) + c*sin(10*t + runif(1)) + rnorm(length(t), sd = 0.1)
  y_outlier_2[i] <- 1
}

# Plots
plot(x = 0, y = 0, type = 'n',
     xlim = c(0, 1), ylim = c(0, 5),
     xlab = 't', ylab = 'X(t)', 
     main = 'Case 2 (Different shape)')
for(i in 1:n_samples)
{
  lines(x = t, y = X_sample_2[ ,i], col = i)
}
depth <- fbplot(fit = X_sample_2, plot = F)$depth
plot(density(depth), xlab = 'Depth', ylab = 'Density',
     main = 'Case 2')

mean_depth <- mean(depth)
sd_depth <- sd(depth)
z_score_depth <- (depth - mean_depth) / sd_depth

outliers_predicted <- as.numeric((z_score_depth <= -1.96) | (z_score_depth >= 1.96))

plot(x = 0, y = 0, type = 'n',
     xlim = c(0, 1), ylim = c(0, 5),
     xlab = 't', ylab = 'Y(t)', main = 'Outliers')
for(i in 1:n_samples)
{
  lines(x = t, y = X_sample_2[ ,i], col = outliers_predicted[i] + 1, 
        lwd = outliers_predicted[i] + 1)
}

print(table(y_outlier_2, outliers_predicted))

##### Generating sample functions with offset type 1 #####

# Outlier generator #2
for(i in 1:(n_samples*(1-prop)))
{
  c <- runif(1, min = 0.5, max = 1)
  X_sample_1[ ,i] <- generate_fn(t = t, max = 0.1) + sin(5*c*t + runif(1)) + rnorm(length(t), sd = 0.1)
} 
for(i in (n_samples*(1-prop) + 1):n_samples)
{
  c <- runif(1, min = 1, max = 1.5)
  X_sample_1[ ,i] <- generate_fn(t = t, max = 0.1) + sin(20*c*t + runif(1)) + rnorm(length(t), sd = 0.1)
  y_outlier_1[i] <- 1
}

# Plots
plot(x = 0, y = 0, type = 'n',
     xlim = c(0, 1), ylim = c(0, 5),
     xlab = 't', ylab = 'X(t)', 
     main = 'Case 1 (Different Frequency)')
for(i in 1:n_samples)
{
  lines(x = t, y = X_sample_1[ ,i], col = i)
}
depth <- fbplot(fit = X_sample_1, plot = F)$depth
plot(density(depth), xlab = 'Depth', ylab = 'Density',
     main = 'Case 2')

mean_depth <- mean(depth)
sd_depth <- sd(depth)
z_score_depth <- (depth - mean_depth) / sd_depth

outliers_predicted <- as.numeric((z_score_depth <= -1.96) | (z_score_depth >= 1.96))

plot(x = 0, y = 0, type = 'n',
     xlim = c(0, 1), ylim = c(0, 5),
     xlab = 't', ylab = 'Y(t)', main = 'Outliers')
for(i in 1:n_samples)
{
  lines(x = t, y = X_sample_1[ ,i], col = outliers_predicted[i] + 1, 
        lwd = outliers_predicted[i] + 1)
}

print(table(y_outlier_1, outliers_predicted))
