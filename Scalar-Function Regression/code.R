library(fda)

set.seed(1)

n_samples <- 300
n_time_pts <- 100

# Evaluates a function given the random coeff.
evaluate_x <- function(coeff, t)
{
  exp_1 <- coeff[1] * exp(t)
  exp_2 <- sin(coeff[2]* 10 * t)
  exp_3 <- coeff[3] * 2 * t
  
  return(exp_3 + exp_2 + exp_1)
}

# Random coeff for the given data
X_sample <- matrix(data = runif(3*n_samples, min = 0, max = 2),
            nrow = n_samples, byrow = T)

# Given Xi's plot
grid_x <- seq(0, 1, length = n_time_pts)
plot(x = 0, y = 0, xlim = c(0, 1), ylim = c(-1, 12), 
     xlab = "t", ylab = "X(t)",
     main = "Generated Data (5 samples)", type = 'n')

for(i in 1:5)
{
  grid_y <- evaluate_x(coeff = X_sample[i, ], t = grid_x)
  lines(grid_x, grid_y, col = i)
}
legend("topleft", legend = paste(1:5), 
       col = 1:5, lty = 1)

# Calculating Yi's
operator_x <- function(coeff)
{
  fn <- integrate(f = function(t){evaluate_x(coeff, t)^2 * (sin(t) + cos(t))},
                  lower = 0, upper = 1)$value
  return(fn)
}

y_sample <- numeric(length = n_samples)
y_sample <- apply(X = X_sample, MARGIN = 1, FUN = operator_x)
y_sample <- y_sample + rnorm(n = n_samples)

# x <- predict for this x
# bw <- bandwidth
# X_train <- given sample
# y_train <- given sample
predict_x <- function(x, bw, X_sample, y_sample)
{
  n <- length(y_sample)
  weights <- numeric(length = n)
  for(i in 1:n)
  {
    distance <- integrate(f = function(t){(evaluate_x(X_sample[i, ], t) - evaluate_x(x, t))^2},
                          lower = 0, upper = 1)$value
    distance <- sqrt(distance)
    weights[i] <- dnorm(x = distance/bw)
  }
  estimate <- weighted.mean(x = y_sample, w = weights)
  
  return(estimate)
}

# Cross Validation
k_cross_val <- function(X_sample, y_sample, bw, k = 4)
{
  # Stores mse for k-folds
  mse <- numeric(length = k)
  
  n <- length(y_sample)
  div <- n / k
  
  for(i in 1:k)
  {
    # Splitting
    X_train <- X_sample[-( ((i-1)*div + 1):(i*div) ), ]
    X_test <- X_sample[((i-1)*div + 1):(i*div), ]
    
    y_train <- y_sample[-( ((i-1)*div + 1):(i*div) )]
    y_test <- y_sample[((i-1)*div + 1):(i*div)]
    
    y_pred <- apply(X = X_test, MARGIN = 1, 
                    FUN = function(x){predict_x(x, bw, X_train, y_train)})
    
    mse[i] <- mean((y_test - y_pred)^2)
  }
  
  return(mse)
}


grid_bw <- seq(from = 0.05, to = 0.55, length = 21)
mse_bw <- numeric(length = length(grid_bw))
for(i in 1:length(grid_bw))
{
  mse_bw[i] <- sum(k_cross_val(X_sample = X_sample, y_sample = y_sample,
                          bw = grid_bw[i], k = 4))
  print(paste(i, "/", length(grid_bw), "done"))
}

plot(x = grid_bw, y = mse_bw,
     type = 'l', lwd = 2,
     xlab = "Band-width", ylab = "MSE")

chosen_bw <- grid_bw[which.min(mse_bw)]

# Estimation
n_test <- 20
X_test <- matrix(data = runif(3*n_test, min = 0, max = 2),
                 nrow = n_test, byrow = T)

grid_y <- evaluate_x(coeff = X_test[1, ], t = grid_x)
lines(grid_x, grid_y, col = 1, lwd = 3)

y_pred <- apply(X = X_test, MARGIN = 1, 
                FUN = function(x){predict_x(x, chosen_bw, X_sample, y_sample)})
y_real <- apply(X = X_test, MARGIN = 1, FUN = operator_x)

mse <- mean((y_real - y_pred)^2)

predict_table <- cbind(y_real, y_pred, y_real - y_pred)
colnames(predict_table) = c("Real y", "Predicted y", "Error")

print(head(predict_table))
print(mse)



##### In-built Library #####

X_data_train <- matrix(0, nrow = n_samples, ncol = n_time_pts)
X_data_test <- matrix(0, nrow = n_test, ncol = n_time_pts)
for(i in 1:n_samples)
{
  X_data_train[i, ] <- evaluate_x(X_sample[i, ], grid_x)
}
for(i in 1:n_test)
{
  X_data_test[i, ] <- evaluate_x(X_test[i, ], grid_x)
}

y_data_train <- y_sample
y_data_test <- y_real

basis <- create.bspline.basis(rangeval = c(0, 1), nbasis = 10)
betalist <- list()
betalist[[1]] <- fdPar(basis, Lfdobj = 0, lambda = 1e-2)

fd_train <- Data2fd(argvals = grid_x, y = t(X_data_train), basisobj = basis)
fd_test <- Data2fd(argvals = grid_x, y = t(X_data_test), basisobj = basis)

fd_model <- fRegress(y_data_train, fd_train, betalist = betalist, method = "fRegress")
y_pred <- predict(fd_model, newdata = list(fd_test))

mse <- mean((y_data_test - y_pred)^2)
print(mse)
