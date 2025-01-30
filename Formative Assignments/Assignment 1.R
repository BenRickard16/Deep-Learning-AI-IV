#### Assignment 1 ####

## Question 1
# This function simulates n random variables from the distribution of X1,X2,Y above.
sim_x12y <- function(n,a,b,c) {
  x1=rnorm(n)
  x2=rnorm(n)
  y=rbinom(n,1,prob= (sin(1/((x1^2/a^2) + (x2^2/b^2) + c)))^2)
  return(data.frame(x1,x2,y))
}


## Question 2
# Simulate 10,000 values of (X1, X2, and Y), a=b=1, c=0.1
data <- sim_x12y(10000,1,1,0.1)


## Question 3
# Splitting dataset into training and test set
train <- data[1:8000,]
test <- data[8001:10000,]

# Fitting a logistic regression model to train data
fit1 <- glm(y~x1+x2,family=binomial(link=logit),data=train)

# Predictions on the test set
y_pred <- predict(fit1, test, type='response')         

# Convert probabilities to class labels
y_pred_class <- ifelse(ypred > 0.5, 1, 0)
table(ypredclass)

# Compute 0-1 Loss
zero_one_loss <- mean(ypredclass != test$y)
# 0.3875


## Question 4
# Fit a logistic regression model with polynomial features
fit_poly <- glm(y ~ poly(x1, 10) + poly(x2, 10), family = binomial(link = "logit"), data = train)

# Predictions on the test set
y_pred_poly <- predict(fit_poly, test, type = "response")

# Convert probabilities to class labels
y_pred_class_poly <- ifelse(y_pred_poly > 0.5, 1, 0)

# Compute 0-1 Loss
zero_one_loss_poly <- mean(y_pred_class_poly != test$y)
# 0.2515


## Question 5
# CV library
library(caret) 
set.seed(123)  

# Define the number of folds
k_folds <- 5
folds <- createFolds(train$y, k = k_folds, list = TRUE, returnTrain = FALSE)

# Store results for different polynomial degrees
results <- data.frame(Degree = 1:10, Loss = rep(NA, 10))

# Loop over polynomial degrees
for (d in 1:10) {
  
  loss_values <- c()
  
  # Perform cross-validation
  for (fold in folds) {
    # Create training and validation sets
    train_fold <- train[-fold, ]
    val_fold <- train[fold, ]
    
    # Fit logistic regression with polynomial terms
    model <- glm(y ~ poly(x1, d) + poly(x2, d), family = binomial(link = "logit"), data = train_fold)
    
    # Predictions on validation set
    ypred_val <- predict(model, val_fold, type = "response")
    
    # Convert probabilities to class labels
    ypred_class_val <- ifelse(ypred_val > 0.5, 1, 0)
    
    # Compute 0-1 loss
    loss <- mean(ypred_class_val != val_fold$y)
    
    # Store loss value
    loss_values <- c(loss_values, loss)
  }
  
  # Store the average loss for this polynomial degree
  results$Loss[d] <- mean(loss_values)
}

# Find the best degree (minimum loss)
best_degree <- results$Degree[which.min(results$Loss)]
# Best performing degree is quadratic which 0-1 loss 0.24975


## Question 6
# Fit the quadratic polynomial logistic model to entire dataset
fit_poly2 <- glm(y ~ poly(x1, 2) + poly(x2, 2), family = binomial(link = "logit"), data = train)

# Simulate new test dataset of 10000 values
test2 <- sim_x12y(10000,1,1,0.1)

# Predictions on the test set
y_pred_poly2 <- predict(fit_poly2, test, type = "response")

# Convert probabilities to class labels
y_pred_class_poly2 <- ifelse(y_pred_poly2 > 0.5, 1, 0)

# Compute 0-1 Loss
zero_one_loss_poly2 <- mean(y_pred_class_poly2 != test$y)
# 0.251
