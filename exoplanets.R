####
# HarvardX Data Science capstone project
# Searching for an exoplanet in a haystack
#
# This project considers the problem of finding
# exoplanets based on flux measurements.
#
# See github.com/vincentkieftenbeld/exoplanets
###

# set random set for reproducibility
set.seed(1729)

####
# Load packages, installing packages if required
####

if (!require(caret)) install.packages("caret")
if (!require(forecast)) install.packages("forecast")
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(xgboost)) install.packages("xgboost")

library(caret)
library(forecast)
library(tidyverse)
library(xgboost)

####
# Data can be found in the accompanying GitHub repository:
# https://github.com/vincentkieftenbeld/exoplanets
#
# train data: data/exoTrain.csv.zip
# test data: data/exoTest.csv.zip
####

####
# Pre-processing and feature extraction functions
####

# normalize a vector x such that its values are between 0 and 1
normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# compute the absolute value of fast Fourier transform of x
# Fourier coefficients are complex numbers,
# taking the absolute value converts these to real numbers
# representing the norm
fourier <- function(x) {
  abs(fft(x))
}

# preprocess and extract features from time series data by
# - identifying and removing outliers,
# - normalize each time series
# - convert to fast Fourier transform
extract_features <- function(data) {
  # drop label and convert data to a matrix
  # transpose matrix for convenience
  data <- select(data, -label)
  df <- t(as.matrix(data))

  # apply preprocessing and feature extraction
  df <- apply(df, 2, tsclean)
  df <- apply(df, 2, normalize)
  df <- apply(df, 2, fourier)

  # because the Fourier series is symmetric,
  # we only need the first half
  n <- ceiling(nrow(df) / 2)
  df <- df[1:n, ]

  # transpose back
  t(df)
}

# recode labels to 0 (no exoplanet, negative) and 1 (exoplanet, positive)
recode_labels <- function(data) {
  data$label <- ifelse(data$LABEL == 2, 1, 0)
  select(data, -LABEL)
}

####
# Read train data, recode labels, and extract features
####

train_data <- read_csv("data/exoTrain.csv.zip")
train_data <- recode_labels(train_data)
train_labels <- train_data$label
train_data <- extract_features(train_data)

dim(train_data)
length(train_labels)
table(train_labels)

####
# Regularized logistic regression
# - down-sampling to correct for imbalance
# - cross-validation to tune parameters
# - randomized search
###

trControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 5,
  search = "random",
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  sampling = "down",
  verboseIter = TRUE
)

model.lr <- caret::train(
  x = train_data,
  y = as.factor(ifelse(train_labels == 1, "Yes", "No")),
  method = "glmnet",
  family = "binomial",
  metric = "ROC",
  trControl = trControl
)

####
# Gradient boosted trees
# - weights used to correct for imbalance
# - 10-fold cross-validation to tune parameters
####

# gradient boosted tree algorithm can use weights to
# correct for the imbalance in negative and positive examples

# weight is the ratio of negative to positive examples
weight <- sum(train_labels == 0) / sum(train_labels == 1)

cv <- xgb.cv(
  data = train_data,
  label = train_labels,

  # cross-validation and task parameters
  nfold = 10,
  objective = "binary:logistic",
  eval_metric = "error",
  scale_pos_weight = weight,

  # booster parameters (iterations, learning rate, gamma)
  nrounds = 20,
  eta = 0.3,
  gamma = 0,

  # tree parameters (default settings)
  max_depth = 6,
  min_child_weight = 1,
  subsample = 1,
  colsample_bytree = 1
)

# cross-validation mean test error
cv$evaluation_log$test_error_mean[20]

png(file = "images/cv-gbt.png")
plot(
  cv$evaluation_log$train_error_mean,
  xlab = "Iteration",
  ylab = "Mean Error",
  type = "b",
  col = "black",
  ylim = c(0, 0.018),
  main = "Cross-Validated Performance Training Gradient Boosted Trees"
)
points(
  x = seq(cv$niter),
  y = cv$evaluation_log$test_error_mean,
  type = "b",
  col = "blue"
)
dev.off()

# since the cv error is small, the default parameters are good

####
# Train gradient boosted trees model with cross-validated parameters
####

model.gbt <- xgboost(
  data = train_data,
  label = train_labels,
  nrounds = cv$niter,
  params = cv$params,
)

####
# Read test data, recode labels, and extract features
####

test_data <- read_csv("data/exoTest.csv.zip")
test_data <- recode_labels(test_data)
test_labels <- test_data$label
test_data <- extract_features(test_data)

dim(test_data)
length(test_labels)

####
# Evaluate the models on the test set
# using accuracy, precision, recall, and F1 score
####

evaluation_metrics <- function(pred, labels) {
  data.frame(
    accuracy = mean(as.factor(pred) == as.factor(labels)),
    precision = precision(as.factor(pred), as.factor(labels)),
    recall = recall(as.factor(pred), as.factor(labels)),
    F1 = F_meas(as.factor(pred), as.factor(labels), beta = 1)
  )
}

pred.lr <- ifelse(predict(model.lr, test_data) == "Yes", 1, 0)
pred.gbt <- as.numeric(predict(model.gbt, test_data) > 0.5)

# logistic regression confusion matrix
cm.lr <- confusionMatrix(
  data = as.factor(pred.lr),
  reference = as.factor(test_labels),
  mode = "prec_recall"
)

cm.lr$table

#         actual
# pred    0   1
#    0  489   0
#    1   76   5

metrics.lr <- evaluation_metrics(pred.lr, test_labels)

# logistic regression
# accuracy  = 0.87
# precision = 1.00
# recall    = 0.87
# F1        = 0.93

# gradient boosted trees confusion matrix
cm.gbt <- confusionMatrix(
  data = as.factor(pred.gbt),
  reference = as.factor(test_labels),
  mode = "prec_recall"
)

cm.gbt$table

#         actual
# pred    0   1
#    0  565   0
#    1    0   5

metrics.gbt <- evaluation_metrics(pred.gbt, test_labels)

metrics.gbt
# gradient boosted trees
# accuracy  = 1.00
# precision = 1.00
# recall    = 1.00
# F1        = 1.00

# model comparison
data.frame(
  lr = t(metrics.lr),
  gbt = t(metrics.gbt)
)
