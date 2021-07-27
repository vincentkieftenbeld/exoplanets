####
# Plot example time series for positive and negative cases
####
library(tidyverse)
library(forecast)

train <- read_csv("data/exoTrain.csv.zip")

png(file = "images/with-exoplanet.png")
par(mfrow = c(3, 1))
plot(as.numeric(train[1, ]), type = "l", main = "Flux Measurements of Stars with Exo-planets", xlab = "Time", ylab = "Flux")
plot(as.numeric(train[2, ]), type = "l", xlab = "Time", ylab = "Flux")
plot(as.numeric(train[4, ]), type = "l", xlab = "Time", ylab = "Flux")
dev.off()

png(file = "images/without-exoplanet.png")
par(mfrow = c(3, 1))
plot(as.numeric(train[38, ]), type = "l", main = "Flux Measurements of Stars without Exo-planets", xlab = "Time", ylab = "Flux")
plot(as.numeric(train[39, ]), type = "l", xlab = "Time", ylab = "Flux")
plot(as.numeric(train[40, ]), type = "l", xlab = "Time", ylab = "Flux")
dev.off()

png(file = "images/outliers.png")
par(mfrow = c(2, 1))
plot(as.numeric(train[1, ]), type = "l", main = "A Time Series Before and After Removing Outliers", xlab = "Time", ylab = "Flux", ylim = c(-1100, 600))
plot(tsclean(as.numeric(train[1, ])), type = "l", main = "Time Series After Removing Outliers", xlab = "Time", ylab = "Flux", ylim = c(-1100, 600))
dev.off()

####
# Fourier transformations
# for the following plots, load the preprocessing and feature extraction part of
# the exoplanets.R script
###
train <- train_data

png(file = "images/fft.png")
par(mfcol = c(3, 2))
plot(as.numeric(train[1, 1:1599]), type = "l", main = "Stars with Exo-planets", xlab = "Time", ylab = "DFT", ylim = c(0, 250))
plot(as.numeric(train[2, 1:1599 ]), type = "l", xlab = "Time", ylab = "DFT", ylim = c(0, 250))
plot(as.numeric(train[4, 1:1599 ]), type = "l", xlab = "Time", ylab = "DFT", ylim = c(0, 250))
plot(as.numeric(train[38, 1:1599]), type = "l", main = "Stars without Exo-planets", xlab = "Time", ylab = "DFT", ylim = c(0, 250))
plot(as.numeric(train[39, 1:1599]), type = "l", xlab = "Time", ylab = "DFT", ylim = c(0, 250))
plot(as.numeric(train[40, 10:1599]), type = "l", xlab = "Time", ylab = "DFT", ylim = c(0, 250))
dev.off()
