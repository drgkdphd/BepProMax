# Usage example
# Read csv file
datafile<- read.csv("C:/Users/DRGKD/Desktop/BepProMax/extdata/datafile.csv", header = TRUE)
View(datafile.csv)
library(readxl)
library(tidyverse)
library(Deriv)
library(ggplot2)
library(ggthemes)
library(BepProMax)
results <- BepProMax(datafile)
print(results)
