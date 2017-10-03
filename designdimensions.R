###################################################################################################
rm(list=ls())
library(tidyr)
library(dplyr)
library(magrittr)
library(neuralnet)
library(NeuralNetTools)

source("/Users/Chaehan/Dropbox/01 IDAS Prof/06 Data Analysis/06 R Scripts/utils.R")
dir <- "/Users/Chaehan/Dropbox/01 IDAS Prof/06 Data Analysis/06 Machine Learning"
setwd(dir)

######################################################################


trainNeuralNetwork <- function(DV, formula, training_set, testing_set, layers, mode, palette)
{
  ######################################################################
  # Linear Regression
  #
  lm.fit <- lm(formula, data=training_set)
  summary(lm.fit)
  predicted.lm <- predict(lm.fit,testing_set) # vector
  MSE.lm <- (sum((predicted.lm - testing_set[DV])^2)/nrow(testing_set) )
  
  ######################################################################
  # Neural Network
  # trainingset must only contain same variables as testing set, NOT MORE!
  nn <- neuralnet(formula,training_set,hidden=layers,linear.output=FALSE)
  # Compute Predictions from testing set. MUST remove DV!!
  predicted.values <- compute(nn,testing_set[-1])
  
  observed.neural <- testing_set[DV] # dataframe
  predicted.neural <- predicted.values$net.result # matrix
  
  # cbind(observed, predicted, (observed-predicted)/observed) %>% round(digits=2) %>% print
  # table(observed,predicted)
  
  # observed.neural.normal <- (testing_set$NPS)*(max(data$NPS)-min(data$NPS))+min(data$NPS)
  # predicted.neural.normal <- predicted.values$net.result*(max(data$NPS)-min(data$NPS))+min(data$NPS)
  # MSE.nn <- sum( ((observed.neural.normal - predicted.neural.normal)^2)/nrow(predicted.neural.normal))
  
  MSE.nn <- sum( ((observed.neural - predicted.neural)^2)/nrow(predicted.neural))
  
  c(MSE.lm,MSE.nn) %>% round(digits = 3) %>% print
  
  ######################################################################
  # Visualize Prediction Quality: NN vs. LM
  #
  ### ifelse does NOT work if ggplot object is returned!!! ###
  # pred <- ifelse(mode == "integrate", 
  #        print_both_predictions(observed.neural, predicted.neural, predicted.lm, palette),
  #        print_separate_predictions(observed.neural, predicted.neural, predicted.lm, palette)
  # )
  
  if(mode == "integrate") 
  {
    pred <- print_both_predictions(observed.neural, predicted.neural, predicted.lm, palette)
  } else {
    pred <- print_separate_predictions(observed.neural, predicted.neural, predicted.lm, palette)
  }
  
  ######################################################################
  # plot neural net with NeuralNetTools
  require(NeuralNetTools)
  plotnet(nn)
  
  return(list(nn, pred))
}


print_both_predictions <- function(DV, predicted_nn, predicted_lm, palette_label) {
  require(ggplot2)
  # coerce into dataframe: df, matrix, vector!
  data <- cbind(DV, predicted_nn, predicted_lm) %>% 
    as.data.frame %>% setNames(., c("observed", "predicted.nn", "predicted.lm"))
  
  gg <- ggplot(data, mapping = aes(x=observed)) +
    ggtitle("Observed vs. Predicted Values") +
    ylab("predicted") +
    geom_point(aes(y=predicted_lm, color="linear")) +
    geom_point(aes(y=predicted_nn, color="neural")) +
    geom_smooth(method=lm, aes(y=predicted_lm, color="linear")) +
    geom_smooth(method=loess, aes(y=predicted_nn, color="neural")) +
    scale_color_brewer(palette = palette_label) +
    theme(legend.title = element_text(margin = 0))
  
  # print(gg)
  return(gg)
}

print_separate_predictions <- function(DV, predicted_nn, predicted_lm, palette_label) {
  require(ggplot2)
  data <- cbind(DV, predicted_nn, predicted_lm) %>% 
    as.data.frame %>% setNames(., c("observed", "predicted.nn", "predicted.lm"))
  
  plot.base <- ggplot(data, mapping = aes(x = observed)) +
    ggtitle("Observed vs. Predicted Values") +
    scale_color_brewer(palette = palette_label)
  
  gg1 <- ggplot(data, mapping = aes(x=observed)) +
    geom_point(aes(y=predicted.nn, color="neural")) +
    geom_smooth(method=lm, aes(y=predicted.nn, color="neural"))
  
  gg2 <- ggplot(data, mapping = aes(x=observed)) +
    geom_point(aes(y=predicted.lm, color="linear")) +
    geom_smooth(method=lm, aes(y=predicted.lm, color="linear"))
  
  require(cowplot)
  plot_grid(gg1, gg2) %T>% print %>% return
  
}


######################################################################
#
# Collapse raw data to feature set 
#
######################################################################

get_data <- function(DV, split_ratio)
{
  # get data
  features  <- read_machinelearning_data("NPS") %>% .[[2]]
  data      <- read_machinelearning_data("NPS") %>% .[[1]] %>% 
    select(., one_of(c(DV, features)))
  
  # create formula from features
  formula <- features %>% paste (., collapse=" + ") %>% paste(DV, "~ ", .) %>%
    as.formula(env= .GlobalEnv)
  
  # get max and min of columns for scaling
  maxs <- data %>% apply(., 2, max)
  mins <- data %>% apply(., 2, min)
  
  # Use scale() and convert the resulting matrix to a data frame
  data.scaled <- as.data.frame(scale(data,center = mins, scale = maxs - mins))
  
  # Create Split (any column is fine)
  require(caTools)
  set.seed(101)
  split <-  sample.split(data.scaled[,1], SplitRatio = split_ratio)
  
  # Split based off of split Boolean Vector
  trainingset <-  subset(data.scaled, split == TRUE)  
  testingset  <-  subset(data.scaled, split == FALSE)
  
  return(list(formula, trainingset, testingset))
}

main <- function(DV, hidden_layers, mode, palette)
{
  
  data <- get_data(DV, split_ratio = 0.70)
  result <- trainNeuralNetwork(DV, formula = data[[1]], 
                               training_set = data[[2]], testing_set = data[[3]], 
                               hidden_layers, 
                               mode, palette) # Paired, RdBu
  return(result)
}

# first run
system.time(
  result <- main("NPS", 
                 hidden_layers=c(3,3,3), 
                 mode="integrate", 
                 palette = "RdBu") # Paired, RdBu
)

# prediction quality
result[[2]] 
+ scale_color_brewer("Set1")
# neural network
result[[1]] %>% plotnet 

