###################################################################################################
rm(list=ls())
library(tidyr)
library(dplyr)
library(magrittr)
library(neuralnet)
library(NeuralNetTools)

prefix <- gsub("^(/.*/.*?)/.*", "\\1", getwd())

if (prefix=="/Users/Chaehan")
{
  source(paste0(prefix, "/Dropbox/01 IDAS Prof/06 Data Analysis/06 R Scripts/utils.R"))
} else {
  source("/home/chaehan/Dropbox/06 machine learning/06 ML Scripts/_common/utils.R")
}

set_input_output_dir()
inputDir 
outputDir

######################################################################

do_linear_regression <- function(DV, formula, training_set, testing_set)
{
  ######################################################################
  # Linear Regression
  #
  lm.fit <- lm(formula, data=training_set)
  summary(lm.fit)
  predicted.lm <- predict(lm.fit,testing_set) # vector
  MSE.lm <- (sum((predicted.lm - testing_set[DV])^2)/nrow(testing_set) ) %>% 
    round(digits = 4)
  MSE.lm %>% paste(., "MSE-Linear Regression") %>% print
  
  return(predicted.lm)
}

trainNeuralNetwork <- function(DV, formula, training_set, testing_set, layers, mode, palette)
{
  
  predicted.lm <- do_linear_regression(DV, formula, training_set, testing_set)
  
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
  
  MSE.nn %>% round(digits = 4) %>% paste(., "MSE-Neural Network") %>% print
  
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
  } else if (mode == "separate") {
    pred <- print_separate_predictions(observed.neural, predicted.neural, predicted.lm, palette)
  }
  
  ######################################################################
  # plot neural net with NeuralNetTools
  require(NeuralNetTools)
  plotnet(nn)
  
  return(list(nn, pred, MSE.nn))
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
  
  gg1 <- plot.base +
    geom_point(aes(y=predicted.nn, color="neural")) +
    geom_smooth(method=lm, aes(y=predicted.nn, color="neural"))
  
  gg2 <- plot.base +
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
get_data <- function(DV, features, split_ratio)
{
  # data  <- read_machinelearning_data(DV, features, inputDir) %>%  
  #   select(., one_of(c(DV, features)))
  require(dplyr)
  require(tidyr)
  setwd(inputDir)
  data <- readRDS("data.raw.1955.rds") %>% tbl_df %>% 
    # explicit dplyr call necessary -> unused argument error (one_of)!!
    dplyr::select(., one_of(c(DV,features))) 
  
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


main_neural_network <- function(DV, features, hidden_layers, mode, palette)
{
  
  data.list <- get_data(DV, features, split_ratio = 0.70)
  
  formula     <- data.list[[1]]
  trainingset <- data.list[[2]]
  testingset  <- data.list[[3]]
  
  
  result <- trainNeuralNetwork(DV, formula, 
                               trainingset, testingset, 
                               hidden_layers, 
                               mode, palette) # Paired, RdBu
  return(result)
}

trainRandomForest <- function(DV, features, formula, 
                              training_set, testing_set, mode, palette)
{
  require(randomForest)
  model <- randomForest(formula, ntree = 500, data = training_set) 
  mse <- model %>% .$mse %>% .[length(.)] %>% round(digits = 4) %T>% 
  { print(paste(., "MSE-Random Forests")) }
  
  # predict
  prediction <- predict(model, newdata = testing_set)
  
  # table(pred, testing_set[,DV]) 
  pred.plot <- plot(prediction) %>% recordPlot
  
  imp.plot <- varImpPlot(model) %>% recordPlot
  
  return(list(model, pred.plot, imp.plot))
  
}

main_boosted_decision_tree <- function(DV, features, mode, palette)
{
  require(gbm)
  # load data
  data.list <- get_data(DV, features, split_ratio = 0.70)
  formula     <- data.list[[1]]
  trainingset <- data.list[[2]]
  testingset  <- data.list[[3]]
  
  do_linear_regression(DV, formula, trainingset, testingset)
  
  fit <- gbm(formula = formula, 
             data = trainingset,
             distribution="gaussian")
  
  # summarize the fit
  par(las=1)
  par(mar=c(3,6,1,2))
  summary(fit) 
  # make predictions
  predictions <- predict(fit, testingset, n.trees = 4)
  # summarize accuracy
  mse <- mean((testingset[,DV] - predictions)^2)
  mse %>% round(digits = 4) %>% paste(., "MSE-Boosted Decision Trees")%>% print
  
  return(fit)
}


main_random_forest <- function(DV, features, mode, palette)
{
  
  data <- get_data(DV, features, split_ratio = 0.70)
  formula     <- data[[1]]
  trainingset <- data[[2]]
  testingset  <- data[[3]]
  
  do_linear_regression(DV, formula, trainingset, testingset)
  
  result <- trainRandomForest(DV, features, formula, 
                              trainingset, testingset, 
                              mode, palette) # Paired, RdBu
  return(result)
}


main <- function(item_type=NULL) 
{
  # define DV
  DV <- "NPS"  
  # define features
  design.descriptives <- inputDir %>% paste0(., "design.descriptives.rds") %>% readRDS
  
  emotions <- inputDir %>% paste0(., "emotions.rds") %>% readRDS %>%
    gsub("pleasantly.surprised", "pleasantly", .)
  
  if(is.null(item_type))
  {
    features <- c(design.descriptives, emotions)
  } else if(item_type=="design")
  {
    features <- design.descriptives 
  } else if (item_type=="emotion")
  {
    features <- emotions
  }
  
  if (mode=="neuralNetwork")
  {
    result <- main_neural_network(DV, features, 
                                  hidden_layers=c(5,5,5),
                                  # mode="integrate", # print predictions integrate | separate
                                  mode="separate", 
                                  # palette = "RdBu") # Paired, RdBu
                                  palette = "Paired") # Paired, RdBu
  } else if (mode=="neuralBenchmark") {
    {
      # prepare k iterations to calculate error
      set.seed(450)
      cv.error <- NULL
      k <- 2
      require(plyr) 
      pbar <- create_progress_bar('text')
      pbar$init(k)
      
      for(i in 1:k)
      {
        result <- main_neural_network(DV, features, 
                                      hidden_layers=c(1),
                                      # mode="integrate", # print predictions integrate | separate
                                      mode="separate", 
                                      # palette = "RdBu") # Paired, RdBu
                                      palette = "Paired") # Paired, RdBu
        cv.error[i] <- result[[3]]
        pbar$step()
      }
      mean(cv.error)
      boxplot(cv.error,xlab='MSE CV',col='cyan',
              border='blue',names='CV error (MSE)',
              main='CV error (MSE) for NN',horizontal=TRUE)
    }
    
  } else if (mode=="boostedDecisionTrees") {
    
    result <- main_boosted_decision_tree(DV, features, 
                                         mode="integrate",
                                         palette = "RdBu") # Paired, RdBu
  } else if (mode=="randomForest") {
    
    result <- main_random_forest(DV, features,
                                 mode="integrate",
                                 palette = "RdBu") # Paired, RdBu
  } 
  return(result)
}

# mode <- "neuralNetwork" # 1l-3n:.0282,1l-2n:.0283, 2l-5n:.0261,3l-3n:
mode <- "neuralBenchmark"
# mode <- "boostedDecisionTrees"
# mode = "randomForest"
system.time(
  result <- main("design")
  # result <- main("emotion")
  # result <- main()
)




#### random forests
result[[1]] # model
result[[2]] # predictions
result[[3]] # variable importance

#### neural
# prediction quality
result[[2]] # predictions
result[[2]] + scale_color_brewer("Paired")
# neural network
result[[1]] %>% plotnet 

