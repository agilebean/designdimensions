###################################################################################################
rm(list=ls())
library(tidyr)
library(dplyr)
library(magrittr)
library(neuralnet)
library(NeuralNetTools)
# library(car) # VIF
# devtools::install_github("agilebean/comfort")
library(comfort)  

######################################################################

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
# Get data: Extracts the feature set from raw data
#
######################################################################
get_data <- function(DV, features, split_ratio, seed = 17)
{
  # data  <- read_machinelearning_data(DV, features, inputML) %>%
  #   select(., one_of(c(DV, features)))
  require(dplyr)
  require(tidyr)
  data <- readRDS("data/data.raw.1955.rds") %>% tbl_df %>%
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
  
  # shuffle data
  set.seed(seed)
  data.scaled <- data.scaled[sample(nrow(data.scaled)),]

  # Create Split (any column is fine)
  require(caTools)
  set.seed(seed)
  split <-  sample.split(data.scaled[,1], SplitRatio = split_ratio)

  # Split based off of split Boolean Vector
  trainingset <-  subset(data.scaled, split == TRUE)
  testingset  <-  subset(data.scaled, split == FALSE)

  return(list(formula, trainingset, testingset))
}

# 
# # calculate cronbach alpha
# lapply(Dimensions, function(dim) {
#   dim %>% psych::alpha(.) %>% .$total %>% .$std.alpha %>% round(digits = 2)
# })
#
# # cronbach alpha results:
# Study1 <> Study2
# 0.80 - 0.88 Tool
# 0.80 - 0.86 Novelty
# 0.77 - 0.81 Simplicity
# 0.78 - 0.73 Energy
# 0.79 - 0.82 Overall

######################################################################
#
# Benchmarking Reference: Linear Regression
#
######################################################################
do_linear_regression <- function(DV, formula, training_set, testing_set)
{
  ######################################################################
  # Linear Regression
  #
  lm.fit <- lm(formula, data=training_set)
  # summary(lm.fit) %>% lapply(., function(x) round(x, digits=2)) # doesn't work
  
  # multicollinearity test with Variance Inflation Factors
  # lm.fit %>% car::vif()
  
  predictions <- predict(lm.fit,testing_set) # vector
  
  lm.fit$mse <- mean((testing_set[, DV] - predictions)^2) %>% 
    round(digits = 4) %T>% 
    { print(paste(., "MSE-Linear Regression")) }

  return(lm.fit)
}

######################################################################
#
# Machine Learning Methods
#
######################################################################


######################################################################
# Gradient Boosting Machines
######################################################################
train_gbm <- function(DV, formula, training_set, testing_set, 
                      n_trees, tune_grid)
{
  require(gbm)
  require(caret)
  
  # train the GBM model
  model.gbm <- caret::train(formula, 
                      method = "gbm",
                      data = training_set,
                      tuneGrid = tune_grid
  )
  
  # summarize the model.gbm
  par(las=1)
  par(mar=c(3,6,1,2))
  summary(model.gbm)
  
  # make predictions
  predictions <- predict(model.gbm, testing_set, n.trees = n_trees)
  
  # summarize accuracy
  mse <- mean((testing_set[,DV] - predictions)^2)
  
  # include mse in output and print to console
  model.gbm$mse <- mse %>% 
    round(digits = 4)  %T>%
    { print(paste(., "MSE-Gradient Boosting Machines")) }
  
  return(model.gbm)
}

######################################################################
# Random Forests
######################################################################
trainRandomForest <- function(DV, formula, training_set, testing_set, seed=17)
{
  require(randomForest)
  
  set.seed(seed)
  model.rf <- randomForest(formula, data = training_set)
  
  # model.rf <- caret::train(formula, 
  #                          data = training_set,
  #                          method = "rf",
  #                          tuneGrid = expand.grid(.mtry = c(3,4,5)),
  #                          # trControl = trainControl(method = "cv", number = 10),
  #                          metric = "RMSE"
  #                          )

  # predict
  # predictions <- predict(model.rf, newdata = testing_set)
  predictions <- predict(model.rf, data = testing_set)
  
  # summarize accuracy
  mse <- mean((testing_set[,DV] - predictions)^2)
  
  # include mse in output and print to console
  model.rf$mse <- mse %>%
    round(digits = 4)  %T>%
    { print(paste(., "MSE-Random Forests")) }

  ## tricky: only for randomForest() not caret::train()!
  # model.rf$plot.imp <- varImpPlot(model.rf) %>% recordPlot

  # return(list(model, plot.pred, plot.imp))
  return(model.rf)
}

######################################################################
# Neural Network
######################################################################
trainNeuralNetwork <- function(DV, formula, training_set, testing_set,
                               hidden_layers, mode, predicted_lm, palette)
{
  ######################################################################
  # train neural network

  # trainingset must only contain same variables as testing set, NOT MORE!
  nn <- neuralnet(formula,training_set,hidden=hidden_layers,linear.output=FALSE)

  # Compute Predictions from testing set. MUST remove DV!!
  predicted.values <- compute(nn,testing_set[-1])


  predicted.neural <- predicted.values$net.result # matrix
  observed.neural <- testing_set[DV] # dataframe

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
    pred <- print_both_predictions(observed.neural, predicted.neural, predicted_lm, palette)
  } else if (mode == "separate") {
    pred <- print_separate_predictions(observed.neural, predicted.neural, predicted_lm, palette)
  } else if (mode == "neuralBenchmark")
  {
    pred <- NULL
  }

  ######################################################################
  # plot neural net with NeuralNetTools
  if (!(mode=="neuralBenchmark"))
  {
    require(NeuralNetTools)
    plotnet(nn)
  }
  return(list(nn, pred, MSE.nn))
}


######################################################################
#
# Mxnet: custom stopping criterion
#
######################################################################
mx.callback.train.stop <- function(tol = 1e-3,
                                   mean.n = 1e2,
                                   period = 100,
                                   min.iter = 100)
{
  function(iteration, nbatch, env, verbose = TRUE) {
    if (nbatch == 0 & !is.null(env$metric)) {
      continue <- TRUE
      acc.train <- env$metric$get(env$train.metric)$value
      if (is.null(env$acc.log)) {
        env$acc.log <- acc.train
      } else {
        if ((abs(acc.train - mean(tail(env$acc.log, mean.n))) < tol &
             abs(acc.train - max(env$acc.log)) < tol &
             iteration > min.iter) |
            acc.train == 1) {
          cat("Training finished with final accuracy: ",
              round(acc.train * 100, 2), " %\n", sep = "")
          continue <- FALSE
        }
        env$acc.log <- c(env$acc.log, acc.train)
      }
    }
    if (iteration %% period == 0) {
      cat("[", iteration,"]"," training accuracy: ",
          round(acc.train * 100, 2), " %\n", sep = "")
    }
    return(continue)
  }
}


######################################################################
#
# trainMxnetNeural
#
######################################################################
trainMxnetNeural <- function(DV, formula, training_set, testing_set,
                             hidden_nodes, mode, predicted_lm, palette)
{
  require(mxnet)
  mx.set.seed(0)

  train.x <- training_set[-1] %>% data.matrix
  train.y <- training_set[,1] # must be a vector <> [1] returns dataframe!
  test.x <- testing_set[-1]  %>% data.matrix
  test.y <- testing_set[,1] # must be a vector <> [1] returns dataframe!

  model <- mx.mlp(data=train.x, label=train.y,
                  hidden_node=hidden_nodes, out_node=1,
                  # activation="tanh",
                  out_activation="softmax",
                  num.round=2000, array.batch.size=100,
                  learning.rate=0.01, momentum=0.9,
                  eval.metric=mx.metric.accuracy,
                  array.layout="rowmajor",
                  # device = mx.ctx.default(), # throws error "unused argument (device ...)"
                  epoch.end.callback = mx.callback.train.stop()
  )
  ## Auto detect layout of input matrix, use rowmajor..
  predicted.neural = predict(model, test.x, array.layout = "rowmajor")
  observed.neural <- testing_set[DV] # dataframe

  ############################
  # FROM HERE: sucks!
  mean((predicted.neural-test.y)^2)

  pred.label = max.col(t(predicted.neural))-1
  pred.table <- table(pred.label, test.y) %>% print

  # predicted.neural <- predicted.neural$net.result # matrix
  observed.neural <- testing_set[DV] # dataframe
  MSE.nn <- sum( ((observed.neural - pred.label)^2)/nrow(pred.label))
  MSE.nn %>% round(digits = 4) %>% paste(., "MSE-Neural Network") %>% print

  # END OF: sucks!
  ############################

  if(mode == "integrate")
  {
    pred <- print_both_predictions(observed.neural, predicted.neural, predicted_lm, palette)
  } else if (mode == "separate") {
    pred <- print_separate_predictions(observed.neural, predicted.neural, predicted_lm, palette)
  } else if (mode == "neuralBenchmark")
  {
    pred <- NULL
  }
  return(list(model, pred, MSE.nn))
}

######################################################################
#
# MAIN
#
######################################################################

main <- function(item_type=NULL, hidden_layers=NULL, palette="Set1", seed = 17, n_trees = 500)
{
  #########################################################################################
  # define DV
  DV <- "NPS"

  #########################################################################################
  # define features
  #
  design.descriptives <-  readRDS("data/design.descriptives.rds")

  emotions <- readRDS("data/emotions.rds") %>%
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


  #########################################################################################
  # generate training/testing set
  #
  set.seed(seed)
  data.list <- get_data(DV, features, split_ratio = 0.70, seed)
  formula     <- data.list[[1]] # contains all features
  trainingset <- data.list[[2]]
  testingset  <- data.list[[3]]

  #########################################################################################
  # create benchmark reference: mse (linear regression)
  #
  predicted.lm <- do_linear_regression(DV, formula, trainingset, testingset)

  #########################################################################################
  # run machine learning model
  #
  library(doParallel)
  cluster.new <- makeCluster(detectCores() -1)
  registerDoParallel(cluster.new)
  
  set.seed(seed)
  if (mode=="neuralNetwork")
  {
    result <- trainNeuralNetwork(DV, formula,
                                 trainingset, testingset,
                                 hidden_layers=hidden_layers,
                                 mode="integrate",  # print predictions 1
                                 # mode="separate",     # print predictions 2
                                 predicted.lm,
                                 palette = palette) # Paired, RdBu

  } else if (mode=="neuralMxnet") {

    result <- trainMxnetNeural(DV, formula,
                               trainingset, testingset,
                               hidden_nodes = 5,
                               mode="integrate",  # print predictions 1
                               # mode="separate",     # print predictions 2
                               predicted.lm,
                               palette = palette) # Paired, RdBu


  } else if (mode=="boostedDecisionTrees") {

    result <- train_boosted_decision_tree(DV, formula, trainingset, testingset,
                                          palette = palette) # Paired, RdBu

  } else if (mode == "gbm") {
    
    tune.grid <- expand.grid(.interaction.depth = c(3, 4, 5), 
                             .n.trees = c(100, 150, 200),
                             .shrinkage = c(0.01, 0.05, 0.1),
                             .n.minobsinnode = c(3, 5, 7))
    
    result <- train_gbm(DV, formula, trainingset, testingset,
                        n_trees = 100, tune_grid = tune.grid)
    
  } else if (mode=="randomForest") {

    result <- trainRandomForest(DV, formula, trainingset, testingset, seed)

  } else if (mode=="neuralBenchmark") {

    # prepare k iterations to calculate error
    # set.seed(450)
    cv.error <- NULL
    k <- 1
    require(plyr)
    pbar <- create_progress_bar('text')
    pbar$init(k)

    for(i in 1:k)
    {
      result <- main_neural_network(DV, features,
                                    hidden_layers=c(2,2),
                                    # mode="integrate", # print predictions integrate | separate
                                    mode="neuralBenchmark",
                                    # palette = "RdBu") # Paired, RdBu
                                    palette = palette) # Paired, RdBu
      cv.error[i] <- result[[3]]
      pbar$step()
    }
    mean(cv.error)

    boxplot(cv.error,xlab='MSE CV',col='cyan',
            border='blue',names='CV error (MSE)',
            main='CV error (MSE) for NN',horizontal=TRUE)
    
  } else if (mode =="vif") {
    
    # multicollinearity test with Variance Inflation Factors
    result <- lm(formula, data=trainingset) %>% car::vif() %>% round(digits = 2)

  }
  stopCluster(cluster.new)

  return(result)
}

# mode <- "neuralNetwork" # 1l-3n:.0282,1l-2n:.0283, 2l-5n:.0261,3l-3n:
# mode <- "neuralMxnet"
# mode <- "neuralBenchmark"
# mode <- "boostedDecisionTrees"
# mode <- "gbm"
mode = "randomForest"
# mode <- "vif"

system.time(
  # result <- main("design", c(4,3,3))
  # result <- main("emotion",  c(5,3,2), palette = "Accent")
  # result <- main("emotion",  c(3), palette = "Dark2")
  # result <- main("emotion",  c(3), palette = "Pastel1")
  # result <- main("emotion",  c(3), palette = "Set1")
  # result <- main("emotion",  c(3), palette = "Set2")
  # result <- main("emotion",  c(3), palette = "Set3")
  # result <- main(, c(5,5,5))
  # result <- main(, c(7))

  # result <- main()
  result <- main(, seed = 6)
  # result <- main("design")
  # result <- main("emotion")
)

result.rf <- NULL
result.rf$all <- result
# result.rf$design <- result
result.rf$emotion <- result
result.rf$emotion$mse

# result.gbm$all <- result
# result.gbm$design <- result
# result.gbm$emotion <- result
# result.gbm$emotion$mse

result.gbm$all$results %>% filter(RMSE==min(RMSE))
# show variable importance (shown in Var)
result %>% summary
result %>% varImp

# show all results and describe best configuration (last line)
result

# show best configuration
result$results %>% 
  filter(RMSE == min(RMSE))


#### random forests
result[[1]] # model
result[[2]] # predictions
result[[3]] # variable importance

#### neural
# prediction quality
result[[2]] # predictions
result[[2]] + scale_color_brewer("Paired") # changes but not with this palette
# neural network
result[[1]] %>% plotnet






#########################################################################################
# generate training/testing set
#
library(dplyr)
design.descriptives <-  readRDS("data/design.descriptives.rds")
emotions <- readRDS("data/emotions.rds") %>%
  gsub("pleasantly.surprised", "pleasantly", .)
features <- c(design.descriptives, emotions)
DV <- "NPS"
data.list <- get_data(DV, features, split_ratio = 0.70)
formula1    <- data.list[[1]] # contains all features
trainingset <- data.list[[2]]
testingset  <- data.list[[3]]


fit1 <- gbm::gbm(formula = formula1,
           data = trainingset,
           # distribution = "gaussian",
           verbose = TRUE)
fit1 %>% attributes
fit1 %>% summary

library(gbm)
library(caret)
set.seed(1)
fit2 <- caret::train(formula1, 
                     method = "gbm",
                     data = trainingset)
fit2
fit2 %>% summary
fit2$results %>% filter(RMSE == min(RMSE))




