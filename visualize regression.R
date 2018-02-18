rm(list=ls())
library(tidyr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(stargazer)
library(rio)
# source("/Users/Chaehan/Dropbox/01 IDAS Prof/06 Data Analysis/06 R Scripts/utils.R")
# prefix <- gsub("^(/.*/.*?)/.*", "\\1", getwd())
# source(paste0(prefix, "/Dropbox/01 IDAS Prof/06 Data Analysis/06 R Scripts/utils.R"))
# set_input_output_dir()
# inputDir
# outputDir

library(comfort)
library(visreg) # visreg > can be replaced by ggplot
library(arm) # coefplot() > great! with confidence intervals!

inputDir <- inputML
outputDir <- "/Users/Chaehan/Google Drive/03 Publishing/20 Design Dimensions/_output"
setwd(inputDir)
setwd(outputDir)

Novelty     <- c("exciting", "unique", "creative") # alternative name: Modern
Energy      <- c("powerful",  "clever", "intuitive") # "clever" replaced "elegant"
Simplicity  <- c("clear", "simple", "minimalistic") # "simple" replaced "dynamic"
Tool        <- c("practical", "functional", "useful")

Dimensions  <- c("Novelty", "Energy", "Simplicity", "Tool")
Emotions    <- c(
  "interested"
  , "inspired"
  # , "pleasantly.surprised"
  , "pleasantly"
  , "pleased"
  , "excited"
)


do_linear_regression <- function(data, DV, regressors)
{
  # create model: lm over regressors
  formula <- regressors %>% rev %>% paste (., collapse=" + ") %>% paste(DV, "~ ", .) %>%
    as.formula(env= .GlobalEnv)
  model <- lm(formula, data)
  
  ### doesn't work bec. coefplot removes 1st element=Intercept!
  # model$coefficients %<>% sort 
  
  # print regression coefficients > wonderful!
  plot.coef <- coefplot(model, mar = c(1,4,5,1),
                        col.pts ="darkblue") %>% recordPlot
  
  return(list(model, plot.coef))
}

# # # # # # # # 
# BUG: Simplicity not shown!
# 

create_df_from_dim_names <- function(dim_names, DV, input_file)
{
  dim.list <- lapply(dim_names, function(dimension_name)
  {
    # Create name for design dimension dataframe per id, e.g. "Novelty"
    dim.item.names <- get(dimension_name)  %>% print
    df_from_items <- lapply(dim.item.names, function(item)
    {
      input_file[, grepl(item, names(input_file)), drop=FALSE]
    }) %>% as.data.frame
    
    dim.means <- rowMeans(df_from_items)
    return(dim.means)
  })
  
  if (length(dim_names) == 1)
  {
    print("Emotions")
    dim.df <- dim.list %>% setNames(., "Emotions") 
  } else {
    dim.df <-  dim.list %>% setNames(., Dimensions)
  }
  dim.df %>% as.data.frame %>% cbind(input_file[DV], .) %>% 
    return(.)
}

create_df_from_items <- function(dim_item_names, input_file)
{
  df_from_items <- lapply(dim_item_names,
                          function(item) input_file[, grepl(item, names(input_file)),
                                                    drop=FALSE]) %>% as.data.frame
  return(df_from_items)
}

main <- function()
{
  require(dplyr)
  # get data
  DV        <- "NPS"
  # data.list <- read_machinelearning_data("NPS", "data.raw.rds")
  
  design.descriptives <- inputDir %>% paste0(., "design.descriptives.rds") %>% readRDS
  emotions <- inputDir %>% paste0(., "emotions.rds") %>% readRDS %>%
    gsub("pleasantly.surprised", "pleasantly", .)
  features <- c(design.descriptives, emotions)
  DV <- "NPS"
  
  setwd(inputDir)
  data.raw  <- readRDS("data.raw.1955.rds")  
  
  # create formula from features
  formula <- features %>% paste (., collapse=" + ") %>% paste(DV, "~ ", .) %>%
    as.formula(env= .GlobalEnv)
  
  # fm <- glm(formula, data, family = binomial)
  
  # model1: lm over all (features + emotions)
  model.all <- do_linear_regression(data.raw, DV, c(features, emotions))

  # model2: lm over only features
  model.design.descriptives <- do_linear_regression(data.raw, DV, design.descriptives)
  
  # model3: lm over only emotions
  model.emotions <- do_linear_regression(data.raw, DV, emotions)
  
  ##########################################################
  
  # model4: lm over only Design Dimensions
  # 

  model.dimensions <- Dimensions %>% 
    create_df_from_dim_names(., DV, data.raw) %>%  
    do_linear_regression(., DV, Dimensions)
  
  # model5: lm over only emotions
  df.emotions <- create_df_from_dim_names("Emotions", DV, data.raw) %>% 
    cbind(., create_df_from_items(Emotions, data.raw))
  
  model.Emotions <- df.emotions %>% 
                    do_linear_regression(., DV, "Emotions")
  
  
  fit <- model.Emotions[[1]]
  
  # pre-model5: plot basic scatterplot for publication
  # lm.Emotions <- 
  ggplot(df.emotions, aes(x=Emotions, y=NPS)) +
    ### see weights better in black-white than grey
    theme_bw() +
    geom_point(position = "jitter", size = 2, alpha = 0.2, color="darkblue") +
    # geom_smooth(method = lm, color = "grey", size = 1, alpha = 0.2) + 
    # geom_line(stat = "smooth", method="lm", color="orange")
    geom_smooth(method = lm, color = "orange", size = 1, alpha = 0.2)
  
  # model5: plot linear regression
  gg.Emotions <- ggplot(df.emotions, aes(x=Emotions, y=NPS)) +
    ### see weights better in black-white than grey
    theme_bw() +
    geom_smooth(method = "lm") +
    
    geom_point(position="jitter", size = 2, alpha = 0.25) +
    geom_point(position="jitter", size = 3, alpha = 0.25, 
               aes(frame = Emotions), color = "red") +
    
    ### moving vertical line
    geom_vline(aes(xintercept = Emotions, frame = Emotions), lty = 2, color = "red") +
    
    ### moving y line from fits
    # stat_function(fun=exp, geom="line", aes(colour="exp")) +
    geom_line(aes(frame = Emotions), color = "red", data = fit) 
    
  # gganimate(gg.Emotions, "gg.Emotions.html")
  require(gganimate)
  gg <- gganimate(gg.Emotions, interval = .5, title_frame = FALSE, "gg.Emotions.gif")
  
  # model5: show regression coefficients
  ### standardize: scale()!
  ### Todo: assign output to variable doesn't work! Capture?
  lm(scale(NPS) ~scale(Emotions) , data = df.emotions) %>% 
    stargazer(., out = "model.nps-emotion.html", type = "text")
  
  # ### 3D
  # require(Rcmdr)
  # dataframe <- cbind(df.dimensions, df.emotions$Emotions)
  # NPS <- dataframe$NPS
  # Emotions <- dataframe$`df.emotions$Emotions`
  # Novelty <- dataframe$Novelty
  # Tool <- dataframe$Tool
  # scatter3d(Novelty, NPS, Emotions)
  # scatter3d(Tool, NPS, Emotions)
  # ###
  
  # require(QuantPsyc)
  # lm.beta(model.Emotions[[1]])

  ## visreg: visualizes regressions
  # visreg(gg = TRUE) does NOT work
  # par("mar" = c(4.5,5,1,1))
  # plot.list <- features %>% .[-1] %>% 
  #   lapply(., function(item) {
  #     visreg(model, item, plot=FALSE) # %>% plot(., gg=TRUE)
  #     })

  # require(cowplot)
  # plot <- plot_grid(plot.list)
  
  return(list(model.all, model.design.descriptives, model.emotions, 
              model.dimensions, model.Emotions, gg))
}

system.time(
  models <- main()  
)

models[[1]][[2]] %>% replayPlot # all
models[[2]][[2]] %>% replayPlot # design descriptives
models[[3]][[2]] %>% replayPlot # emotions
models[[4]][[2]] %>% replayPlot # Design Dimensions
models[[5]][[1]] %>% print # lm(Emotions)
models[[6]]      %>% print # gganimate ojbect

# fit <- lm(Ozone ~ Solar.R + Wind + Temp + I(Wind^2) + I(Temp^2) +
#             I(Wind*Temp)+I(Wind*Temp^2) + I(Temp*Wind^2) + I(Temp^2*Wind^2),
#           data=airquality)
# visreg2d(fit,x="Wind",y="Temp",plot.type="persp")


