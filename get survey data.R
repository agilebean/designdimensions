######################################################################
#
# Get raw data for training set
#
######################################################################

source("/Users/Chaehan/Dropbox/01 IDAS Prof/03 Publishing/20 Design Dimensions/DD4 Analysis v2.8 STABLE.R")

start.date  <- "2017-03-01"
end.date    <- "2017-06-07"

sheet_key <- "1o7Hy5pEFv4-yK_ITfnT6dpyWHDyg6F_HBAGKdGuQiTY"
survey.numeric <- sheet_key %>% 
  create_survey_data(sheet_key, 
                     start.date, end.date, 
                     c("Timestamp", "I'm the Rater")) %>% 
  .[[2]]
# remove NA
survey.numeric %>% .[, !apply(is.na(.), 2, any)] %>% saveRDS(., file = "data.raw.rds")


######################################################################
# create items: features + emotions
######################################################################
# source("/Users/Chaehan/Dropbox/01 IDAS Prof/03 Publishing/20 Design Dimensions/DesignDimensions v1.8 n=291.R")
# features <- items.minimum %>% names %>% saveRDS(., file = "input/features.rds")
# emotions <- items.emotions.reduced %>% names %>% saveRDS(., file = "input/emotions.rds")
#

######################################################################
# create survey.numeric
######################################################################
create_survey_numeric <- function()
{
  source("/Users/Chaehan/Dropbox/01 IDAS Prof/03 Publishing/20 Design Dimensions/DD4 Analysis v2.6 end_date.R")
  survey <- create_survey_data(sheet_key, 
                               start.date, end.date,
                               c("Timestamp", "I'm the Rater"))
  
  outputDir <- "/Users/Chaehan/Dropbox/01 IDAS Prof/06 Data Analysis/06 Machine Learning/06 Data"
  setwd(outputDir)
  
  survey.descriptive  <- survey[[1]]
  survey.numeric      <- survey[[2]] %>% .[, !apply(is.na(.), 2, any)] %T>%
    saveRDS(., file="survey.numeric.rds")  
}



