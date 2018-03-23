######################################################################
#
# Get raw data for training set
#
######################################################################

library(magrittr)
source("/Users/Chaehan/Dropbox/01 IDAS Prof/06 Data Analysis/06 R Scripts/utils.R")

prefix <- gsub("^(/.*/.*?)/.*", "\\1", getwd())
source(paste0(prefix, "/Dropbox/01 IDAS Prof/06 Data Analysis/06 R Scripts/utils.R"))
set_input_output_dir()
inputDir
outputDir
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

create_survey_numeric <- function(sheet_key, start_date, end_date)
{
  survey.numeric <- create_survey_data(sheet_key, 
                                       start_date, end_date, 
                                       c("Timestamp", "I'm the Rater")) %>% 
    .[[2]]
  # remove NA
  survey.numeric %>% .[, !apply(is.na(.), 2, any)] %>% return
}

sheet_key_dd3_ecd <- "13eBL967CDTGwPKPmKRIJcsbzn3VVTHx57aDPEs4Iuh4"
# sheet_key_dd3_pod <- "1QHQy-ffOoqsxDqRhiUmFiW9hO3lsoifpYRppbtEjVS0"
sheet_key_dd3_pod <- "1kCNHIyfJ0eRu15muXFsXj7CDBZfW-2_JvZzwAIa-eNk"

sheet_key_dd4     <- "1o7Hy5pEFv4-yK_ITfnT6dpyWHDyg6F_HBAGKdGuQiTY"

start.date.1  <- "2017-03-10"
end.date.1    <- "2017-04-03"
start.date.2  <- "2017-03-10"
end.date.2    <- "2017-04-04"
start.date.3  <- "2017-04-07"
end.date.3    <- "2017-06-03"

# survey_key <- sheet_key_dd3_ecd
# start_date <- start.date.1
# end_date <- end.date.1
# descriptive_columns <- c("Timestamp", "I'm the Rater")

### 
# Get survey data
###

# get survey data for first-month-ecd (55 responses)
survey.numeric.1 <- create_survey_data(sheet_key_dd3_ecd, start.date.1, end.date.1)
# remove empty columns by column name
survey.numeric.1[[1]] %<>% 
  remove_columns(c("B599235.unique", "B599235.pleasantly", "B599235.NPS")) 
# remove empty columns by student id
survey.numeric.1[[1]] %<>% .[,!grepl("B599008", names(.))]
survey.numeric.1[[1]] %>% View

# get survey data for first-month-pod (21 responses)
survey.numeric.2 <- create_survey_data(sheet_key_dd3_pod, start.date.2, end.date.2)
survey.numeric.2[[1]] %>% dim

# get survey data for 8-weeks-ecd+pod (140 responses)
survey.numeric.3 <- create_survey_data(sheet_key_dd4, start.date.3, end.date.3)
survey.numeric.3[[1]] %>% dim

###
# define features
###
design.descriptives <- inputDir %>% paste0(., "design.descriptives.rds") %>% readRDS
emotions <- inputDir %>% paste0(., "emotions.rds") %>% readRDS %>%
  gsub("pleasantly.surprised", "pleasantly", .)
features <- c(design.descriptives, emotions)
DV <- "NPS"

readNEW <- function(DV, features, path_base, data_file = NULL)
{
  require(tidyr)
  require(dplyr)
  if (is.null(data_file)) {
    data_file <- "data.raw.rds"
    data.raw <- path_base %>% paste0(., data_file) %>% readRDS %>% as.data.frame
  } else {
    data.raw <- data_file
  }
  
  # get columns for each feature into feature-df
  # collapse each feature-df into one column
  column.list <- lapply(c(features), function(item) {
    data.raw %>%
      .[, grepl(item, names(.))]  %>%
      { 
        if (dim(.)[2] > 0) {
          gather(., key=item, value=value) %>% .[2] %>%
            setNames(., item) 
        } else {
          # print("missing value!!!")
        }
      }
  })  %>% print
  
  column.list.stripped <- column.list[!sapply(column.list, is.null)] 
  
  result.data <- column.list.stripped 
  return(result.data)
}


read_machinelearning_data <- function(DV, features, path_base, data_file = NULL)
{
  require(tidyr)
  require(dplyr)
  if (is.null(data_file)) {
    data_file <- "data.raw.rds"
    data.raw <- path_base %>% paste0(., data_file) %>% readRDS %>% as.data.frame
  } else {
    data.raw <- data_file
  }
  
  # get columns for each feature into feature-df
  # collapse each feature-df into one column
  # column.list <- lapply(c(DV, features), function(item) {
  #   data.raw %>%
  #     .[, grepl(item, names(.))]  %>%
  #     {
  #       if (dim(.)[2] > 0) {
  #         gather(., key=item, value=value) %>% .[2] %>%
  #           setNames(., item)
  #       }
  #     }
  # })
  column.list <- lapply(c(DV, features), function(item) {
    data.raw %>%
      .[, grepl(item, names(.))]  %>%
      {
        if (dim(.)[2] > 0) {
          gather(., key=item, value=value) %>% .[2] %>%
            setNames(., item)
        } else {
          # print("missing value!!!")
        }
      }
  })  %>% print
  
  # strip empty columns
  result.data <- column.list[!sapply(column.list, is.null)]
  return(result.data)
}

data1  <- read_machinelearning_data(DV, features, inputDir, survey.numeric.1[[1]]) %>% print # 5des 55 = 275 rows
data2  <- read_machinelearning_data(DV, features, inputDir, survey.numeric.2[[1]]) %>% print # 13des 21 = 273 rows
data3  <- read_machinelearning_data(DV, features, inputDir, survey.numeric.3[[1]]) %>% print # 6des 140 = 840 rows

data1 %>% View
data2 %>% View
data3 %>% names

setwd(inputDir)
rbind(data1,data2, data3) %>% print

saveRDS("data.raw.1955.rds")

data2  <- read_machinelearning_data(DV, features, inputDir, survey.numeric.2[[1]]) %>%
  dplyr::select(., one_of(c(DV, features)))

# save features
c("practical", "functional", "useful", "exciting", "creative", "unique",
  "simple", "clear", "minimalistic", "powerful", "clever", "intuitive") %>%
  saveRDS("design.descriptives.rds")

c("excited", "inspired", "pleased", "interested", "pleasantly") %>%
  saveRDS("emotions.rds")




