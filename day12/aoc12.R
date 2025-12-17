#adding new line
remove(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#required libraries
library(dplyr)
library(stringr)
library(readr)
#load in data

test_ind <-F



filepath <- paste0(getwd(),"/","test.txt")
test_input <- read.csv(
  filepath,
  sep=" ",
  header=F,
  col.names=paste0("V",1:7)
)


filepath <- paste0(getwd(),"/","input.txt")
full_input <- read.csv(
  filepath,
  sep=" ",
  header=F,
  col.names=paste0("V",1:7)
)

if (test_ind == T){
  input <- test_input
} else {
  input <- full_input
}

present_sizes <- data.frame(
  present=1:6,
  size=0
)

for (present_id in 1:6){
  present_sizes$size[present_id] <- sum(
    str_count(
      input[2:4+(present_id-1)*4,1],
      pattern="#"
    )
  )
}

#start at row 25

problems <- input[25:nrow(input),]

results <- data_frame(
  problem=1:nrow(problems),
  required=0,
  available=0
)

for (problem_id in 1:nrow(problems)){
  blocks_required <- sum(problems[problem_id,-1]*present_sizes$size)
  area_dims <- as.numeric(unlist(str_extract_all(problems[problem_id,1],pattern="[:digit:]+")))
  blocks_available <- area_dims[1]*area_dims[2]
  print(paste0("problem ",problem_id," blocks required: ",blocks_required," blocks available: ",blocks_available))
  results$required[problem_id] <- blocks_required
  results$available[problem_id] <- blocks_available
  }

sum(results$available>results$required)

