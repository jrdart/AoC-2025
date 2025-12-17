#clear the workspace

remove(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#required libraries
library(dplyr)
library(stringr)

filepath <- paste0(getwd(),"/","input.txt")
test_input <- read.csv(filepath,
                       header=F)

num_ranges <- length(test_input)/2
running_total <- 0

for (range_id in 1:num_ranges){
  for (value in test_input[1,(2*range_id-1)]:test_input[1,(2*range_id)]){
    if (str_detect(as.character(value),"(^[0-9]+)(?=\\1$)")){
      print(value)
      running_total <- running_total+value
    }
  }
}

running_total

str_detect("121212","(^[0-9]+)(?=\\1+$)")

#part 2


running_total <- 0

for (range_id in 1:num_ranges){
  for (value in test_input[1,(2*range_id-1)]:test_input[1,(2*range_id)]){
    if (str_detect(as.character(value),"(^[0-9]+)(?=\\1+$)")){
      print(value)
      running_total <- running_total+value
    }
  }
}

running_total

