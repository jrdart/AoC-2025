remove(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#required libraries
library(dplyr)
library(stringr)
library(readr)
#load in data



filepath <- paste0(getwd(),"/","input.txt")
check <- read.csv(
  filepath,
  header=F)

num_cols <- str_length(check[1,1])
num_rows <- nrow(check)

input <- read.fwf("input.txt",rep(1,num_cols))

input_matrix <- array(
  data=0,
  dim=dim(input)
)

input_matrix[which(input=="S")] <- 1
input_matrix[which(input=="^")] <- 2

splits_total <- 0

for (row_idx in 2:nrow(input_matrix)){
  #first do the ones where it just moves down
  input_matrix[row_idx,which(
    input_matrix[row_idx-1,]==1 &
      input_matrix[row_idx,]==0)
  ] <- 1
  
  #also count the splits
  splits_total <- splits_total+
    sum(
      input_matrix[row_idx-1,]==1 &
        input_matrix[row_idx,]==2)
  
  #then do the ones where it splits
  input_matrix[row_idx,which(
    input_matrix[row_idx-1,]==1 &
      input_matrix[row_idx,]==2)+1
  ] <- 1
  input_matrix[row_idx,which(
    input_matrix[row_idx-1,]==1 &
      input_matrix[row_idx,]==2)-1
  ] <- 1
  
}

splits_total

#part 2

input <- read.fwf("input.txt",rep(1,num_cols))

input_matrix <- array(
  data=0,
  dim=dim(input)
)

input_matrix[which(input=="S")] <- 1
input_matrix[which(input=="^")] <- -1

for (row_idx in 2:nrow(input_matrix)){
  for(col_idx in 1:num_cols){
    #always add the one above
    if(input_matrix[row_idx,col_idx]!=-1){
      input_matrix[row_idx,col_idx] <- input_matrix[row_idx,col_idx] + max(0,input_matrix[row_idx-1,col_idx])
    }
    #if theres a splitter to the left add up + left
    if(col_idx!=1 && input_matrix[row_idx,col_idx]!=-1 && input_matrix[row_idx,col_idx-1]==-1){
      input_matrix[row_idx,col_idx] <- input_matrix[row_idx,col_idx] + max(0,input_matrix[row_idx-1,col_idx-1])
    }
    #if there's a splitter to the right add up + right
    if(col_idx!=num_cols && input_matrix[row_idx,col_idx]!=-1 && input_matrix[row_idx,col_idx+1]==-1){
      input_matrix[row_idx,col_idx] <- input_matrix[row_idx,col_idx] + max(0,input_matrix[row_idx-1,col_idx+1])
    }
    
  }
}

sum(input_matrix[nrow(input_matrix),])
