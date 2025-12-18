remove(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#required libraries
library(dplyr)
library(stringr)
library(readr)
#load in data



filepath <- paste0(getwd(),"/","input.txt")
input <- read.csv(
  filepath,
  header=F)

colnames(input) <- c("X","Y")
num_elements <- nrow(input)
biggest_rectangle_size <- 0

for (tile_idx in 1:2){
  
  rectangle_tiles <- c(0,0)
  
  rectangles <- (abs((input[tile_idx,1]-input[,1]))+1)*
                      ((abs(input[tile_idx,2]-input[,2]))+1)
  
  if(max(rectangles)>biggest_rectangle_size){
    biggest_rectangle_size <- max(rectangles)
    rectangle_tiles[1] <- tile_idx
    rectangle_tiles[2] <- which(rectangles==max(rectangles))
    
  }
}

#part 2

centre <- c(
  round(mean(input[,1])),
  round(mean(input[,2])))

transformed_input <- input
transformed_input[,1] <- transformed_input[,1]-min(transformed_input[,1])+1
transformed_input[,2] <- transformed_input[,2]-min(transformed_input[,2])+1

big_matrix <- array(
  data=0,
  dim=c(max(transformed_input[,2]),max(transformed_input[,1]))
)
