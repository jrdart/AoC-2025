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

for (tile_idx in 1:num_elements){
  
  rectangle_tiles <- c(0,0)
  
  rectangles <- (abs((input[tile_idx,1]-input[,1]))+1)*
    ((abs(input[tile_idx,2]-input[,2]))+1)
  
  if(max(rectangles)>biggest_rectangle_size){
    biggest_rectangle_size <- max(rectangles)
    rectangle_tiles[1] <- tile_idx
    rectangle_tiles[2] <- which(rectangles==max(rectangles))
    
  }
}

biggest_rectangle_size

#part 2
biggest_rectangle_size <- 0
rectangle_tiles <- c(0,0)



bottom <- input[input[,2]<=48498,]
top <- input[input[,2]>=50265,]

num_top_elements <- nrow(top)
num_bottom_elements <- nrow(bottom)

#checking the top first
biggest_rectangle_size <- 0
rectangle_tiles <- c(0,0)
for (first_tile in 1:num_top_elements){
  for(second_tile in 1:num_top_elements){
    
    
    
    rectangle <- (abs((top[first_tile,1]-top[second_tile,1]))+1)*
      ((abs(top[first_tile,2]-top[second_tile,2]))+1)
    
    #check if there are any other points inside the rectangle
    check <- any(
      top[,1]<max(top[c(first_tile,second_tile),1]) &
        top[,1]>min(top[c(first_tile,second_tile),1]) &
        top[,2]<max(top[c(first_tile,second_tile),2]) &
        top[,2]>min(top[c(first_tile,second_tile),2])
    )
    
    
    
    
    if(rectangle>biggest_rectangle_size && check==F){
      biggest_rectangle_size <- rectangle
      rectangle_tiles[1] <- first_tile
      rectangle_tiles[2] <- second_tile
      
    }
  }
}

biggest_top_rectangle <- biggest_rectangle_size

#checking the bottom ones next
biggest_rectangle_size <- 0
rectangle_tiles <- c(0,0)
for (first_tile in 1:num_bottom_elements){
  for(second_tile in 1:num_bottom_elements){
    
    
    
    rectangle <- (abs((bottom[first_tile,1]-bottom[second_tile,1]))+1)*
      ((abs(bottom[first_tile,2]-bottom[second_tile,2]))+1)
    
    check <- any(
      bottom[,1]<max(bottom[c(first_tile,second_tile),1]) &
        bottom[,1]>min(bottom[c(first_tile,second_tile),1]) &
        bottom[,2]<max(bottom[c(first_tile,second_tile),2]) &
        bottom[,2]>min(bottom[c(first_tile,second_tile),2])
    )
    
    
    
    
    if(rectangle>biggest_rectangle_size && check==F){
      biggest_rectangle_size <- rectangle
      rectangle_tiles[1] <- first_tile
      rectangle_tiles[2] <- second_tile
      
    }
  }
}

biggest_bottom_rectangle <- biggest_rectangle_size

max(biggest_top_rectangle,biggest_bottom_rectangle)
