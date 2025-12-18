#clear the workspace

remove(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#required libraries
library(dplyr)
library(stringr)

#load in data

filepath <- paste0(getwd(),"/","ranges.txt")
ranges <- read.csv(filepath,
                   header=F,
                   sep="-")

colnames(ranges) <- c("bottom","top")

filepath <- paste0(getwd(),"/","test ingredients.txt")
ingredients <- read.csv(filepath,
                        header=F,
                        sep="-")

in_ranges <- function(input_ingredient){
  which(input_ingredient>=ranges$bottom & input_ingredient<=ranges$top)
}

min_bottom <- min(ranges$bottom)
max_top <-  max(ranges$top)



blocks <- data.frame(
  bottom=0,
  top=0
)


empty_blocks <- data.frame(
  bottom=0,
  top=0
)

#start at bottom of first range
current_position <- min_bottom
current_blocks <- which(ranges$bottom==current_position)


blocks$bottom[1] <- min_bottom

current_position <- max(ranges$top[current_blocks])

repeat{
  #how many ranges are you in?
  current_blocks <- in_ranges(current_position)
  #go to the top of the current range
  current_position <- max(ranges$top[current_blocks])
  #check if youre at the final top of the all the ranges
  if(current_position==max_top){
    blocks$top[nrow(blocks)] <- current_position
    break
  }
  if(current_position==max(ranges$top[in_ranges(current_position)])){
    #are you at the maximum of all the ranges you're in?
    #if yes:
    #youre at the top of a chunk of blocks so update blocks
    blocks$top[nrow(blocks)] <- current_position
    current_position <- min(ranges$bottom[ranges$bottom>current_position])
    #set up the next block
    blocks <- rbind(blocks,c(current_position,0))
    #print(current_position)
  } else{
    #if you're in several blocks go to the top of them then start agaimn
    current_position <- max(ranges$top[current_blocks])
  }
  
  
}

sum(blocks$top-blocks$bottom+1)
