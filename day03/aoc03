#clear the workspace
remove(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#required libraries
library(dplyr)
library(stringr)

filepath <- paste0(getwd(),"/","input.txt")
test_input <- read.csv(filepath,
                       header=F,
                       colClasses = "character")
running_total <- 0

for (row_idx in 1:nrow(test_input)){

  temp <- data.frame(
    V1=1:str_length(test_input[row_idx,1])
  )
  
  temp <- mutate(temp,V2=as.numeric(str_sub(test_input[row_idx,1],V1,V1)))
  
  print(paste0("row ",row_idx))
  x1 <- max(temp$V2[-nrow(temp)])
  x2 <- max(temp$V2[
    (min(which(temp$V2==x1))+1):nrow(temp)
    ]
    )
  #has to be digits AFTER initial one
  running_total <- running_total+10*x1+x2
  #print(
  #  10*as.numeric(sort(temp$V2,decreasing=T)[1])+
   #   as.numeric(sort(temp$V2,decreasing=T)[2]))
  #not right as the position matters
  
  #find the max in everything except last
  #find the max excluding the earliest version of that digit
}
print(running_total)


#part2

#part 2
running_total2 <- 0
options(scipen = 10)
num_inputs <- nrow(test_input)
size_inputs <- str_length(test_input[row_idx,1])
x <- rep(0,12)

for (row_idx in 1:nrow(test_input)){
  
  temp <- data.frame(
    V1=1:str_length(test_input[row_idx,1])
  )
  
  temp <- mutate(temp,V2=as.numeric(str_sub(test_input[row_idx,1],V1,V1)))
  
  current_digits <- temp$V2[(-size_inputs+11):-size_inputs]
  
  for (digit_idx in 1:12){

    current_digits <- c(current_digits,temp$V2[(size_inputs-12+digit_idx)])

    x[digit_idx] <- max(current_digits)

    current_digits <- current_digits[-1:(-min(which(current_digits==x[digit_idx])))]

  }
  x2 <- sum(x*(10^(11:0)))
  running_total2 <- running_total2+x2

}

running_total2
