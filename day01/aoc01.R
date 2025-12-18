#clear the workspace
remove(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#required libraries
library(dplyr)
library(stringr)

filepath <- paste0(getwd(),"/","input.txt")
test_input <- read.csv(filepath,
                       header=F)

new_input <- data.frame(
  direction=rep("L",nrow(test_input)+1),
  turn=0,
  pos=50
)

R_occ <- lapply(test_input,str_detect,pattern="R")
new_input$direction[R_occ$V1] <- "R"


turns <- lapply(test_input$V1,str_extract,pattern="[:digit:]+")
new_input$turn[1:nrow(new_input)-1] <- as.numeric(unlist(turns))
new_input$turn[new_input$direction=="L"] <- new_input$turn[new_input$direction=="L"]*-1

for (idx in 2:nrow(new_input)){
  new_input$pos[idx] <- new_input$pos[(idx-1)]+new_input$turn[(idx-1)]
}

sum(new_input$pos %% 100==0)

#part 2
#brute force way

current_pos <- 50
new_input$pos_end <- 0
new_input$num_zeros <- 0


for (row_id in 1:nrow(new_input)){
  step_dir <- if(new_input$direction[row_id]=="R"){1} else {-1}
  num_zeros <- 0
  for (step in 1:abs(new_input$turn[row_id])){
    current_pos <- current_pos+step_dir
    if(current_pos %% 100 ==0){
      num_zeros <- num_zeros+1
      
    }
    if (step==abs(new_input$turn[row_id])){
      new_input$num_zeros[row_id] <- num_zeros
      new_input$pos_end[row_id] <- current_pos
    }
    
  }
}

sum(new_input$num_zeros)
