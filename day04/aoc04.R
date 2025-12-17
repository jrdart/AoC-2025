#clear the workspace
remove(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#required libraries
library(dplyr)
library(stringr)

#load in data

filepath <- paste0(getwd(),"/","input.txt")
test_input <- read.csv(filepath,
                       header=F,)
new_test_input <- sapply(test_input,
                         str_replace_all,
                         c("[^@]"="0","@"="1"))

num_rows <- nrow(new_test_input)
num_cols <- str_length(new_test_input[1])

#convert to a big matrix

#num_rows <- num_rows
#num_cols <- num_cols

big_map <- matrix(
  data=0,
  nrow=num_rows+2,
  ncol=num_cols+2
)

for (row_idx in 1:num_rows){
  for(col_idx in 1:num_cols){
    big_map[row_idx+1,col_idx+1] <- as.numeric(
      str_sub(new_test_input[row_idx],col_idx,col_idx)
    )
  }
}

paper_locs <- as.data.frame(
  which(big_map==1,arr.ind=T)
)

paper_locs$checked <- 0

for (paper_idx in 1:length(paper_locs[,1])){
  if(
    sum(big_map[
      -1:+1+paper_locs$row[paper_idx],
      -1:+1+paper_locs$col[paper_idx]])<5) {
    paper_locs$checked[paper_idx] <- 1
    
  }
}
initial_paper <- sum(big_map)
sum(paper_locs$remove)


#part 2
repeat{
  #get rid of last ones
  removal_list <- paper_locs[paper_locs$remove==1,]
  for (paper_idx in 1:length(removal_list$row)){
    big_map[removal_list$row[paper_idx],removal_list$col[paper_idx]] <- 0
  }
  
  #redo the test
  paper_locs <- as.data.frame(
    which(big_map==1,arr.ind=T)
  )
  
  paper_locs$remove <- 0
  
  for (paper_idx in 1:length(paper_locs[,1])){
    if(
      sum(big_map[
        -1:+1+paper_locs$row[paper_idx],
        -1:+1+paper_locs$col[paper_idx]])<5) {
      paper_locs$remove[paper_idx] <- 1
      
    }
  }
  
  print(sum(paper_locs$remove))
  if(sum(paper_locs$remove)==0){
    break
  }
  #were any removed? if not then stop
}
print(initial_paper-sum(big_map))

