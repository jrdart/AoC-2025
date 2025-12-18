#clear the workspace
remove(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#required libraries
library(dplyr)
library(stringr)

#load in data

filepath <- paste0(getwd(),"/","input.txt")
input <- read.csv(filepath,
                  header=F,
                  sep="")

num_equations <- length(colnames(input))
num_numbers <- nrow(input)-1
num_digits <- 4

running_total <- 0

for (col_idx in 1:num_equations){
  curr_eqn <- paste(
    input[-nrow(input),col_idx],
    collapse=input[nrow(input),col_idx]
  )
  running_total <- running_total+
    eval(
      parse(
        text=curr_eqn))
}

print(running_total)

#part 2

running_total <- 0

symbols <- input[nrow(input),]
symbols <- str_replace_all(symbols," ","")

input <- read.csv(filepath,
                  header=F)
for (eqn_idx in 1:num_equations){
  numbers <- NULL
  for (digit_idx in 1:num_digits) {
    number_i <- str_sub(input[-(num_numbers+1),1],
                        digit_idx+((num_digits+1)*(eqn_idx-1)),
                        digit_idx+((num_digits+1)*(eqn_idx-1)))
    #print(digit_idx+((num_digits+1)*(eqn_idx-1)))
    number_i <- number_i[!number_i==" "]
    
    number_i <- paste0(number_i,collapse="")
    if(number_i!=""){
      numbers <- c(numbers,number_i)
    }
  }
  
  #print(paste0(numbers,collapse=symbols[eqn_idx]))
  
  curr_eqn <- paste0(numbers,collapse=symbols[eqn_idx])
  if(curr_eqn!=""){
    running_total <- running_total+
      eval(
        parse(
          text=curr_eqn))
  }
}

#output a checking file

big_array2 <- array(data="",
                    dim=c(5,3720))

for (col_idx in 1:3720){
  for (row_idx in 1:5){
    big_array2[row_idx,col_idx] <-
      str_sub(input[row_idx,1],col_idx,col_idx)
    
  }
}

big_array2[which(big_array2==" ")] <- ""

eq_starts <- which(big_array2[5,] %in% c("+","*"))
num_equations <- length(eq_starts)

running_total <- 0

for (eq_idx in 1:num_equations){
  if(eq_idx!=num_equations){
    num_terms <- eq_starts[eq_idx+1]-eq_starts[eq_idx]-1
  } else {num_terms <- 3720-eq_starts[eq_idx]+1}
  equation_i <- ""
  for (number_idx in 1:num_terms){
    number_i <- paste0(big_array2[1:4,(eq_starts[eq_idx]+number_idx-1)],collapse="")
    equation_i <- paste0(equation_i,
                    if(number_idx>1){symbols[eq_idx]},
                    number_i)
    if(number_idx == num_terms){
      #print(equation_i)
      #print(eval(parse(text=equation_i)))
      running_total <- running_total + 
        eval(
          parse(
            text=equation_i
          )
      )
      }
  }
}

running_total
