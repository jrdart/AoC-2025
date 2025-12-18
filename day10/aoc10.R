remove(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#required libraries
library(dplyr)
library(stringr)
library(readr)
library(gtools)
#load in data

test_ind <- F

if(test_ind==T){
  filepath <- paste0(getwd(),"/","test.txt")
  input <- read.csv(
    filepath,
    sep=" ",
    header=F,
    col.names=paste0("V",1:8))  
}

if(test_ind==F){
  filepath <- paste0(getwd(),"/","input.txt")
  input <- read.csv(
    filepath,
    sep=" ",
    header=F,
    col.names=paste0("V",1:15))
}

#work out the indicators
indicators <- input[,1]
indicators <- indicators %>% str_replace_all(c("\\[|\\]"="","\\."="0","\\#"="1"))

#work out the joltage
joltage_locs <- sapply(input,str_detect,pattern="\\{")
joltage <- t(input)[t(joltage_locs==1)]

#work out the buttons
button_locs <- sapply(input,str_detect,pattern="\\(")
buttons <- t(input)[t(button_locs==1)]
num_buttons <- rowSums(button_locs)

num_problems <- nrow(input)

coeffs <- list("")

for(indicator_index in 1:str_length(indicators[problem_id])){
  button_indexes <- 1:num_buttons[problem_id]
  if (problem_id>1){
    button_indexes <- button_indexes+sum(num_buttons[1:(problem_id-1)])
  }
  coeffs[[indicator_index]] <- 
    c(which(
      str_detect(buttons[button_indexes],as.character(indicator_index-1)
      )
    ))
  
}

target <- unlist(str_extract_all(indicators[1],"\\d{1}"))


#define a function that takes in presses and coefficients for a problem and gives you the output
test_presses <- function(presses_input,coeffs_input){
  output <- sum(presses_input[coeffs_input[[1]]])
  num_coeffs <- length(coeffs_input)
  if(num_coeffs>1){
    for (function_idx in 2:num_coeffs){
      output <- c(output,
                  (sum(presses_input[coeffs_input[[function_idx]]])))
    }
  }
  return(output)
}

#set up the answer df
solution <- data.frame(
  problem=1:num_problems,
  presses=99999
)

for (problem_id in 1:num_problems){
  #set up the coefficients
  #set up the target
  coeffs <- list("")
  
  for(indicator_index in 1:str_length(indicators[problem_id])){
    button_indexes <- 1:num_buttons[problem_id]
    if (problem_id>1){
      button_indexes <- button_indexes+sum(num_buttons[1:(problem_id-1)])
    }
    coeffs[[indicator_index]] <- 
      c(which(
        str_detect(buttons[button_indexes],as.character(indicator_index-1)
        )
      ))
    
  }
  
  target <- unlist(str_extract_all(indicators[problem_id],"\\d{1}"))
  
  for (buttons_pressed in 1:num_buttons[problem_id]){
    possible_combs <- combn(num_buttons[problem_id],buttons_pressed)
    for (comb_id in 1:ncol(possible_combs)){
      presses <- rep(0,num_buttons[problem_id])
      presses[possible_combs[,comb_id]] <- 1
      if(all(
        test_presses(presses,coeffs) %%2 ==target
      )) {
        #print(paste0("presses for problem ",problem_id))
        #print(presses)
        solution$presses[problem_id] <- min(solution$presses[problem_id],buttons_pressed)
        
      }
    }
  }
  
}

sum(solution$presses)

#part 2

#can you define a function to solve 0101010 for given buttons (given button id
#inputs are target and which buttons you're using

target_input <- unlist(str_extract_all(indicators[1],"\\d{1}"))
button_id <- 1

#need to work out coeffs for these buttons

solve0101 <- function(target_input,button_id){
  if(sum(as.numeric(target_input))==0){
    return(0)
  } else if (any(as.numeric(target_input)<0)){
    #print("went negative with")
    #print(paste0(target_input,collapse=""))
    return(Inf)} else if (all(even(target_input))){
      return(2*solve0101(target_input/2,button_id))
    } else {
      
      potential_presses <- NULL
      coeffs <- list("")
      temp_solution <- Inf
      for(indicator_index in 1:length(target_input)){
        button_indexes <- 1:num_buttons[button_id]
        if (button_id>1){
          button_indexes <- button_indexes+sum(num_buttons[1:(button_id-1)])
        }
        coeffs[[indicator_index]] <- 
          c(which(
            str_detect(buttons[button_indexes],as.character(indicator_index-1)
            )
          ))
        
      }
      #print(coeffs)
      
      
      for (buttons_pressed in 1:num_buttons[button_id]){
        possible_combs <- combn(num_buttons[button_id],buttons_pressed)
        for (comb_id in 1:ncol(possible_combs)){
          presses <- rep(0,num_buttons[button_id])
          presses[possible_combs[,comb_id]] <- 1
          if(all(
            test_presses(presses,coeffs) %% 2 ==target_input %% 2
          )) {
            #print(paste0("presses for problem ",problem_id))
            # print(presses)
            temp_solution <- min(temp_solution,buttons_pressed)
            
            potential_presses <- rbind(potential_presses,presses)
            #print(potential_presses)
            #print(test_presses(potential_presses[nrow(potential_presses),],coeffs))
          }
        }
      }
      #print(potential_presses)
      temp_solution <- Inf
      if(length(potential_presses==0)){
        for(recursion_id in 1:nrow(potential_presses)){
          new_target <- target_input-test_presses(potential_presses[recursion_id,],coeffs)
          #print(target_input)
          #print(-test_presses(potential_presses[recursion_id,],coeffs))
          #print(new_target)
          #print(new_target)
          #print(paste0("total presses ",sum(potential_presses[recursion_id,])," so trying ",paste0(new_target,collapse="")))
          temp_solution <- min(
            temp_solution,
            sum(potential_presses[recursion_id,])+solve0101(new_target,button_id) #need to put in the divide by two bit here and in the input chekcing section
            
          )  
        }
      } else {
        #print(paste0("got stuck with no solutions for ",paste0(target_input,collapse=",")))
        return(Inf)
      }
      return(temp_solution)
    }
}

final_output <- data.frame(
  problem=1:nrow(input),
  presses=99999
)

for (problem_id in 1:nrow(input)){
  current_target <- as.numeric(unlist(str_extract_all(joltage[problem_id],"\\d+")))
  final_output$presses[problem_id] <- solve0101(current_target,problem_id)
  #print(problem_id/nrow(input))
}

sum(final_output$presses)


