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

colnames(input) <- c("X","Y","Z")
num_elements <- nrow(input)
input$element <- 1:num_elements

#create a function
check_dists <- function(element,input_array){
  (input[element,1] - input_array[,1])^2 + (input[element,2] - input_array[,2])^2 + (input[element,3] - input_array[,3])^2
}
#check it works
check_dists(1,input[-2,])

connections <- array(
  data=0,
  dim=c(num_elements,num_elements)
)

for (i in 1:num_elements){
  connections[i,i] <- 1
}

connection_idx <- 1
repeat{
  
  current_connection <- c(0,0)
  curr_dist <- Inf
  
  
  #go through all the rows finding the smallest distance
  for (element_idx in 1:num_elements){
    
    #for eligible connections (i.e. not itself or previous connections)
    eligible_array <- input[-which(connections[element_idx,]==1),]
    #if its the first loop use the min
    dists <- check_dists(element_idx,eligible_array)
    if (min(dists)<curr_dist){
      curr_dist <- min(dists)
      current_connection[1] <- element_idx
      current_connection[2] <- eligible_array$element[which(dists==min(dists))]
      #print(min(dists))
      #otherwise check is the minimum smaller than the current saved one
      #when you get to the end that's your best one 
    }
    if (element_idx==num_elements){
      connections[current_connection[1],current_connection[2]] <- 1
      connections[current_connection[2],current_connection[1]] <- 1
      #print(paste0("current connection ",connection_idx))
      #print(current_connection)
      #make little lists of which ones are now connected
      if(connection_idx==1){
        networks <- list(
          current_connection
        )
        connection_idx <- 2
      }
      
      if(connection_idx>1){
        #are either of the current connections in one of the networks?
        el_1_networks <- sapply(networks, function(y) current_connection[1] %in% y)
        el_2_networks <- sapply(networks, function(y) current_connection[2] %in% y)
        #if element 1 in one and element 2 in none
        if(sum(el_1_networks)==1 && sum(el_2_networks)==0){
          networks[[which(el_1_networks)]] <- 
            c(networks[[which(el_1_networks)]],
              current_connection[2])
          
        }
        #if element 2 in one and element 1 in none
        if(sum(el_1_networks)==0 && sum(el_2_networks)==1){
          networks[[which(el_2_networks)]] <- 
            c(networks[[which(el_2_networks)]],
              current_connection[1])
          
        }
        #its breaking cause sometimes you join two networks together
        #if theyre both in different ones
        if(sum(el_1_networks)==1 && sum(el_2_networks)==1 && any(el_1_networks!=el_2_networks)){
          networks[[which(el_1_networks)]] <- 
            c(networks[[which(el_1_networks)]],networks[[which(el_2_networks)]])
          networks[[which(el_2_networks)]] <- NULL
          
          
        }
        #if so add the OTHER one to that one  
        #if not add a new network
        if(!any(sapply(networks, function(y) current_connection %in% y))){
          networks[[length(networks)+1]] <- current_connection
        }
      }
    }
  }
  if(length(networks[[1]])==num_elements){
    break
  }  
}

as.numeric(input$X[current_connection[1]])*as.numeric(input$X[current_connection[2]])

