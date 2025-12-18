{
  remove(list = ls())
  
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  
  #required libraries
  library(dplyr)
  library(stringr)
  library(readr)
  #load in data
  
  test_ind <-3
  
  if (test_ind == 1){
    
    filepath <- paste0(getwd(),"/","test.txt")
    input <- read.csv(
      filepath,
      sep=" ",
      header=F#,
      #col.names=paste0("V",1:15)
    )
    
  } else if(test_ind==2){
    filepath <- paste0(getwd(),"/","test2.txt")
    input <- read.csv(
      filepath,
      sep=" ",
      header=F#,
      #col.names=paste0("V",1:15)
    )
  } else {
    filepath <- paste0(getwd(),"/","input.txt")
    input <- read.csv(
      filepath,
      sep=" ",
      header=F,
      col.names=paste0("V",1:30)
    )
    
  }
  
  
  input[,1] <- str_remove_all(input[,1],"[:punct:]")
  input <- input[,!is.na(input[1,])]
  input <- rbind(input,c("out",rep("",ncol(input)-1)))
  
  
  
  path_record <- data.frame(
    location=input[,1],
    paths=-1
  )
  
  
  #part 1
  
  location_list <- list("you")
  
  
  #path_record$paths[which(num_paths$location=="you")] <- 1
  location_idx <- 1
  
}

#part 1

paths_to <- function(location_input){
  if(location_input=="you"){
    num_paths <- 1
    path_record$paths[path_record$location==location_input] <<- num_paths
    return(num_paths)
    }
  else if (!any(input[,-1]==location_input))
    {
    num_paths <- 0
    path_record$paths[path_record$location==location_input] <<- num_paths
    return(num_paths)
    } else if (path_record$paths[path_record$location==location_input]!=-1) {
      return(path_record$paths[path_record$location==location_input])
    } else {
      locs_to_check <- which(input[,-1]==location_input,arr.ind = T)
      num_paths <- 0
      for (loc_idx in 1:nrow(locs_to_check)){
        num_paths <- 
          num_paths +
          paths_to(input[locs_to_check[loc_idx,1],1])
        
      }
    }
  path_record$paths[path_record$location==location_input] <<- num_paths
  return(num_paths)
}

paths_to("out")

#part 2

path_record <- data.frame(
  location=input[,1],
  paths=-1
)

paths_2 <- function(location_input){
  if(location_input=="svr"){
    num_paths <- 1
    path_record$paths[path_record$location==location_input] <<- num_paths
    return(num_paths)
  }
  else if (!any(input[,-1]==location_input))
  {
    num_paths <- 0
    path_record$paths[path_record$location==location_input] <<- num_paths
    return(num_paths)
  } else if (path_record$paths[path_record$location==location_input]!=-1) {
    return(path_record$paths[path_record$location==location_input])
  } else {
    locs_to_check <- which(input[,-1]==location_input,arr.ind = T)
    num_paths <- 0
    for (loc_idx in 1:nrow(locs_to_check)){
      num_paths <- 
        num_paths +
        paths_2(input[locs_to_check[loc_idx,1],1])
      
    }
  }
  path_record$paths[path_record$location==location_input] <<- num_paths
  return(num_paths)
}

#paths_2("svr")
svr2fft <- paths_2("fft")
svr2dac <- paths_2("dac")

#from fft

path_record <- data.frame(
  location=input[,1],
  paths=-1
)

paths_3 <- function(location_input){
  if(location_input=="fft"){
    num_paths <- 1
    path_record$paths[path_record$location==location_input] <<- num_paths
    return(num_paths)
  }
  else if (!any(input[,-1]==location_input))
  {
    num_paths <- 0
    path_record$paths[path_record$location==location_input] <<- num_paths
    return(num_paths)
  } else if (path_record$paths[path_record$location==location_input]!=-1) {
    return(path_record$paths[path_record$location==location_input])
  } else {
    locs_to_check <- which(input[,-1]==location_input,arr.ind = T)
    num_paths <- 0
    for (loc_idx in 1:nrow(locs_to_check)){
      num_paths <- 
        num_paths +
        paths_3(input[locs_to_check[loc_idx,1],1])
      
    }
  }
  path_record$paths[path_record$location==location_input] <<- num_paths
  return(num_paths)
}

#paths_3("fft")
fft2dac <- paths_3("dac")
fft2out <- paths_3("out")

#from dac


path_record <- data.frame(
  location=input[,1],
  paths=-1
)

paths_4 <- function(location_input){
  if(location_input=="dac"){
    num_paths <- 1
    path_record$paths[path_record$location==location_input] <<- num_paths
    return(num_paths)
  }
  else if (!any(input[,-1]==location_input))
  {
    num_paths <- 0
    path_record$paths[path_record$location==location_input] <<- num_paths
    return(num_paths)
  } else if (path_record$paths[path_record$location==location_input]!=-1) {
    return(path_record$paths[path_record$location==location_input])
  } else {
    locs_to_check <- which(input[,-1]==location_input,arr.ind = T)
    num_paths <- 0
    for (loc_idx in 1:nrow(locs_to_check)){
      num_paths <- 
        num_paths +
        paths_4(input[locs_to_check[loc_idx,1],1])
      
    }
  }
  path_record$paths[path_record$location==location_input] <<- num_paths
  return(num_paths)
}

#paths_4("dac")
dac2fft<- paths_4("fft")
dac2out <- paths_4("out")

options(scipen=999)
svr2fft*fft2dac*dac2out
