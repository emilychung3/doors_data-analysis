# K. Garner, 2025, a set of functions to compute routine scores from door data
####################################################################
### functions to compute routine scores
####################################################################

data_2_counts_matrix <- function(data, n_doors){
  # turn data into a counts matrix of transitions made
  # this function makes an 'n_doors' x n_doors matrix (e.g. 16 x 16)
  # where the rows are the "from" door and the columns are the "to"
  # e.g. if the participant went from door 1 to door 3 for a total of 5 times across 
  # the whole experiment, then after applying this function, 
  # the number in row 1, column 3 of the matrix would be 5.
  
  # 'data' is the vector of doors visited across the whole experiment: e.g. c(1,3,2,1,1,4,3,2,...)
  # note: in the original data, we just took all the door selections across the whole 
  # session and sum the transitions, ignoring trial
  # but now we have the home button at the start of each trial, we'll need to sum the
  # transitions for each trial, and then at the end, sum the transition matrices across trials
  mat <- matrix(rep(0, times=n_doors*n_doors), nrow=n_doors, ncol=n_doors) #create transition matrix here with count
  idxs <- matrix(c(data[1:length(data)-1], data[2:length(data)]), nrow=2, byrow=TRUE) # takes first selection and matches it with the next
  for(i in 1:ncol(idxs)){
    mat[idxs[1,i],idxs[2,i]] <- mat[idxs[1,i],idxs[2,i]] + 1 # get the doors for that transition and add 1 count
  }
  mat
}

p_st1_gs <- function(counts_matrix, n_doors){
  # this function takes a counts matrix which has been output by the data_2_counts_matrix function
  # then, for each row of the matrix, the probabilities of going to each door are computed    #E.C. out of all the transitions for each door
  # e.g. if the participant went from door 1 to door 3 for a total of 2 times across the experiment
  # and from door 1 to door 4 for a total of 8 times, then out[1,3] = 0.2 and out[1,4] = 0.8.
  denom <- matrix(rep(rowSums(counts_matrix), n_doors), nrow=n_doors, byrow=FALSE) 
  out <- counts_matrix / denom
  out[is.na(out)] = 0
  out
}

H <- function(x){
  # apply this function over the rows of the output of p_st1_gs
  # to get the entropy of each row
  # the routines score is computed by summing the outputs
  # i.e. if there are 16 doors, then you should apply this
  # function to get 16 entropy values, that are then summed together
  # to get the total routine score.
  -sum(x * log(x), na.rm = TRUE)
}
