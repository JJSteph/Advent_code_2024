##
## Advent of code 2024 day 02
##

# Reading in data -------------

example <- read.table('./02/example.txt')

example


# Transpose so rows are now columns
example_t <- as.data.frame(t(example))

example_t

safe_or_not <- sapply(X = seq_len(ncol(example_t)), function(x){
  
  safe <- FALSE
  
  # Take lag diff
  diff <- example_t[[x]][2:length(example_t[[x]])] - example_t[[x]][1:length(example_t[[x]])- 1]
  
  if((all(diff > 0) || all(diff < 0)) && all(abs(diff) %in% c(1:3)))
    safe <- TRUE
  
  return(safe)
  
})

sum(safe_or_not)


# Create function ---------------------


report_num_safe <- function(x)
{

  # Transpose so rows are now columns
  data_t <- as.data.frame(t(x))
  
  safe_or_not <- sapply(X = seq_len(ncol(data_t)), function(y){
    
    safe <- FALSE
    
    current_col <- na.omit(data_t[[y]])
    
    # Take lag diff
    diff <- current_col[2:length(current_col)] - current_col[1:length(current_col)- 1]
    
    if((all(diff > 0) || all(diff < 0)) && all(abs(diff) %in% c(1:3)))
      safe <- TRUE
    
    return(safe)
    
  })
  
  sum(safe_or_not)
  
}

report_num_safe(example)




# Calculate on input -----------------

input <- read.table('./02/input.txt', fill = T)

report_num_safe(input)



# Part 2 ------------------------


# Transpose so rows are now columns
example_t <- as.data.frame(t(example))

example_t


# Take lag diff
diff <- example_t[[1]][2:length(example_t[[1]])] - example_t[[1]][1:length(example_t[[1]])- 1]

diff


if((all(diff > 0) || all(diff < 0)) && all(abs(diff) %in% c(1:3)))
  safe <- TRUE

if(!safe)
{
  # Try removing one at a time
  
  for(z in seq_len(diff))
  {

    current_diff <- diff[-z]
        
    if((all(current_diff > 0) || all(current_diff < 0)) && all(abs(current_diff) %in% c(1:3)))
    {
      safe <- TRUE
      break
    }
      
  }
  
}


safe_or_not <- sapply(X = seq_len(ncol(example_t)), function(x){
  
  safe <- FALSE
  
  # Take lag diff
  diff <- example_t[[x]][2:length(example_t[[x]])] - example_t[[x]][1:length(example_t[[x]])- 1]
  
  if((all(diff > 0) || all(diff < 0)) && all(abs(diff) %in% c(1:3)))
    safe <- TRUE
  
  if(!safe)
  {
    # Try removing one at a time
    
    for(z in seq_len(length(diff)))
    {
      current_col <- example_t[[x]][-z]
      
      # Take lag diff
      current_diff <- current_col[2:length(current_col)] - current_col[1:length(current_col)- 1]
      
      if((all(current_diff > 0) || all(current_diff < 0)) && all(abs(current_diff) %in% c(1:3)))
      {
        safe <- TRUE
        break
      }
      
    }
    
  }
  
  return(safe)
  
})

sum(safe_or_not)


# Create function ----------------

report_num_safe_pt2 <- function(x)
{
  
  # Transpose so rows are now columns
  data_t <- as.data.frame(t(x))
  
  safe_or_not <- sapply(X = seq_len(ncol(data_t)), function(y){
    
    safe <- FALSE
    
    current_col <- na.omit(data_t[[y]])
    
    # Take lag diff
    diff <- current_col[2:length(current_col)] - current_col[1:length(current_col)- 1]
    
    if((all(diff > 0) || all(diff < 0)) && all(abs(diff) %in% c(1:3)))
      safe <- TRUE
    
    if(!safe)
    {
      # Try removing one at a time
      
      for(z in seq_len(length(current_col)))
      {
        current_col <- na.omit(data_t[[y]])[-z]
        
        # Take lag diff
        current_diff <- current_col[2:length(current_col)] - current_col[1:length(current_col)- 1]
        
        if((all(current_diff > 0) || all(current_diff < 0)) && all(abs(current_diff) %in% c(1:3)))
        {
          safe <- TRUE
          break
        }
        
      }
      
    }
    
    return(safe)
    
  })
  
  sum(safe_or_not)
  
}


report_num_safe_pt2(example)

report_num_safe_pt2(input)

