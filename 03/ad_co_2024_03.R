# Advent of code 2024
# Day 03

library(stringr)

# Day 03 pt 1 -----------------

example <- 'xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))'

mult_inst <- str_extract_all(string = example, pattern = '(?<=mul\\()\\d+\\,\\d+(?=\\))')

mult_inst_num <- sapply(X = mult_inst[[1]], FUN = function(x)
{
  num_list <- str_extract_all(string = x, pattern = '\\d+')
})


num_prod <- sapply(mult_inst_num, function(y)
{
  prod(as.numeric(y))
})

sum(num_prod)



# Create function --------------------

mult_prod <- function(input_string)
{
  mult_inst <- str_extract_all(string = input_string, pattern = '(?<=mul\\()\\d+\\,\\d+(?=\\))')
  
  mult_inst_num <- sapply(X = mult_inst[[1]], FUN = function(x)
  {
    num_list <- str_extract_all(string = x, pattern = '\\d+')
  })
  
  num_prod <- sapply(mult_inst_num, function(y)
  {
    prod(as.numeric(y))
  })
  
  sum(num_prod)
  
}

mult_prod(example)

input <- scan('./03/input.txt', what = 'character', sep = NULL)
input <- paste(input, collapse = '')

mult_prod(input)


# Part 2 ------------------------

example <- "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

# Capture the functions and their index

mult_inst <- str_extract_all(string = example, pattern = "mul\\(\\d+\\,\\d+\\)|do\\(\\)|don\\'t\\(\\)")

mult_inst <- mult_inst[[1]]


# Loop through directions

running_total <- 0
do_switch <- TRUE

for(i in mult_inst)
{

  if(grepl('mul\\(\\d+\\,\\d+\\)', i) && do_switch)
  {
    num_list <- str_extract_all(string = i, pattern = '\\d+')
    current_prod <- prod(as.numeric(num_list[[1]]))
    running_total <- running_total + current_prod
  } else if(grepl('do\\(\\)', i))
  {
    do_switch <- TRUE
  } else if(grepl("don\\'t\\(\\)", i))
  {
    do_switch <- FALSE
  }
    
}

running_total


# Create function --------------

mult_prod_pt2 <- function(input_string)
{
  
  mult_inst <- str_extract_all(string = input_string, pattern = "mul\\(\\d+\\,\\d+\\)|do\\(\\)|don\\'t\\(\\)")
  
  mult_inst <- mult_inst[[1]]

  # Loop through directions
  
  running_total <- 0
  do_switch <- TRUE
  
  for(i in mult_inst)
  {
    
    if(grepl('mul\\(\\d+\\,\\d+\\)', i) && do_switch)
    {
      num_list <- str_extract_all(string = i, pattern = '\\d+')
      current_prod <- prod(as.numeric(num_list[[1]]))
      running_total <- running_total + current_prod
    } else if(grepl('do\\(\\)', i))
    {
      do_switch <- TRUE
    } else if(grepl("don\\'t\\(\\)", i))
    {
      do_switch <- FALSE
    }
    
  }
  
  running_total

}

mult_prod_pt2(input)
