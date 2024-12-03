# Advent of code 2024
# Day 01

# Day 01 pt 1 -----------------

example <- read.table('./01/example.txt', header = F, sep = '', col.names = c('id_1','id_2'))

## Match both rows ------------

pair_match <- data.frame(id_1 = sort(example$id_1), 
                         id_2 = sort(example$id_2))

pair_match$dist <- abs(pair_match$id_1 - pair_match$id_2)

sum(pair_match$dist)


## Define function ------------------

pair_match_dist <- function(x)
{
  pair_match <- data.frame(id_1 = sort(x$id_1), 
                           id_2 = sort(x$id_2))
  
  pair_match$dist <- abs(pair_match$id_1 - pair_match$id_2)
  
  return(sum(pair_match$dist))
}

pair_match_dist(example)

## Day 01 pt 1 input -----------------

input <- read.table('./01/input.txt', header = F, sep = '', col.names = c('id_1','id_2'))

pair_match_dist(input)


# Day 01 pt 2 ------------------------

# Create a vector of counts

example <- read.table('./01/example.txt', header = F, sep = '', col.names = c('id_1','id_2'))

id_counts <- example
  
id_counts$count <- sapply(example$id_1, FUN = function(x)
{
  sum(x == example$id_2)
}, USE.NAMES = F)

sum(id_counts$id_1 * id_counts$count)


## Define function -------------

id_counts_func <- function(x)
{
  id_counts <- x
  
  id_counts$count <- sapply(x$id_1, FUN = function(y)
  {
    sum(y == x$id_2)
  }, USE.NAMES = F)
  
  return(sum(id_counts$id_1 * id_counts$count))
}


id_counts_func(example)

## Day 01 pt 2 input -----------------

input <- read.table('./01/input.txt', header = F, sep = '', col.names = c('id_1','id_2'))

id_counts_func(input)
