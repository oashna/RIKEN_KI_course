1.Write a function that calculates the ratio of the mean and the median
of a given vector

``` r
ratio <- function(x) {
  if(!is.numeric(x)) {
    stop("Input must be a numeric vector")
  }
  if(length(x) == 0) {
    stop("Vector is empty")
  }
  m_mean <- mean(x, na.rm = TRUE)
  m_median <- median(x, na.rm = TRUE)
  
  if(m_median == 0) {
    stop("Median is zero, ratio cannot be calculated")
  }
  
  return(m_mean / m_median)
}
```

2.Write a function that ignores the lowest and the highest value from a
given vector and calculate the mean

``` r
trim_mean <- function(x) {
  if (!is.numeric(x)) {
    stop("Input must be a numeric vector")
  }
  if (length(x) <= 2) {
    stop("Vector must have more than 2 elements")
  }
  
  # Remove the lowest and highest values
  x_trimmed <- x[!(x %in% c(min(x, na.rm = TRUE), max(x, na.rm = TRUE)))]
  
  # Return mean of the remaining values
  return(mean(x_trimmed, na.rm = TRUE))
}
```

3.Why, how and when not to use pipes?

Pipes (%\>%) make sequential data transformations readable by listing
operations step by step. We should not use them when operations are
long, involve multiple inputs/outputs or have complex dependency
relationships rather than linear.

4.Why the apply-family of functions could be useful in your work?

Apply-family (e.g. apply, lapply, sapply) let us run a function over
vectors, lists or matrices without the need of loops. They allow more
compact and readable code than loop. We can use them when we need to
apply the same operation to many elements.
