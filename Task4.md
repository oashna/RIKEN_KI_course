1.  What is the square root of 10?

``` r
sqrt(10)
```

    ## [1] 3.162278

1.  What is the logarithm of 32 to the base2?

``` r
log2(32)
```

    ## [1] 5

1.  What is the sum of the numbers from 1 to 1000?

``` r
sum(1:1000)
```

    ## [1] 500500

1.  What is the sum of all even numbers from 2 to 1000?

``` r
even <- seq(2, 1000, by = 2)
sum(even)
```

    ## [1] 250500

1.  How many pairwise comparisons are there for 100 genes?

``` r
pairs <- 100*(100-1)/2
pairs
```

    ## [1] 4950

1.  How many ways to arrange 100 genes in triples?

``` r
#number of combinations of x genes out of n
combi <- function(n, x) {
  if(x > n || x < 0) {
    stop(paste0("x must be between 0 and ", n))
  }
  return(choose(n, x))
}
combi(100,3)
```

    ## [1] 161700
