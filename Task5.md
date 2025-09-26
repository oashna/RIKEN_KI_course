1.Use the R internal CO2 dataset.

``` r
data("CO2")
```

2.Describe briefly the content of the CO2 dataset using the help
function.

``` r
help("CO2")
```

3.What is the average and median CO2 uptake of the plants from Quebec
and Mississippi?

``` r
library(dplyr)
data(CO2)
CO2 %>%
  group_by(Type) %>%
  summarise(
    mean = mean(uptake),
    median = median(uptake)
  )
```

    ## # A tibble: 2 × 3
    ##   Type         mean median
    ##   <fct>       <dbl>  <dbl>
    ## 1 Quebec       33.5   37.2
    ## 2 Mississippi  20.9   19.3

4.\[Optional\] In the “airway” example data from Bioconductor, how many
genes are expressed in each sample? How many genes are not expressed in
any sample?

``` r
library(BiocManager)
BiocManager::install("airway")
library(airway)
data("airway")
help(airway)
browseVignettes("airway")
count <- assay(airway)
expressed <- colSums(count > 0)
not_expressed <- sum(rowSums(count > 0) == 0)
expressed
```

    ## SRR1039508 SRR1039509 SRR1039512 SRR1039513 SRR1039516 SRR1039517 SRR1039520 
    ##      24633      24527      25699      23124      25508      25998      24662 
    ## SRR1039521 
    ##      23991

``` r
not_expressed
```

    ## [1] 30208
