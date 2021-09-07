Homework 4
================

# Calculating the Sum of Two Variables

Here we will calculate the sum of two variables.

``` r
library(tidyverse)

#assigning values to variables and printing the sum
a <- 3
b <- 2
a + b
```

    ## [1] 5

``` r
#using the sum function to calculate the sum of the two assigned variables
sum(a, b)
```

    ## [1] 5

# American Airlines Departure Delay vs. Arrival Delay Plot

``` r
library(tidyverse)
library(dplyr)
library(nycflights13)
nycflights13::flights
```

    ## # A tibble: 336,776 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     1      517            515         2      830            819
    ##  2  2013     1     1      533            529         4      850            830
    ##  3  2013     1     1      542            540         2      923            850
    ##  4  2013     1     1      544            545        -1     1004           1022
    ##  5  2013     1     1      554            600        -6      812            837
    ##  6  2013     1     1      554            558        -4      740            728
    ##  7  2013     1     1      555            600        -5      913            854
    ##  8  2013     1     1      557            600        -3      709            723
    ##  9  2013     1     1      557            600        -3      838            846
    ## 10  2013     1     1      558            600        -2      753            745
    ## # ... with 336,766 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

``` r
#This filters the American Airlines flights and assigns it to "AA_flights"
(AA_flights <- filter(flights, carrier == "AA"))
```

    ## # A tibble: 32,729 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     1      542            540         2      923            850
    ##  2  2013     1     1      558            600        -2      753            745
    ##  3  2013     1     1      559            600        -1      941            910
    ##  4  2013     1     1      606            610        -4      858            910
    ##  5  2013     1     1      623            610        13      920            915
    ##  6  2013     1     1      628            630        -2     1137           1140
    ##  7  2013     1     1      629            630        -1      824            810
    ##  8  2013     1     1      635            635         0     1028            940
    ##  9  2013     1     1      656            700        -4      854            850
    ## 10  2013     1     1      656            659        -3      949            959
    ## # ... with 32,719 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

``` r
ggplot(data = AA_flights) +
  geom_point(mapping = aes(x = dep_delay, y = arr_delay)) +
  labs(title = "Departure Delay vs. Arrival Delay", x = "Departure Delay", y = "Arriva Delay")
```

    ## Warning: Removed 782 rows containing missing values (geom_point).

![](hw_4_files/figure-gfm/plot_AAdata-1.png)<!-- -->
