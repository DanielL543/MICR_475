Homework 3
================

We will be looking at the popularity of the name “Daniel” over time, and
when it reached its peak popularity

# Load Library

We first want to open the library we want to work with. For this
homework, I decided to use the babynames package.

``` r
library(babynames)
library(dplyr)
library(ggplot2)
```

# Plot

This is where we plot the data.

``` r
#filter out the data from the package
dataset_1 <- filter(babynames, name == "Daniel", sex == "M")

#plot out filtered data and name axes
ggplot(data = dataset_1) +
  geom_line(mapping = aes(x = year, y =n)) + 
  labs (title = "Popularity of Daniel over time", x = "Year", y = "Number of People")
```

![](hw_3_files/figure-gfm/plot_data-1.png)<!-- -->

# Conclusion

From the data, we can see that the name, “Daniel”, has increased in
popularity since the 1880s. It peaked in popularity around the late
1900s.
