Function: Define Alpha Diversity Indices
================

# Purpose of the function

This is a function to calculate the alpha diversity of a given OTU
table. It will calculate the Shannon, Simpson, Chao1, and ACE indices.

## Load the needed libraries and convert the .csv file into a data set

``` r
library(tidyverse)
library(vegan)
library(phyloseq)

OTU_data <- read.csv("otutable_grazing.csv")
```

# Function

After converting the .csv file into a data set, the user is able to use
this function to obtain the alpha indices.

``` r
#function to calculate the alpha indices for a given OTU table
calc_alpha_indices <- function(data){
  #make a data frame with just the sample IDs
  sample.ID <- data[, 1]
  samples <- data.frame(sample.ID)
  samples
  
  #data frame with the shannon data and the sample IDs
  shannon.indices <- diversity(data[, -1], index = "shannon", MARGIN = 1, base = exp(1))
  shannon_index <- data.frame(shannon.indices)
  shannon_data <- cbind(samples, shannon_index)
  
  #data frame which includes both shannon and simpson
  simpson.indices <- diversity(data[, -1], index = "simpson", MARGIN = 1, base = exp(1))
  simpson_index <- data.frame(simpson.indices)
  simpson_data <- cbind(shannon_data, simpson_index)
  
  #calculate chao and ACE indices and add them to a final matrix 
  chao.out <- estimateR(data[, -1])
  chao_index <- data.frame(chao.out)
  chao_transpose <- t(chao_index)
  final_matrix <- cbind(simpson_data, chao_transpose)
  
  return(final_matrix)
}
```

Example of running the function and assigning the output to
“alpha\_matrix”

``` r
#using the function to create a matrix called alpha_matrix
(alpha_matrix <- calc_alpha_indices(OTU_data))
```

    ##     sample.ID shannon.indices simpson.indices S.obs  S.chao1   se.chao1
    ## X1     187-2b        7.520576       0.9993555  2347 2347.240 0.56622294
    ## X2     189-2b        6.847203       0.9987210  1211 1211.000 0.00000000
    ## X3     191-2b        6.345248       0.9978261   760  760.000 0.00000000
    ## X4     194-2b        6.884009       0.9987779  1250 1250.000 0.04998000
    ## X5     197-2b        6.393640       0.9979200   782  782.000 0.00000000
    ## X6     198-2b        6.417644       0.9980298   798  798.000 0.00000000
    ## X7     201-2b        6.732678       0.9985246  1098 1098.111 0.40972199
    ## X8     301-2b        6.952892       0.9988559  1333 1333.000 0.00000000
    ## X9     302-2b        6.071585       0.9970753   592  592.000 0.00000000
    ## X10    304-2b        6.585205       0.9983301   932  932.000 0.05552574
    ## X11    306-2b        7.125398       0.9990361  1582 1582.000 0.03570300
    ## X12    309-2b        7.526478       0.9993547  2351 2351.577 0.92612922
    ## X13    310-2b        6.890842       0.9987946  1237 1237.077 0.32243589
    ## X14    311-2b        6.789724       0.9986468  1143 1143.000 0.00000000
    ## X15    312-2b        6.543799       0.9982807   887  887.000 0.00000000
    ## X16    316-2b        7.196636       0.9991052  1672 1672.250 0.62045944
    ## X17    317-2b        5.391976       0.9940253   311  311.000 0.00000000
    ## X18    320-2b        6.914441       0.9988059  1280 1280.000 0.03332031
    ## X19    322-2b        5.526942       0.9948910   345  345.000 0.00000000
    ## X20    325-2b        6.812319       0.9986828  1144 1144.000 0.00000000
    ## X21    326-2b        7.317087       0.9991976  1929 1929.300 0.65327494
    ## X22    331-2b        6.488999       0.9981521   865  865.000 0.00000000
    ## X23    332-2b        7.337137       0.9992159  1980 1980.111 0.37049938
    ## X24    337-2b        6.958944       0.9988640  1332 1332.000 0.02271874
    ## X25    339-2b        7.200062       0.9991201  1667 1667.250 0.62045941
    ## X26    340-2b        6.481240       0.9981173   865  865.000 0.00000000
    ## X27    344-2b        7.146516       0.9990601  1589 1589.000 0.03570305
    ## X28    352-2b        6.731097       0.9985561  1094 1094.000 0.00000000
    ## X29    357-2b        6.989190       0.9988938  1386 1386.000 0.02499098
    ## X30    359-2b        6.839151       0.9987196  1199 1199.000 0.00000000
    ##         S.ACE    se.ACE
    ## X1  2347.6716 15.043666
    ## X2  1211.0000 11.127220
    ## X3   760.0000  9.034364
    ## X4  1250.1517 11.068732
    ## X5   782.0000  9.217811
    ## X6   798.0000  9.189729
    ## X7  1098.3095 10.198111
    ## X8  1333.0000 11.050997
    ## X9   592.0000  8.273921
    ## X10  932.1585  9.759352
    ## X11 1582.1747 11.642665
    ## X12 2352.2679 13.573159
    ## X13 1237.3592 10.190751
    ## X14 1143.0000 10.697707
    ## X15  887.0000  8.720384
    ## X16 1672.5132 11.259671
    ## X17  311.0000  6.958767
    ## X18 1280.1689 10.258585
    ## X19  345.0000  6.697609
    ## X20 1144.0000  9.106019
    ## X21 1929.7992 12.448016
    ## X22  865.0000  9.798549
    ## X23 1980.5612 14.097613
    ## X24 1332.2590  9.008226
    ## X25 1667.4582 11.065709
    ## X26  865.0000  8.935478
    ## X27 1589.1514 10.874926
    ## X28 1094.0000 10.667186
    ## X29 1386.1826 11.173442
    ## X30 1199.0000 11.085384
