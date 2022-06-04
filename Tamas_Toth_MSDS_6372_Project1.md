MSDS_6372_Project1
================
Miguel Bonilla, Reuven Derner, Milan Patel, Tamas Toth
2022-05-27

#### Loading the necessary R libraries for the analysis

``` r
# Load the necessary libraries
library(knitr)
library(rmarkdown)
library(ggpubr)
library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2)
library(tidyverse)
library(ggthemes)
library(e1071)
library(class)
library(caret)
library(stringr)
library(sjPlot)
library(data.table)
library(reshape2)
library(corrplot)
library(naivebayes)
library(car)
library(egg)
library(rworldmap)
library(Hmisc)
library(DataExplorer)
library(selectiveInference)
library(dlookr)
```

``` r
# Turn off scientific notation
options(scipen = 100, digits = 4)
```

#### Read the data

``` r
#Read the data
#setwd('/Users/ttoth76/Downloads/SMU/Semester_2/DS 6372 Applied Statistics_Inference & Modeling/FLS/Project1_Summer2022/GitContent/LifeExpectancy')
LifeExp = read.csv(file = 'Life_Expectancy_Data.csv',header = TRUE, sep = ",")
# take a sample of 15 from the dataframe
LifeExp_sample = sample_n(LifeExp, 5)
knitr::kable(LifeExp_sample, "html")
```

<table>
<thead>
<tr>
<th style="text-align:left;">
Country
</th>
<th style="text-align:right;">
Year
</th>
<th style="text-align:left;">
Status
</th>
<th style="text-align:right;">
Life.expectancy
</th>
<th style="text-align:right;">
Adult.Mortality
</th>
<th style="text-align:right;">
infant.deaths
</th>
<th style="text-align:right;">
Alcohol
</th>
<th style="text-align:right;">
percentage.expenditure
</th>
<th style="text-align:right;">
Hepatitis.B
</th>
<th style="text-align:right;">
Measles
</th>
<th style="text-align:right;">
BMI
</th>
<th style="text-align:right;">
under.five.deaths
</th>
<th style="text-align:right;">
Polio
</th>
<th style="text-align:right;">
Total.expenditure
</th>
<th style="text-align:right;">
Diphtheria
</th>
<th style="text-align:right;">
HIV.AIDS
</th>
<th style="text-align:right;">
GDP
</th>
<th style="text-align:right;">
Population
</th>
<th style="text-align:right;">
thinness..1.19.years
</th>
<th style="text-align:right;">
thinness.5.9.years
</th>
<th style="text-align:right;">
Income.composition.of.resources
</th>
<th style="text-align:right;">
Schooling
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Benin
</td>
<td style="text-align:right;">
2008
</td>
<td style="text-align:left;">
Developing
</td>
<td style="text-align:right;">
57.6
</td>
<td style="text-align:right;">
278
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:right;">
1.28
</td>
<td style="text-align:right;">
7.615
</td>
<td style="text-align:right;">
75
</td>
<td style="text-align:right;">
928
</td>
<td style="text-align:right;">
22.0
</td>
<td style="text-align:right;">
39
</td>
<td style="text-align:right;">
77
</td>
<td style="text-align:right;">
4.20
</td>
<td style="text-align:right;">
75
</td>
<td style="text-align:right;">
1.8
</td>
<td style="text-align:right;">
82.15
</td>
<td style="text-align:right;">
8696916
</td>
<td style="text-align:right;">
8.1
</td>
<td style="text-align:right;">
8.0
</td>
<td style="text-align:right;">
0.444
</td>
<td style="text-align:right;">
9.1
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:right;">
2011
</td>
<td style="text-align:left;">
Developed
</td>
<td style="text-align:right;">
82.1
</td>
<td style="text-align:right;">
63
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
9.62
</td>
<td style="text-align:right;">
4873.819
</td>
<td style="text-align:right;">
97
</td>
<td style="text-align:right;">
3802
</td>
<td style="text-align:right;">
64.1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
97
</td>
<td style="text-align:right;">
9.48
</td>
<td style="text-align:right;">
97
</td>
<td style="text-align:right;">
0.1
</td>
<td style="text-align:right;">
31834.22
</td>
<td style="text-align:right;">
46742697
</td>
<td style="text-align:right;">
0.6
</td>
<td style="text-align:right;">
0.5
</td>
<td style="text-align:right;">
0.867
</td>
<td style="text-align:right;">
16.9
</td>
</tr>
<tr>
<td style="text-align:left;">
Bhutan
</td>
<td style="text-align:right;">
2002
</td>
<td style="text-align:left;">
Developing
</td>
<td style="text-align:right;">
62.5
</td>
<td style="text-align:right;">
282
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.29
</td>
<td style="text-align:right;">
151.976
</td>
<td style="text-align:right;">
83
</td>
<td style="text-align:right;">
27
</td>
<td style="text-align:right;">
14.9
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
89
</td>
<td style="text-align:right;">
7.75
</td>
<td style="text-align:right;">
86
</td>
<td style="text-align:right;">
0.1
</td>
<td style="text-align:right;">
885.64
</td>
<td style="text-align:right;">
66399
</td>
<td style="text-align:right;">
18.6
</td>
<td style="text-align:right;">
19.4
</td>
<td style="text-align:right;">
0.000
</td>
<td style="text-align:right;">
8.0
</td>
</tr>
<tr>
<td style="text-align:left;">
Papua New Guinea
</td>
<td style="text-align:right;">
2003
</td>
<td style="text-align:left;">
Developing
</td>
<td style="text-align:right;">
59.6
</td>
<td style="text-align:right;">
321
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
0.59
</td>
<td style="text-align:right;">
124.379
</td>
<td style="text-align:right;">
67
</td>
<td style="text-align:right;">
3863
</td>
<td style="text-align:right;">
39.6
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
62
</td>
<td style="text-align:right;">
6.79
</td>
<td style="text-align:right;">
68
</td>
<td style="text-align:right;">
1.4
</td>
<td style="text-align:right;">
588.36
</td>
<td style="text-align:right;">
61724
</td>
<td style="text-align:right;">
1.5
</td>
<td style="text-align:right;">
1.4
</td>
<td style="text-align:right;">
0.433
</td>
<td style="text-align:right;">
7.2
</td>
</tr>
<tr>
<td style="text-align:left;">
Democratic Republic of the Congo
</td>
<td style="text-align:right;">
2001
</td>
<td style="text-align:left;">
Developing
</td>
<td style="text-align:right;">
51.8
</td>
<td style="text-align:right;">
34
</td>
<td style="text-align:right;">
227
</td>
<td style="text-align:right;">
1.90
</td>
<td style="text-align:right;">
0.000
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
8072
</td>
<td style="text-align:right;">
15.3
</td>
<td style="text-align:right;">
332
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
2.67
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
2.4
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
12.2
</td>
<td style="text-align:right;">
12.1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
</tr>
</tbody>
</table>

#### Address the missing values in each column (NA as well as empty strings).

``` r
# Address the missing values in each column (NA as well as empty strings).
missing_df = as.data.frame(sapply(LifeExp, function(x) sum(is.na(x))))
colnames(missing_df) = c("variable missing")
knitr::kable(missing_df, "html")
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
variable missing
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Country
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Year
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Status
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Life.expectancy
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
Adult.Mortality
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
infant.deaths
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Alcohol
</td>
<td style="text-align:right;">
194
</td>
</tr>
<tr>
<td style="text-align:left;">
percentage.expenditure
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hepatitis.B
</td>
<td style="text-align:right;">
553
</td>
</tr>
<tr>
<td style="text-align:left;">
Measles
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
BMI
</td>
<td style="text-align:right;">
34
</td>
</tr>
<tr>
<td style="text-align:left;">
under.five.deaths
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Polio
</td>
<td style="text-align:right;">
19
</td>
</tr>
<tr>
<td style="text-align:left;">
Total.expenditure
</td>
<td style="text-align:right;">
226
</td>
</tr>
<tr>
<td style="text-align:left;">
Diphtheria
</td>
<td style="text-align:right;">
19
</td>
</tr>
<tr>
<td style="text-align:left;">
HIV.AIDS
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
GDP
</td>
<td style="text-align:right;">
448
</td>
</tr>
<tr>
<td style="text-align:left;">
Population
</td>
<td style="text-align:right;">
652
</td>
</tr>
<tr>
<td style="text-align:left;">
thinness..1.19.years
</td>
<td style="text-align:right;">
34
</td>
</tr>
<tr>
<td style="text-align:left;">
thinness.5.9.years
</td>
<td style="text-align:right;">
34
</td>
</tr>
<tr>
<td style="text-align:left;">
Income.composition.of.resources
</td>
<td style="text-align:right;">
167
</td>
</tr>
<tr>
<td style="text-align:left;">
Schooling
</td>
<td style="text-align:right;">
163
</td>
</tr>
</tbody>
</table>

``` r
empty_string_df = as.data.frame(sapply(LifeExp, function(x) sum(x == "")))
colnames(empty_string_df) = c("variable empty")
knitr::kable(empty_string_df, "html")
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
variable empty
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Country
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Year
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Status
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Life.expectancy
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
Adult.Mortality
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
infant.deaths
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Alcohol
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
percentage.expenditure
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hepatitis.B
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
Measles
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
BMI
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
under.five.deaths
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Polio
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
Total.expenditure
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
Diphtheria
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
HIV.AIDS
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
GDP
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
Population
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
thinness..1.19.years
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
thinness.5.9.years
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
Income.composition.of.resources
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
Schooling
</td>
<td style="text-align:right;">
NA
</td>
</tr>
</tbody>
</table>

``` r
#set random seed
set.seed(329)
```

``` r
# Function to Identify different characteristics of the data frame 
# Getting a concise summary of the dataframe: str()
# Listing the column labels of the dataframe: colnames()
# Size of the dataset: dim()
# # Verify if there is any negative values in the dataset
dfinfo = function(df_name)
  {
  df_structure = str(df_name)
  df_colnames = colnames(df_name)
  df_dimensions = dim(df_name)
  df_neg = print(paste("Negative values in the Data Frame:", 
                       sapply(df_name, function(x) sum(x < 0))))
  outparam = list(df_structure, df_colnames, df_dimensions, df_neg)
  return (outparam)
}
```

``` r
dfinfo(LifeExp)
```

    ## 'data.frame':    2938 obs. of  22 variables:
    ##  $ Country                        : chr  "Afghanistan" "Afghanistan" "Afghanistan" "Afghanistan" ...
    ##  $ Year                           : int  2015 2014 2013 2012 2011 2010 2009 2008 2007 2006 ...
    ##  $ Status                         : chr  "Developing" "Developing" "Developing" "Developing" ...
    ##  $ Life.expectancy                : num  65 59.9 59.9 59.5 59.2 58.8 58.6 58.1 57.5 57.3 ...
    ##  $ Adult.Mortality                : int  263 271 268 272 275 279 281 287 295 295 ...
    ##  $ infant.deaths                  : int  62 64 66 69 71 74 77 80 82 84 ...
    ##  $ Alcohol                        : num  0.01 0.01 0.01 0.01 0.01 0.01 0.01 0.03 0.02 0.03 ...
    ##  $ percentage.expenditure         : num  71.3 73.5 73.2 78.2 7.1 ...
    ##  $ Hepatitis.B                    : int  65 62 64 67 68 66 63 64 63 64 ...
    ##  $ Measles                        : int  1154 492 430 2787 3013 1989 2861 1599 1141 1990 ...
    ##  $ BMI                            : num  19.1 18.6 18.1 17.6 17.2 16.7 16.2 15.7 15.2 14.7 ...
    ##  $ under.five.deaths              : int  83 86 89 93 97 102 106 110 113 116 ...
    ##  $ Polio                          : int  6 58 62 67 68 66 63 64 63 58 ...
    ##  $ Total.expenditure              : num  8.16 8.18 8.13 8.52 7.87 9.2 9.42 8.33 6.73 7.43 ...
    ##  $ Diphtheria                     : int  65 62 64 67 68 66 63 64 63 58 ...
    ##  $ HIV.AIDS                       : num  0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 ...
    ##  $ GDP                            : num  584.3 612.7 631.7 670 63.5 ...
    ##  $ Population                     : num  33736494 327582 31731688 3696958 2978599 ...
    ##  $ thinness..1.19.years           : num  17.2 17.5 17.7 17.9 18.2 18.4 18.6 18.8 19 19.2 ...
    ##  $ thinness.5.9.years             : num  17.3 17.5 17.7 18 18.2 18.4 18.7 18.9 19.1 19.3 ...
    ##  $ Income.composition.of.resources: num  0.479 0.476 0.47 0.463 0.454 0.448 0.434 0.433 0.415 0.405 ...
    ##  $ Schooling                      : num  10.1 10 9.9 9.8 9.5 9.2 8.9 8.7 8.4 8.1 ...
    ##  [1] "Negative values in the Data Frame: 0" 
    ##  [2] "Negative values in the Data Frame: 0" 
    ##  [3] "Negative values in the Data Frame: 0" 
    ##  [4] "Negative values in the Data Frame: NA"
    ##  [5] "Negative values in the Data Frame: NA"
    ##  [6] "Negative values in the Data Frame: 0" 
    ##  [7] "Negative values in the Data Frame: NA"
    ##  [8] "Negative values in the Data Frame: 0" 
    ##  [9] "Negative values in the Data Frame: NA"
    ## [10] "Negative values in the Data Frame: 0" 
    ## [11] "Negative values in the Data Frame: NA"
    ## [12] "Negative values in the Data Frame: 0" 
    ## [13] "Negative values in the Data Frame: NA"
    ## [14] "Negative values in the Data Frame: NA"
    ## [15] "Negative values in the Data Frame: NA"
    ## [16] "Negative values in the Data Frame: 0" 
    ## [17] "Negative values in the Data Frame: NA"
    ## [18] "Negative values in the Data Frame: NA"
    ## [19] "Negative values in the Data Frame: NA"
    ## [20] "Negative values in the Data Frame: NA"
    ## [21] "Negative values in the Data Frame: NA"
    ## [22] "Negative values in the Data Frame: NA"

    ## [[1]]
    ## NULL
    ## 
    ## [[2]]
    ##  [1] "Country"                         "Year"                           
    ##  [3] "Status"                          "Life.expectancy"                
    ##  [5] "Adult.Mortality"                 "infant.deaths"                  
    ##  [7] "Alcohol"                         "percentage.expenditure"         
    ##  [9] "Hepatitis.B"                     "Measles"                        
    ## [11] "BMI"                             "under.five.deaths"              
    ## [13] "Polio"                           "Total.expenditure"              
    ## [15] "Diphtheria"                      "HIV.AIDS"                       
    ## [17] "GDP"                             "Population"                     
    ## [19] "thinness..1.19.years"            "thinness.5.9.years"             
    ## [21] "Income.composition.of.resources" "Schooling"                      
    ## 
    ## [[3]]
    ## [1] 2938   22
    ## 
    ## [[4]]
    ##  [1] "Negative values in the Data Frame: 0" 
    ##  [2] "Negative values in the Data Frame: 0" 
    ##  [3] "Negative values in the Data Frame: 0" 
    ##  [4] "Negative values in the Data Frame: NA"
    ##  [5] "Negative values in the Data Frame: NA"
    ##  [6] "Negative values in the Data Frame: 0" 
    ##  [7] "Negative values in the Data Frame: NA"
    ##  [8] "Negative values in the Data Frame: 0" 
    ##  [9] "Negative values in the Data Frame: NA"
    ## [10] "Negative values in the Data Frame: 0" 
    ## [11] "Negative values in the Data Frame: NA"
    ## [12] "Negative values in the Data Frame: 0" 
    ## [13] "Negative values in the Data Frame: NA"
    ## [14] "Negative values in the Data Frame: NA"
    ## [15] "Negative values in the Data Frame: NA"
    ## [16] "Negative values in the Data Frame: 0" 
    ## [17] "Negative values in the Data Frame: NA"
    ## [18] "Negative values in the Data Frame: NA"
    ## [19] "Negative values in the Data Frame: NA"
    ## [20] "Negative values in the Data Frame: NA"
    ## [21] "Negative values in the Data Frame: NA"
    ## [22] "Negative values in the Data Frame: NA"

#### Generate summary statistics

``` r
# Generate summary statistics
summary(LifeExp)
```

    ##    Country               Year         Status          Life.expectancy
    ##  Length:2938        Min.   :2000   Length:2938        Min.   :36.3   
    ##  Class :character   1st Qu.:2004   Class :character   1st Qu.:63.1   
    ##  Mode  :character   Median :2008   Mode  :character   Median :72.1   
    ##                     Mean   :2008                      Mean   :69.2   
    ##                     3rd Qu.:2012                      3rd Qu.:75.7   
    ##                     Max.   :2015                      Max.   :89.0   
    ##                                                       NA's   :10     
    ##  Adult.Mortality infant.deaths       Alcohol      percentage.expenditure
    ##  Min.   :  1     Min.   :   0.0   Min.   : 0.01   Min.   :    0         
    ##  1st Qu.: 74     1st Qu.:   0.0   1st Qu.: 0.88   1st Qu.:    5         
    ##  Median :144     Median :   3.0   Median : 3.76   Median :   65         
    ##  Mean   :165     Mean   :  30.3   Mean   : 4.60   Mean   :  738         
    ##  3rd Qu.:228     3rd Qu.:  22.0   3rd Qu.: 7.70   3rd Qu.:  442         
    ##  Max.   :723     Max.   :1800.0   Max.   :17.87   Max.   :19480         
    ##  NA's   :10                       NA's   :194                           
    ##   Hepatitis.B      Measles            BMI       under.five.deaths
    ##  Min.   : 1.0   Min.   :     0   Min.   : 1.0   Min.   :   0     
    ##  1st Qu.:77.0   1st Qu.:     0   1st Qu.:19.3   1st Qu.:   0     
    ##  Median :92.0   Median :    17   Median :43.5   Median :   4     
    ##  Mean   :80.9   Mean   :  2420   Mean   :38.3   Mean   :  42     
    ##  3rd Qu.:97.0   3rd Qu.:   360   3rd Qu.:56.2   3rd Qu.:  28     
    ##  Max.   :99.0   Max.   :212183   Max.   :87.3   Max.   :2500     
    ##  NA's   :553                     NA's   :34                      
    ##      Polio      Total.expenditure   Diphtheria      HIV.AIDS    
    ##  Min.   : 3.0   Min.   : 0.37     Min.   : 2.0   Min.   : 0.10  
    ##  1st Qu.:78.0   1st Qu.: 4.26     1st Qu.:78.0   1st Qu.: 0.10  
    ##  Median :93.0   Median : 5.76     Median :93.0   Median : 0.10  
    ##  Mean   :82.5   Mean   : 5.94     Mean   :82.3   Mean   : 1.74  
    ##  3rd Qu.:97.0   3rd Qu.: 7.49     3rd Qu.:97.0   3rd Qu.: 0.80  
    ##  Max.   :99.0   Max.   :17.60     Max.   :99.0   Max.   :50.60  
    ##  NA's   :19     NA's   :226       NA's   :19                    
    ##       GDP           Population         thinness..1.19.years thinness.5.9.years
    ##  Min.   :     2   Min.   :        34   Min.   : 0.10        Min.   : 0.10     
    ##  1st Qu.:   464   1st Qu.:    195793   1st Qu.: 1.60        1st Qu.: 1.50     
    ##  Median :  1767   Median :   1386542   Median : 3.30        Median : 3.30     
    ##  Mean   :  7483   Mean   :  12753375   Mean   : 4.84        Mean   : 4.87     
    ##  3rd Qu.:  5911   3rd Qu.:   7420359   3rd Qu.: 7.20        3rd Qu.: 7.20     
    ##  Max.   :119173   Max.   :1293859294   Max.   :27.70        Max.   :28.60     
    ##  NA's   :448      NA's   :652          NA's   :34           NA's   :34        
    ##  Income.composition.of.resources   Schooling   
    ##  Min.   :0.00                    Min.   : 0.0  
    ##  1st Qu.:0.49                    1st Qu.:10.1  
    ##  Median :0.68                    Median :12.3  
    ##  Mean   :0.63                    Mean   :12.0  
    ##  3rd Qu.:0.78                    3rd Qu.:14.3  
    ##  Max.   :0.95                    Max.   :20.7  
    ##  NA's   :167                     NA's   :163

### Observations:

-   The dataset is comprised of 2938 observations and 22 variables
-   There are numerical and categorical variables (Country and Status)
    in the dataset
-   Column names have spaces and special characters that has been
    replaced by R with “.”
-   There are missing values or empty strings in the dataset
-   ????? duplicated records????????
-   ‘Life.expectancy’ is the dependent variable - There are 10 missing
    observations in the dependent variable
-   We need to predict Salary however there is no salary variable in the
    dataset but MonthlyIncome variable seems to be sufficient for this
    purpose.

### Scatterplots

``` r
#####################################################################################
#                        Scatter plots for checking linearity                       #
#####################################################################################

################### Linear - Linear ###################
LifeExp$Status = as.factor(LifeExp$Status)
#num_cols = LifeExp %>% select(where(is.numeric)) %>% colnames()
#MLR_num_LE = LifeExp[, num_cols]
pairs(Life.expectancy~Year+Adult.Mortality+infant.deaths+Alcohol, data=LifeExp, col=ifelse(LifeExp$Status=="Developed", "green", "red"))
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Linear%20regression-1.png)<!-- -->

``` r
pairs(Life.expectancy~log(percentage.expenditure)+Hepatitis.B+Measles+BMI, data=LifeExp, col=ifelse(LifeExp$Status=="Developed", "green", "red"))
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Linear%20regression-2.png)<!-- -->

``` r
pairs(Life.expectancy~under.five.deaths+Polio+Total.expenditure+Diphtheria, data=LifeExp, col=ifelse(LifeExp$Status=="Developed", "green", "red"))
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Linear%20regression-3.png)<!-- -->

``` r
pairs(Life.expectancy~HIV.AIDS+GDP+Population+thinness..1.19.years, data=LifeExp, col=ifelse(LifeExp$Status=="Developed", "green", "red"))
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Linear%20regression-4.png)<!-- -->

``` r
pairs(Life.expectancy~thinness.5.9.years+Income.composition.of.resources+Schooling, data=LifeExp, col=ifelse(LifeExp$Status=="Developed", "green", "red"))
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Linear%20regression-5.png)<!-- -->

``` r
################### Linear - Log transformation ###################
LifeExp$Status = as.factor(LifeExp$Status)
#num_cols = LifeExp %>% select(where(is.numeric)) %>% colnames()
#MLR_num_LE = LifeExp[, num_cols]
pairs(Life.expectancy~Year+Adult.Mortality+infant.deaths+Alcohol, data=LifeExp, col=ifelse(LifeExp$Status=="Developed", "green", "red"))
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Linear%20regression-6.png)<!-- -->

``` r
pairs(Life.expectancy~log(percentage.expenditure)+Hepatitis.B+Measles+BMI, data=LifeExp, col=ifelse(LifeExp$Status=="Developed", "green", "red"))
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Linear%20regression-7.png)<!-- -->

``` r
pairs(Life.expectancy~under.five.deaths+Polio+Total.expenditure+Diphtheria, data=LifeExp, col=ifelse(LifeExp$Status=="Developed", "green", "red"))
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Linear%20regression-8.png)<!-- -->

``` r
pairs(Life.expectancy~log(HIV.AIDS)+log(GDP)+Population+thinness..1.19.years, data=LifeExp, col=ifelse(LifeExp$Status=="Developed", "green", "red"))
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Linear%20regression-9.png)<!-- -->

``` r
pairs(Life.expectancy~thinness.5.9.years+Income.composition.of.resources+Schooling, data=LifeExp, col=ifelse(LifeExp$Status=="Developed", "green", "red"))
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Linear%20regression-10.png)<!-- -->

``` r
################### Log - Log transformation ###################
LifeExp$Status = as.factor(LifeExp$Status)
#num_cols = LifeExp %>% select(where(is.numeric)) %>% colnames()
#MLR_num_LE = LifeExp[, num_cols]
pairs(log(Life.expectancy)~Year+Adult.Mortality+infant.deaths+Alcohol, data=LifeExp, col=ifelse(LifeExp$Status=="Developed", "green", "red"))
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Linear%20regression-11.png)<!-- -->

``` r
pairs(log(Life.expectancy)~log(percentage.expenditure)+Hepatitis.B+Measles+BMI, data=LifeExp, col=ifelse(LifeExp$Status=="Developed", "green", "red"))
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Linear%20regression-12.png)<!-- -->

``` r
pairs(log(Life.expectancy)~under.five.deaths+Polio+Total.expenditure+Diphtheria, data=LifeExp, col=ifelse(LifeExp$Status=="Developed", "green", "red"))
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Linear%20regression-13.png)<!-- -->

``` r
pairs(log(Life.expectancy)~log(HIV.AIDS)+log(GDP)+Population+thinness..1.19.years, data=LifeExp, col=ifelse(LifeExp$Status=="Developed", "green", "red"))
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Linear%20regression-14.png)<!-- -->

``` r
pairs(log(Life.expectancy)~thinness.5.9.years+Income.composition.of.resources+Schooling, data=LifeExp, col=ifelse(LifeExp$Status=="Developed", "green", "red"))
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Linear%20regression-15.png)<!-- -->

### Observations

We can observe linear relationship between Life Expectancy and the
following variables

-   Income composition of resources
-   Schooling
-   Log(HIV.AIDS)
-   Log(GDP)
-   Log(percentage.expenditure)
-   BMI
-   Year
-   Adult.Mortality

\#adding region column to do regional imputation

``` r
###rename ivory coast and remove parenthesis from country names
LifeExp$Country <- replace(LifeExp$Country,LifeExp$Country == "Côte d'Ivoire","Ivory Coast")
LifeExp$Country <-  str_replace(LifeExp$Country,"\\(.*\\)","")
LifeExp$Country <- trimws(LifeExp$Country,"right")

### add region using rworldmap package
regions <- rworldmap::countryRegions
regions <- regions[,c(2,6)]
colnames(regions) <- c("Country","Region")
regions$Country <- replace(regions$Country, regions$Country == "The Bahamas", "Bahamas")
regions$Country <- replace(regions$Country, regions$Country == "Brunei", "Brunei Darussalam")
regions$Country <- replace(regions$Country, regions$Country == "Cape Verde", "Cabo Verde")
regions$Country <- replace(regions$Country, regions$Country == "Republic of the Congo", "Congo")
regions$Country <- replace(regions$Country, regions$Country == "Czech Republic", "Czechia")
regions$Country <- replace(regions$Country, regions$Country == "North Korea", "Democratic People's Republic of Korea")
regions$Country <- replace(regions$Country, regions$Country == "Guinea Bissau", "Guinea-Bissau")
regions$Country <- replace(regions$Country, regions$Country == "Laos","Lao People's Democratic Republic")
regions$Country <- replace(regions$Country, regions$Country == "Federated States of Micronesia", "Micronesia")
regions$Country <- replace(regions$Country, regions$Country == "South Korea", "Republic of Korea")
regions$Country <- replace(regions$Country, regions$Country == "Moldova", "Republic of Moldova")
regions$Country <- replace(regions$Country, regions$Country == "Russia", "Russian Federation")
regions$Country <- replace(regions$Country, regions$Country == "Republic of Serbia", "Serbia")
regions$Country <- replace(regions$Country, regions$Country == "Syria", "Syrian Arab Republic")
regions$Country <- replace(regions$Country, regions$Country == "Macedonia", "The former Yugoslav republic of Macedonia")
regions$Country <- replace(regions$Country, regions$Country == "East Timor", "Timor-Leste")
regions$Country <- replace(regions$Country, regions$Country == "United Kingdom", "United Kingdom of Great Britain and Northern Ireland")
regions$Country <- replace(regions$Country, regions$Country == "Vietnam", "Viet Nam")

LifeExp <-  join(LifeExp,regions,by = "Country", type = 'left')
LifeExp$Country <- replace(LifeExp$Country, LifeExp$Country == "Micronesia", "Micronesia (Federated States of)")
LifeExp$Region <- as.factor(LifeExp$Region)
```

## Fixing the missing values by replacing with median

``` r
# Drop missing values from the dependent variable
LifeExp = LifeExp[!(is.na(LifeExp$Life.expectancy)),]

na_list = colnames(LifeExp)[apply(LifeExp, 2, anyNA)]
# LifeExp_db = apply(LifeExp[,colnames(LifeExp) %in% na_list],2,median,na.rm =  TRUE)
LifeExp = LifeExp %>% group_by(Country) %>% dplyr::mutate(Alcohol = replace(Alcohol,is.na(Alcohol), median(Alcohol, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Country) %>% dplyr::mutate(Hepatitis.B = replace(Hepatitis.B,is.na(Hepatitis.B), median(Hepatitis.B, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Country) %>% dplyr::mutate(BMI = replace(BMI,is.na(BMI), median(BMI, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Country) %>% dplyr::mutate(Polio = replace(Polio,is.na(Polio), median(Polio, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Country) %>% dplyr::mutate(Total.expenditure = replace(Total.expenditure,is.na(Total.expenditure), median(Total.expenditure, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Country) %>% dplyr::mutate(Diphtheria = replace(Diphtheria,is.na(Diphtheria), median(Diphtheria, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Country) %>% dplyr::mutate(GDP = replace(GDP,is.na(GDP), median(GDP, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Country) %>% dplyr::mutate(Population = replace(Population,is.na(Population), median(Population, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Country) %>% dplyr::mutate(thinness..1.19.years = replace(thinness..1.19.years,is.na(thinness..1.19.years), median(thinness..1.19.years, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Country) %>% dplyr::mutate(thinness.5.9.years = replace(thinness.5.9.years,is.na(thinness.5.9.years), median(thinness.5.9.years, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Country) %>% dplyr::mutate(Income.composition.of.resources = replace(Income.composition.of.resources,is.na(Income.composition.of.resources), median(Income.composition.of.resources, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Country) %>% dplyr::mutate(Schooling = replace(Schooling,is.na(Schooling), median(Schooling, na.rm = TRUE)))

#repeat grouping by Region for countries which have only NAs
LifeExp = LifeExp %>% group_by(Region) %>% dplyr::mutate(Alcohol = replace(Alcohol,is.na(Alcohol), median(Alcohol, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Region) %>% dplyr::mutate(Hepatitis.B = replace(Hepatitis.B,is.na(Hepatitis.B), median(Hepatitis.B, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Region) %>% dplyr::mutate(BMI = replace(BMI,is.na(BMI), median(BMI, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Region) %>% dplyr::mutate(Polio = replace(Polio,is.na(Polio), median(Polio, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Region) %>% dplyr::mutate(Total.expenditure = replace(Total.expenditure,is.na(Total.expenditure), median(Total.expenditure, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Region) %>% dplyr::mutate(Diphtheria = replace(Diphtheria,is.na(Diphtheria), median(Diphtheria, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Region) %>% dplyr::mutate(GDP = replace(GDP,is.na(GDP), median(GDP, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Region) %>% dplyr::mutate(Population = replace(Population,is.na(Population), median(Population, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Region) %>% dplyr::mutate(thinness..1.19.years = replace(thinness..1.19.years,is.na(thinness..1.19.years), median(thinness..1.19.years, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Region) %>% dplyr::mutate(thinness.5.9.years = replace(thinness.5.9.years,is.na(thinness.5.9.years), median(thinness.5.9.years, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Region) %>% dplyr::mutate(Income.composition.of.resources = replace(Income.composition.of.resources,is.na(Income.composition.of.resources), median(Income.composition.of.resources, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Region) %>% dplyr::mutate(Schooling = replace(Schooling,is.na(Schooling), median(Schooling, na.rm = TRUE)))
# convert the tibble to data frame
LifeExp = as.data.frame(LifeExp)

#impute USA values
#USA GDP
US_GDP <- read.csv('API_NY.GDP.PCAP.CD_DS2_en_csv_v2_4150786.csv',header = FALSE)
colnames(US_GDP) <- US_GDP[3,]
US_GDP <- rename(US_GDP,c("Country Name"="Country"))
US_GDP <- US_GDP[US_GDP$Country == "United States",c(1,45:60)]
US_GDP <- US_GDP %>% pivot_longer(!Country,names_to = "Year",values_to = "GDP2")
US_GDP$Country <- replace(US_GDP$Country, US_GDP$Country == "United States", "United States of America")
US_GDP$Year <- as.integer(US_GDP$Year)

LifeExp <- left_join(LifeExp,US_GDP, by=c("Country","Year"))
LifeExp <- LifeExp %>% dplyr::mutate(GDP = ifelse(LifeExp$Country == "United States of America", LifeExp$GDP2, LifeExp$GDP))

#US Schooling
US_Scho <- read.csv("Expected years of schooling (years).csv",skip = 6,header = FALSE)
colnames(US_Scho) <- US_Scho[1,]
US_Scho$Country <- trimws(US_Scho$Country, which = "both")
US_Scho <- US_Scho[US_Scho$Country == "United States", colSums(is.na(US_Scho)) !=nrow(US_Scho)]
US_Scho <- US_Scho[,c(2,13:28)]
US_Scho <- US_Scho %>% pivot_longer(!Country,names_to = "Year",values_to = "Schooling2")
US_Scho$Year <- as.integer(US_Scho$Year)
US_Scho$Schooling2 <- as.numeric(US_Scho$Schooling2)
US_Scho$Country <- replace(US_Scho$Country, US_Scho$Country == "United States", "United States of America")


LifeExp <- left_join(LifeExp,US_Scho,by=c("Country","Year"))
LifeExp <- LifeExp %>% dplyr::mutate(Schooling =ifelse(LifeExp$Country == "United States of America",LifeExp$Schooling2,LifeExp$Schooling))

#US income composition
US_Inc <- read.csv('Income index.csv',skip = 5, header = FALSE)
colnames(US_Inc) <- US_Inc[1,]
US_Inc$Country <- trimws(US_Inc$Country,which = "both")
US_Inc <- US_Inc[US_Inc$Country == "United States",colSums(is.na(US_Inc)) !=nrow(US_Inc)]
US_Inc <- US_Inc[,c(2,13:28)]
US_Inc <- US_Inc %>%  pivot_longer(!Country,names_to = "Year", values_to = "comp2")
US_Inc$Year <- as.integer(US_Inc$Year)
US_Inc$comp2 <- as.numeric(US_Inc$comp2)
US_Inc$Country <- replace(US_Inc$Country,US_Inc$Country == "United States", "United States of America")

LifeExp <- left_join(LifeExp,US_Inc,by=c("Country","Year"))
LifeExp <- LifeExp %>% dplyr::mutate("Income.composition.of.resources" = ifelse(LifeExp$Country == "United States of America", LifeExp$comp2,LifeExp$Income.composition.of.resources))

#Add population from external source
#data from UN in thousands 
pop_all <- read.csv('WPP2019_TotalPopulationBySex.csv')
pop_all <- pop_all %>% dplyr::select(Country = Location, Year = Time, Population2 = PopTotal) %>% mutate(Population2 = Population2*1000) %>% filter(Year %in% c(2000:2015))

#clean country names to match before merge
pop_all$Country <- replace(pop_all$Country, pop_all$Country == "Bolivia (Plurinational State of)", "Bolivia")
pop_all$Country <- replace(pop_all$Country,pop_all$Country == "Côte d'Ivoire", "Ivory Coast")
pop_all$Country <- replace(pop_all$Country,pop_all$Country == "Dem. People's Republic of Korea","Democratic People's Republic of Korea")
pop_all$Country <- replace(pop_all$Country, pop_all$Country == "Iran (Islamic Republic of)", "Iran")
pop_all$Country <- replace(pop_all$Country, pop_all$Country == "Micronesia (Fed. States of)", "Micronesia (Federated States of)")
pop_all$Country <- replace(pop_all$Country, pop_all$Country == "Eswatini", "Swaziland")
pop_all$Country <- replace(pop_all$Country, pop_all$Country == "North Macedonia", "The former Yugoslav republic of Macedonia")
pop_all$Country <- replace(pop_all$Country, pop_all$Country == "United Kingdom", "United Kingdom of Great Britain and Northern Ireland")
pop_all$Country <- replace(pop_all$Country, pop_all$Country == "Venezuela (Bolivarian Republic of)", "Venezuela")
LifeExp<- left_join(LifeExp,pop_all,by=c("Country","Year")) %>% mutate(Population = Population2)

#replace adult.mortality since there are clear mistakes with the data that could
#not be resolved with transformation of the variable
adlt_mort <- read.csv('Adult_mort.csv',header = TRUE)
adlt_mort <- adlt_mort %>% dplyr::select("Country" = Location,"Year" = Period,"Adult.Mort2" = Value)
adlt_mort$Country <- replace(adlt_mort$Country,adlt_mort$Country == "Côte d’Ivoire","Ivory Coast")
adlt_mort$Country <- replace(adlt_mort$Country,adlt_mort$Country == "Bolivia (Plurinational State of)","Bolivia")
adlt_mort$Country <- replace(adlt_mort$Country,adlt_mort$Country == "Venezuela (Bolivarian Republic of)","Venezuela")
adlt_mort$Country <- replace(adlt_mort$Country,adlt_mort$Country == "Iran (Islamic Republic of)", "Iran")
adlt_mort$Country <- replace(adlt_mort$Country,adlt_mort$Country == "Eswatini", "Swaziland")
adlt_mort$Country <- replace(adlt_mort$Country,adlt_mort$Country == "The former Yugoslav Republic of Macedonia", "The former Yugoslav republic of Macedonia")

LifeExp <- left_join(LifeExp,adlt_mort, by=c("Country","Year"))
LifeExp <- LifeExp %>% 
  dplyr::mutate("Adult.Mortality" = Adult.Mort2)
#drop new variables
drop = c("GDP2","Schooling2","comp2", "Population2","Adult.Mort2")
LifeExp <- LifeExp[,!colnames(LifeExp) %in% drop]

### Transform countries to continents
library(countrycode)
LifeExp$Continent = countrycode(sourcevar = LifeExp[, "Country"], origin = "country.name", destination = "continent")
LifeExp$Continent = as.factor(LifeExp$Continent)

#LifeExpKNN = LifeExp
```

### Full Correlation Matrix for Linear Regression (Life.expectancy)

``` r
#####################################################################################
#      Full Correlation Matrix for Linear Regression (Life.expectancy)              #
#####################################################################################
# Filter for data to be included
num_cols = LifeExp %>% dplyr::select(where(is.numeric)) %>% colnames()
LifeExpcorr = LifeExp[,num_cols]
corrplot(cor(LifeExpcorr), method = 'square', order = 'AOE', addCoef.col = 'black', 
         cl.pos = 'n', col = COL2('BrBG'))
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-9-1.png" angle=90 style="display: block; margin: auto;" />

``` r
plot_correlate(LifeExp)
```

    ## Warning: 'plot_correlate' is deprecated.
    ## Use 'plot.correlate' instead.
    ## See help("Deprecated")

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-9-2.png" angle=90 style="display: block; margin: auto;" />

### Observations

-   Under five death and infant death are perfectly correlated. They are
    describing the same thing. One of the variable is redundant.
-   GDP and percentage expenditure are perfectly correlated. They are
    describing the same thing. One of the variable is redundant.
-   Schooling and Income Composition of Resources have a strong positive
    correlation
-   Life Expectancy and Adult Mortality are highly negatively correlated
-   Life Expectancy and HIV.AIDS are moderately correlated
-   Life Expectancy and BMI are moderately correlated
-   Life Expectancy and Schooling are highly correlated
-   Life Expectancy and Income Composition of Resources are highly
    correlated

## Uni-variate analysis

``` r
#####################################################################################
#                               Uni-variate analysis                                #
#####################################################################################
# Let's plot the summary statistics
# Univariate analysis
num_cols = LifeExp %>% dplyr::select(where(is.numeric)) %>% colnames()
num_cols_exclude = c('Year')
num_cols_plots = noquote(unlist(num_cols[!( num_cols %in% num_cols_exclude)]))
nrows = length(num_cols_plots)
for (i in num_cols_plots)
{
box_p = LifeExp %>%
  ggplot(aes(x="", y = .data[[i]])) +
  geom_boxplot(fill = "sandybrown", color = "black") + 
  coord_flip() + theme_classic() + xlab("") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  ylab(i)
hist_p = LifeExp %>%
  ggplot() +
  geom_histogram(aes(x = .data[[i]], y = (..count..)/sum(..count..)),
                 position = "identity", bins = 30, 
                 fill = "sandybrown", color = "black") +
  ylab("Relative Frequency") +
  theme_classic() + xlab(i) + ggtitle(paste(i, "- Univariate Analysis")) + 
  theme(plot.title = element_text(hjust = 0.5))
egg::ggarrange(hist_p, box_p, heights = 2:1) 
}
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-10-1.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-10-2.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-10-3.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-10-4.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-10-5.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-10-6.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-10-7.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-10-8.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-10-9.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-10-10.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-10-11.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-10-12.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-10-13.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-10-14.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-10-15.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-10-16.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-10-17.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-10-18.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-10-19.png" angle=90 style="display: block; margin: auto;" />

### Categorical data plots

``` r
#####################################################################################
#                               Categorical data plots                              #
#####################################################################################
num_var = LifeExp %>% dplyr::select(where(is.numeric)) %>% colnames()
cat_cols = LifeExp %>% dplyr::select(where(is.factor)) %>% colnames()
num_ex = c('Year')
num_var_plots = noquote(unlist(num_var[!( num_var %in% num_ex)]))
# Plot all categorical variables
for (c in cat_cols)
{
  cat_plot = LifeExp %>% ggplot(aes(x= .data[[c]], group = 1)) + 
    geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
    geom_text(aes( label = scales::percent(..prop..),
                   y= ..prop.. ), stat= "count", vjust = -.5) +
    labs(y = "Percent") +
    scale_y_continuous(labels = scales::percent) + theme(legend.position = "none") +
    ggtitle(paste(c, "Categorical Analysis")) + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) # +
    #scale_fill_brewer(palette="Oranges")
    egg::ggarrange(cat_plot, ncol=2) 
}
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-11-1.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-11-2.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-11-3.png" angle=90 style="display: block; margin: auto;" />

## Bi-variate analysis with Status variable

``` r
#####################################################################################
#                     Bi-variate analysis with Status variable                      #
#####################################################################################
for (i in num_var_plots)
{
multibox = LifeExp %>%
  ggplot(aes(x=Status, y = .data[[i]])) +
  geom_boxplot(fill = "sandybrown", color = "black") + 
  xlab("Status") +
  ylab(i) + stat_summary(fun=mean, geom="point", shape=20, size=7, color="red", fill="red") +
  ggtitle(paste(i, "vs Status bi-variate analysis")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette = "Oranges")  
egg::ggarrange(multibox, ncol=2)
}
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-12-1.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-12-2.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-12-3.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-12-4.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-12-5.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-12-6.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-12-7.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-12-8.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-12-9.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-12-10.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-12-11.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-12-12.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-12-13.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-12-14.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-12-15.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-12-16.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-12-17.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-12-18.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-12-19.png" angle=90 style="display: block; margin: auto;" />

## Bi-variate analysis with Region variable

``` r
#####################################################################################
#                     Bi-variate analysis with Region variable                   #
#####################################################################################
for (i in num_var_plots)
{
multibox = LifeExp %>%
  ggplot(aes(x=Region, y = .data[[i]])) +
  geom_boxplot(fill = "sandybrown", color = "black") + 
  xlab("Region") +
  ylab(i) + stat_summary(fun=mean, geom="point", shape=20, size=7, color="red", fill="red") +
  ggtitle(paste(i, "vs Region bi-variate analysis")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    scale_fill_brewer(palette = "Oranges")  
egg::ggarrange(multibox, ncol=2)
}
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-13-1.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-13-2.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-13-3.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-13-4.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-13-5.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-13-6.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-13-7.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-13-8.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-13-9.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-13-10.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-13-11.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-13-12.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-13-13.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-13-14.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-13-15.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-13-16.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-13-17.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-13-18.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-13-19.png" angle=90 style="display: block; margin: auto;" />
\## Overall life expectancy over time

``` r
#####################################################################################
#                          Overall life expectancy over time                        #
#####################################################################################

mean_LifeExp = LifeExp %>% group_by(Year) %>% summarise_at(vars(Life.expectancy), list(meanle = mean))
mean_LifeExp_reg = LifeExp %>% group_by(Year, Region) %>% summarise_at(vars(Life.expectancy), list(meanle = mean))
mean_LifeExp_cont = LifeExp %>% group_by(Year, Continent) %>% summarise_at(vars(Life.expectancy), list(meanle = mean))

#Overall life expectancy
ggplot(data=mean_LifeExp, aes(x=Year, y=meanle)) +
  geom_line()+
  geom_point() +
   ggtitle("Mean life expactancy by year") +
  theme(plot.title = element_text(hjust = 0.5))+
   xlab("Year") + ylab("Average Life Expectancy")
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-14-1.png" angle=90 style="display: block; margin: auto;" />

``` r
#Overall life expectancy by Region
ggplot(data=mean_LifeExp_reg, aes(x=Year, y=meanle, group = Region)) +
  geom_line(aes(color=Region))+
  geom_point(aes(color=Region)) +
  ggtitle("Mean life expactancy by year by region") +
theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Year") + ylab("Average Life Expectancy")
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-14-2.png" angle=90 style="display: block; margin: auto;" />

``` r
#Overall life expectancy by Continent
ggplot(data=mean_LifeExp_cont, aes(x=Year, y=meanle, group = Continent)) +
  geom_line(aes(color=Continent))+
  geom_point(aes(color=Continent)) +
  ggtitle("Mean life expactancy by year by Continent") +
theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Year") + ylab("Average Life Expectancy")
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-14-3.png" angle=90 style="display: block; margin: auto;" />

``` r
#####################################################################################
#                                     Data Preparation                              #
#####################################################################################

### Log Transform variables
LifeExp = mutate(LifeExp, log.HIV.AIDS = ifelse(HIV.AIDS==0, log(HIV.AIDS+1), log(HIV.AIDS)))
LifeExp = mutate(LifeExp, log.GDP = ifelse(GDP==0, log(GDP+1), log(GDP)))
LifeExp = mutate(LifeExp, log.percentage.expenditure = ifelse(percentage.expenditure==0, log(percentage.expenditure+1), log(percentage.expenditure)))



### Make country as factor
LifeExp$Country = as.factor(LifeExp$Country)

#drop = c('HIV.AIDS', 'percentage.expenditure', 'GDP')#keeping country in
#LifeExp = LifeExp[, !(colnames(LifeExp) %in% drop)]
```

``` r
#####################################################################################
#                    Split the Data to Train and Test sets (85%-15%)               #
#####################################################################################
index<-sample(1:dim(LifeExp)[1],round(dim(LifeExp)[1]*0.85),replace=F)
train = LifeExp[index,]
test = LifeExp[-index,]

# Create training and test set for regression models
# Droping the variables which were log transformed, as well as country and region or countinent
#drop_for_reg = c(-1, -8, -16, -17, -27)
drop_for_reg = c("Country", "HIV.AIDS", "GDP", "percentage.expenditure", "Region")
rtrain = train[,!colnames(train) %in% drop_for_reg]
rtest = test[,!colnames(train) %in% drop_for_reg]
x=model.matrix(Life.expectancy~.,rtrain)[,-1]
y=rtrain$Life.expectancy
xtest = model.matrix(Life.expectancy~.,rtest)[,-1]
ytest = rtest$Life.expectancy

# Create training and test set for KNN model

incl_for_knn = c(1, 5, 7, 16, 18, 3, 21, 4)
ktrain = train[,incl_for_knn]
ktest = test[,incl_for_knn]
kx=ktrain[,-4]
ky=ktrain$Life.expectancy
kxtest = ktest[,-4]
kytest = ktest$Life.expectancy
```

``` r
#####################################################################################
#                             EDA on Train sets                                    #
#####################################################################################

describe(train)
```

    ## # A tibble: 23 × 26
    ##    described_variabl…     n    na   mean     sd se_mean    IQR skewness kurtosis
    ##    <chr>              <int> <int>  <dbl>  <dbl>   <dbl>  <dbl>    <dbl>    <dbl>
    ##  1 Year                2489     0 2.01e3 4.59e0 9.20e-2   7    -0.00819   -1.20 
    ##  2 Life.expectancy     2489     0 6.91e1 9.61e0 1.93e-1  12.7  -0.629     -0.253
    ##  3 Adult.Mortality     2489     0 1.96e2 1.16e2 2.33e+0 146.    1.33       1.85 
    ##  4 infant.deaths       2489     0 3.18e1 1.22e2 2.44e+0  23     9.33     105.   
    ##  5 Alcohol             2489     0 4.56e0 4.04e0 8.09e-2   6.71  0.621     -0.748
    ##  6 percentage.expend…  2489     0 7.16e2 1.96e3 3.92e+1 419.    4.80      28.7  
    ##  7 Hepatitis.B         2489     0 8.02e1 2.45e1 4.91e-1  21    -1.82       2.44 
    ##  8 Measles             2489     0 2.54e3 1.18e4 2.37e+2 410     9.27     111.   
    ##  9 BMI                 2489     0 3.80e1 2.00e1 4.01e-1  36.7  -0.227     -1.32 
    ## 10 under.five.deaths   2489     0 4.41e1 1.66e2 3.33e+0  30     9.09     101.   
    ## # … with 13 more rows, and 17 more variables: p00 <dbl>, p01 <dbl>, p05 <dbl>,
    ## #   p10 <dbl>, p20 <dbl>, p25 <dbl>, p30 <dbl>, p40 <dbl>, p50 <dbl>,
    ## #   p60 <dbl>, p70 <dbl>, p75 <dbl>, p80 <dbl>, p90 <dbl>, p95 <dbl>,
    ## #   p99 <dbl>, p100 <dbl>

## Observations

\*\* A High degree of skewness can be identified in Infant Deaths \*\* A
High degree of skewness can be identified in Measles \*\* A High degree
of skewness can be identified in under.five.deaths \*\* A High degree of
skewness can be identified in Population

``` r
normality(train) 
```

    ## # A tibble: 23 × 4
    ##    vars                   statistic  p_value sample
    ##    <chr>                      <dbl>    <dbl>  <dbl>
    ##  1 Year                       0.948 7.20e-29   2489
    ##  2 Life.expectancy            0.956 9.84e-27   2489
    ##  3 Adult.Mortality            0.890 7.28e-39   2489
    ##  4 infant.deaths              0.239 2.12e-72   2489
    ##  5 Alcohol                    0.908 2.33e-36   2489
    ##  6 percentage.expenditure     0.399 9.65e-68   2489
    ##  7 Hepatitis.B                0.723 1.44e-53   2489
    ##  8 Measles                    0.212 4.17e-73   2489
    ##  9 BMI                        0.926 2.73e-33   2489
    ## 10 under.five.deaths          0.247 3.40e-72   2489
    ## # … with 13 more rows

``` r
#Runs a Shapario-Wilk Tests, if the p-value is >= .05 then the data is normally distrusted, if <0.05 the data is not normally distrusted.

#Find Features that are not normally distributed 

train %>%
  normality() %>%
  filter(p_value < 0.05) %>%
  arrange(abs(p_value))
```

    ## # A tibble: 23 × 4
    ##    vars                   statistic  p_value sample
    ##    <chr>                      <dbl>    <dbl>  <dbl>
    ##  1 Measles                    0.212 4.17e-73   2489
    ##  2 Population                 0.226 9.42e-73   2489
    ##  3 infant.deaths              0.239 2.12e-72   2489
    ##  4 under.five.deaths          0.247 3.40e-72   2489
    ##  5 HIV.AIDS                   0.359 5.46e-69   2489
    ##  6 percentage.expenditure     0.399 9.65e-68   2489
    ##  7 GDP                        0.543 1.47e-62   2489
    ##  8 Diphtheria                 0.690 1.58e-55   2489
    ##  9 Polio                      0.692 2.12e-55   2489
    ## 10 log.HIV.AIDS               0.712 3.11e-54   2489
    ## # … with 13 more rows

``` r
# Verify non normality and transformation options of the variability 
plot_normality(train)
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/normality-1.png)<!-- -->![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/normality-2.png)<!-- -->![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/normality-3.png)<!-- -->![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/normality-4.png)<!-- -->![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/normality-5.png)<!-- -->![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/normality-6.png)<!-- -->![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/normality-7.png)<!-- -->![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/normality-8.png)<!-- -->![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/normality-9.png)<!-- -->![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/normality-10.png)<!-- -->![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/normality-11.png)<!-- -->![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/normality-12.png)<!-- -->![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/normality-13.png)<!-- -->![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/normality-14.png)<!-- -->![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/normality-15.png)<!-- -->![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/normality-16.png)<!-- -->![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/normality-17.png)<!-- -->![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/normality-18.png)<!-- -->![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/normality-19.png)<!-- -->![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/normality-20.png)<!-- -->![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/normality-21.png)<!-- -->![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/normality-22.png)<!-- -->![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/normality-23.png)<!-- -->

## Observations

\*\* The p-value of the Population is less than 0.0001 signifying it is
non normally and should be transformed if utilized in the following
models.  
\*\* The p-value of the Measles is less than 0.0001 signifying it is non
normally and should be transformed if utilized in the following models.
\*\* The p-value of the infant.deaths is less than 0.0001 signifying it
is non normally and should be transformed if utilized in the following
models. \*\* The p-value of the under.five.deaths is less than 0.0001
signifying it is non normally and should be transformed if utilized in
the following models. \*\* The p-value of the Hepatitis.B is less than
0.0001 signifying it is non normally and should be transformed if
utilized in the following models. \*\* The p-value of the Diphtheria is
less than 0.0001 signifying it is non normally and should be transformed
if utilized in the following models. \*\* The p-value of the Polio is
less than 0.0001 signifying it is non normally and should be transformed
if utilized in the following models. \*\* The p-value of the
log.HIV.AIDS is less than 0.0001 signifying it is non normally and
should be transformed if utilized in the following models. \*\* The
p-value of the thinness.5.9.years is less than 0.0001 signifying it is
non normally and should be transformed if utilized in the following
models. \*\* The p-value of the thinness..1.19.years is less than 0.0001
signifying it is non normally and should be transformed if utilized in
the following models.

##################################################################################### 

# Modeling

##################################################################################### 

``` r
#Prediction function
predict.regsubsets =function (object , newdata ,id ,...){
  form=as.formula (object$call [[2]])
  mat=model.matrix(form ,newdata )
  coefi=coef(object ,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

### Evaluation Data Frame
### test_ASE, R-squared/Adjusted R-squared
eval_df = data.frame()
eval_df = eval_df %>% dplyr::mutate(ID = row_number())

#####################################################################################
#                                       Lasso                                       #
#####################################################################################
library(glmnet)

grid=10^seq(10,-2, length =100)
lasso.mod=glmnet(x,y,alpha=1, lambda =grid)
cv.out=cv.glmnet(x,y,alpha=1)
plot(cv.out)
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-1.png)<!-- -->

``` r
bestlambda = cv.out$lambda.min  #Optimal penalty parameter.  You can make this call visually.
lasso.pred=predict(lasso.mod ,s=bestlambda ,newx=xtest)

testMSE_LASSO<-mean((ytest-lasso.pred)^2)
testMSE_LASSO
```

    ## [1] 6.109

``` r
coef(lasso.mod,s=bestlambda)
```

    ## 25 x 1 sparse Matrix of class "dgCMatrix"
    ##                                              s1
    ## (Intercept)                     38.913158907625
    ## Year                             0.015210240258
    ## StatusDeveloping                -0.682427202646
    ## Adult.Mortality                 -0.055330690370
    ## infant.deaths                    .             
    ## Alcohol                          0.068195557874
    ## Hepatitis.B                     -0.004353091479
    ## Measles                         -0.000015002591
    ## BMI                              0.009516994883
    ## under.five.deaths               -0.002159911431
    ## Polio                            0.012583053007
    ## Total.expenditure                .             
    ## Diphtheria                       0.015037712650
    ## Population                       0.000000001792
    ## thinness..1.19.years            -0.002961333914
    ## thinness.5.9.years              -0.012291062869
    ## Income.composition.of.resources  2.254910145188
    ## Schooling                        0.405398343412
    ## ContinentAmericas                2.554524807036
    ## ContinentAsia                    0.983989599495
    ## ContinentEurope                  1.868664377329
    ## ContinentOceania                 0.788391884258
    ## log.HIV.AIDS                     .             
    ## log.GDP                          0.134116940276
    ## log.percentage.expenditure       0.062280687460

``` r
lasso_residuals = (ytest - lasso.pred)
hist(lasso_residuals, main = "Histogram of Residuals (LASSO)")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-2.png)<!-- -->

``` r
plot(lasso_residuals, main = "Residuals plot (LASSO)") 
abline(h=0, col="blue")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-3.png)<!-- -->

``` r
# Metrics RMSE; R-squared; MAE
postResample(pred = lasso.pred, obs = ytest)
```

    ##     RMSE Rsquared      MAE 
    ##   2.4717   0.9252   1.7455

``` r
##### Fit Linear Model based on LASSO regularization without factors to measure VIF####
#fit.lasso.lm = lm(Life.expectancy ~ Year + Adult.Mortality + Alcohol + Hepatitis.B + Measles + BMI + under.five.deaths + Polio + #Diphtheria + Population + thinness.5.9.years + Income.composition.of.resources + Schooling + log.GDP + #log.percentage.expenditure, data = train_lasso)

### Continent included
fit.lasso.lm = lm(Life.expectancy ~ Year + Adult.Mortality + Alcohol + Hepatitis.B + Measles + BMI + under.five.deaths + Polio + Diphtheria + Population + thinness..1.19.years + thinness.5.9.years + Income.composition.of.resources + Schooling + log.GDP + log.percentage.expenditure, data = rtrain)
summary(fit.lasso.lm)
```

    ## 
    ## Call:
    ## lm(formula = Life.expectancy ~ Year + Adult.Mortality + Alcohol + 
    ##     Hepatitis.B + Measles + BMI + under.five.deaths + Polio + 
    ##     Diphtheria + Population + thinness..1.19.years + thinness.5.9.years + 
    ##     Income.composition.of.resources + Schooling + log.GDP + log.percentage.expenditure, 
    ##     data = rtrain)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -13.423  -1.592  -0.324   1.313  11.001 
    ## 
    ## Coefficients:
    ##                                       Estimate     Std. Error t value
    ## (Intercept)                     71.72412075037 24.28963230217    2.95
    ## Year                            -0.00086511486  0.01214067889   -0.07
    ## Adult.Mortality                 -0.05876804423  0.00064871388  -90.59
    ## Alcohol                          0.17087342098  0.01699616336   10.05
    ## Hepatitis.B                     -0.00661790601  0.00265926580   -2.49
    ## Measles                         -0.00002307951  0.00000536028   -4.31
    ## BMI                              0.01461271593  0.00337832608    4.33
    ## under.five.deaths               -0.00214694352  0.00049818829   -4.31
    ## Polio                            0.01165443879  0.00310372893    3.75
    ## Diphtheria                       0.01780219327  0.00334287918    5.33
    ## Population                       0.00000000236  0.00000000054    4.37
    ## thinness..1.19.years            -0.02328214244  0.03528439109   -0.66
    ## thinness.5.9.years              -0.02332357531  0.03454830111   -0.68
    ## Income.composition.of.resources  2.27179031933  0.45032265892    5.04
    ## Schooling                        0.41926017342  0.03083507702   13.60
    ## log.GDP                          0.14840654019  0.04267701648    3.48
    ## log.percentage.expenditure       0.05208147975  0.02320813005    2.24
    ##                                             Pr(>|t|)    
    ## (Intercept)                                  0.00318 ** 
    ## Year                                         0.94320    
    ## Adult.Mortality                 < 0.0000000000000002 ***
    ## Alcohol                         < 0.0000000000000002 ***
    ## Hepatitis.B                                  0.01289 *  
    ## Measles                                   0.00001730 ***
    ## BMI                                       0.00001583 ***
    ## under.five.deaths                         0.00001700 ***
    ## Polio                                        0.00018 ***
    ## Diphtheria                                0.00000011 ***
    ## Population                                0.00001305 ***
    ## thinness..1.19.years                         0.50942    
    ## thinness.5.9.years                           0.49968    
    ## Income.composition.of.resources           0.00000049 ***
    ## Schooling                       < 0.0000000000000002 ***
    ## log.GDP                                      0.00051 ***
    ## log.percentage.expenditure                   0.02491 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.58 on 2472 degrees of freedom
    ## Multiple R-squared:  0.928,  Adjusted R-squared:  0.928 
    ## F-statistic: 2e+03 on 16 and 2472 DF,  p-value: <0.0000000000000002

``` r
### Visualize VIF
fit.lasso.lm_VIF = vif(fit.lasso.lm)
barplot(fit.lasso.lm_VIF, main = 'VIF Values (LASSO)', horiz = TRUE, col="blue", xlim = c(0,12))
abline(v=10, col="red")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-4.png)<!-- -->

``` r
### Fit linear model with factors
#fit.lasso.lm3 = lm(Life.expectancy ~ Status + Region + Year + Adult.Mortality + Alcohol + Hepatitis.B + Measles + BMI + under.five.deaths + Polio + Diphtheria + Population + thinness.5.9.years + Income.composition.of.resources + Schooling + log.GDP + log.percentage.expenditure, data = train_lasso)

### Continent included
fit.lasso.lm3 = lm(Life.expectancy ~ Status + Continent + Year + Adult.Mortality + Alcohol + Hepatitis.B + Measles + BMI + under.five.deaths + Polio + Diphtheria + Population + thinness..1.19.years + thinness.5.9.years + Income.composition.of.resources + Schooling + log.GDP + log.percentage.expenditure, data = rtrain)

#### Hypothesis testing ####
summary(fit.lasso.lm3)
```

    ## 
    ## Call:
    ## lm(formula = Life.expectancy ~ Status + Continent + Year + Adult.Mortality + 
    ##     Alcohol + Hepatitis.B + Measles + BMI + under.five.deaths + 
    ##     Polio + Diphtheria + Population + thinness..1.19.years + 
    ##     thinness.5.9.years + Income.composition.of.resources + Schooling + 
    ##     log.GDP + log.percentage.expenditure, data = rtrain)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -12.41  -1.50  -0.29   1.14  10.15 
    ## 
    ## Coefficients:
    ##                                        Estimate      Std. Error t value
    ## (Intercept)                     26.701702526520 24.042250034403    1.11
    ## StatusDeveloping                -0.731227025190  0.207556926596   -3.52
    ## ContinentAmericas                2.737744817628  0.207040167773   13.22
    ## ContinentAsia                    1.148916818553  0.178604538637    6.43
    ## ContinentEurope                  2.052569171618  0.250700876160    8.19
    ## ContinentOceania                 0.992539269468  0.276200569704    3.59
    ## Year                             0.021282736363  0.012014794441    1.77
    ## Adult.Mortality                 -0.054955070137  0.000729411659  -75.34
    ## Alcohol                          0.068153244868  0.021010789789    3.24
    ## Hepatitis.B                     -0.005792826269  0.002583081966   -2.24
    ## Measles                         -0.000016148459  0.000005208101   -3.10
    ## BMI                              0.009385770611  0.003377710366    2.78
    ## under.five.deaths               -0.002447758320  0.000485215599   -5.04
    ## Polio                            0.013026077820  0.003006133091    4.33
    ## Diphtheria                       0.015639225010  0.003245372742    4.82
    ## Population                       0.000000002110  0.000000000528    4.00
    ## thinness..1.19.years             0.001066950114  0.034242755071    0.03
    ## thinness.5.9.years              -0.013689549029  0.033842314209   -0.40
    ## Income.composition.of.resources  2.214555341527  0.443608409009    4.99
    ## Schooling                        0.398911026518  0.030181787447   13.22
    ## log.GDP                          0.129272221073  0.041668306039    3.10
    ## log.percentage.expenditure       0.069164565191  0.022583379583    3.06
    ##                                             Pr(>|t|)    
    ## (Intercept)                                  0.26684    
    ## StatusDeveloping                             0.00043 ***
    ## ContinentAmericas               < 0.0000000000000002 ***
    ## ContinentAsia                    0.00000000015001429 ***
    ## ContinentEurope                  0.00000000000000042 ***
    ## ContinentOceania                             0.00033 ***
    ## Year                                         0.07662 .  
    ## Adult.Mortality                 < 0.0000000000000002 ***
    ## Alcohol                                      0.00120 ** 
    ## Hepatitis.B                                  0.02501 *  
    ## Measles                                      0.00195 ** 
    ## BMI                                          0.00550 ** 
    ## under.five.deaths                0.00000048733102222 ***
    ## Polio                            0.00001528655356616 ***
    ## Diphtheria                       0.00000153088929503 ***
    ## Population                       0.00006576862056827 ***
    ## thinness..1.19.years                         0.97515    
    ## thinness.5.9.years                           0.68587    
    ## Income.composition.of.resources  0.00000063881649327 ***
    ## Schooling                       < 0.0000000000000002 ***
    ## log.GDP                                      0.00194 ** 
    ## log.percentage.expenditure                   0.00222 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.49 on 2467 degrees of freedom
    ## Multiple R-squared:  0.933,  Adjusted R-squared:  0.933 
    ## F-statistic: 1.64e+03 on 21 and 2467 DF,  p-value: <0.0000000000000002

``` r
# At alpha = 0.05 the following variables are not significant therefore don't contribute to the model performance:
# Hepatitis.B, thinness.5.9.years, log.GDP, log.percentage.expenditure

# Predicting
train_pred = predict(fit.lasso.lm3, rtrain)
test_pred = predict(fit.lasso.lm3, rtest)

# Scoring the final model on Training and Test set
residuals = resid(fit.lasso.lm3)
train_score = postResample(pred = train_pred, obs = rtrain$Life.expectancy)
test_score = postResample(pred = test_pred, obs = rtest$Life.expectancy)
sm = summary(fit.lasso.lm3)
#mse = mean(sm$residuals^2)
#print(paste("Lasso MLR MSE:", mse))

## Test scores
rmse = test_score[1]
rsqd = test_score[2]
mse = rmse^2
n=dim(xtest)[1]
p = length(fit.lasso.lm3$coefficients)-1
adjrsqd = 1 - (1 - rsqd) * ((n - 1)/(n-p-1))

### Checking Multiple Liner Regression model assumptions
confint(fit.lasso.lm3)
```

    ##                                            2.5 %          97.5 %
    ## (Intercept)                     -20.443371869109 73.846776922150
    ## StatusDeveloping                 -1.138230809433 -0.324223240947
    ## ContinentAmericas                 2.331754359223  3.143735276032
    ## ContinentAsia                     0.798686526073  1.499147111033
    ## ContinentEurope                   1.560963292771  2.544175050464
    ## ContinentOceania                  0.450930377281  1.534148161655
    ## Year                             -0.002277387043  0.044842859769
    ## Adult.Mortality                  -0.056385392460 -0.053524747814
    ## Alcohol                           0.026952639838  0.109353849897
    ## Hepatitis.B                      -0.010858058985 -0.000727593552
    ## Measles                          -0.000026361159 -0.000005935759
    ## BMI                               0.002762330365  0.016009210856
    ## under.five.deaths                -0.003399230229 -0.001496286412
    ## Polio                             0.007131273131  0.018920882509
    ## Diphtheria                        0.009275289058  0.022003160962
    ## Population                        0.000000001075  0.000000003145
    ## thinness..1.19.years             -0.066080560331  0.068214460560
    ## thinness.5.9.years               -0.080051824557  0.052672726500
    ## Income.composition.of.resources   1.344672056255  3.084438626798
    ## Schooling                         0.339726773274  0.458095279762
    ## log.GDP                           0.047563754298  0.210980687848
    ## log.percentage.expenditure        0.024880227867  0.113448902516

``` r
hist(residuals, main = "Histogram of Residuals (Lasso MLR fit)")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-5.png)<!-- -->

``` r
plot(residuals, main = "Residuals plot (Lasso MLR fit)") 
abline(h=0, col="blue")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-6.png)<!-- -->

``` r
plot(fit.lasso.lm3, which = 2)
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-7.png)<!-- -->

``` r
plot(fit.lasso.lm3, which = 4)
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-8.png)<!-- -->

``` r
##### Visualize prediction vs actual
x_lasso = 1:dim(xtest)[1]
plot(x_lasso, ytest, col = "red", type = "l", lwd=2,
     main = "Life Expectancy prediction (LASSO)", ylab="Life expectancy")
lines(x_lasso, test_pred, col = "blue", lwd=2)
legend("topright",  legend = c("Original observation", "predicted life expectancy"), 
       fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
grid()
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-9.png)<!-- -->

``` r
#### Scatter plot
plot(test_pred ~ ytest, main = "Original vs Predicted scatter plot (LASSO MLR fit)", xlab = 'Original observations', ylab='Predicted values')
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-10.png)<!-- -->

``` r
### Evaluation Data Frame
### test_ASE, R-squared/Adjusted R-squared, AIC, and BIC
eval_df = data.frame(model_name = 'LASSO', MSE=format(round(mse,4),nsmall=4), R_Squared=format(round(rsqd,4),nsmall=4), AdjR_Squared=format(round(adjrsqd,4),nsmall=4), RMSE = format(round(rmse,4),nsmall=4))



#####################################################################################
#                                  Forward Selection                                #
#####################################################################################
library(leaps)

mlr.fwd=regsubsets(Life.expectancy~.,data=rtrain,method="forward",nvmax=24)
testASE<-c()
for (i in 1:24){
  predictions = predict.regsubsets(object=mlr.fwd,newdata=rtest,id=i) 
  testASE[i] = mean((rtest$Life.expectancy-predictions)^2)
}
par(mfrow=c(1,1))
plot(1:24,testASE,type="l",xlab="# of predictors",ylab="test vs train ASE", main='Forward Selection plot')
index<-which(testASE==min(testASE))
points(index,testASE[index],col="red",pch=10)
rss<-summary(mlr.fwd)$rss
lines(1:24,rss/dim(rtrain)[1],lty=3,col="blue")  
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-11.png)<!-- -->

``` r
mlr.fwd.final=regsubsets(Life.expectancy~.,data=LifeExp[,!colnames(train) %in% drop_for_reg],method="forward",nvmax=24)
coef(mlr.fwd.final,14)
```

    ##                     (Intercept)                StatusDeveloping 
    ##                        68.85409                        -0.72160 
    ##                 Adult.Mortality                   infant.deaths 
    ##                        -0.05489                         0.02599 
    ##                         Alcohol               under.five.deaths 
    ##                         0.05509                        -0.02092 
    ##                           Polio                      Diphtheria 
    ##                         0.01125                         0.01423 
    ## Income.composition.of.resources                       Schooling 
    ##                         2.41027                         0.41296 
    ##               ContinentAmericas                   ContinentAsia 
    ##                         2.83007                         1.02407 
    ##                 ContinentEurope                ContinentOceania 
    ##                         2.20075                         1.23535 
    ##                         log.GDP 
    ##                         0.21963

``` r
summary(mlr.fwd.final)
```

    ## Subset selection object
    ## Call: regsubsets.formula(Life.expectancy ~ ., data = LifeExp[, !colnames(train) %in% 
    ##     drop_for_reg], method = "forward", nvmax = 24)
    ## 24 Variables  (and intercept)
    ##                                 Forced in Forced out
    ## Year                                FALSE      FALSE
    ## StatusDeveloping                    FALSE      FALSE
    ## Adult.Mortality                     FALSE      FALSE
    ## infant.deaths                       FALSE      FALSE
    ## Alcohol                             FALSE      FALSE
    ## Hepatitis.B                         FALSE      FALSE
    ## Measles                             FALSE      FALSE
    ## BMI                                 FALSE      FALSE
    ## under.five.deaths                   FALSE      FALSE
    ## Polio                               FALSE      FALSE
    ## Total.expenditure                   FALSE      FALSE
    ## Diphtheria                          FALSE      FALSE
    ## Population                          FALSE      FALSE
    ## thinness..1.19.years                FALSE      FALSE
    ## thinness.5.9.years                  FALSE      FALSE
    ## Income.composition.of.resources     FALSE      FALSE
    ## Schooling                           FALSE      FALSE
    ## ContinentAmericas                   FALSE      FALSE
    ## ContinentAsia                       FALSE      FALSE
    ## ContinentEurope                     FALSE      FALSE
    ## ContinentOceania                    FALSE      FALSE
    ## log.HIV.AIDS                        FALSE      FALSE
    ## log.GDP                             FALSE      FALSE
    ## log.percentage.expenditure          FALSE      FALSE
    ## 1 subsets of each size up to 24
    ## Selection Algorithm: forward
    ##           Year StatusDeveloping Adult.Mortality infant.deaths Alcohol
    ## 1  ( 1 )  " "  " "              "*"             " "           " "    
    ## 2  ( 1 )  " "  " "              "*"             " "           " "    
    ## 3  ( 1 )  " "  " "              "*"             " "           "*"    
    ## 4  ( 1 )  " "  " "              "*"             " "           "*"    
    ## 5  ( 1 )  " "  " "              "*"             " "           "*"    
    ## 6  ( 1 )  " "  " "              "*"             " "           "*"    
    ## 7  ( 1 )  " "  " "              "*"             " "           "*"    
    ## 8  ( 1 )  " "  " "              "*"             " "           "*"    
    ## 9  ( 1 )  " "  " "              "*"             " "           "*"    
    ## 10  ( 1 ) " "  " "              "*"             "*"           "*"    
    ## 11  ( 1 ) " "  " "              "*"             "*"           "*"    
    ## 12  ( 1 ) " "  " "              "*"             "*"           "*"    
    ## 13  ( 1 ) " "  " "              "*"             "*"           "*"    
    ## 14  ( 1 ) " "  "*"              "*"             "*"           "*"    
    ## 15  ( 1 ) " "  "*"              "*"             "*"           "*"    
    ## 16  ( 1 ) " "  "*"              "*"             "*"           "*"    
    ## 17  ( 1 ) " "  "*"              "*"             "*"           "*"    
    ## 18  ( 1 ) "*"  "*"              "*"             "*"           "*"    
    ## 19  ( 1 ) "*"  "*"              "*"             "*"           "*"    
    ## 20  ( 1 ) "*"  "*"              "*"             "*"           "*"    
    ## 21  ( 1 ) "*"  "*"              "*"             "*"           "*"    
    ## 22  ( 1 ) "*"  "*"              "*"             "*"           "*"    
    ## 23  ( 1 ) "*"  "*"              "*"             "*"           "*"    
    ## 24  ( 1 ) "*"  "*"              "*"             "*"           "*"    
    ##           Hepatitis.B Measles BMI under.five.deaths Polio Total.expenditure
    ## 1  ( 1 )  " "         " "     " " " "               " "   " "              
    ## 2  ( 1 )  " "         " "     " " " "               " "   " "              
    ## 3  ( 1 )  " "         " "     " " " "               " "   " "              
    ## 4  ( 1 )  " "         " "     " " " "               " "   " "              
    ## 5  ( 1 )  " "         " "     " " " "               " "   " "              
    ## 6  ( 1 )  " "         " "     " " " "               " "   " "              
    ## 7  ( 1 )  " "         " "     " " " "               " "   " "              
    ## 8  ( 1 )  " "         " "     " " " "               " "   " "              
    ## 9  ( 1 )  " "         " "     " " "*"               " "   " "              
    ## 10  ( 1 ) " "         " "     " " "*"               " "   " "              
    ## 11  ( 1 ) " "         " "     " " "*"               " "   " "              
    ## 12  ( 1 ) " "         " "     " " "*"               " "   " "              
    ## 13  ( 1 ) " "         " "     " " "*"               "*"   " "              
    ## 14  ( 1 ) " "         " "     " " "*"               "*"   " "              
    ## 15  ( 1 ) " "         " "     "*" "*"               "*"   " "              
    ## 16  ( 1 ) " "         " "     "*" "*"               "*"   " "              
    ## 17  ( 1 ) "*"         " "     "*" "*"               "*"   " "              
    ## 18  ( 1 ) "*"         " "     "*" "*"               "*"   " "              
    ## 19  ( 1 ) "*"         "*"     "*" "*"               "*"   " "              
    ## 20  ( 1 ) "*"         "*"     "*" "*"               "*"   " "              
    ## 21  ( 1 ) "*"         "*"     "*" "*"               "*"   " "              
    ## 22  ( 1 ) "*"         "*"     "*" "*"               "*"   " "              
    ## 23  ( 1 ) "*"         "*"     "*" "*"               "*"   " "              
    ## 24  ( 1 ) "*"         "*"     "*" "*"               "*"   "*"              
    ##           Diphtheria Population thinness..1.19.years thinness.5.9.years
    ## 1  ( 1 )  " "        " "        " "                  " "               
    ## 2  ( 1 )  " "        " "        " "                  " "               
    ## 3  ( 1 )  " "        " "        " "                  " "               
    ## 4  ( 1 )  "*"        " "        " "                  " "               
    ## 5  ( 1 )  "*"        " "        " "                  " "               
    ## 6  ( 1 )  "*"        " "        " "                  " "               
    ## 7  ( 1 )  "*"        " "        " "                  " "               
    ## 8  ( 1 )  "*"        " "        " "                  " "               
    ## 9  ( 1 )  "*"        " "        " "                  " "               
    ## 10  ( 1 ) "*"        " "        " "                  " "               
    ## 11  ( 1 ) "*"        " "        " "                  " "               
    ## 12  ( 1 ) "*"        " "        " "                  " "               
    ## 13  ( 1 ) "*"        " "        " "                  " "               
    ## 14  ( 1 ) "*"        " "        " "                  " "               
    ## 15  ( 1 ) "*"        " "        " "                  " "               
    ## 16  ( 1 ) "*"        " "        " "                  " "               
    ## 17  ( 1 ) "*"        " "        " "                  " "               
    ## 18  ( 1 ) "*"        " "        " "                  " "               
    ## 19  ( 1 ) "*"        " "        " "                  " "               
    ## 20  ( 1 ) "*"        "*"        " "                  " "               
    ## 21  ( 1 ) "*"        "*"        "*"                  " "               
    ## 22  ( 1 ) "*"        "*"        "*"                  " "               
    ## 23  ( 1 ) "*"        "*"        "*"                  "*"               
    ## 24  ( 1 ) "*"        "*"        "*"                  "*"               
    ##           Income.composition.of.resources Schooling ContinentAmericas
    ## 1  ( 1 )  " "                             " "       " "              
    ## 2  ( 1 )  " "                             "*"       " "              
    ## 3  ( 1 )  " "                             "*"       " "              
    ## 4  ( 1 )  " "                             "*"       " "              
    ## 5  ( 1 )  " "                             "*"       "*"              
    ## 6  ( 1 )  " "                             "*"       "*"              
    ## 7  ( 1 )  " "                             "*"       "*"              
    ## 8  ( 1 )  "*"                             "*"       "*"              
    ## 9  ( 1 )  "*"                             "*"       "*"              
    ## 10  ( 1 ) "*"                             "*"       "*"              
    ## 11  ( 1 ) "*"                             "*"       "*"              
    ## 12  ( 1 ) "*"                             "*"       "*"              
    ## 13  ( 1 ) "*"                             "*"       "*"              
    ## 14  ( 1 ) "*"                             "*"       "*"              
    ## 15  ( 1 ) "*"                             "*"       "*"              
    ## 16  ( 1 ) "*"                             "*"       "*"              
    ## 17  ( 1 ) "*"                             "*"       "*"              
    ## 18  ( 1 ) "*"                             "*"       "*"              
    ## 19  ( 1 ) "*"                             "*"       "*"              
    ## 20  ( 1 ) "*"                             "*"       "*"              
    ## 21  ( 1 ) "*"                             "*"       "*"              
    ## 22  ( 1 ) "*"                             "*"       "*"              
    ## 23  ( 1 ) "*"                             "*"       "*"              
    ## 24  ( 1 ) "*"                             "*"       "*"              
    ##           ContinentAsia ContinentEurope ContinentOceania log.HIV.AIDS log.GDP
    ## 1  ( 1 )  " "           " "             " "              " "          " "    
    ## 2  ( 1 )  " "           " "             " "              " "          " "    
    ## 3  ( 1 )  " "           " "             " "              " "          " "    
    ## 4  ( 1 )  " "           " "             " "              " "          " "    
    ## 5  ( 1 )  " "           " "             " "              " "          " "    
    ## 6  ( 1 )  " "           "*"             " "              " "          " "    
    ## 7  ( 1 )  " "           "*"             " "              " "          "*"    
    ## 8  ( 1 )  " "           "*"             " "              " "          "*"    
    ## 9  ( 1 )  " "           "*"             " "              " "          "*"    
    ## 10  ( 1 ) " "           "*"             " "              " "          "*"    
    ## 11  ( 1 ) "*"           "*"             " "              " "          "*"    
    ## 12  ( 1 ) "*"           "*"             "*"              " "          "*"    
    ## 13  ( 1 ) "*"           "*"             "*"              " "          "*"    
    ## 14  ( 1 ) "*"           "*"             "*"              " "          "*"    
    ## 15  ( 1 ) "*"           "*"             "*"              " "          "*"    
    ## 16  ( 1 ) "*"           "*"             "*"              " "          "*"    
    ## 17  ( 1 ) "*"           "*"             "*"              " "          "*"    
    ## 18  ( 1 ) "*"           "*"             "*"              " "          "*"    
    ## 19  ( 1 ) "*"           "*"             "*"              " "          "*"    
    ## 20  ( 1 ) "*"           "*"             "*"              " "          "*"    
    ## 21  ( 1 ) "*"           "*"             "*"              " "          "*"    
    ## 22  ( 1 ) "*"           "*"             "*"              "*"          "*"    
    ## 23  ( 1 ) "*"           "*"             "*"              "*"          "*"    
    ## 24  ( 1 ) "*"           "*"             "*"              "*"          "*"    
    ##           log.percentage.expenditure
    ## 1  ( 1 )  " "                       
    ## 2  ( 1 )  " "                       
    ## 3  ( 1 )  " "                       
    ## 4  ( 1 )  " "                       
    ## 5  ( 1 )  " "                       
    ## 6  ( 1 )  " "                       
    ## 7  ( 1 )  " "                       
    ## 8  ( 1 )  " "                       
    ## 9  ( 1 )  " "                       
    ## 10  ( 1 ) " "                       
    ## 11  ( 1 ) " "                       
    ## 12  ( 1 ) " "                       
    ## 13  ( 1 ) " "                       
    ## 14  ( 1 ) " "                       
    ## 15  ( 1 ) " "                       
    ## 16  ( 1 ) "*"                       
    ## 17  ( 1 ) "*"                       
    ## 18  ( 1 ) "*"                       
    ## 19  ( 1 ) "*"                       
    ## 20  ( 1 ) "*"                       
    ## 21  ( 1 ) "*"                       
    ## 22  ( 1 ) "*"                       
    ## 23  ( 1 ) "*"                       
    ## 24  ( 1 ) "*"

``` r
# Metrics RMSE; R-squared; MAE
postResample(pred = predictions, obs = ytest)
```

    ##     RMSE Rsquared      MAE 
    ##    2.476    0.925    1.755

``` r
##### Fit Linear Model based on Forward Selection without factors to measure VIF####
fit.fwd.lm = lm(Life.expectancy ~ Adult.Mortality+infant.deaths+Alcohol+under.five.deaths+Polio+
                                  Diphtheria + Income.composition.of.resources +
                                  Schooling + log.GDP, data = rtrain)
summary(fit.fwd.lm)
```

    ## 
    ## Call:
    ## lm(formula = Life.expectancy ~ Adult.Mortality + infant.deaths + 
    ##     Alcohol + under.five.deaths + Polio + Diphtheria + Income.composition.of.resources + 
    ##     Schooling + log.GDP, data = rtrain)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -13.763  -1.562  -0.302   1.250  10.321 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value
    ## (Intercept)                     69.544649   0.458119  151.80
    ## Adult.Mortality                 -0.059220   0.000642  -92.30
    ## infant.deaths                    0.035306   0.005666    6.23
    ## Alcohol                          0.203073   0.016361   12.41
    ## under.five.deaths               -0.028038   0.004177   -6.71
    ## Polio                            0.010223   0.003091    3.31
    ## Diphtheria                       0.013694   0.003126    4.38
    ## Income.composition.of.resources  2.497496   0.447785    5.58
    ## Schooling                        0.434116   0.030573   14.20
    ## log.GDP                          0.224843   0.038249    5.88
    ##                                             Pr(>|t|)    
    ## (Intercept)                     < 0.0000000000000002 ***
    ## Adult.Mortality                 < 0.0000000000000002 ***
    ## infant.deaths                         0.000000000541 ***
    ## Alcohol                         < 0.0000000000000002 ***
    ## under.five.deaths                     0.000000000024 ***
    ## Polio                                        0.00096 ***
    ## Diphtheria                            0.000012300054 ***
    ## Income.composition.of.resources       0.000000027054 ***
    ## Schooling                       < 0.0000000000000002 ***
    ## log.GDP                               0.000000004701 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.6 on 2479 degrees of freedom
    ## Multiple R-squared:  0.927,  Adjusted R-squared:  0.927 
    ## F-statistic: 3.5e+03 on 9 and 2479 DF,  p-value: <0.0000000000000002

``` r
### Visualize VIF
fit.fwd.lm_VIF = vif(fit.fwd.lm)
barplot(fit.fwd.lm_VIF, main = 'VIF Values (FWD selection)', horiz = TRUE, col="blue", xlim = c(0,12))
abline(v=10, col="red")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-12.png)<!-- -->

``` r
# We can see that Forward Selection did not remove infant.deaths or under.five.deaths as those are perfectly correlated. Let's remove the one with the smallest coefficient (infant.deaths).

##### Fit Linear Model based on FWD selection without factors to measure VIF####
fit.fwd.lm2 = lm(Life.expectancy ~ Adult.Mortality+Alcohol+under.five.deaths+Polio+
                                  Diphtheria + Income.composition.of.resources +
                                  Schooling + log.GDP, data = rtrain)
summary(fit.fwd.lm2)
```

    ## 
    ## Call:
    ## lm(formula = Life.expectancy ~ Adult.Mortality + Alcohol + under.five.deaths + 
    ##     Polio + Diphtheria + Income.composition.of.resources + Schooling + 
    ##     log.GDP, data = rtrain)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -13.976  -1.582  -0.291   1.285  10.411 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value
    ## (Intercept)                     69.412920   0.461109  150.53
    ## Adult.Mortality                 -0.059888   0.000637  -93.95
    ## Alcohol                          0.186374   0.016263   11.46
    ## under.five.deaths               -0.002090   0.000327   -6.39
    ## Polio                            0.011049   0.003112    3.55
    ## Diphtheria                       0.015588   0.003135    4.97
    ## Income.composition.of.resources  2.663278   0.450391    5.91
    ## Schooling                        0.437343   0.030801   14.20
    ## log.GDP                          0.218220   0.038525    5.66
    ##                                             Pr(>|t|)    
    ## (Intercept)                     < 0.0000000000000002 ***
    ## Adult.Mortality                 < 0.0000000000000002 ***
    ## Alcohol                         < 0.0000000000000002 ***
    ## under.five.deaths                       0.0000000002 ***
    ## Polio                                        0.00039 ***
    ## Diphtheria                              0.0000007056 ***
    ## Income.composition.of.resources         0.0000000038 ***
    ## Schooling                       < 0.0000000000000002 ***
    ## log.GDP                                 0.0000000165 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.62 on 2480 degrees of freedom
    ## Multiple R-squared:  0.926,  Adjusted R-squared:  0.926 
    ## F-statistic: 3.88e+03 on 8 and 2480 DF,  p-value: <0.0000000000000002

``` r
### re-run Visualize VIF
fit.fwd.lm2_VIF = vif(fit.fwd.lm2)
barplot(fit.fwd.lm2_VIF, main = 'Re-test of VIF Values (FWD selection)', horiz = TRUE, col="blue", xlim = c(0,12))
abline(v=10, col="red")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-13.png)<!-- -->

``` r
##### Fit Linear Model based on Forward Selection regularization and removed multicollinearity and categorical variables####
fit.fwd.lm3 = lm(Life.expectancy ~ Status+Continent+Adult.Mortality+Alcohol+under.five.deaths+Polio+
                                  Diphtheria + Income.composition.of.resources +
                                  Schooling + log.GDP, data = rtrain)

#### Hypothesis testing ####
summary(fit.fwd.lm3)
```

    ## 
    ## Call:
    ## lm(formula = Life.expectancy ~ Status + Continent + Adult.Mortality + 
    ##     Alcohol + under.five.deaths + Polio + Diphtheria + Income.composition.of.resources + 
    ##     Schooling + log.GDP, data = rtrain)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -12.532  -1.497  -0.249   1.110  10.073 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value
    ## (Intercept)                     68.714463   0.513675  133.77
    ## StatusDeveloping                -0.659861   0.206801   -3.19
    ## ContinentAmericas                2.889912   0.198793   14.54
    ## ContinentAsia                    1.110255   0.172118    6.45
    ## ContinentEurope                  2.248488   0.242507    9.27
    ## ContinentOceania                 1.289408   0.260456    4.95
    ## Adult.Mortality                 -0.055438   0.000724  -76.58
    ## Alcohol                          0.057913   0.020809    2.78
    ## under.five.deaths               -0.001786   0.000319   -5.59
    ## Polio                            0.012628   0.002993    4.22
    ## Diphtheria                       0.014414   0.003029    4.76
    ## Income.composition.of.resources  2.658460   0.436928    6.08
    ## Schooling                        0.407231   0.030031   13.56
    ## log.GDP                          0.207836   0.037172    5.59
    ##                                             Pr(>|t|)    
    ## (Intercept)                     < 0.0000000000000002 ***
    ## StatusDeveloping                              0.0014 ** 
    ## ContinentAmericas               < 0.0000000000000002 ***
    ## ContinentAsia                          0.00000000013 ***
    ## ContinentEurope                 < 0.0000000000000002 ***
    ## ContinentOceania                       0.00000078972 ***
    ## Adult.Mortality                 < 0.0000000000000002 ***
    ## Alcohol                                       0.0054 ** 
    ## under.five.deaths                      0.00000002498 ***
    ## Polio                                  0.00002544692 ***
    ## Diphtheria                             0.00000205863 ***
    ## Income.composition.of.resources        0.00000000135 ***
    ## Schooling                       < 0.0000000000000002 ***
    ## log.GDP                                0.00000002502 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.51 on 2475 degrees of freedom
    ## Multiple R-squared:  0.932,  Adjusted R-squared:  0.932 
    ## F-statistic: 2.61e+03 on 13 and 2475 DF,  p-value: <0.0000000000000002

``` r
# At alpha = 0.05 the following variables are not significant therefore don't contribute to the model performance:
# thinness.5.9.years, Measles, BMI, Total.expenditure.

# Predicting
train_pred = predict(fit.fwd.lm3, rtrain)
test_pred = predict(fit.fwd.lm3, rtest)

# Scoring the final model on Training and Test set
residuals = resid(fit.fwd.lm3)
train_score = postResample(pred = train_pred, obs = rtrain$Life.expectancy)
test_score = postResample(pred = test_pred, obs = rtest$Life.expectancy)
sm = summary(fit.fwd.lm3)
mse = mean(sm$residuals^2)

### Checking Multiple Liner Regression model assumptions
confint(fit.fwd.lm3)
```

    ##                                     2.5 %    97.5 %
    ## (Intercept)                     67.707187 69.721740
    ## StatusDeveloping                -1.065383 -0.254339
    ## ContinentAmericas                2.500095  3.279730
    ## ContinentAsia                    0.772745  1.447766
    ## ContinentEurope                  1.772950  2.724025
    ## ContinentOceania                 0.778674  1.800141
    ## Adult.Mortality                 -0.056858 -0.054019
    ## Alcohol                          0.017109  0.098717
    ## under.five.deaths               -0.002412 -0.001159
    ## Polio                            0.006759  0.018498
    ## Diphtheria                       0.008475  0.020353
    ## Income.composition.of.resources  1.801679  3.515242
    ## Schooling                        0.348342  0.466119
    ## log.GDP                          0.134945  0.280727

``` r
hist(residuals, main = "Histogram of Residuals")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-14.png)<!-- -->

``` r
plot(residuals, main = "Residuals plot") 
abline(h=0, col="blue")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-15.png)<!-- -->

``` r
plot(fit.fwd.lm3, which = 2)
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-16.png)<!-- -->

``` r
plot(fit.fwd.lm3, which = 4)
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-17.png)<!-- -->

``` r
##### Visualize prediction vs actual
x_fwd = 1:dim(xtest)[1]
plot(x_fwd, ytest, col = "red", type = "l", lwd=2,
     main = "Life Expectancy prediction (FWD selection)", ylab="Life expectancy")
lines(x_fwd, test_pred, col = "blue", lwd=2)
legend("topright",  legend = c("original observation", "predicted life expectancy"), 
       fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
grid()
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-18.png)<!-- -->

``` r
#### Scatter plot
plot(test_pred ~ ytest, main = "Original vs Predicted scatter plot (FWD Selection fit)", xlab = 'Original observations', ylab='Predicted values')
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-19.png)<!-- -->

``` r
#### Model test scores
rmse = test_score[1]
rsqd = test_score[2]
mse = rmse^2
n=dim(xtest)[1]
p = length(fit.lasso.lm3$coefficients)-1
adjrsqd = 1 - (1 - rsqd) * ((n - 1)/(n-p-1))
eval_df = rbind(eval_df, c('FWD Selection', format(round(mse,4),nsmall=4), format(round(rsqd,4),nsmall=4), format(round(adjrsqd,4),nsmall=4), format(round(rmse,4),nsmall=4)))
#####################################################################################
#                               Backward Elimination                                #
#####################################################################################

mlr.bck=regsubsets(Life.expectancy~.,data=rtrain,method="backward",nvmax=24)
testASE<-c()
for (i in 1:24){

  predictions = predict.regsubsets(object=mlr.bck,newdata=test,id=i) 
  testASE[i] = mean((test$Life.expectancy-predictions)^2)
}

par(mfrow=c(1,1))
plot(1:24,testASE,type="l",xlab="# of predictors",ylab="test vs train ASE")
index<-which(testASE==min(testASE))
points(index,testASE[index],col="red",pch=10)
rss<-summary(mlr.bck)$rss
lines(1:24,rss/dim(rtrain)[1],lty=3,col="blue")  
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-20.png)<!-- -->

``` r
mlr.bck.final=regsubsets(Life.expectancy~.,data=LifeExp[,!colnames(train) %in% drop_for_reg],method="backward",nvmax=24)
coef(mlr.bck.final,14)
```

    ##                     (Intercept)                StatusDeveloping 
    ##                        68.85409                        -0.72160 
    ##                 Adult.Mortality                   infant.deaths 
    ##                        -0.05489                         0.02599 
    ##                         Alcohol               under.five.deaths 
    ##                         0.05509                        -0.02092 
    ##                           Polio                      Diphtheria 
    ##                         0.01125                         0.01423 
    ## Income.composition.of.resources                       Schooling 
    ##                         2.41027                         0.41296 
    ##               ContinentAmericas                   ContinentAsia 
    ##                         2.83007                         1.02407 
    ##                 ContinentEurope                ContinentOceania 
    ##                         2.20075                         1.23535 
    ##                         log.GDP 
    ##                         0.21963

``` r
summary(mlr.bck.final)
```

    ## Subset selection object
    ## Call: regsubsets.formula(Life.expectancy ~ ., data = LifeExp[, !colnames(train) %in% 
    ##     drop_for_reg], method = "backward", nvmax = 24)
    ## 24 Variables  (and intercept)
    ##                                 Forced in Forced out
    ## Year                                FALSE      FALSE
    ## StatusDeveloping                    FALSE      FALSE
    ## Adult.Mortality                     FALSE      FALSE
    ## infant.deaths                       FALSE      FALSE
    ## Alcohol                             FALSE      FALSE
    ## Hepatitis.B                         FALSE      FALSE
    ## Measles                             FALSE      FALSE
    ## BMI                                 FALSE      FALSE
    ## under.five.deaths                   FALSE      FALSE
    ## Polio                               FALSE      FALSE
    ## Total.expenditure                   FALSE      FALSE
    ## Diphtheria                          FALSE      FALSE
    ## Population                          FALSE      FALSE
    ## thinness..1.19.years                FALSE      FALSE
    ## thinness.5.9.years                  FALSE      FALSE
    ## Income.composition.of.resources     FALSE      FALSE
    ## Schooling                           FALSE      FALSE
    ## ContinentAmericas                   FALSE      FALSE
    ## ContinentAsia                       FALSE      FALSE
    ## ContinentEurope                     FALSE      FALSE
    ## ContinentOceania                    FALSE      FALSE
    ## log.HIV.AIDS                        FALSE      FALSE
    ## log.GDP                             FALSE      FALSE
    ## log.percentage.expenditure          FALSE      FALSE
    ## 1 subsets of each size up to 24
    ## Selection Algorithm: backward
    ##           Year StatusDeveloping Adult.Mortality infant.deaths Alcohol
    ## 1  ( 1 )  " "  " "              "*"             " "           " "    
    ## 2  ( 1 )  " "  " "              "*"             " "           " "    
    ## 3  ( 1 )  " "  " "              "*"             " "           " "    
    ## 4  ( 1 )  " "  " "              "*"             " "           " "    
    ## 5  ( 1 )  " "  " "              "*"             " "           " "    
    ## 6  ( 1 )  " "  " "              "*"             " "           " "    
    ## 7  ( 1 )  " "  " "              "*"             " "           " "    
    ## 8  ( 1 )  " "  " "              "*"             " "           " "    
    ## 9  ( 1 )  " "  " "              "*"             " "           " "    
    ## 10  ( 1 ) " "  " "              "*"             " "           " "    
    ## 11  ( 1 ) " "  " "              "*"             "*"           " "    
    ## 12  ( 1 ) " "  "*"              "*"             "*"           " "    
    ## 13  ( 1 ) " "  "*"              "*"             "*"           " "    
    ## 14  ( 1 ) " "  "*"              "*"             "*"           "*"    
    ## 15  ( 1 ) " "  "*"              "*"             "*"           "*"    
    ## 16  ( 1 ) " "  "*"              "*"             "*"           "*"    
    ## 17  ( 1 ) " "  "*"              "*"             "*"           "*"    
    ## 18  ( 1 ) " "  "*"              "*"             "*"           "*"    
    ## 19  ( 1 ) " "  "*"              "*"             "*"           "*"    
    ## 20  ( 1 ) "*"  "*"              "*"             "*"           "*"    
    ## 21  ( 1 ) "*"  "*"              "*"             "*"           "*"    
    ## 22  ( 1 ) "*"  "*"              "*"             "*"           "*"    
    ## 23  ( 1 ) "*"  "*"              "*"             "*"           "*"    
    ## 24  ( 1 ) "*"  "*"              "*"             "*"           "*"    
    ##           Hepatitis.B Measles BMI under.five.deaths Polio Total.expenditure
    ## 1  ( 1 )  " "         " "     " " " "               " "   " "              
    ## 2  ( 1 )  " "         " "     " " " "               " "   " "              
    ## 3  ( 1 )  " "         " "     " " " "               " "   " "              
    ## 4  ( 1 )  " "         " "     " " " "               " "   " "              
    ## 5  ( 1 )  " "         " "     " " " "               " "   " "              
    ## 6  ( 1 )  " "         " "     " " " "               " "   " "              
    ## 7  ( 1 )  " "         " "     " " " "               " "   " "              
    ## 8  ( 1 )  " "         " "     " " "*"               " "   " "              
    ## 9  ( 1 )  " "         " "     " " "*"               " "   " "              
    ## 10  ( 1 ) " "         " "     " " "*"               " "   " "              
    ## 11  ( 1 ) " "         " "     " " "*"               " "   " "              
    ## 12  ( 1 ) " "         " "     " " "*"               " "   " "              
    ## 13  ( 1 ) " "         " "     " " "*"               "*"   " "              
    ## 14  ( 1 ) " "         " "     " " "*"               "*"   " "              
    ## 15  ( 1 ) " "         " "     "*" "*"               "*"   " "              
    ## 16  ( 1 ) " "         " "     "*" "*"               "*"   " "              
    ## 17  ( 1 ) "*"         " "     "*" "*"               "*"   " "              
    ## 18  ( 1 ) "*"         "*"     "*" "*"               "*"   " "              
    ## 19  ( 1 ) "*"         "*"     "*" "*"               "*"   " "              
    ## 20  ( 1 ) "*"         "*"     "*" "*"               "*"   " "              
    ## 21  ( 1 ) "*"         "*"     "*" "*"               "*"   " "              
    ## 22  ( 1 ) "*"         "*"     "*" "*"               "*"   " "              
    ## 23  ( 1 ) "*"         "*"     "*" "*"               "*"   " "              
    ## 24  ( 1 ) "*"         "*"     "*" "*"               "*"   "*"              
    ##           Diphtheria Population thinness..1.19.years thinness.5.9.years
    ## 1  ( 1 )  " "        " "        " "                  " "               
    ## 2  ( 1 )  " "        " "        " "                  " "               
    ## 3  ( 1 )  " "        " "        " "                  " "               
    ## 4  ( 1 )  " "        " "        " "                  " "               
    ## 5  ( 1 )  "*"        " "        " "                  " "               
    ## 6  ( 1 )  "*"        " "        " "                  " "               
    ## 7  ( 1 )  "*"        " "        " "                  " "               
    ## 8  ( 1 )  "*"        " "        " "                  " "               
    ## 9  ( 1 )  "*"        " "        " "                  " "               
    ## 10  ( 1 ) "*"        " "        " "                  " "               
    ## 11  ( 1 ) "*"        " "        " "                  " "               
    ## 12  ( 1 ) "*"        " "        " "                  " "               
    ## 13  ( 1 ) "*"        " "        " "                  " "               
    ## 14  ( 1 ) "*"        " "        " "                  " "               
    ## 15  ( 1 ) "*"        " "        " "                  " "               
    ## 16  ( 1 ) "*"        " "        " "                  " "               
    ## 17  ( 1 ) "*"        " "        " "                  " "               
    ## 18  ( 1 ) "*"        " "        " "                  " "               
    ## 19  ( 1 ) "*"        "*"        " "                  " "               
    ## 20  ( 1 ) "*"        "*"        " "                  " "               
    ## 21  ( 1 ) "*"        "*"        "*"                  " "               
    ## 22  ( 1 ) "*"        "*"        "*"                  " "               
    ## 23  ( 1 ) "*"        "*"        "*"                  "*"               
    ## 24  ( 1 ) "*"        "*"        "*"                  "*"               
    ##           Income.composition.of.resources Schooling ContinentAmericas
    ## 1  ( 1 )  " "                             " "       " "              
    ## 2  ( 1 )  " "                             "*"       " "              
    ## 3  ( 1 )  " "                             "*"       "*"              
    ## 4  ( 1 )  " "                             "*"       "*"              
    ## 5  ( 1 )  " "                             "*"       "*"              
    ## 6  ( 1 )  " "                             "*"       "*"              
    ## 7  ( 1 )  "*"                             "*"       "*"              
    ## 8  ( 1 )  "*"                             "*"       "*"              
    ## 9  ( 1 )  "*"                             "*"       "*"              
    ## 10  ( 1 ) "*"                             "*"       "*"              
    ## 11  ( 1 ) "*"                             "*"       "*"              
    ## 12  ( 1 ) "*"                             "*"       "*"              
    ## 13  ( 1 ) "*"                             "*"       "*"              
    ## 14  ( 1 ) "*"                             "*"       "*"              
    ## 15  ( 1 ) "*"                             "*"       "*"              
    ## 16  ( 1 ) "*"                             "*"       "*"              
    ## 17  ( 1 ) "*"                             "*"       "*"              
    ## 18  ( 1 ) "*"                             "*"       "*"              
    ## 19  ( 1 ) "*"                             "*"       "*"              
    ## 20  ( 1 ) "*"                             "*"       "*"              
    ## 21  ( 1 ) "*"                             "*"       "*"              
    ## 22  ( 1 ) "*"                             "*"       "*"              
    ## 23  ( 1 ) "*"                             "*"       "*"              
    ## 24  ( 1 ) "*"                             "*"       "*"              
    ##           ContinentAsia ContinentEurope ContinentOceania log.HIV.AIDS log.GDP
    ## 1  ( 1 )  " "           " "             " "              " "          " "    
    ## 2  ( 1 )  " "           " "             " "              " "          " "    
    ## 3  ( 1 )  " "           " "             " "              " "          " "    
    ## 4  ( 1 )  " "           "*"             " "              " "          " "    
    ## 5  ( 1 )  " "           "*"             " "              " "          " "    
    ## 6  ( 1 )  " "           "*"             " "              " "          "*"    
    ## 7  ( 1 )  " "           "*"             " "              " "          "*"    
    ## 8  ( 1 )  " "           "*"             " "              " "          "*"    
    ## 9  ( 1 )  "*"           "*"             " "              " "          "*"    
    ## 10  ( 1 ) "*"           "*"             "*"              " "          "*"    
    ## 11  ( 1 ) "*"           "*"             "*"              " "          "*"    
    ## 12  ( 1 ) "*"           "*"             "*"              " "          "*"    
    ## 13  ( 1 ) "*"           "*"             "*"              " "          "*"    
    ## 14  ( 1 ) "*"           "*"             "*"              " "          "*"    
    ## 15  ( 1 ) "*"           "*"             "*"              " "          "*"    
    ## 16  ( 1 ) "*"           "*"             "*"              " "          "*"    
    ## 17  ( 1 ) "*"           "*"             "*"              " "          "*"    
    ## 18  ( 1 ) "*"           "*"             "*"              " "          "*"    
    ## 19  ( 1 ) "*"           "*"             "*"              " "          "*"    
    ## 20  ( 1 ) "*"           "*"             "*"              " "          "*"    
    ## 21  ( 1 ) "*"           "*"             "*"              " "          "*"    
    ## 22  ( 1 ) "*"           "*"             "*"              "*"          "*"    
    ## 23  ( 1 ) "*"           "*"             "*"              "*"          "*"    
    ## 24  ( 1 ) "*"           "*"             "*"              "*"          "*"    
    ##           log.percentage.expenditure
    ## 1  ( 1 )  " "                       
    ## 2  ( 1 )  " "                       
    ## 3  ( 1 )  " "                       
    ## 4  ( 1 )  " "                       
    ## 5  ( 1 )  " "                       
    ## 6  ( 1 )  " "                       
    ## 7  ( 1 )  " "                       
    ## 8  ( 1 )  " "                       
    ## 9  ( 1 )  " "                       
    ## 10  ( 1 ) " "                       
    ## 11  ( 1 ) " "                       
    ## 12  ( 1 ) " "                       
    ## 13  ( 1 ) " "                       
    ## 14  ( 1 ) " "                       
    ## 15  ( 1 ) " "                       
    ## 16  ( 1 ) "*"                       
    ## 17  ( 1 ) "*"                       
    ## 18  ( 1 ) "*"                       
    ## 19  ( 1 ) "*"                       
    ## 20  ( 1 ) "*"                       
    ## 21  ( 1 ) "*"                       
    ## 22  ( 1 ) "*"                       
    ## 23  ( 1 ) "*"                       
    ## 24  ( 1 ) "*"

``` r
# Metrics RMSE; R-squared; MAE
postResample(pred = predictions, obs = ytest)
```

    ##     RMSE Rsquared      MAE 
    ##    2.476    0.925    1.755

``` r
##different result now, will repeat steps from earlier model
### Continent
fit.bck.lm <- lm(Life.expectancy ~ Adult.Mortality+infant.deaths+Alcohol+under.five.deaths+Polio+
                                   Diphtheria+Income.composition.of.resources + Schooling + log.GDP
                 ,data = rtrain)

summary(fit.bck.lm)
```

    ## 
    ## Call:
    ## lm(formula = Life.expectancy ~ Adult.Mortality + infant.deaths + 
    ##     Alcohol + under.five.deaths + Polio + Diphtheria + Income.composition.of.resources + 
    ##     Schooling + log.GDP, data = rtrain)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -13.763  -1.562  -0.302   1.250  10.321 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value
    ## (Intercept)                     69.544649   0.458119  151.80
    ## Adult.Mortality                 -0.059220   0.000642  -92.30
    ## infant.deaths                    0.035306   0.005666    6.23
    ## Alcohol                          0.203073   0.016361   12.41
    ## under.five.deaths               -0.028038   0.004177   -6.71
    ## Polio                            0.010223   0.003091    3.31
    ## Diphtheria                       0.013694   0.003126    4.38
    ## Income.composition.of.resources  2.497496   0.447785    5.58
    ## Schooling                        0.434116   0.030573   14.20
    ## log.GDP                          0.224843   0.038249    5.88
    ##                                             Pr(>|t|)    
    ## (Intercept)                     < 0.0000000000000002 ***
    ## Adult.Mortality                 < 0.0000000000000002 ***
    ## infant.deaths                         0.000000000541 ***
    ## Alcohol                         < 0.0000000000000002 ***
    ## under.five.deaths                     0.000000000024 ***
    ## Polio                                        0.00096 ***
    ## Diphtheria                            0.000012300054 ***
    ## Income.composition.of.resources       0.000000027054 ***
    ## Schooling                       < 0.0000000000000002 ***
    ## log.GDP                               0.000000004701 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.6 on 2479 degrees of freedom
    ## Multiple R-squared:  0.927,  Adjusted R-squared:  0.927 
    ## F-statistic: 3.5e+03 on 9 and 2479 DF,  p-value: <0.0000000000000002

``` r
### Visualize VIF
fit.bck.lm_VIF = vif(fit.bck.lm)
barplot(fit.bck.lm_VIF, main = 'VIF Values (Backward elimination)', horiz = TRUE, col="blue", xlim = c(0,12))
abline(v=10, col="red")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-21.png)<!-- -->

``` r
#two are correlated and giving a high VIF it's under.five and infant.deaths
fit.bck.lm2 <- lm(Life.expectancy ~ Adult.Mortality+Alcohol+under.five.deaths+Polio+
                                   Diphtheria+Income.composition.of.resources + Schooling + log.GDP,data = rtrain)


fit.bck.lm2_VIF = vif(fit.bck.lm2)
barplot(fit.bck.lm2_VIF, main = 'VIF Values (Backward elimination)', horiz = TRUE, col="blue", xlim = c(0,12))
abline(v=10, col="red")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-22.png)<!-- -->

``` r
##fit model with categorical variables and removed colinearity
# Continent
fit.bck.lm3 = lm(Life.expectancy ~ Status + Continent + Adult.Mortality+Alcohol+under.five.deaths+Polio+
                                   Diphtheria+Income.composition.of.resources + Schooling + log.GDP,data = rtrain)


#### Hypothesis testing ####
summary(fit.bck.lm3)
```

    ## 
    ## Call:
    ## lm(formula = Life.expectancy ~ Status + Continent + Adult.Mortality + 
    ##     Alcohol + under.five.deaths + Polio + Diphtheria + Income.composition.of.resources + 
    ##     Schooling + log.GDP, data = rtrain)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -12.532  -1.497  -0.249   1.110  10.073 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value
    ## (Intercept)                     68.714463   0.513675  133.77
    ## StatusDeveloping                -0.659861   0.206801   -3.19
    ## ContinentAmericas                2.889912   0.198793   14.54
    ## ContinentAsia                    1.110255   0.172118    6.45
    ## ContinentEurope                  2.248488   0.242507    9.27
    ## ContinentOceania                 1.289408   0.260456    4.95
    ## Adult.Mortality                 -0.055438   0.000724  -76.58
    ## Alcohol                          0.057913   0.020809    2.78
    ## under.five.deaths               -0.001786   0.000319   -5.59
    ## Polio                            0.012628   0.002993    4.22
    ## Diphtheria                       0.014414   0.003029    4.76
    ## Income.composition.of.resources  2.658460   0.436928    6.08
    ## Schooling                        0.407231   0.030031   13.56
    ## log.GDP                          0.207836   0.037172    5.59
    ##                                             Pr(>|t|)    
    ## (Intercept)                     < 0.0000000000000002 ***
    ## StatusDeveloping                              0.0014 ** 
    ## ContinentAmericas               < 0.0000000000000002 ***
    ## ContinentAsia                          0.00000000013 ***
    ## ContinentEurope                 < 0.0000000000000002 ***
    ## ContinentOceania                       0.00000078972 ***
    ## Adult.Mortality                 < 0.0000000000000002 ***
    ## Alcohol                                       0.0054 ** 
    ## under.five.deaths                      0.00000002498 ***
    ## Polio                                  0.00002544692 ***
    ## Diphtheria                             0.00000205863 ***
    ## Income.composition.of.resources        0.00000000135 ***
    ## Schooling                       < 0.0000000000000002 ***
    ## log.GDP                                0.00000002502 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.51 on 2475 degrees of freedom
    ## Multiple R-squared:  0.932,  Adjusted R-squared:  0.932 
    ## F-statistic: 2.61e+03 on 13 and 2475 DF,  p-value: <0.0000000000000002

``` r
# Predicting
train_pred = predict(fit.bck.lm3, rtrain)
test_pred = predict(fit.bck.lm3, rtest)

# Scoring the final model on Training and Test set
residuals = resid(fit.bck.lm3)
train_score = postResample(pred = train_pred, obs = train$Life.expectancy)
test_score = postResample(pred = test_pred, obs = test$Life.expectancy)
sm = summary(fit.bck.lm3)
mse = mean(sm$residuals^2)

### Checking Multiple Liner Regression model assumptions
confint(fit.bck.lm3)
```

    ##                                     2.5 %    97.5 %
    ## (Intercept)                     67.707187 69.721740
    ## StatusDeveloping                -1.065383 -0.254339
    ## ContinentAmericas                2.500095  3.279730
    ## ContinentAsia                    0.772745  1.447766
    ## ContinentEurope                  1.772950  2.724025
    ## ContinentOceania                 0.778674  1.800141
    ## Adult.Mortality                 -0.056858 -0.054019
    ## Alcohol                          0.017109  0.098717
    ## under.five.deaths               -0.002412 -0.001159
    ## Polio                            0.006759  0.018498
    ## Diphtheria                       0.008475  0.020353
    ## Income.composition.of.resources  1.801679  3.515242
    ## Schooling                        0.348342  0.466119
    ## log.GDP                          0.134945  0.280727

``` r
hist(residuals, main = "Histogram of Residuals (Backward elimination)")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-23.png)<!-- -->

``` r
plot(residuals, main = "Residuals plot (Backward elimination)") 
abline(h=0, col="blue")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-24.png)<!-- -->

``` r
plot(fit.fwd.lm3, which = 2)
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-25.png)<!-- -->

``` r
plot(fit.fwd.lm3, which = 4)
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-26.png)<!-- -->

``` r
##### Visualize prediction vs actual
x_fwd = 1:dim(xtest)[1]
plot(x_fwd, ytest, col = "red", type = "l", lwd=2,
     main = "Life Expectancy prediction (Backward elimination)", ylab="Life expectancy")
lines(x_fwd, test_pred, col = "blue", lwd=2)
legend("topright",  legend = c("original observation", "predicted life expectancy"), 
       fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
grid()
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-27.png)<!-- -->

``` r
#### Scatter plot
plot(test_pred ~ ytest, main = "Original vs Predicted scatter plot (Backward elimination)", xlab = 'Original observations', ylab='Predicted values')
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-28.png)<!-- -->

``` r
#### Model test scores
rmse = test_score[1]
rsqd = test_score[2]
mse = rmse^2
n=dim(xtest)[1]
p = length(fit.bck.lm3$coefficients)-1
adjrsqd = 1 - (1 - rsqd) * ((n - 1)/(n-p-1))
eval_df = rbind(eval_df, c('Backward Elim.', format(round(mse,4),nsmall=4), format(round(rsqd,4),nsmall=4), format(round(adjrsqd,4),nsmall=4), format(round(rmse,4),nsmall=4)))

#####################################################################################
#                                  Ridge regression                                 #
#####################################################################################

grid=10^seq(10,-2, length =100)
ridge.mod=glmnet(x,y,alpha=0, lambda =grid, family = 'gaussian') # alpha is 0 for Ridge
cv.out=cv.glmnet(x,y,alpha=0)
plot(cv.out)
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-29.png)<!-- -->

``` r
bestlambda = cv.out$lambda.min  #Optimal penalty parameter.  You can make this call visually.
ridge.pred=predict(ridge.mod ,s=bestlambda ,newx=xtest)

testMSE_RIDGE<-mean((ytest-ridge.pred)^2)
testMSE_RIDGE
```

    ## [1] 6.308

``` r
coef(ridge.mod,s=bestlambda)
```

    ## 25 x 1 sparse Matrix of class "dgCMatrix"
    ##                                              s1
    ## (Intercept)                     -6.953245823684
    ## Year                             0.035785530923
    ## StatusDeveloping                -1.194643319721
    ## Adult.Mortality                 -0.040190310300
    ## infant.deaths                   -0.000797400323
    ## Alcohol                          0.050596274481
    ## Hepatitis.B                     -0.003481492104
    ## Measles                         -0.000014728467
    ## BMI                              0.012643399859
    ## under.five.deaths               -0.001562761653
    ## Polio                            0.015097198409
    ## Total.expenditure                0.001651533509
    ## Diphtheria                       0.016998064261
    ## Population                       0.000000002233
    ## thinness..1.19.years            -0.017902225075
    ## thinness.5.9.years              -0.029773042974
    ## Income.composition.of.resources  3.598357691516
    ## Schooling                        0.365014675731
    ## ContinentAmericas                2.494335192816
    ## ContinentAsia                    0.901707468522
    ## ContinentEurope                  1.542321625092
    ## ContinentOceania                 0.431555807767
    ## log.HIV.AIDS                    -0.719934927419
    ## log.GDP                          0.182096178355
    ## log.percentage.expenditure       0.112513092145

``` r
# Metrics RMSE; R-squared; MAE
test_score = postResample(pred = ridge.pred, obs = ytest)

##### Visualize prediction vs actual
x_ridge = 1:dim(xtest)[1]
plot(x_fwd, ytest, col = "red", type = "l", lwd=2,
     main = "Life Expectancy prediction (Ridge Regression)", ylab="Life expectancy")
lines(x_fwd, ridge.pred, col = "blue", lwd=2)
legend("topright",  legend = c("original observation", "predicted life expectancy"), 
       fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
grid()
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-30.png)<!-- -->

``` r
#### Scatter plot
plot(ridge.pred ~ ytest, main = "Original vs Predicted scatter plot (Ridge regression)", xlab = 'Original observations', ylab='Predicted values')
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-31.png)<!-- -->

``` r
#### Model test scores
rmse = test_score[1]
rsqd = test_score[2]
mse = rmse^2
n=dim(xtest)[1]
p = length(coef(ridge.mod,s=bestlambda))-1
adjrsqd = 1 - (1 - rsqd) * ((n - 1)/(n-p-1))
eval_df = rbind(eval_df, c('Ridge', format(round(mse,4),nsmall=4), format(round(rsqd,4),nsmall=4), format(round(adjrsqd,4),nsmall=4), format(round(rmse,4),nsmall=4)))

#####################################################################################
#                            Elastic Net Regression                                 #
#####################################################################################

library(glmnetUtils)
```

    ## 
    ## Attaching package: 'glmnetUtils'

    ## The following objects are masked from 'package:glmnet':
    ## 
    ##     cv.glmnet, glmnet

``` r
cva.out = cva.glmnet(x,y)
plot(cva.out)
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-32.png)<!-- -->

``` r
alpha = cva.out$alpha
mse = sapply(cva.out$modlist, function(mod) {min(mod$cvm)})
lambdaMin <- sapply(cva.out$modlist, `[[`, "lambda.min")
min_mse <- which.min(mse)
cva.min = data.frame(alpha = alpha[min_mse], lambdaMin = lambdaMin[min_mse], mse = mse[min_mse])
cva.min
```

    ##   alpha lambdaMin   mse
    ## 1     1   0.01455 6.316

``` r
elastic.mod = glmnet(x,y, alpha = cva.min$alpha, lambda = cva.min$lambdaMin)
elastic.pred=predict(elastic.mod ,s=cva.min$lambdaMin ,newx=xtest)
elastic.pred_coef=predict(elastic.mod ,s=cva.min$lambdaMin ,newx=xtest, type = "coef")

testMSE_ELASTIC<-mean((ytest-elastic.pred)^2)
testMSE_ELASTIC
```

    ## [1] 6.109

``` r
# Metrics RMSE; R-squared; MAE
test_score = postResample(pred = elastic.pred, obs = ytest)

##### Visualize prediction vs actual
x_eNET = 1:dim(xtest)[1]
plot(x_eNET, ytest, col = "red", type = "l", lwd=2,
     main = "Life Expectancy prediction (ElasticNet Regression)", ylab="Life expectancy")
lines(x_fwd, elastic.pred, col = "blue", lwd=2)
legend("topright",  legend = c("original observation", "predicted life expectancy"), 
       fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
grid()
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-33.png)<!-- -->

``` r
#### Scatter plot
plot(elastic.pred ~ ytest, main = "Original vs Predicted scatter plot (ElasticNet regression)", xlab = 'Original observations', ylab='Predicted values')
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-34.png)<!-- -->

``` r
#### Model test scores
rmse = test_score[1]
rsqd = test_score[2]
mse = rmse^2
n=dim(xtest)[1]
p = length(coef(elastic.mod))-1
adjrsqd = 1 - (1 - rsqd) * ((n - 1)/(n-p-1))
eval_df = rbind(eval_df, c('ElasticNet', format(round(mse,4),nsmall=4), format(round(rsqd,4),nsmall=4), format(round(adjrsqd,4),nsmall=4), format(round(rmse,4),nsmall=4)))

#####################################################################################
#                                      Manual MLR - Tamas                           #
#####################################################################################
# 5-fold cross validation
cv <- trainControl(
  method = "cv", 
  number = 5,
  savePredictions = TRUE
)

MLRT = train(
  Life.expectancy ~ Status + Continent + Income.composition.of.resources + Schooling + log.percentage.expenditure + Year + Adult.Mortality + infant.deaths,
  data = rtrain,
  method = "lm",
   preProcess = c("center", "scale"),
  trControl = cv)

### Visualize VIF
MLR_VIF = vif(MLRT$finalModel)
barplot(MLR_VIF, main = 'VIF Values (Custom MLR - Tamas)', horiz = TRUE, col="blue", xlim = c(0,12))
abline(v=10, col="red")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-35.png)<!-- -->

``` r
### Hypothesis testing
summary(MLRT$finalModel)
```

    ## 
    ## Call:
    ## lm(formula = .outcome ~ ., data = dat)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -11.955  -1.475  -0.267   1.135  10.815 
    ## 
    ## Coefficients:
    ##                                 Estimate Std. Error t value
    ## (Intercept)                      69.1443     0.0515 1342.23
    ## StatusDeveloping                 -0.3360     0.0765   -4.39
    ## ContinentAmericas                 1.2624     0.0745   16.95
    ## ContinentAsia                     0.5754     0.0782    7.36
    ## ContinentEurope                   1.0985     0.0947   11.60
    ## ContinentOceania                  0.2431     0.0597    4.07
    ## Income.composition.of.resources   0.6185     0.0939    6.59
    ## Schooling                         1.6051     0.0987   16.26
    ## log.percentage.expenditure        0.2799     0.0574    4.87
    ## Year                              0.1342     0.0558    2.41
    ## Adult.Mortality                  -6.5059     0.0818  -79.54
    ## infant.deaths                    -0.3459     0.0534   -6.48
    ##                                             Pr(>|t|)    
    ## (Intercept)                     < 0.0000000000000002 ***
    ## StatusDeveloping                    0.00001167597011 ***
    ## ContinentAmericas               < 0.0000000000000002 ***
    ## ContinentAsia                       0.00000000000026 ***
    ## ContinentEurope                 < 0.0000000000000002 ***
    ## ContinentOceania                    0.00004774710542 ***
    ## Income.composition.of.resources     0.00000000005453 ***
    ## Schooling                       < 0.0000000000000002 ***
    ## log.percentage.expenditure          0.00000116051310 ***
    ## Year                                           0.016 *  
    ## Adult.Mortality                 < 0.0000000000000002 ***
    ## infant.deaths                       0.00000000011261 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.57 on 2477 degrees of freedom
    ## Multiple R-squared:  0.929,  Adjusted R-squared:  0.928 
    ## F-statistic: 2.94e+03 on 11 and 2477 DF,  p-value: <0.0000000000000002

``` r
# Predicting
train_pred = predict(MLRT, rtrain)
test_pred = predict(MLRT, rtest)

# Scoring the final model on Training and Test set
summary(MLRT$finalModel)
```

    ## 
    ## Call:
    ## lm(formula = .outcome ~ ., data = dat)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -11.955  -1.475  -0.267   1.135  10.815 
    ## 
    ## Coefficients:
    ##                                 Estimate Std. Error t value
    ## (Intercept)                      69.1443     0.0515 1342.23
    ## StatusDeveloping                 -0.3360     0.0765   -4.39
    ## ContinentAmericas                 1.2624     0.0745   16.95
    ## ContinentAsia                     0.5754     0.0782    7.36
    ## ContinentEurope                   1.0985     0.0947   11.60
    ## ContinentOceania                  0.2431     0.0597    4.07
    ## Income.composition.of.resources   0.6185     0.0939    6.59
    ## Schooling                         1.6051     0.0987   16.26
    ## log.percentage.expenditure        0.2799     0.0574    4.87
    ## Year                              0.1342     0.0558    2.41
    ## Adult.Mortality                  -6.5059     0.0818  -79.54
    ## infant.deaths                    -0.3459     0.0534   -6.48
    ##                                             Pr(>|t|)    
    ## (Intercept)                     < 0.0000000000000002 ***
    ## StatusDeveloping                    0.00001167597011 ***
    ## ContinentAmericas               < 0.0000000000000002 ***
    ## ContinentAsia                       0.00000000000026 ***
    ## ContinentEurope                 < 0.0000000000000002 ***
    ## ContinentOceania                    0.00004774710542 ***
    ## Income.composition.of.resources     0.00000000005453 ***
    ## Schooling                       < 0.0000000000000002 ***
    ## log.percentage.expenditure          0.00000116051310 ***
    ## Year                                           0.016 *  
    ## Adult.Mortality                 < 0.0000000000000002 ***
    ## infant.deaths                       0.00000000011261 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.57 on 2477 degrees of freedom
    ## Multiple R-squared:  0.929,  Adjusted R-squared:  0.928 
    ## F-statistic: 2.94e+03 on 11 and 2477 DF,  p-value: <0.0000000000000002

``` r
residuals = resid(MLRT$finalModel)
train_score = postResample(pred = train_pred, obs = rtrain$Life.expectancy)
test_score = postResample(pred = test_pred, obs = rtest$Life.expectancy)

### Checking Multiple Liner Regression model assumptions
fit = lm(Life.expectancy ~ Status + Continent + Income.composition.of.resources + Schooling + log.percentage.expenditure + Year + Adult.Mortality + infant.deaths, data = rtrain)
confint(fit)
```

    ##                                      2.5 %   97.5 %
    ## (Intercept)                     -34.972628 60.48978
    ## StatusDeveloping                 -1.284245 -0.49151
    ## ContinentAmericas                 2.900276  3.65908
    ## ContinentAsia                     0.967813  1.67143
    ## ContinentEurope                   2.233943  3.14275
    ## ContinentOceania                  0.562456  1.60657
    ## Income.composition.of.resources   2.076537  3.83686
    ## Schooling                         0.421909  0.53760
    ## log.percentage.expenditure        0.060767  0.14257
    ## Year                              0.005408  0.05304
    ## Adult.Mortality                  -0.057466 -0.05470
    ## infant.deaths                    -0.003699 -0.00198

``` r
hist(residuals, main = "Histogram of Residuals")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-36.png)<!-- -->

``` r
plot(residuals, main = "Residuals plot") 
abline(h=0, col="blue")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-37.png)<!-- -->

``` r
plot(fit, which = 2)
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-38.png)<!-- -->

``` r
plot(fit, which = 4)
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-39.png)<!-- -->

``` r
##### Visualize prediction vs actual
x_TMLR = 1:dim(xtest)[1]
plot(x_TMLR, ytest, col = "red", type = "l", lwd=2,
     main = "Life Expectancy prediction (Manual MLR)", ylab="Life expectancy")
lines(x_fwd, test_pred, col = "blue", lwd=2)
legend("topright",  legend = c("original observation", "predicted life expectancy"), 
       fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
grid()
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-40.png)<!-- -->

``` r
#### Scatter plot
plot(test_pred ~ ytest, main = "Original vs Predicted scatter plot (Custom MLR Tamas)", xlab = 'Original observations', ylab='Predicted values')
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-41.png)<!-- -->

``` r
#### Model test scores
rmse = test_score[1]
rsqd = test_score[2]
mse = rmse^2
n=dim(xtest)[1]
p = length(fit$coefficients)-1
adjrsqd = 1 - (1 - rsqd) * ((n - 1)/(n-p-1))
eval_df = rbind(eval_df, c('MLR - Tamas', format(round(mse,4),nsmall=4), format(round(rsqd,4),nsmall=4), format(round(adjrsqd,4),nsmall=4), format(round(rmse,4),nsmall=4)))
```

``` r
#####################################################################################
#                                      Objective 2                                  #
#                   Model with complexity (adding interaction terms)                #
#####################################################################################
fit_interaction = lm(Life.expectancy ~ Continent + Schooling + log.percentage.expenditure + Year + Adult.Mortality + infant.deaths + Income.composition.of.resources:Status, train)

interact_pred_train = predict(fit_interaction, rtrain)
train_score = postResample(pred = interact_pred_train, obs = rtrain$Life.expectancy)

interact_pred = predict(fit_interaction, rtest)
test_score = postResample(pred = interact_pred, obs = rtest$Life.expectancy)

summary(fit_interaction)
```

    ## 
    ## Call:
    ## lm(formula = Life.expectancy ~ Continent + Schooling + log.percentage.expenditure + 
    ##     Year + Adult.Mortality + infant.deaths + Income.composition.of.resources:Status, 
    ##     data = train)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -11.950  -1.479  -0.271   1.140  10.824 
    ## 
    ## Coefficients:
    ##                                                   Estimate Std. Error t value
    ## (Intercept)                                      11.828876  24.329370    0.49
    ## ContinentAmericas                                 3.294628   0.193541   17.02
    ## ContinentAsia                                     1.333031   0.179469    7.43
    ## ContinentEurope                                   2.685283   0.229854   11.68
    ## ContinentOceania                                  1.083966   0.266069    4.07
    ## Schooling                                         0.477428   0.029510   16.18
    ## log.percentage.expenditure                        0.100902   0.020850    4.84
    ## Year                                              0.029253   0.012128    2.41
    ## Adult.Mortality                                  -0.056042   0.000705  -79.48
    ## infant.deaths                                    -0.002849   0.000438   -6.50
    ## Income.composition.of.resources:StatusDeveloped   4.043146   0.475581    8.50
    ## Income.composition.of.resources:StatusDeveloping  2.937192   0.448537    6.55
    ##                                                              Pr(>|t|)    
    ## (Intercept)                                                     0.627    
    ## ContinentAmericas                                < 0.0000000000000002 ***
    ## ContinentAsia                                        0.00000000000015 ***
    ## ContinentEurope                                  < 0.0000000000000002 ***
    ## ContinentOceania                                     0.00004766289016 ***
    ## Schooling                                        < 0.0000000000000002 ***
    ## log.percentage.expenditure                           0.00000138162929 ***
    ## Year                                                            0.016 *  
    ## Adult.Mortality                                  < 0.0000000000000002 ***
    ## infant.deaths                                        0.00000000009481 ***
    ## Income.composition.of.resources:StatusDeveloped  < 0.0000000000000002 ***
    ## Income.composition.of.resources:StatusDeveloping     0.00000000007048 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.57 on 2477 degrees of freedom
    ## Multiple R-squared:  0.929,  Adjusted R-squared:  0.929 
    ## F-statistic: 2.94e+03 on 11 and 2477 DF,  p-value: <0.0000000000000002

``` r
confint(fit_interaction)
```

    ##                                                       2.5 %   97.5 %
    ## (Intercept)                                      -35.879125 59.53688
    ## ContinentAmericas                                  2.915110  3.67415
    ## ContinentAsia                                      0.981106  1.68496
    ## ContinentEurope                                    2.234558  3.13601
    ## ContinentOceania                                   0.562227  1.60571
    ## Schooling                                          0.419561  0.53529
    ## log.percentage.expenditure                         0.060017  0.14179
    ## Year                                               0.005471  0.05303
    ## Adult.Mortality                                   -0.057425 -0.05466
    ## infant.deaths                                     -0.003708 -0.00199
    ## Income.composition.of.resources:StatusDeveloped    3.110568  4.97572
    ## Income.composition.of.resources:StatusDeveloping   2.057645  3.81674

``` r
hist(residuals, main = "Histogram of Residuals (Interaction)")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Objective%202%20interaction-1.png)<!-- -->

``` r
plot(residuals, main = "Residuals plot (Interaction)") 
abline(h=0, col="blue")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Objective%202%20interaction-2.png)<!-- -->

``` r
plot(fit_interaction, which = 2)
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Objective%202%20interaction-3.png)<!-- -->

``` r
plot(fit_interaction, which = 4)
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Objective%202%20interaction-4.png)<!-- -->

``` r
##### Visualize prediction vs actual
x_TMLR = 1:dim(xtest)[1]
plot(x_TMLR, ytest, col = "red", type = "l", lwd=2,
     main = "Life Expectancy prediction (Interaction)", ylab="Life expectancy")
lines(x_fwd, test_pred, col = "blue", lwd=2)
legend("topright",  legend = c("original observation", "predicted life expectancy"), 
       fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
grid()
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Objective%202%20interaction-5.png)<!-- -->

``` r
#### Scatter plot
plot(test_pred ~ ytest, main = "Original vs Predicted scatter plot (Interaction)", xlab = 'Original observations', ylab='Predicted values')
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Objective%202%20interaction-6.png)<!-- -->

``` r
#### Model test scores
rmse = test_score[1]
rsqd = test_score[2]
mse = rmse^2
n=dim(xtest)[1]
p = length(fit_interaction$coefficients)-1
adjrsqd = 1 - (1 - rsqd) * ((n - 1)/(n-p-1))
eval_df = rbind(eval_df, c('MLR Interact - Tamas', format(round(mse,4),nsmall=4), format(round(rsqd,4),nsmall=4), format(round(adjrsqd,4),nsmall=4), format(round(rmse,4),nsmall=4)))
```

``` r
#####################################################################################
#                                    Reuven's  Manual MLR                           #
#####################################################################################

# 5-fold cross validation
cv <- trainControl(
  method = "cv", 
  number = 5,
  savePredictions = TRUE
)
MLRT = train(
  Life.expectancy ~ Income.composition.of.resources + Schooling*log.percentage.expenditure  +  log.HIV.AIDS + log.GDP + BMI + Year + Adult.Mortality,
  data = rtrain,
  method = "lm",
  trControl = cv)

### Visualize VIF
MLR_VIF = vif(MLRT$finalModel)
barplot(MLR_VIF, main = 'VIF Values', horiz = TRUE, col="blue", xlim = c(0,12))
abline(v=10, col="red")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
### Hypothesis testing
summary(MLRT$finalModel)
```

    ## 
    ## Call:
    ## lm(formula = .outcome ~ ., data = dat)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -12.932  -1.648  -0.253   1.290  12.389 
    ## 
    ## Coefficients:
    ##                                         Estimate Std. Error t value
    ## (Intercept)                            112.71600   25.31631    4.45
    ## Income.composition.of.resources          3.00378    0.47323    6.35
    ## Schooling                                0.63430    0.03923   16.17
    ## log.percentage.expenditure               0.20986    0.08758    2.40
    ## log.HIV.AIDS                            -0.05812    0.07013   -0.83
    ## log.GDP                                  0.23494    0.04415    5.32
    ## BMI                                      0.02178    0.00335    6.51
    ## Year                                    -0.02215    0.01263   -1.75
    ## Adult.Mortality                         -0.05829    0.00107  -54.72
    ## `Schooling:log.percentage.expenditure`  -0.01233    0.00639   -1.93
    ##                                                    Pr(>|t|)    
    ## (Intercept)                                  0.000008871388 ***
    ## Income.composition.of.resources              0.000000000260 ***
    ## Schooling                              < 0.0000000000000002 ***
    ## log.percentage.expenditure                            0.017 *  
    ## log.HIV.AIDS                                          0.407    
    ## log.GDP                                      0.000000112469 ***
    ## BMI                                          0.000000000092 ***
    ## Year                                                  0.080 .  
    ## Adult.Mortality                        < 0.0000000000000002 ***
    ## `Schooling:log.percentage.expenditure`                0.054 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.73 on 2479 degrees of freedom
    ## Multiple R-squared:  0.92,   Adjusted R-squared:  0.919 
    ## F-statistic: 3.15e+03 on 9 and 2479 DF,  p-value: <0.0000000000000002

``` r
# Predicting
train_pred = predict(MLRT, rtrain)
test_pred = predict(MLRT, rtest)

# Scoring the final model on Training and Validation set
summary(MLRT$finalModel)
```

    ## 
    ## Call:
    ## lm(formula = .outcome ~ ., data = dat)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -12.932  -1.648  -0.253   1.290  12.389 
    ## 
    ## Coefficients:
    ##                                         Estimate Std. Error t value
    ## (Intercept)                            112.71600   25.31631    4.45
    ## Income.composition.of.resources          3.00378    0.47323    6.35
    ## Schooling                                0.63430    0.03923   16.17
    ## log.percentage.expenditure               0.20986    0.08758    2.40
    ## log.HIV.AIDS                            -0.05812    0.07013   -0.83
    ## log.GDP                                  0.23494    0.04415    5.32
    ## BMI                                      0.02178    0.00335    6.51
    ## Year                                    -0.02215    0.01263   -1.75
    ## Adult.Mortality                         -0.05829    0.00107  -54.72
    ## `Schooling:log.percentage.expenditure`  -0.01233    0.00639   -1.93
    ##                                                    Pr(>|t|)    
    ## (Intercept)                                  0.000008871388 ***
    ## Income.composition.of.resources              0.000000000260 ***
    ## Schooling                              < 0.0000000000000002 ***
    ## log.percentage.expenditure                            0.017 *  
    ## log.HIV.AIDS                                          0.407    
    ## log.GDP                                      0.000000112469 ***
    ## BMI                                          0.000000000092 ***
    ## Year                                                  0.080 .  
    ## Adult.Mortality                        < 0.0000000000000002 ***
    ## `Schooling:log.percentage.expenditure`                0.054 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.73 on 2479 degrees of freedom
    ## Multiple R-squared:  0.92,   Adjusted R-squared:  0.919 
    ## F-statistic: 3.15e+03 on 9 and 2479 DF,  p-value: <0.0000000000000002

``` r
residuals = resid(MLRT$finalModel)
train_score = postResample(pred = train_pred, obs = rtrain$Life.expectancy)
test_score = postResample(pred = test_pred, obs = rtest$Life.expectancy)

### Checking Multiple Liner Regression model assumptions
fit = lm(Life.expectancy ~ Income.composition.of.resources + Schooling*log.percentage.expenditure  +  log.HIV.AIDS + log.GDP + BMI + Year + Adult.Mortality, rtrain)
confint(fit)
```

    ##                                         2.5 %      97.5 %
    ## (Intercept)                          63.07271 162.3592821
    ## Income.composition.of.resources       2.07581   3.9317423
    ## Schooling                             0.55737   0.7112318
    ## log.percentage.expenditure            0.03813   0.3815934
    ## log.HIV.AIDS                         -0.19564   0.0793929
    ## log.GDP                               0.14836   0.3215254
    ## BMI                                   0.01522   0.0283421
    ## Year                                 -0.04691   0.0026159
    ## Adult.Mortality                      -0.06038  -0.0562056
    ## Schooling:log.percentage.expenditure -0.02486   0.0001915

``` r
hist(residuals, main = "Histogram of Residuals")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-15-2.png)<!-- -->

``` r
plot(residuals, main = "Residuals plot") 
abline(h=0, col="blue")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-15-3.png)<!-- -->

``` r
plot(fit, which = 2)
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-15-4.png)<!-- -->

``` r
plot(fit, which = 4)
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-15-5.png)<!-- -->

``` r
anova(fit)
```

    ## Analysis of Variance Table
    ## 
    ## Response: Life.expectancy
    ##                                        Df Sum Sq Mean Sq  F value
    ## Income.composition.of.resources         1 122886  122886 16500.47
    ## Schooling                               1  18332   18332  2461.47
    ## log.percentage.expenditure              1   1105    1105   148.39
    ## log.HIV.AIDS                            1  45526   45526  6112.92
    ## log.GDP                                 1    596     596    80.05
    ## BMI                                     1    361     361    48.48
    ## Year                                    1    106     106    14.27
    ## Adult.Mortality                         1  22284   22284  2992.18
    ## Schooling:log.percentage.expenditure    1     28      28     3.73
    ## Residuals                            2479  18462       7         
    ##                                                    Pr(>F)    
    ## Income.composition.of.resources      < 0.0000000000000002 ***
    ## Schooling                            < 0.0000000000000002 ***
    ## log.percentage.expenditure           < 0.0000000000000002 ***
    ## log.HIV.AIDS                         < 0.0000000000000002 ***
    ## log.GDP                              < 0.0000000000000002 ***
    ## BMI                                       0.0000000000043 ***
    ## Year                                              0.00016 ***
    ## Adult.Mortality                      < 0.0000000000000002 ***
    ## Schooling:log.percentage.expenditure              0.05360 .  
    ## Residuals                                                    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
x_TMLR = 1:dim(xtest)[1]
plot(x_TMLR, ytest, col = "red", type = "l", lwd=2,
     main = "Life Expectancy prediction (Manual MLR)", ylab="Life expectancy")
lines(x_fwd, test_pred, col = "blue", lwd=2)
legend("topright",  legend = c("original observation", "predicted life expectancy"), 
       fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
grid()
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-15-6.png)<!-- -->

``` r
#### Scatter plot
plot(test_pred ~ ytest, main = "Original vs Predicted scatter plot (Custom MLR Tamas)", xlab = 'Original observations', ylab='Predicted values')
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-15-7.png)<!-- -->

``` r
#### Model test scores
rmse = test_score[1]
rsqd = test_score[2]
mse = rmse^2
n=dim(xtest)[1]
p = length(fit$coefficients)-1
adjrsqd = 1 - (1 - rsqd) * ((n - 1)/(n-p-1))
eval_df = rbind(eval_df, c('MLR Interact - Reuven', format(round(mse,4),nsmall=4), format(round(rsqd,4),nsmall=4), format(round(adjrsqd,4),nsmall=4), format(round(rmse,4),nsmall=4)))
```

## Observations

-   The interaction of Schooling:log.percentage.expenditure is not
    statistically significant (p-value=0.48) at the .05 alpha level,
    this indicates that the effect of schooling does not depend on the
    linear log of percentage expenditure as it relates to Life
    expectancy holding all other variables constant

-   The interaction of Status:Continent is statistically significant
    (p-value \< 0.0001) at the .05 alpha level, this indicates that the
    effect of whether a continent is developed or not does directly
    correspond to a higher Life expectancy holding all other variables
    constant.

-   All other variables are statistically signficant for the model with
    an R-Squared on our prediction set of 0.8566.

``` r
#######################################################################################
#                               Manual MLR Miguel                                     #
#######################################################################################
#resplit here for now
index<-sample(1:dim(LifeExp)[1],round(dim(LifeExp)[1]*0.85),replace=F)
train = LifeExp[index,]
test = LifeExp[-index,]
#duplicate dataframe
LifeExp2 <- LifeExp
#create cat variables for HIV and Meas as binary No/Yes
LifeExp2 <- LifeExp2 %>% dplyr::mutate(HIV_cat = if_else(LifeExp2$log.HIV.AIDS>log(.1),1,0),
                                       Meas_cat= if_else(LifeExp2$Measles>0,1,0),
                                       log.Adult.Mortality = log(Adult.Mortality),
                                       log.infant.deaths = log(infant.deaths))
train_custom <- LifeExp2[index,]
test_custom <- LifeExp2[index,]
```

``` r
#retrain/fit/test/predict and measure accuracy
fit.custom <- lm(Life.expectancy ~log.Adult.Mortality + infant.deaths + log.GDP + Measles + HIV_cat + Country + Country:infant.deaths +Country:log.Adult.Mortality + log.Adult.Mortality:infant.deaths:Country,data = train_custom)
#model has interactions between HIV and Measles variables and their respective
#binary categories to have a sort of conditional
#interaction between Country:infant.deaths
#check model performance on the test set and test set prediction
test_custom_pred <- predict(fit.custom,test_custom) #rank deficient warning comes from the self-interactions (ex. HIV_cat:HIV)
```

    ## Warning in predict.lm(fit.custom, test_custom): prediction from a rank-deficient
    ## fit may be misleading

``` r
test_score = caret::postResample(pred = test_custom_pred,obs = test_custom$Life.expectancy)
forecast::accuracy(test_custom_pred,test_custom$Life.expectancy)
```

    ## Registered S3 method overwritten by 'quantmod':
    ##   method            from
    ##   as.zoo.data.frame zoo

    ##                         ME  RMSE    MAE      MPE   MAPE
    ## Test set 0.000000000001163 1.207 0.6611 -0.02953 0.9493

``` r
#### Scatter plot
plot(test_pred ~ ytest, main = "Original vs Predicted scatter plot (Custom MLR Tamas)", xlab = 'Original observations', ylab='Predicted values')
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/run%20custom%20model-1.png)<!-- -->

``` r
#### Model test scores
rmse = test_score[1]
rsqd = test_score[2]
mse = rmse^2
n=dim(xtest)[1]
p = length(fit.custom$coefficients)-1
adjrsqd = 1 - (1 - rsqd) * ((n - 1)/(n-p-1))
eval_df = rbind(eval_df, c('MLR Interact - Miguel', format(round(mse,4),nsmall=4), format(round(rsqd,4),nsmall=4), format(round(adjrsqd,4),nsmall=4), format(round(rmse,4),nsmall=4)))
```

``` r
#####################################################################################
#                                      Objective 2                                  #
#                                     KNN - regression                              #
#####################################################################################
# Check for zero variance
caret::nearZeroVar(LifeExp %>% dplyr::select(where(is.numeric)), saveMetrics = TRUE) %>% 
  tibble::rownames_to_column() %>% 
  filter(nzv)
```

    ## [1] rowname       freqRatio     percentUnique zeroVar       nzv          
    ## <0 rows> (or 0-length row.names)

``` r
cat("No variable with zero variance in the selected list of variables.") 
```

    ## No variable with zero variance in the selected list of variables.

``` r
# Create training and test set for KNN model


#incl_for_knn = c(3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23)
#ktrain = train[,incl_for_knn]
#ktest = test[,incl_for_knn]
#x=ktrain[,-4]
#y=ktrain$Life.expectancy
#xtest = ktest[,-4]
#ytest = ktest$Life.expectancy


# Search for optimal k
k_grid = expand.grid(k = seq(3, 25, by = 2))

# Model Training
KNNRegressor = train(
  Life.expectancy~.,
  data = ktrain,
  method = "knn",
  preProcess = c("center", "scale"), 
  tuneGrid = k_grid,
  metric     = "RMSE",
  trControl = cv
  )

prediction_test = predict(KNNRegressor, ktrain)
train_score = postResample(pred = prediction_test, obs = ktrain$Life.expectancy)

prediction_test = predict(KNNRegressor, ktest)
test_score = postResample(pred = prediction_test, obs = ktest$Life.expectancy)

plot(KNNRegressor)
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Objective%202%20KNN-1.png)<!-- -->

``` r
varImp(KNNRegressor)
```

    ## loess r-squared variable importance
    ## 
    ##                                  Overall
    ## Adult.Mortality                 100.0000
    ## Income.composition.of.resources  85.1323
    ## HIV.AIDS                         34.3617
    ## Status                           26.0152
    ## Alcohol                          20.9686
    ## Country                           0.0583
    ## Population                        0.0000

``` r
#KNNRegressor$results$Rsquared
#KNNRegressor.sorted <- KNNRegressor$results[order(KNNRegressor$results$Rsquared),]
#KNNRegressor.sorted[1,'Rsquared']

x = 1:length(ktest$Life.expectancy)
plot(x, ktest$Life.expectancy, col = "red", type = "l", lwd=2,
     main = "Life Expectancy prediction")
lines(x, prediction_test, col = "blue", lwd=2)
legend("topright",  legend = c("original observation", "predicted life expectancy"), 
       fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
grid()
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Objective%202%20KNN-2.png)<!-- -->

``` r
#### Model test scores
rmse = test_score[1]
rsqd = test_score[2]
mse = rmse^2
n=dim(xtest)[1]
p = length(KNNRegressor$coefnames)
adjrsqd = 1 - (1 - rsqd) * ((n - 1)/(n-p-1))
eval_df = rbind(eval_df, c('KNN', format(round(mse,4),nsmall=4), format(round(rsqd,4),nsmall=4), format(round(adjrsqd,4),nsmall=4), format(round(rmse,4),nsmall=4)))
```

### Visualize ‘k’ and the most important features

``` r
# Visualize 'k' and the most important features
ggplot(KNNRegressor) + ggtitle("Optimal k value for the highest accuracy") +
  theme(plot.title = element_text(hjust = 0.5))
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-16-1.png" angle=90 style="display: block; margin: auto;" />

``` r
KNNvarImp = varImp(KNNRegressor)
plot(KNNvarImp, top = 5, main='Top 5 Variable predicting life expectancy (KNN)')
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-16-2.png" angle=90 style="display: block; margin: auto;" />

``` r
knitr::kable(eval_df, "html")
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:left;">
model_name
</th>
<th style="text-align:left;">
MSE
</th>
<th style="text-align:left;">
R_Squared
</th>
<th style="text-align:left;">
AdjR_Squared
</th>
<th style="text-align:left;">
RMSE
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
RMSE
</td>
<td style="text-align:left;">
LASSO
</td>
<td style="text-align:left;">
6.1035
</td>
<td style="text-align:left;">
0.9253
</td>
<td style="text-align:left;">
0.9216
</td>
<td style="text-align:left;">
2.4705
</td>
</tr>
<tr>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
FWD Selection
</td>
<td style="text-align:left;">
6.0930
</td>
<td style="text-align:left;">
0.9255
</td>
<td style="text-align:left;">
0.9217
</td>
<td style="text-align:left;">
2.4684
</td>
</tr>
<tr>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
Backward Elim.
</td>
<td style="text-align:left;">
6.0930
</td>
<td style="text-align:left;">
0.9255
</td>
<td style="text-align:left;">
0.9232
</td>
<td style="text-align:left;">
2.4684
</td>
</tr>
<tr>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
Ridge
</td>
<td style="text-align:left;">
6.3077
</td>
<td style="text-align:left;">
0.9225
</td>
<td style="text-align:left;">
0.9180
</td>
<td style="text-align:left;">
2.5115
</td>
</tr>
<tr>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
ElasticNet
</td>
<td style="text-align:left;">
6.1092
</td>
<td style="text-align:left;">
0.9252
</td>
<td style="text-align:left;">
0.9209
</td>
<td style="text-align:left;">
2.4717
</td>
</tr>
<tr>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
MLR - Tamas
</td>
<td style="text-align:left;">
6.4192
</td>
<td style="text-align:left;">
0.9214
</td>
<td style="text-align:left;">
0.9193
</td>
<td style="text-align:left;">
2.5336
</td>
</tr>
<tr>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
MLR Interact - Tamas
</td>
<td style="text-align:left;">
6.4158
</td>
<td style="text-align:left;">
0.9214
</td>
<td style="text-align:left;">
0.9194
</td>
<td style="text-align:left;">
2.5329
</td>
</tr>
<tr>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
MLR Interact - Reuven
</td>
<td style="text-align:left;">
7.2968
</td>
<td style="text-align:left;">
0.9110
</td>
<td style="text-align:left;">
0.9092
</td>
<td style="text-align:left;">
2.7013
</td>
</tr>
<tr>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
MLR Interact - Miguel
</td>
<td style="text-align:left;">
1.4579
</td>
<td style="text-align:left;">
0.9838
</td>
<td style="text-align:left;">
1.0240
</td>
<td style="text-align:left;">
1.2074
</td>
</tr>
<tr>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
KNN
</td>
<td style="text-align:left;">
2.8531
</td>
<td style="text-align:left;">
0.9671
</td>
<td style="text-align:left;">
0.9424
</td>
<td style="text-align:left;">
1.6891
</td>
</tr>
</tbody>
</table>
