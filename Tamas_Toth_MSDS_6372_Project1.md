Tamas_Toth_MSDS_6372_Project1
================
Tamas Toth
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
```

``` r
# Turn off scientific notation
options(scipen = 100, digits = 4)
```

#### Read the data

``` r
#Read the data
setwd('/Users/ttoth76/Downloads/SMU/Semester_2/DS 6372 Applied Statistics_Inference & Modeling/FLS/Project1_Summer2022/GitContent/LifeExpectancy')
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
Iran (Islamic Republic of)
</td>
<td style="text-align:right;">
2014
</td>
<td style="text-align:left;">
Developing
</td>
<td style="text-align:right;">
75.4
</td>
<td style="text-align:right;">
83
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
99
</td>
<td style="text-align:right;">
99
</td>
<td style="text-align:right;">
58.5
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
99
</td>
<td style="text-align:right;">
6.89
</td>
<td style="text-align:right;">
99
</td>
<td style="text-align:right;">
0.1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
8.3
</td>
<td style="text-align:right;">
8.4
</td>
<td style="text-align:right;">
0.770
</td>
<td style="text-align:right;">
14.9
</td>
</tr>
<tr>
<td style="text-align:left;">
Venezuela (Bolivarian Republic of)
</td>
<td style="text-align:right;">
2009
</td>
<td style="text-align:left;">
Developing
</td>
<td style="text-align:right;">
73.6
</td>
<td style="text-align:right;">
166
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
7.59
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
84
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
58.8
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
75
</td>
<td style="text-align:right;">
5.81
</td>
<td style="text-align:right;">
84
</td>
<td style="text-align:right;">
0.1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
1.6
</td>
<td style="text-align:right;">
1.5
</td>
<td style="text-align:right;">
0.754
</td>
<td style="text-align:right;">
14.0
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:right;">
2003
</td>
<td style="text-align:left;">
Developed
</td>
<td style="text-align:right;">
82.0
</td>
<td style="text-align:right;">
69
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
6.90
</td>
<td style="text-align:right;">
5067.41
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
54.4
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
99
</td>
<td style="text-align:right;">
9.31
</td>
<td style="text-align:right;">
99
</td>
<td style="text-align:right;">
0.1
</td>
<td style="text-align:right;">
36961.4
</td>
<td style="text-align:right;">
8958229
</td>
<td style="text-align:right;">
1.3
</td>
<td style="text-align:right;">
1.3
</td>
<td style="text-align:right;">
0.882
</td>
<td style="text-align:right;">
15.9
</td>
</tr>
<tr>
<td style="text-align:left;">
Burkina Faso
</td>
<td style="text-align:right;">
2006
</td>
<td style="text-align:left;">
Developing
</td>
<td style="text-align:right;">
54.3
</td>
<td style="text-align:right;">
32
</td>
<td style="text-align:right;">
47
</td>
<td style="text-align:right;">
4.73
</td>
<td style="text-align:right;">
64.24
</td>
<td style="text-align:right;">
76
</td>
<td style="text-align:right;">
125
</td>
<td style="text-align:right;">
14.6
</td>
<td style="text-align:right;">
86
</td>
<td style="text-align:right;">
94
</td>
<td style="text-align:right;">
6.58
</td>
<td style="text-align:right;">
86
</td>
<td style="text-align:right;">
2.0
</td>
<td style="text-align:right;">
422.6
</td>
<td style="text-align:right;">
13829177
</td>
<td style="text-align:right;">
1.0
</td>
<td style="text-align:right;">
9.6
</td>
<td style="text-align:right;">
0.325
</td>
<td style="text-align:right;">
4.7
</td>
</tr>
<tr>
<td style="text-align:left;">
Angola
</td>
<td style="text-align:right;">
2011
</td>
<td style="text-align:left;">
Developing
</td>
<td style="text-align:right;">
51.0
</td>
<td style="text-align:right;">
361
</td>
<td style="text-align:right;">
75
</td>
<td style="text-align:right;">
8.06
</td>
<td style="text-align:right;">
239.89
</td>
<td style="text-align:right;">
72
</td>
<td style="text-align:right;">
1449
</td>
<td style="text-align:right;">
21.0
</td>
<td style="text-align:right;">
115
</td>
<td style="text-align:right;">
73
</td>
<td style="text-align:right;">
3.38
</td>
<td style="text-align:right;">
71
</td>
<td style="text-align:right;">
2.5
</td>
<td style="text-align:right;">
4299.1
</td>
<td style="text-align:right;">
24218565
</td>
<td style="text-align:right;">
8.9
</td>
<td style="text-align:right;">
8.8
</td>
<td style="text-align:right;">
0.495
</td>
<td style="text-align:right;">
9.4
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
pairs(Life.expectancy~log(HIV.AIDS)+log(GDP)+Population+thinness..1.19.years, data=LifeExp, col=ifelse(LifeExp$Status=="Developed", "green", "red"))
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Linear%20regression-4.png)<!-- -->

``` r
pairs(Life.expectancy~thinness.5.9.years+Income.composition.of.resources+Schooling, data=LifeExp, col=ifelse(LifeExp$Status=="Developed", "green", "red"))
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Linear%20regression-5.png)<!-- -->
\### Observations We can observe linear relationship between Life
Expectancy and the following variables

-   Income composition of resources
-   Schooling
-   Log(HIV.AIDS)
-   Log(GDP)
-   Log(percentage.expenditure)
-   BMI
-   Year
-   Adult.Mortality

## Fixing the missing values by replacing with median

``` r
# Drop missing values from the dependent variable
LifeExp = LifeExp[!(is.na(LifeExp$Life.expectancy)),]

na_list = colnames(LifeExp)[apply(LifeExp, 2, anyNA)]
# LifeExp_db = apply(LifeExp[,colnames(LifeExp) %in% na_list],2,median,na.rm =  TRUE)
LifeExp = LifeExp %>% group_by(Country) %>% mutate(Alcohol = replace(Alcohol,is.na(Alcohol), median(Alcohol, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Country) %>% mutate(Hepatitis.B = replace(Hepatitis.B,is.na(Hepatitis.B), median(Hepatitis.B, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Country) %>% mutate(BMI = replace(BMI,is.na(BMI), median(BMI, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Country) %>% mutate(Polio = replace(Polio,is.na(Polio), median(Polio, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Country) %>% mutate(Total.expenditure = replace(Total.expenditure,is.na(Total.expenditure), median(Total.expenditure, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Country) %>% mutate(Diphtheria = replace(Diphtheria,is.na(Diphtheria), median(Diphtheria, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Country) %>% mutate(GDP = replace(GDP,is.na(GDP), median(GDP, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Country) %>% mutate(Population = replace(Population,is.na(Population), median(Population, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Country) %>% mutate(thinness..1.19.years = replace(thinness..1.19.years,is.na(thinness..1.19.years), median(thinness..1.19.years, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Country) %>% mutate(thinness.5.9.years = replace(thinness.5.9.years,is.na(thinness.5.9.years), median(thinness.5.9.years, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Country) %>% mutate(Income.composition.of.resources = replace(Income.composition.of.resources,is.na(Income.composition.of.resources), median(Income.composition.of.resources, na.rm = TRUE)))
LifeExp = LifeExp %>% group_by(Country) %>% mutate(Schooling = replace(Schooling,is.na(Schooling), median(Schooling, na.rm = TRUE)))

# convert the tibble to data frame
LifeExp = as.data.frame(LifeExp)
LifeExpKNN = LifeExp
```

### Full Correlation Matrix for Linear Regression (Life.expectancy)

``` r
#####################################################################################
#      Full Correlation Matrix for Linear Regression (Life.expectancy)              #
#####################################################################################
# Filter for data to be included
num_cols = LifeExp %>% select(where(is.numeric)) %>% colnames()
LifeExpcorr = LifeExp[,num_cols]
corrplot(cor(LifeExpcorr), method = 'square', order = 'AOE', addCoef.col = 'black', 
         cl.pos = 'n', col = COL2('BrBG'))
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-9-1.png" angle=90 style="display: block; margin: auto;" />

### Observations

-   Under five death and infant death are perfectly correlated. They are
    describing the same thing. One of the variable is redundant.
-   GDP and percentage expenditure are perfectly correlated. They are
    describing the same thing. One of the variable is redundant.
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
num_cols = LifeExp %>% select(where(is.numeric)) %>% colnames()
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

``` r
#####################################################################################
#                                     Data Preparation                              #
#####################################################################################

### Transform variables
LifeExp = mutate(LifeExp, log.HIV.AIDS = ifelse(HIV.AIDS==0, log(HIV.AIDS+1), log(HIV.AIDS)))
LifeExp = mutate(LifeExp, log.GDP = ifelse(GDP==0, log(GDP+1), log(GDP)))
LifeExp = mutate(LifeExp, log.percentage.expenditure = ifelse(percentage.expenditure==0, log(percentage.expenditure+1), log(percentage.expenditure)))


### Transform countries to continents
library(countrycode)
LifeExp$Continent = countrycode(sourcevar = LifeExp[, "Country"], origin = "country.name", destination = "continent")
LifeExp$Continent = as.factor(LifeExp$Continent)
LifeExp$Status = as.factor(LifeExp$Status)

drop = c('HIV.AIDS', 'GDP', 'percentage.expenditure', 'Country')
LifeExp = LifeExp[, !(colnames(LifeExp) %in% drop)]
```

``` r
#####################################################################################
#                    Split the Data to Train and Test sets (85%-15%)               #
#####################################################################################
index<-sample(1:dim(LifeExp)[1],round(dim(LifeExp)[1]*0.85),replace=F)
train = LifeExp[index,]
test = LifeExp[-index,]
```

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

#####################################################################################
#                                       Lasso                                       #
#####################################################################################
library(glmnet)
```

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'Matrix'

    ## The following objects are masked from 'package:tidyr':
    ## 
    ##     expand, pack, unpack

    ## Loaded glmnet 4.1-4

``` r
x=model.matrix(Life.expectancy~.,train)[,-1]
y=train$Life.expectancy
xtest = model.matrix(Life.expectancy~.,test)[,-1]
ytest = test$Life.expectancy

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

    ## [1] 11.16

``` r
coef(lasso.mod,s=bestlambda)
```

    ## 25 x 1 sparse Matrix of class "dgCMatrix"
    ##                                                s1
    ## (Intercept)                     -133.371105433646
    ## Year                               0.094816156836
    ## StatusDeveloping                  -2.777282261515
    ## Adult.Mortality                   -0.014435238787
    ## infant.deaths                      0.002488421333
    ## Alcohol                           -0.020658404447
    ## Hepatitis.B                       -0.012142132181
    ## Measles                           -0.000008567015
    ## BMI                                0.009412450572
    ## under.five.deaths                 -0.003506847451
    ## Polio                              0.022563815520
    ## Total.expenditure                  0.001033676184
    ## Diphtheria                         0.027381017554
    ## Population                         0.000000001019
    ## thinness..1.19.years               .             
    ## thinness.5.9.years                -0.053236220465
    ## Income.composition.of.resources    4.646939738667
    ## Schooling                          0.367891010075
    ## log.HIV.AIDS                      -2.235540813368
    ## log.GDP                            0.194920436427
    ## log.percentage.expenditure         0.200606833857
    ## ContinentAmericas                  3.948899828122
    ## ContinentAsia                      1.448604068417
    ## ContinentEurope                    2.084751253082
    ## ContinentOceania                   0.398738233381

``` r
lasso_residuals = (ytest - lasso.pred)
hist(lasso_residuals, main = "Histogram of Residuals")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-2.png)<!-- -->

``` r
plot(lasso_residuals, main = "Residuals plot") 
abline(h=0, col="blue")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-3.png)<!-- -->

``` r
# Metrics RMSE; R-squared; MAE
postResample(pred = lasso.pred, obs = ytest)
```

    ##     RMSE Rsquared      MAE 
    ##   3.3399   0.8634   2.5363

``` r
##### Fit Linear Model based on LASSO regularization without factors to measure VIF####
fit.lasso.lm = lm(Life.expectancy ~ Year + Adult.Mortality + infant.deaths + Alcohol + Hepatitis.B + Measles + BMI + under.five.deaths + Polio + Total.expenditure + Diphtheria + thinness.5.9.years + Income.composition.of.resources + Schooling + log.HIV.AIDS + log.GDP + log.percentage.expenditure, data = train)
summary(fit.lasso.lm)
```

    ## 
    ## Call:
    ## lm(formula = Life.expectancy ~ Year + Adult.Mortality + infant.deaths + 
    ##     Alcohol + Hepatitis.B + Measles + BMI + under.five.deaths + 
    ##     Polio + Total.expenditure + Diphtheria + thinness.5.9.years + 
    ##     Income.composition.of.resources + Schooling + log.HIV.AIDS + 
    ##     log.GDP + log.percentage.expenditure, data = train)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -19.418  -2.051  -0.014   2.090  16.285 
    ## 
    ## Coefficients:
    ##                                     Estimate   Std. Error t value
    ## (Intercept)                     -87.86816864  35.60863574   -2.47
    ## Year                              0.07075261   0.01780786    3.97
    ## Adult.Mortality                  -0.01619133   0.00080049  -20.23
    ## infant.deaths                     0.04917870   0.00826023    5.95
    ## Alcohol                           0.17713579   0.02451893    7.22
    ## Hepatitis.B                      -0.01000520   0.00369094   -2.71
    ## Measles                          -0.00001810   0.00000739   -2.45
    ## BMI                               0.01181600   0.00497976    2.37
    ## under.five.deaths                -0.03685229   0.00607112   -6.07
    ## Polio                             0.01947973   0.00445188    4.38
    ## Total.expenditure                 0.08037701   0.03375995    2.38
    ## Diphtheria                        0.02787326   0.00469797    5.93
    ## thinness.5.9.years               -0.11280442   0.02319791   -4.86
    ## Income.composition.of.resources   5.32836971   0.64342634    8.28
    ## Schooling                         0.39147770   0.04345582    9.01
    ## log.HIV.AIDS                     -2.43538150   0.07013381  -34.72
    ## log.GDP                           0.23424593   0.06421676    3.65
    ## log.percentage.expenditure        0.20004434   0.03539932    5.65
    ##                                             Pr(>|t|)    
    ## (Intercept)                                  0.01367 *  
    ## Year                                0.00007297061612 ***
    ## Adult.Mortality                 < 0.0000000000000002 ***
    ## infant.deaths                       0.00000000299476 ***
    ## Alcohol                             0.00000000000067 ***
    ## Hepatitis.B                                  0.00676 ** 
    ## Measles                                      0.01438 *  
    ## BMI                                          0.01773 *  
    ## under.five.deaths                   0.00000000147528 ***
    ## Polio                               0.00001261170940 ***
    ## Total.expenditure                            0.01735 *  
    ## Diphtheria                          0.00000000339056 ***
    ## thinness.5.9.years                  0.00000123068280 ***
    ## Income.composition.of.resources < 0.0000000000000002 ***
    ## Schooling                       < 0.0000000000000002 ***
    ## log.HIV.AIDS                    < 0.0000000000000002 ***
    ## log.GDP                                      0.00027 ***
    ## log.percentage.expenditure          0.00000001777414 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.73 on 2471 degrees of freedom
    ## Multiple R-squared:  0.85,   Adjusted R-squared:  0.849 
    ## F-statistic:  825 on 17 and 2471 DF,  p-value: <0.0000000000000002

``` r
### Visualize VIF
fit.lasso.lm_VIF = vif(fit.lasso.lm)
barplot(fit.lasso.lm_VIF, main = 'VIF Values', horiz = TRUE, col="blue", xlim = c(0,12))
abline(v=10, col="red")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-4.png)<!-- -->

``` r
# We can see that LASSO did not remove infant.deaths or under.five.deaths as those are perfectly correlated. Let's remove the one with the smallest coefficient (infant.death).

##### Fit Linear Model based on LASSO regularization without factors to measure VIF####
fit.lasso.lm2 = lm(Life.expectancy ~ Year + Adult.Mortality + Alcohol + Hepatitis.B + Measles + BMI + under.five.deaths + Polio + Total.expenditure + Diphtheria + thinness.5.9.years + Income.composition.of.resources + Schooling + log.HIV.AIDS + log.GDP + log.percentage.expenditure, data = train)
summary(fit.lasso.lm2)
```

    ## 
    ## Call:
    ## lm(formula = Life.expectancy ~ Year + Adult.Mortality + Alcohol + 
    ##     Hepatitis.B + Measles + BMI + under.five.deaths + Polio + 
    ##     Total.expenditure + Diphtheria + thinness.5.9.years + Income.composition.of.resources + 
    ##     Schooling + log.HIV.AIDS + log.GDP + log.percentage.expenditure, 
    ##     data = train)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -19.399  -2.096  -0.018   2.100  16.487 
    ## 
    ## Coefficients:
    ##                                     Estimate   Std. Error t value
    ## (Intercept)                     -85.66301509  35.85393285   -2.39
    ## Year                              0.06938775   0.01793002    3.87
    ## Adult.Mortality                  -0.01609347   0.00080587  -19.97
    ## Alcohol                           0.16005087   0.02451949    6.53
    ## Hepatitis.B                      -0.01176134   0.00370468   -3.17
    ## Measles                          -0.00001827   0.00000744   -2.46
    ## BMI                               0.01147329   0.00501400    2.29
    ## under.five.deaths                -0.00087933   0.00059649   -1.47
    ## Polio                             0.02070473   0.00447800    4.62
    ## Total.expenditure                 0.08547817   0.03398340    2.52
    ## Diphtheria                        0.03120475   0.00469691    6.64
    ## thinness.5.9.years               -0.09972801   0.02325405   -4.29
    ## Income.composition.of.resources   5.61225756   0.64611221    8.69
    ## Schooling                         0.39315805   0.04375662    8.99
    ## log.HIV.AIDS                     -2.52112777   0.06911568  -36.48
    ## log.GDP                           0.23144017   0.06466089    3.58
    ## log.percentage.expenditure        0.19501967   0.03563497    5.47
    ##                                             Pr(>|t|)    
    ## (Intercept)                                  0.01696 *  
    ## Year                                         0.00011 ***
    ## Adult.Mortality                 < 0.0000000000000002 ***
    ## Alcohol                               0.000000000081 ***
    ## Hepatitis.B                                  0.00152 ** 
    ## Measles                                      0.01415 *  
    ## BMI                                          0.02221 *  
    ## under.five.deaths                            0.14056    
    ## Polio                                 0.000003964443 ***
    ## Total.expenditure                            0.01196 *  
    ## Diphtheria                            0.000000000038 ***
    ## thinness.5.9.years                    0.000018668187 ***
    ## Income.composition.of.resources < 0.0000000000000002 ***
    ## Schooling                       < 0.0000000000000002 ***
    ## log.HIV.AIDS                    < 0.0000000000000002 ***
    ## log.GDP                                      0.00035 ***
    ## log.percentage.expenditure            0.000000048784 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.76 on 2472 degrees of freedom
    ## Multiple R-squared:  0.848,  Adjusted R-squared:  0.847 
    ## F-statistic:  862 on 16 and 2472 DF,  p-value: <0.0000000000000002

``` r
### re-run Visualize VIF
fit.lasso.lm_VIF2 = vif(fit.lasso.lm2)
barplot(fit.lasso.lm_VIF2, main = 'VIF Values', horiz = TRUE, col="blue", xlim = c(0,12))
abline(v=10, col="red")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-5.png)<!-- -->

``` r
##### Fit Linear Model based on LASSO regularization and removed multicollinearity####
fit.lasso.lm3 = lm(Life.expectancy ~ Year + Adult.Mortality + Alcohol + Hepatitis.B + Measles + BMI + under.five.deaths + Polio + Total.expenditure + Diphtheria + thinness.5.9.years + Income.composition.of.resources + Schooling + log.HIV.AIDS + log.GDP + log.percentage.expenditure + Status + Continent, data = train)

#### Hypothesis testing ####
summary(fit.lasso.lm3)
```

    ## 
    ## Call:
    ## lm(formula = Life.expectancy ~ Year + Adult.Mortality + Alcohol + 
    ##     Hepatitis.B + Measles + BMI + under.five.deaths + Polio + 
    ##     Total.expenditure + Diphtheria + thinness.5.9.years + Income.composition.of.resources + 
    ##     Schooling + log.HIV.AIDS + log.GDP + log.percentage.expenditure + 
    ##     Status + Continent, data = train)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -20.394  -1.969   0.021   1.889  14.375 
    ## 
    ## Coefficients:
    ##                                      Estimate    Std. Error t value
    ## (Intercept)                     -138.99090467   34.48224392   -4.03
    ## Year                               0.09760623    0.01725078    5.66
    ## Adult.Mortality                   -0.01442514    0.00077355  -18.65
    ## Alcohol                           -0.03407461    0.02875441   -1.19
    ## Hepatitis.B                       -0.01314677    0.00354890   -3.70
    ## Measles                           -0.00000866    0.00000712   -1.22
    ## BMI                                0.00919231    0.00490235    1.88
    ## under.five.deaths                 -0.00154142    0.00057032   -2.70
    ## Polio                              0.02295096    0.00426399    5.38
    ## Total.expenditure                  0.00348062    0.03315257    0.10
    ## Diphtheria                         0.02804974    0.00449357    6.24
    ## thinness.5.9.years                -0.05194805    0.02394674   -2.17
    ## Income.composition.of.resources    4.63899692    0.62693105    7.40
    ## Schooling                          0.36709828    0.04202002    8.74
    ## log.HIV.AIDS                      -2.21182774    0.08043372  -27.50
    ## log.GDP                            0.19599500    0.06190934    3.17
    ## log.percentage.expenditure         0.20409125    0.03396550    6.01
    ## StatusDeveloping                  -2.82237445    0.29358703   -9.61
    ## ContinentAmericas                  4.13248992    0.30055027   13.75
    ## ContinentAsia                      1.59747742    0.27943008    5.72
    ## ContinentEurope                    2.29303977    0.37386749    6.13
    ## ContinentOceania                   0.56287765    0.41275543    1.36
    ##                                             Pr(>|t|)    
    ## (Intercept)                         0.00005727243091 ***
    ## Year                                0.00000001707659 ***
    ## Adult.Mortality                 < 0.0000000000000002 ***
    ## Alcohol                                      0.23612    
    ## Hepatitis.B                                  0.00022 ***
    ## Measles                                      0.22379    
    ## BMI                                          0.06090 .  
    ## under.five.deaths                            0.00692 ** 
    ## Polio                               0.00000008038547 ***
    ## Total.expenditure                            0.91639    
    ## Diphtheria                          0.00000000050633 ***
    ## thinness.5.9.years                           0.03015 *  
    ## Income.composition.of.resources     0.00000000000019 ***
    ## Schooling                       < 0.0000000000000002 ***
    ## log.HIV.AIDS                    < 0.0000000000000002 ***
    ## log.GDP                                      0.00157 ** 
    ## log.percentage.expenditure          0.00000000214586 ***
    ## StatusDeveloping                < 0.0000000000000002 ***
    ## ContinentAmericas               < 0.0000000000000002 ***
    ## ContinentAsia                       0.00000001215511 ***
    ## ContinentEurope                     0.00000000099951 ***
    ## ContinentOceania                             0.17278    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.57 on 2467 degrees of freedom
    ## Multiple R-squared:  0.863,  Adjusted R-squared:  0.862 
    ## F-statistic:  740 on 21 and 2467 DF,  p-value: <0.0000000000000002

``` r
# At alpha = 0.05 the following variables are not significant therefore don't contribute to the model performance:
# Alcohol, Measles, BMI, Total.expenditure.

# Predicting
train_pred = predict(fit.lasso.lm3, train)
test_pred = predict(fit.lasso.lm3, test)

# Scoring the final model on Training and Test set
residuals = resid(fit.lasso.lm3)
postResample(pred = train_pred, obs = train$Life.expectancy)
```

    ##     RMSE Rsquared      MAE 
    ##   3.5567   0.8629   2.6435

``` r
postResample(pred = test_pred, obs = test$Life.expectancy)
```

    ##     RMSE Rsquared      MAE 
    ##   3.3412   0.8633   2.5391

``` r
sm = summary(fit.lasso.lm3)
mse = mean(sm$residuals^2)

### Checking Multiple Liner Regression model assumptions
confint(fit.lasso.lm3)
```

    ##                                         2.5 %        97.5 %
    ## (Intercept)                     -206.60803504 -71.373774293
    ## Year                               0.06377872   0.131433731
    ## Adult.Mortality                   -0.01594202  -0.012908269
    ## Alcohol                           -0.09045988   0.022310664
    ## Hepatitis.B                       -0.02010589  -0.006187639
    ## Measles                           -0.00002261   0.000005296
    ## BMI                               -0.00042084   0.018805468
    ## under.five.deaths                 -0.00265977  -0.000423062
    ## Polio                              0.01458959   0.031312325
    ## Total.expenditure                 -0.06152912   0.068490365
    ## Diphtheria                         0.01923819   0.036861297
    ## thinness.5.9.years                -0.09890584  -0.004990261
    ## Income.composition.of.resources    3.40963150   5.868362348
    ## Schooling                          0.28470012   0.449496433
    ## log.HIV.AIDS                      -2.36955233  -2.054103159
    ## log.GDP                            0.07459536   0.317394641
    ## log.percentage.expenditure         0.13748742   0.270695083
    ## StatusDeveloping                  -3.39807692  -2.246671991
    ## ContinentAmericas                  3.54313307   4.721846771
    ## ContinentAsia                      1.04953571   2.145419139
    ## ContinentEurope                    1.55991326   3.026166272
    ## ContinentOceania                  -0.24650523   1.372260522

``` r
hist(residuals, main = "Histogram of Residuals")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-6.png)<!-- -->

``` r
plot(residuals, main = "Residuals plot") 
abline(h=0, col="blue")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-7.png)<!-- -->

``` r
plot(fit.lasso.lm3, which = 2)
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-8.png)<!-- -->

``` r
plot(fit.lasso.lm3, which = 4)
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-9.png)<!-- -->

``` r
#####################################################################################
#                                  Forward Selection                                #
#####################################################################################
library(leaps)
mlr.fwd=regsubsets(Life.expectancy~.,data=train,method="forward",nvmax=22)
testASE<-c()
for (i in 1:22){
  predictions = predict.regsubsets(object=mlr.fwd,newdata=test,id=i) 
  testASE[i] = mean((test$Life.expectancy-predictions)^2)
}
par(mfrow=c(1,1))
plot(1:22,testASE,type="l",xlab="# of predictors",ylab="test vs train ASE")
index<-which(testASE==min(testASE))
points(index,testASE[index],col="red",pch=10)
rss<-summary(mlr.fwd)$rss
lines(1:22,rss/dim(train)[1],lty=3,col="blue")  
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-10.png)<!-- -->

``` r
mlr.fwd.final=regsubsets(Life.expectancy~.,data=LifeExp,method="forward",nvmax=22)
coef(mlr.fwd.final,17)
```

    ##                     (Intercept)                            Year 
    ##                      -149.10722                         0.10297 
    ##                StatusDeveloping                 Adult.Mortality 
    ##                        -2.69083                        -0.01420 
    ##                   infant.deaths                     Hepatitis.B 
    ##                         0.03946                        -0.01251 
    ##               under.five.deaths                           Polio 
    ##                        -0.03083                         0.02189 
    ##                      Diphtheria              thinness.5.9.years 
    ##                         0.02386                        -0.08257 
    ## Income.composition.of.resources                       Schooling 
    ##                         4.39971                         0.38232 
    ##                    log.HIV.AIDS                         log.GDP 
    ##                        -2.26481                         0.21454 
    ##      log.percentage.expenditure               ContinentAmericas 
    ##                         0.20427                         3.70312 
    ##                   ContinentAsia                 ContinentEurope 
    ##                         1.35750                         1.81299

``` r
summary(mlr.fwd.final)
```

    ## Subset selection object
    ## Call: regsubsets.formula(Life.expectancy ~ ., data = LifeExp, method = "forward", 
    ##     nvmax = 22)
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
    ## log.HIV.AIDS                        FALSE      FALSE
    ## log.GDP                             FALSE      FALSE
    ## log.percentage.expenditure          FALSE      FALSE
    ## ContinentAmericas                   FALSE      FALSE
    ## ContinentAsia                       FALSE      FALSE
    ## ContinentEurope                     FALSE      FALSE
    ## ContinentOceania                    FALSE      FALSE
    ## 1 subsets of each size up to 22
    ## Selection Algorithm: forward
    ##           Year StatusDeveloping Adult.Mortality infant.deaths Alcohol
    ## 1  ( 1 )  " "  " "              " "             " "           " "    
    ## 2  ( 1 )  " "  " "              " "             " "           " "    
    ## 3  ( 1 )  " "  " "              "*"             " "           " "    
    ## 4  ( 1 )  " "  " "              "*"             " "           " "    
    ## 5  ( 1 )  " "  " "              "*"             " "           " "    
    ## 6  ( 1 )  " "  " "              "*"             " "           " "    
    ## 7  ( 1 )  " "  "*"              "*"             " "           " "    
    ## 8  ( 1 )  " "  "*"              "*"             " "           " "    
    ## 9  ( 1 )  " "  "*"              "*"             " "           " "    
    ## 10  ( 1 ) " "  "*"              "*"             " "           " "    
    ## 11  ( 1 ) "*"  "*"              "*"             " "           " "    
    ## 12  ( 1 ) "*"  "*"              "*"             " "           " "    
    ## 13  ( 1 ) "*"  "*"              "*"             "*"           " "    
    ## 14  ( 1 ) "*"  "*"              "*"             "*"           " "    
    ## 15  ( 1 ) "*"  "*"              "*"             "*"           " "    
    ## 16  ( 1 ) "*"  "*"              "*"             "*"           " "    
    ## 17  ( 1 ) "*"  "*"              "*"             "*"           " "    
    ## 18  ( 1 ) "*"  "*"              "*"             "*"           " "    
    ## 19  ( 1 ) "*"  "*"              "*"             "*"           " "    
    ## 20  ( 1 ) "*"  "*"              "*"             "*"           " "    
    ## 21  ( 1 ) "*"  "*"              "*"             "*"           "*"    
    ## 22  ( 1 ) "*"  "*"              "*"             "*"           "*"    
    ##           Hepatitis.B Measles BMI under.five.deaths Polio Total.expenditure
    ## 1  ( 1 )  " "         " "     " " " "               " "   " "              
    ## 2  ( 1 )  " "         " "     " " " "               " "   " "              
    ## 3  ( 1 )  " "         " "     " " " "               " "   " "              
    ## 4  ( 1 )  " "         " "     " " " "               " "   " "              
    ## 5  ( 1 )  " "         " "     " " " "               " "   " "              
    ## 6  ( 1 )  " "         " "     " " " "               " "   " "              
    ## 7  ( 1 )  " "         " "     " " " "               " "   " "              
    ## 8  ( 1 )  " "         " "     " " " "               " "   " "              
    ## 9  ( 1 )  " "         " "     " " " "               "*"   " "              
    ## 10  ( 1 ) " "         " "     " " " "               "*"   " "              
    ## 11  ( 1 ) " "         " "     " " " "               "*"   " "              
    ## 12  ( 1 ) " "         " "     " " "*"               "*"   " "              
    ## 13  ( 1 ) " "         " "     " " "*"               "*"   " "              
    ## 14  ( 1 ) " "         " "     " " "*"               "*"   " "              
    ## 15  ( 1 ) " "         " "     " " "*"               "*"   " "              
    ## 16  ( 1 ) " "         " "     " " "*"               "*"   " "              
    ## 17  ( 1 ) "*"         " "     " " "*"               "*"   " "              
    ## 18  ( 1 ) "*"         " "     "*" "*"               "*"   " "              
    ## 19  ( 1 ) "*"         "*"     "*" "*"               "*"   " "              
    ## 20  ( 1 ) "*"         "*"     "*" "*"               "*"   " "              
    ## 21  ( 1 ) "*"         "*"     "*" "*"               "*"   " "              
    ## 22  ( 1 ) "*"         "*"     "*" "*"               "*"   " "              
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
    ## 16  ( 1 ) "*"        " "        " "                  "*"               
    ## 17  ( 1 ) "*"        " "        " "                  "*"               
    ## 18  ( 1 ) "*"        " "        " "                  "*"               
    ## 19  ( 1 ) "*"        " "        " "                  "*"               
    ## 20  ( 1 ) "*"        " "        " "                  "*"               
    ## 21  ( 1 ) "*"        " "        " "                  "*"               
    ## 22  ( 1 ) "*"        "*"        " "                  "*"               
    ##           Income.composition.of.resources Schooling log.HIV.AIDS log.GDP
    ## 1  ( 1 )  " "                             " "       "*"          " "    
    ## 2  ( 1 )  " "                             "*"       "*"          " "    
    ## 3  ( 1 )  " "                             "*"       "*"          " "    
    ## 4  ( 1 )  " "                             "*"       "*"          " "    
    ## 5  ( 1 )  " "                             "*"       "*"          "*"    
    ## 6  ( 1 )  " "                             "*"       "*"          "*"    
    ## 7  ( 1 )  " "                             "*"       "*"          "*"    
    ## 8  ( 1 )  "*"                             "*"       "*"          "*"    
    ## 9  ( 1 )  "*"                             "*"       "*"          "*"    
    ## 10  ( 1 ) "*"                             "*"       "*"          "*"    
    ## 11  ( 1 ) "*"                             "*"       "*"          "*"    
    ## 12  ( 1 ) "*"                             "*"       "*"          "*"    
    ## 13  ( 1 ) "*"                             "*"       "*"          "*"    
    ## 14  ( 1 ) "*"                             "*"       "*"          "*"    
    ## 15  ( 1 ) "*"                             "*"       "*"          "*"    
    ## 16  ( 1 ) "*"                             "*"       "*"          "*"    
    ## 17  ( 1 ) "*"                             "*"       "*"          "*"    
    ## 18  ( 1 ) "*"                             "*"       "*"          "*"    
    ## 19  ( 1 ) "*"                             "*"       "*"          "*"    
    ## 20  ( 1 ) "*"                             "*"       "*"          "*"    
    ## 21  ( 1 ) "*"                             "*"       "*"          "*"    
    ## 22  ( 1 ) "*"                             "*"       "*"          "*"    
    ##           log.percentage.expenditure ContinentAmericas ContinentAsia
    ## 1  ( 1 )  " "                        " "               " "          
    ## 2  ( 1 )  " "                        " "               " "          
    ## 3  ( 1 )  " "                        " "               " "          
    ## 4  ( 1 )  " "                        " "               " "          
    ## 5  ( 1 )  " "                        " "               " "          
    ## 6  ( 1 )  " "                        "*"               " "          
    ## 7  ( 1 )  " "                        "*"               " "          
    ## 8  ( 1 )  " "                        "*"               " "          
    ## 9  ( 1 )  " "                        "*"               " "          
    ## 10  ( 1 ) "*"                        "*"               " "          
    ## 11  ( 1 ) "*"                        "*"               " "          
    ## 12  ( 1 ) "*"                        "*"               " "          
    ## 13  ( 1 ) "*"                        "*"               " "          
    ## 14  ( 1 ) "*"                        "*"               " "          
    ## 15  ( 1 ) "*"                        "*"               "*"          
    ## 16  ( 1 ) "*"                        "*"               "*"          
    ## 17  ( 1 ) "*"                        "*"               "*"          
    ## 18  ( 1 ) "*"                        "*"               "*"          
    ## 19  ( 1 ) "*"                        "*"               "*"          
    ## 20  ( 1 ) "*"                        "*"               "*"          
    ## 21  ( 1 ) "*"                        "*"               "*"          
    ## 22  ( 1 ) "*"                        "*"               "*"          
    ##           ContinentEurope ContinentOceania
    ## 1  ( 1 )  " "             " "             
    ## 2  ( 1 )  " "             " "             
    ## 3  ( 1 )  " "             " "             
    ## 4  ( 1 )  " "             " "             
    ## 5  ( 1 )  " "             " "             
    ## 6  ( 1 )  " "             " "             
    ## 7  ( 1 )  " "             " "             
    ## 8  ( 1 )  " "             " "             
    ## 9  ( 1 )  " "             " "             
    ## 10  ( 1 ) " "             " "             
    ## 11  ( 1 ) " "             " "             
    ## 12  ( 1 ) " "             " "             
    ## 13  ( 1 ) " "             " "             
    ## 14  ( 1 ) "*"             " "             
    ## 15  ( 1 ) "*"             " "             
    ## 16  ( 1 ) "*"             " "             
    ## 17  ( 1 ) "*"             " "             
    ## 18  ( 1 ) "*"             " "             
    ## 19  ( 1 ) "*"             " "             
    ## 20  ( 1 ) "*"             "*"             
    ## 21  ( 1 ) "*"             "*"             
    ## 22  ( 1 ) "*"             "*"

``` r
# Metrics RMSE; R-squared; MAE
postResample(pred = predictions, obs = ytest)
```

    ##     RMSE Rsquared      MAE 
    ##   3.3266   0.8644   2.5177

``` r
##### Fit Linear Model based on FOrward Selection without factors to measure VIF####
fit.fwd.lm = lm(Life.expectancy ~ Year+Adult.Mortality+infant.deaths+Hepatitis.B+under.five.deaths+Polio+Diphtheria+thinness.5.9.years+Income.composition.of.resources +Schooling+log.HIV.AIDS+log.GDP+log.percentage.expenditure, data = train)

### Visualize VIF
fit.fwd.lm_VIF = vif(fit.fwd.lm)
barplot(fit.fwd.lm_VIF, main = 'VIF Values', horiz = TRUE, col="blue", xlim = c(0,12))
abline(v=10, col="red")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-11.png)<!-- -->

``` r
# We can see that Forward Selection did not remove infant.deaths or under.five.deaths as those are perfectly correlated. Let's remove the one with the smallest coefficient (under.five.deaths).

##### Fit Linear Model based on LASSO regularization without factors to measure VIF####
fit.fwd.lm2 = lm(Life.expectancy ~ Year + Adult.Mortality + Alcohol + Hepatitis.B + Measles + BMI + Polio + Total.expenditure + Diphtheria + thinness.5.9.years + Income.composition.of.resources + Schooling + log.HIV.AIDS + log.GDP + log.percentage.expenditure, data = train)
summary(fit.fwd.lm2)
```

    ## 
    ## Call:
    ## lm(formula = Life.expectancy ~ Year + Adult.Mortality + Alcohol + 
    ##     Hepatitis.B + Measles + BMI + Polio + Total.expenditure + 
    ##     Diphtheria + thinness.5.9.years + Income.composition.of.resources + 
    ##     Schooling + log.HIV.AIDS + log.GDP + log.percentage.expenditure, 
    ##     data = train)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -19.349  -2.104  -0.021   2.089  16.424 
    ## 
    ## Coefficients:
    ##                                     Estimate   Std. Error t value
    ## (Intercept)                     -83.27366817  35.82577493   -2.32
    ## Year                              0.06817413   0.01791536    3.81
    ## Adult.Mortality                  -0.01606601   0.00080585  -19.94
    ## Alcohol                           0.15600083   0.02437087    6.40
    ## Hepatitis.B                      -0.01109161   0.00367759   -3.02
    ## Measles                          -0.00002325   0.00000664   -3.50
    ## BMI                               0.01106911   0.00500769    2.21
    ## Polio                             0.02089389   0.00447722    4.67
    ## Total.expenditure                 0.08513273   0.03399065    2.50
    ## Diphtheria                        0.03134604   0.00469705    6.67
    ## thinness.5.9.years               -0.11391421   0.02117451   -5.38
    ## Income.composition.of.resources   5.56755543   0.64555333    8.62
    ## Schooling                         0.39749243   0.04366808    9.10
    ## log.HIV.AIDS                     -2.51714615   0.06907927  -36.44
    ## log.GDP                           0.23492127   0.06463309    3.63
    ## log.percentage.expenditure        0.19341807   0.03562685    5.43
    ##                                             Pr(>|t|)    
    ## (Intercept)                                  0.02018 *  
    ## Year                                         0.00015 ***
    ## Adult.Mortality                 < 0.0000000000000002 ***
    ## Alcohol                               0.000000000184 ***
    ## Hepatitis.B                                  0.00259 ** 
    ## Measles                                      0.00047 ***
    ## BMI                                          0.02717 *  
    ## Polio                                 0.000003223960 ***
    ## Total.expenditure                            0.01232 *  
    ## Diphtheria                            0.000000000031 ***
    ## thinness.5.9.years                    0.000000081580 ***
    ## Income.composition.of.resources < 0.0000000000000002 ***
    ## Schooling                       < 0.0000000000000002 ***
    ## log.HIV.AIDS                    < 0.0000000000000002 ***
    ## log.GDP                                      0.00028 ***
    ## log.percentage.expenditure            0.000000062193 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.76 on 2473 degrees of freedom
    ## Multiple R-squared:  0.848,  Adjusted R-squared:  0.847 
    ## F-statistic:  919 on 15 and 2473 DF,  p-value: <0.0000000000000002

``` r
### re-run Visualize VIF
fit.fwd.lm2_VIF = vif(fit.fwd.lm2)
barplot(fit.fwd.lm2_VIF, main = 'Re-test of VIF Values', horiz = TRUE, col="blue", xlim = c(0,12))
abline(v=10, col="red")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-12.png)<!-- -->

``` r
##### Fit Linear Model based on Forward Selection regularization and removed multicollinearity####
fit.fwd.lm3 = lm(Life.expectancy ~ Year + Adult.Mortality + Alcohol + Hepatitis.B + Measles + BMI + Polio + Total.expenditure + Diphtheria + thinness.5.9.years + Income.composition.of.resources + Schooling + log.HIV.AIDS + log.GDP + log.percentage.expenditure + Status + Continent, data = train)

#### Hypothesis testing ####
summary(fit.fwd.lm3)
```

    ## 
    ## Call:
    ## lm(formula = Life.expectancy ~ Year + Adult.Mortality + Alcohol + 
    ##     Hepatitis.B + Measles + BMI + Polio + Total.expenditure + 
    ##     Diphtheria + thinness.5.9.years + Income.composition.of.resources + 
    ##     Schooling + log.HIV.AIDS + log.GDP + log.percentage.expenditure + 
    ##     Status + Continent, data = train)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -20.423  -2.003   0.032   1.895  14.288 
    ## 
    ## Coefficients:
    ##                                      Estimate    Std. Error t value
    ## (Intercept)                     -134.19081329   34.48043377   -3.89
    ## Year                               0.09516000    0.01724901    5.52
    ## Adult.Mortality                   -0.01439729    0.00077447  -18.59
    ## Alcohol                           -0.03810641    0.02875234   -1.33
    ## Hepatitis.B                       -0.01202042    0.00352884   -3.41
    ## Measles                           -0.00001748    0.00000633   -2.76
    ## BMI                                0.00871269    0.00490539    1.78
    ## Polio                              0.02324859    0.00426801    5.45
    ## Total.expenditure                  0.00468394    0.03319190    0.14
    ## Diphtheria                         0.02824480    0.00449872    6.28
    ## thinness.5.9.years                -0.07868847    0.02183512   -3.60
    ## Income.composition.of.resources    4.55753661    0.62700547    7.27
    ## Schooling                          0.37603989    0.04194305    8.97
    ## log.HIV.AIDS                      -2.20948736    0.08053173  -27.44
    ## log.GDP                            0.20138472    0.06195620    3.25
    ## log.percentage.expenditure         0.20165867    0.03399691    5.93
    ## StatusDeveloping                  -2.78386113    0.29361532   -9.48
    ## ContinentAmericas                  4.06908147    0.30001567   13.56
    ## ContinentAsia                      1.58687294    0.27975918    5.67
    ## ContinentEurope                    2.24373576    0.37389884    6.00
    ## ContinentOceania                   0.48408652    0.41225020    1.17
    ##                                             Pr(>|t|)    
    ## (Intercept)                                  0.00010 ***
    ## Year                                0.00000003811165 ***
    ## Adult.Mortality                 < 0.0000000000000002 ***
    ## Alcohol                                      0.18518    
    ## Hepatitis.B                                  0.00067 ***
    ## Measles                                      0.00582 ** 
    ## BMI                                          0.07583 .  
    ## Polio                               0.00000005624054 ***
    ## Total.expenditure                            0.88779    
    ## Diphtheria                          0.00000000040286 ***
    ## thinness.5.9.years                           0.00032 ***
    ## Income.composition.of.resources     0.00000000000048 ***
    ## Schooling                       < 0.0000000000000002 ***
    ## log.HIV.AIDS                    < 0.0000000000000002 ***
    ## log.GDP                                      0.00117 ** 
    ## log.percentage.expenditure          0.00000000341903 ***
    ## StatusDeveloping                < 0.0000000000000002 ***
    ## ContinentAmericas               < 0.0000000000000002 ***
    ## ContinentAsia                       0.00000001573484 ***
    ## ContinentEurope                     0.00000000225073 ***
    ## ContinentOceania                             0.24041    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.58 on 2468 degrees of freedom
    ## Multiple R-squared:  0.863,  Adjusted R-squared:  0.861 
    ## F-statistic:  774 on 20 and 2468 DF,  p-value: <0.0000000000000002

``` r
# At alpha = 0.05 the following variables are not significant therefore don't contribute to the model performance:
# Alcohol, Measles, BMI, Total.expenditure.

# Predicting
train_pred = predict(fit.fwd.lm3, train)
test_pred = predict(fit.fwd.lm3, test)

# Scoring the final model on Training and Test set
residuals = resid(fit.fwd.lm3)
postResample(pred = train_pred, obs = train$Life.expectancy)
```

    ##     RMSE Rsquared      MAE 
    ##   3.5619   0.8625   2.6505

``` r
postResample(pred = test_pred, obs = test$Life.expectancy)
```

    ##     RMSE Rsquared      MAE 
    ##   3.3468   0.8629   2.5537

``` r
sm = summary(fit.fwd.lm3)
mse = mean(sm$residuals^2)

### Checking Multiple Liner Regression model assumptions
confint(fit.fwd.lm3)
```

    ##                                        2.5 %        97.5 %
    ## (Intercept)                     -201.8043806 -66.577245948
    ## Year                               0.0613360   0.128984028
    ## Adult.Mortality                   -0.0159160  -0.012878614
    ## Alcohol                           -0.0944876   0.018274798
    ## Hepatitis.B                       -0.0189402  -0.005100618
    ## Measles                           -0.0000299  -0.000005061
    ## BMI                               -0.0009064   0.018331804
    ## Polio                              0.0148793   0.031617841
    ## Total.expenditure                 -0.0604029   0.069770782
    ## Diphtheria                         0.0194231   0.037066462
    ## thinness.5.9.years                -0.1215055  -0.035871424
    ## Income.composition.of.resources    3.3280255   5.787047720
    ## Schooling                          0.2937927   0.458287094
    ## log.HIV.AIDS                      -2.3674041  -2.051570626
    ## log.GDP                            0.0798932   0.322876219
    ## log.percentage.expenditure         0.1349933   0.268324093
    ## StatusDeveloping                  -3.3596190  -2.208103303
    ## ContinentAmericas                  3.4807731   4.657389893
    ## ContinentAsia                      1.0382860   2.135459897
    ## ContinentEurope                    1.5105479   2.976923590
    ## ContinentOceania                  -0.3243055   1.292478504

``` r
hist(residuals, main = "Histogram of Residuals")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-13.png)<!-- -->

``` r
plot(residuals, main = "Residuals plot") 
abline(h=0, col="blue")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-14.png)<!-- -->

``` r
plot(fit.fwd.lm3, which = 2)
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-15.png)<!-- -->

``` r
plot(fit.fwd.lm3, which = 4)
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-16.png)<!-- -->

``` r
#####################################################################################
#                               Backward Elimination                                #
#####################################################################################

mlr.bck=regsubsets(Life.expectancy~.,data=train,method="backward",nvmax=22)
testASE<-c()
for (i in 1:22){
  predictions = predict.regsubsets(object=mlr.bck,newdata=test,id=i) 
  testASE[i] = mean((test$Life.expectancy-predictions)^2)
}
par(mfrow=c(1,1))
plot(1:22,testASE,type="l",xlab="# of predictors",ylab="test vs train ASE")
index<-which(testASE==min(testASE))
points(index,testASE[index],col="red",pch=10)
rss<-summary(mlr.bck)$rss
lines(1:22,rss/dim(train)[1],lty=3,col="blue")  
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-17.png)<!-- -->

``` r
mlr.bck.final=regsubsets(Life.expectancy~.,data=LifeExp,method="backward",nvmax=22)
coef(mlr.bck.final,17)
```

    ##                     (Intercept)                            Year 
    ##                      -149.10722                         0.10297 
    ##                StatusDeveloping                 Adult.Mortality 
    ##                        -2.69083                        -0.01420 
    ##                   infant.deaths                     Hepatitis.B 
    ##                         0.03946                        -0.01251 
    ##               under.five.deaths                           Polio 
    ##                        -0.03083                         0.02189 
    ##                      Diphtheria              thinness.5.9.years 
    ##                         0.02386                        -0.08257 
    ## Income.composition.of.resources                       Schooling 
    ##                         4.39971                         0.38232 
    ##                    log.HIV.AIDS                         log.GDP 
    ##                        -2.26481                         0.21454 
    ##      log.percentage.expenditure               ContinentAmericas 
    ##                         0.20427                         3.70312 
    ##                   ContinentAsia                 ContinentEurope 
    ##                         1.35750                         1.81299

``` r
# Metrics RMSE; R-squared; MAE
postResample(pred = predictions, obs = ytest)
```

    ##     RMSE Rsquared      MAE 
    ##   3.3266   0.8644   2.5177

``` r
##### Result is the same as the forward model therefore does not need to fit another linear model ###### 

#####################################################################################
#                                  Ridge regression                                 #
#####################################################################################
x=model.matrix(Life.expectancy~.,train)[,-1]
y=train$Life.expectancy
xtest = model.matrix(Life.expectancy~.,test)[,-1]
ytest = test$Life.expectancy

grid=10^seq(10,-2, length =100)
ridge.mod=glmnet(x,y,alpha=0, lambda =grid, family = 'gaussian') # alpha is 0 for Ridge
cv.out=cv.glmnet(x,y,alpha=0)
plot(cv.out)
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-18.png)<!-- -->

``` r
bestlambda = cv.out$lambda.min  #Optimal penalty parameter.  You can make this call visually.
ridge.pred=predict(ridge.mod ,s=bestlambda ,newx=xtest)

testMSE_RIDGE<-mean((ytest-ridge.pred)^2)
testMSE_RIDGE
```

    ## [1] 11.22

``` r
coef(ridge.mod,s=bestlambda)
```

    ## 25 x 1 sparse Matrix of class "dgCMatrix"
    ##                                                s1
    ## (Intercept)                     -121.237094245132
    ## Year                               0.088411523023
    ## StatusDeveloping                  -2.504785174406
    ## Adult.Mortality                   -0.014745110402
    ## infant.deaths                      0.000539467064
    ## Alcohol                           -0.002498580462
    ## Hepatitis.B                       -0.010840470220
    ## Measles                           -0.000011476578
    ## BMI                                0.015190527668
    ## under.five.deaths                 -0.001836775172
    ## Polio                              0.023558919058
    ## Total.expenditure                  0.017348394782
    ## Diphtheria                         0.028035743376
    ## Population                         0.000000001501
    ## thinness..1.19.years              -0.026550903279
    ## thinness.5.9.years                -0.051291902877
    ## Income.composition.of.resources    4.792812464249
    ## Schooling                          0.362631536019
    ## log.HIV.AIDS                      -1.999633987209
    ## log.GDP                            0.250111363224
    ## log.percentage.expenditure         0.178952680701
    ## ContinentAmericas                  3.571891816569
    ## ContinentAsia                      1.615134532568
    ## ContinentEurope                    1.961302877035
    ## ContinentOceania                   0.423829889525

``` r
# Metrics RMSE; R-squared; MAE
postResample(pred = ridge.pred, obs = ytest)
```

    ##     RMSE Rsquared      MAE 
    ##    3.350    0.862    2.530

``` r
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

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-19.png)<!-- -->

``` r
alpha = cva.out$alpha
mse = sapply(cva.out$modlist, function(mod) {min(mod$cvm)})
lambdaMin <- sapply(cva.out$modlist, `[[`, "lambda.min")
min_mse <- which.min(mse)
cva.min = data.frame(alpha = alpha[min_mse], lambdaMin = lambdaMin[min_mse], mse = mse[min_mse])
cva.min
```

    ##   alpha lambdaMin   mse
    ## 1 0.125  0.006239 12.83

``` r
elastic.mod = glmnet(x,y, alpha = cva.min$alpha, lambda = cva.min$lambdaMin)
elastic.pred=predict(elastic.mod ,s=cva.min$lambdaMin ,newx=xtest)
elastic.pred_coef=predict(elastic.mod ,s=cva.min$lambdaMin ,newx=xtest, type = "coef")

testMSE_ELASTIC<-mean((ytest-elastic.pred)^2)
testMSE_ELASTIC
```

    ## [1] 11.08

``` r
# Metrics RMSE; R-squared; MAE
postResample(pred = elastic.pred, obs = ytest)
```

    ##     RMSE Rsquared      MAE 
    ##   3.3291   0.8642   2.5245

``` r
#####################################################################################
#                                      Manual MLR                                   #
#####################################################################################
# 5-fold cross validation
cv <- trainControl(
  method = "cv", 
  number = 5,
  savePredictions = TRUE
)

MLRT = train(
  Life.expectancy ~ Status + Continent + Income.composition.of.resources + Schooling + log.HIV.AIDS + log.GDP + log.percentage.expenditure + BMI + Year + Adult.Mortality,
  data = train,
  method = "lm",
  trControl = cv)

### Visualize VIF
MLR_VIF = vif(MLRT$finalModel)
barplot(MLR_VIF, main = 'VIF Values', horiz = TRUE, col="blue", xlim = c(0,12))
abline(v=10, col="red")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-20.png)<!-- -->

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
    ## -21.120  -2.076   0.054   1.970  15.273 
    ## 
    ## Coefficients:
    ##                                    Estimate  Std. Error t value
    ## (Intercept)                     -173.010301   35.101155   -4.93
    ## StatusDeveloping                  -2.781233    0.288515   -9.64
    ## ContinentAmericas                  4.281700    0.289391   14.80
    ## ContinentAsia                      1.516151    0.283759    5.34
    ## ContinentEurope                    2.345345    0.361180    6.49
    ## ContinentOceania                   0.398256    0.416730    0.96
    ## Income.composition.of.resources    5.032472    0.645252    7.80
    ## Schooling                          0.419672    0.042667    9.84
    ## log.HIV.AIDS                      -2.386342    0.081713  -29.20
    ## log.GDP                            0.198538    0.063721    3.12
    ## log.percentage.expenditure         0.220937    0.035140    6.29
    ## BMI                                0.018319    0.004894    3.74
    ## Year                               0.115071    0.017551    6.56
    ## Adult.Mortality                   -0.014221    0.000797  -17.84
    ##                                             Pr(>|t|)    
    ## (Intercept)                       0.0000008815981793 ***
    ## StatusDeveloping                < 0.0000000000000002 ***
    ## ContinentAmericas               < 0.0000000000000002 ***
    ## ContinentAsia                     0.0000000997167012 ***
    ## ContinentEurope                   0.0000000001009618 ***
    ## ContinentOceania                             0.33933    
    ## Income.composition.of.resources   0.0000000000000091 ***
    ## Schooling                       < 0.0000000000000002 ***
    ## log.HIV.AIDS                    < 0.0000000000000002 ***
    ## log.GDP                                      0.00186 ** 
    ## log.percentage.expenditure        0.0000000003805994 ***
    ## BMI                                          0.00019 ***
    ## Year                              0.0000000000668247 ***
    ## Adult.Mortality                 < 0.0000000000000002 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.71 on 2475 degrees of freedom
    ## Multiple R-squared:  0.852,  Adjusted R-squared:  0.851 
    ## F-statistic: 1.1e+03 on 13 and 2475 DF,  p-value: <0.0000000000000002

``` r
# Predicting
train_pred = predict(MLRT, train)
test_pred = predict(MLRT, test)

# Scoring the final model on Training and Validation set
summary(MLRT$finalModel)
```

    ## 
    ## Call:
    ## lm(formula = .outcome ~ ., data = dat)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -21.120  -2.076   0.054   1.970  15.273 
    ## 
    ## Coefficients:
    ##                                    Estimate  Std. Error t value
    ## (Intercept)                     -173.010301   35.101155   -4.93
    ## StatusDeveloping                  -2.781233    0.288515   -9.64
    ## ContinentAmericas                  4.281700    0.289391   14.80
    ## ContinentAsia                      1.516151    0.283759    5.34
    ## ContinentEurope                    2.345345    0.361180    6.49
    ## ContinentOceania                   0.398256    0.416730    0.96
    ## Income.composition.of.resources    5.032472    0.645252    7.80
    ## Schooling                          0.419672    0.042667    9.84
    ## log.HIV.AIDS                      -2.386342    0.081713  -29.20
    ## log.GDP                            0.198538    0.063721    3.12
    ## log.percentage.expenditure         0.220937    0.035140    6.29
    ## BMI                                0.018319    0.004894    3.74
    ## Year                               0.115071    0.017551    6.56
    ## Adult.Mortality                   -0.014221    0.000797  -17.84
    ##                                             Pr(>|t|)    
    ## (Intercept)                       0.0000008815981793 ***
    ## StatusDeveloping                < 0.0000000000000002 ***
    ## ContinentAmericas               < 0.0000000000000002 ***
    ## ContinentAsia                     0.0000000997167012 ***
    ## ContinentEurope                   0.0000000001009618 ***
    ## ContinentOceania                             0.33933    
    ## Income.composition.of.resources   0.0000000000000091 ***
    ## Schooling                       < 0.0000000000000002 ***
    ## log.HIV.AIDS                    < 0.0000000000000002 ***
    ## log.GDP                                      0.00186 ** 
    ## log.percentage.expenditure        0.0000000003805994 ***
    ## BMI                                          0.00019 ***
    ## Year                              0.0000000000668247 ***
    ## Adult.Mortality                 < 0.0000000000000002 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.71 on 2475 degrees of freedom
    ## Multiple R-squared:  0.852,  Adjusted R-squared:  0.851 
    ## F-statistic: 1.1e+03 on 13 and 2475 DF,  p-value: <0.0000000000000002

``` r
residuals = resid(MLRT$finalModel)
postResample(pred = train_pred, obs = train$Life.expectancy)
```

    ##     RMSE Rsquared      MAE 
    ##   3.6965   0.8519   2.7336

``` r
postResample(pred = test_pred, obs = test$Life.expectancy)
```

    ##     RMSE Rsquared      MAE 
    ##   3.4205   0.8566   2.5967

``` r
### Checking Multiple Liner Regression model assumptions
fit = lm(Life.expectancy ~ Status + Continent + Income.composition.of.resources + Schooling + log.HIV.AIDS + log.GDP + log.percentage.expenditure + BMI + Year + Adult.Mortality, train)
confint(fit)
```

    ##                                       2.5 %     97.5 %
    ## (Intercept)                     -241.840961 -104.17964
    ## StatusDeveloping                  -3.346988   -2.21548
    ## ContinentAmericas                  3.714226    4.84917
    ## ContinentAsia                      0.959721    2.07258
    ## ContinentEurope                    1.637099    3.05359
    ## ContinentOceania                  -0.418918    1.21543
    ## Income.composition.of.resources    3.767183    6.29776
    ## Schooling                          0.336005    0.50334
    ## log.HIV.AIDS                      -2.546574   -2.22611
    ## log.GDP                            0.073587    0.32349
    ## log.percentage.expenditure         0.152030    0.28984
    ## BMI                                0.008722    0.02792
    ## Year                               0.080655    0.14949
    ## Adult.Mortality                   -0.015784   -0.01266

``` r
hist(residuals, main = "Histogram of Residuals")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-21.png)<!-- -->

``` r
plot(residuals, main = "Residuals plot") 
abline(h=0, col="blue")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-22.png)<!-- -->

``` r
plot(fit, which = 2)
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-23.png)<!-- -->

``` r
plot(fit, which = 4)
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-24.png)<!-- -->

``` r
#####################################################################################
#                                      Objective 2                                  #
#                   Model with complexity (adding interaction terms)                #
#####################################################################################
```

``` r
#####################################################################################
#                                      Objective 2                                  #
#                                     KNN - regression                              #
#####################################################################################
# Check for zero variance
caret::nearZeroVar(LifeExpKNN %>% select(where(is.numeric)), saveMetrics = TRUE) %>% 
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
# Scale the numerical variables as KNN is sensitive to that
LifeExpKNN_scale = LifeExpKNN %>% mutate_if(is.numeric, scale)

### KNN Data Preparation
index<-sample(1:dim(LifeExpKNN)[1],round(dim(LifeExpKNN)[1]*0.85),replace=F)
KNNtrain = LifeExpKNN[index,]
KNNtest = LifeExpKNN[-index,]

x=KNNtrain[,-4]
y=KNNtrain$Life.expectancy
xtest = KNNtest[,-4]
ytest = KNNtest$Life.expectancy

# Search for optimal k
k_grid = expand.grid(k = seq(2, 25, by = 1))

# Model Training
KNNRegressor = train(
  Life.expectancy~.,
  data = KNNtrain,
  method = "knn",
  preProcess = c("center", "scale"), 
  tuneGrid = k_grid,
  trControl = cv
  )

KNNRegressor$finalModel
```

    ## 3-nearest neighbor regression model

``` r
prediction_train = predict(KNNRegressor, KNNtrain)
prediction_test = predict(KNNRegressor, KNNtest)

x = 1:length(KNNtest$Life.expectancy)
plot(x, KNNtest$Life.expectancy, col = "red", type = "l", lwd=2,
     main = "Life Expectancy prediction")
lines(x, prediction_test, col = "blue", lwd=2)
legend("topright",  legend = c("original observation", "predicted life expectancy"), 
       fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
grid()
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Objective%202%20KNN-1.png)<!-- -->

### Visualize ‘k’ and the most important features

``` r
# Visualize 'k' and the most important features
ggplot(KNNRegressor) + ggtitle("Optimal k value for the highest accuracy") +
  theme(plot.title = element_text(hjust = 0.5))
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-11-1.png" angle=90 style="display: block; margin: auto;" />

``` r
KNNvarImp = varImp(KNNRegressor)
plot(KNNvarImp, top = 5, main='Top 5 Variable predicting life expectancy (KNN)')
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-11-2.png" angle=90 style="display: block; margin: auto;" />
