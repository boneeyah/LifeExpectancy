MSDS_6372_Project1
================
Miguel Bonilla, Reuven Derner, Milan Patel, Tamas Toth
2022-05-27

#### Loading the necessary R libraries for the analysis

``` r
# Load the necessary libraries
library(knitr)
library(rmarkdown)
```

    ## Warning: package 'rmarkdown' was built under R version 4.1.3

``` r
library(ggpubr)
```

    ## Warning: package 'ggpubr' was built under R version 4.1.3

``` r
library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2)
library(tidyverse)
library(ggthemes)
library(e1071)
library(class)
library(caret)
```

    ## Warning: package 'caret' was built under R version 4.1.3

``` r
library(stringr)
library(sjPlot)
```

    ## Warning: package 'sjPlot' was built under R version 4.1.3

``` r
library(data.table)
library(reshape2)
library(corrplot)
```

    ## Warning: package 'corrplot' was built under R version 4.1.3

``` r
library(naivebayes)
```

    ## Warning: package 'naivebayes' was built under R version 4.1.3

``` r
library(car)
library(egg)
```

    ## Warning: package 'egg' was built under R version 4.1.3

``` r
library(rworldmap)
```

    ## Warning: package 'rworldmap' was built under R version 4.1.3

``` r
library(Hmisc)
```

    ## Warning: package 'Hmisc' was built under R version 4.1.3

``` r
library(DataExplorer)
```

    ## Warning: package 'DataExplorer' was built under R version 4.1.3

``` r
library(selectiveInference)
```

    ## Warning: package 'selectiveInference' was built under R version 4.1.3

    ## Warning: package 'glmnet' was built under R version 4.1.3

    ## Warning: package 'adaptMCMC' was built under R version 4.1.3

``` r
library(dlookr)
```

    ## Warning: package 'dlookr' was built under R version 4.1.3

``` r
# Turn off scientific notation
options(scipen = 100, digits = 4)
```

#### Read the data

``` r
#Read the data
#setwd('/Users/ttoth76/Downloads/SMU/Semester_2/DS 6372 Applied Statistics_Inference & Modeling/FLS/Project1_Summer2022/GitContent/LifeExpectancy')
LifeExp = read.csv(file = 'https://raw.githubusercontent.com/boneeyah/LifeExpectancy/main/Data%20Files/Life_Expectancy_Data.csv',header = TRUE, sep = ",", encoding = "UTF-8")
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
Tonga
</td>
<td style="text-align:right;">
2002
</td>
<td style="text-align:left;">
Developing
</td>
<td style="text-align:right;">
71.9
</td>
<td style="text-align:right;">
155
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.51
</td>
<td style="text-align:right;">
310.8203
</td>
<td style="text-align:right;">
88
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
67.0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
85
</td>
<td style="text-align:right;">
5.30
</td>
<td style="text-align:right;">
84
</td>
<td style="text-align:right;">
0.1
</td>
<td style="text-align:right;">
1842.44
</td>
<td style="text-align:right;">
99184
</td>
<td style="text-align:right;">
0.1
</td>
<td style="text-align:right;">
0.1
</td>
<td style="text-align:right;">
0.679
</td>
<td style="text-align:right;">
13.5
</td>
</tr>
<tr>
<td style="text-align:left;">
Venezuela (Bolivarian Republic of)
</td>
<td style="text-align:right;">
2014
</td>
<td style="text-align:left;">
Developing
</td>
<td style="text-align:right;">
73.9
</td>
<td style="text-align:right;">
158
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
6.47
</td>
<td style="text-align:right;">
0.0000
</td>
<td style="text-align:right;">
78
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
61.5
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
79
</td>
<td style="text-align:right;">
5.26
</td>
<td style="text-align:right;">
78
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
0.771
</td>
<td style="text-align:right;">
14.2
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
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
123
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
9.09
</td>
<td style="text-align:right;">
8.6172
</td>
<td style="text-align:right;">
92
</td>
<td style="text-align:right;">
37
</td>
<td style="text-align:right;">
59.4
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
93
</td>
<td style="text-align:right;">
1.37
</td>
<td style="text-align:right;">
93
</td>
<td style="text-align:right;">
0.1
</td>
<td style="text-align:right;">
62.17
</td>
<td style="text-align:right;">
713576
</td>
<td style="text-align:right;">
2.1
</td>
<td style="text-align:right;">
2.1
</td>
<td style="text-align:right;">
0.771
</td>
<td style="text-align:right;">
14.3
</td>
</tr>
<tr>
<td style="text-align:left;">
India
</td>
<td style="text-align:right;">
2009
</td>
<td style="text-align:left;">
Developing
</td>
<td style="text-align:right;">
66.0
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1300
</td>
<td style="text-align:right;">
2.50
</td>
<td style="text-align:right;">
0.8442
</td>
<td style="text-align:right;">
37
</td>
<td style="text-align:right;">
56188
</td>
<td style="text-align:right;">
15.4
</td>
<td style="text-align:right;">
1700
</td>
<td style="text-align:right;">
73
</td>
<td style="text-align:right;">
4.38
</td>
<td style="text-align:right;">
74
</td>
<td style="text-align:right;">
0.2
</td>
<td style="text-align:right;">
19.32
</td>
<td style="text-align:right;">
121427132
</td>
<td style="text-align:right;">
27.0
</td>
<td style="text-align:right;">
27.8
</td>
<td style="text-align:right;">
0.563
</td>
<td style="text-align:right;">
10.5
</td>
</tr>
<tr>
<td style="text-align:left;">
Comoros
</td>
<td style="text-align:right;">
2015
</td>
<td style="text-align:left;">
Developing
</td>
<td style="text-align:right;">
63.5
</td>
<td style="text-align:right;">
227
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
0.0000
</td>
<td style="text-align:right;">
91
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
24.2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
92
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
91
</td>
<td style="text-align:right;">
0.8
</td>
<td style="text-align:right;">
727.65
</td>
<td style="text-align:right;">
777424
</td>
<td style="text-align:right;">
6.7
</td>
<td style="text-align:right;">
6.5
</td>
<td style="text-align:right;">
0.498
</td>
<td style="text-align:right;">
11.1
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
-   No duplicated records
-   ‘Life.expectancy’ is the dependent variable - There are 10 missing
    observations in the dependent variable
-   We need to predict Salary however there is no salary variable in the
    dataset but MonthlyIncome variable seems to be sufficient for this
    purpose.

### Scatterplots

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

#adding region column to do regional imputation

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
US_GDP <- read.csv('https://raw.githubusercontent.com/boneeyah/LifeExpectancy/main/Data%20Files/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_4150786.csv',header = FALSE,encoding = "UTF-8")
colnames(US_GDP) <- US_GDP[3,]
US_GDP <- rename(US_GDP,c("Country Name"="Country"))
US_GDP <- US_GDP[US_GDP$Country == "United States",c(1,45:60)]
US_GDP <- US_GDP %>% pivot_longer(!Country,names_to = "Year",values_to = "GDP2")
US_GDP$Country <- replace(US_GDP$Country, US_GDP$Country == "United States", "United States of America")
US_GDP$Year <- as.integer(US_GDP$Year)

LifeExp <- left_join(LifeExp,US_GDP, by=c("Country","Year"))
LifeExp <- LifeExp %>% dplyr::mutate(GDP = ifelse(LifeExp$Country == "United States of America", LifeExp$GDP2, LifeExp$GDP))

#US Schooling
US_Scho <- read.csv("https://raw.githubusercontent.com/boneeyah/LifeExpectancy/main/Data%20Files/Expected%20years%20of%20schooling%20(years).csv",skip = 6,header = FALSE)
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
US_Inc <- read.csv('https://raw.githubusercontent.com/boneeyah/LifeExpectancy/main/Data%20Files/Income%20index.csv',skip = 5, header = FALSE)
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
pop_all <- read.csv('https://raw.githubusercontent.com/boneeyah/LifeExpectancy/main/Data%20Files/WPP2019_TotalPopulationBySex.csv',encoding = "UTF-8")
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
adlt_mort <- read.csv('https://raw.githubusercontent.com/boneeyah/LifeExpectancy/main/Data%20Files/Adult_mort.csv',header = TRUE,encoding = "UTF-8")
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
```

    ## Warning: package 'countrycode' was built under R version 4.1.3

``` r
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
#num_cols = LifeExp %>% dplyr::select(where(is.numeric)) %>% colnames()
#LifeExpcorr = LifeExp[,num_cols]
#corrplot(cor(LifeExpcorr), method = 'square', order = 'AOE', addCoef.col = 'black', 
#         cl.pos = 'n', col = COL2('BrBG'))

plot_correlate(LifeExp)
```

    ## Warning: 'plot_correlate' is deprecated.
    ## Use 'plot.correlate' instead.
    ## See help("Deprecated")

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-9-1.png" angle=90 style="display: block; margin: auto;" />

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

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-11-1.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-11-2.png" angle=90 style="display: block; margin: auto;" />

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
## Overall life expectancy over time

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
  ggtitle("Mean life expactancy by year by continent") +
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
drop_for_knn = c("log.HIV.AIDS", "log.GDP", "log.percentage.expenditure", "Region" ,"Continent")
ktrain = train[,!colnames(train) %in% drop_for_knn]
ktest = test[,!colnames(train) %in% drop_for_knn]
```

``` r
#####################################################################################
#                             EDA on Train sets                                    #
#####################################################################################

describe(train)
```

    ## # A tibble: 23 x 26
    ##    described_variabl~     n    na   mean     sd se_mean    IQR skewness kurtosis
    ##    <chr>              <int> <int>  <dbl>  <dbl>   <dbl>  <dbl>    <dbl>    <dbl>
    ##  1 Year                2489     0 2.01e3 4.59e0 9.20e-2   7    -0.00819   -1.20 
    ##  2 Life.expectancy     2489     0 6.91e1 9.61e0 1.93e-1  12.7  -0.629     -0.253
    ##  3 Adult.Mortality     2489     0 1.96e2 1.16e2 2.33e+0 146.    1.33       1.85 
    ##  4 infant.deaths       2489     0 3.18e1 1.22e2 2.44e+0  23     9.33     105.   
    ##  5 Alcohol             2489     0 4.56e0 4.04e0 8.09e-2   6.71  0.621     -0.748
    ##  6 percentage.expend~  2489     0 7.16e2 1.96e3 3.92e+1 419.    4.80      28.7  
    ##  7 Hepatitis.B         2489     0 8.02e1 2.45e1 4.91e-1  21    -1.82       2.44 
    ##  8 Measles             2489     0 2.54e3 1.18e4 2.37e+2 410     9.27     111.   
    ##  9 BMI                 2489     0 3.80e1 2.00e1 4.01e-1  36.7  -0.227     -1.32 
    ## 10 under.five.deaths   2489     0 4.41e1 1.66e2 3.33e+0  30     9.09     101.   
    ## # ... with 13 more rows, and 17 more variables: p00 <dbl>, p01 <dbl>,
    ## #   p05 <dbl>, p10 <dbl>, p20 <dbl>, p25 <dbl>, p30 <dbl>, p40 <dbl>,
    ## #   p50 <dbl>, p60 <dbl>, p70 <dbl>, p75 <dbl>, p80 <dbl>, p90 <dbl>,
    ## #   p95 <dbl>, p99 <dbl>, p100 <dbl>

## Observations

\*\* A High degree of skewness can be identified in Infant Deaths \*\* A
High degree of skewness can be identified in Measles \*\* A High degree
of skewness can be identified in under.five.deaths \*\* A High degree of
skewness can be identified in Population

``` r
normality(train) 
```

    ## # A tibble: 23 x 4
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
    ## # ... with 13 more rows

``` r
#Runs a Shapario-Wilk Tests, if the p-value is >= .05 then the data is normally distrusted, if <0.05 the data is not normally distrusted.

#Find Features that are not normally distributed 

train %>%
  normality() %>%
  filter(p_value < 0.05) %>%
  arrange(abs(p_value))
```

    ## # A tibble: 23 x 4
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
    ## # ... with 13 more rows

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
eval_train_df = data.frame()
eval_train_df = eval_train_df %>% dplyr::mutate(ID = row_number())

eval_test_df = data.frame()
eval_test_df = eval_test_df %>% dplyr::mutate(ID = row_number())

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
    ## (Intercept)                     26.701702526521 24.042250034403    1.11
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
mse_trn = mean(sm$residuals^2)

## Train scores
rmse_trn = train_score[1]
rsqd_trn = train_score[2]
mse_trndf = mse_trn
adjrsqd_trn = sm$adj.r.squared


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
    ## (Intercept)                     -20.443371869108 73.846776922150
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
### test_ASE, R-squared/Adjusted R-squared, RMSE
eval_test_df = data.frame(model_name = 'LASSO', MSE=format(round(mse,4),nsmall=4), R_Squared=format(round(rsqd,4),nsmall=4), AdjR_Squared=format(round(adjrsqd,4),nsmall=4), RMSE = format(round(rmse,4),nsmall=4))

### train_ASE, R-squared/Adjusted R-squared, RMSE
eval_train_df = data.frame(model_name = 'LASSO', MSE=format(round(mse_trndf,4),nsmall=4), R_Squared=format(round(rsqd_trn,4),nsmall=4), AdjR_Squared=format(round(adjrsqd_trn,4),nsmall=4), RMSE = format(round(rmse_trn,4),nsmall=4))


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
mse_trn = mean(sm$residuals^2)

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

## Train scores
rmse_trn = train_score[1]
rsqd_trn = train_score[2]
mse_trndf = mse_trn
adjrsqd_trn = sm$adj.r.squared

eval_test_df = rbind(eval_test_df, c('FWD Selection', format(round(mse,4),nsmall=4), format(round(rsqd,4),nsmall=4), format(round(adjrsqd,4),nsmall=4), format(round(rmse,4),nsmall=4)))

eval_train_df = rbind(eval_train_df, c('FWD Selection', format(round(mse_trndf,4),nsmall=4), format(round(rsqd_trn,4),nsmall=4), format(round(adjrsqd_trn,4),nsmall=4), format(round(rmse_trn,4),nsmall=4)))
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
mse_trn = mean(sm$residuals^2)

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

## Train scores
rmse_trn = train_score[1]
rsqd_trn = train_score[2]
mse_trndf = mse_trn
adjrsqd_trn = sm$adj.r.squared




eval_test_df = rbind(eval_test_df, c('Backward Elim.', format(round(mse,4),nsmall=4), format(round(rsqd,4),nsmall=4), format(round(adjrsqd,4),nsmall=4), format(round(rmse,4),nsmall=4)))

eval_train_df = rbind(eval_train_df, c('Backward Elim.', format(round(mse_trndf,4),nsmall=4), format(round(rsqd_trn,4),nsmall=4), format(round(adjrsqd_trn,4),nsmall=4), format(round(rmse_trn,4),nsmall=4)))


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
ridge.pred_trn=predict(ridge.mod ,s=bestlambda ,newx=x)
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
train_score = postResample(pred = ridge.pred_trn, obs = rtrain$Life.expectancy)


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

## Train scores
rmse_trn = train_score[1]
rsqd_trn = train_score[2]
mse_trndf = rmse_trn^2
n=dim(x)[1]
p = length(coef(ridge.mod,s=bestlambda))-1
adjrsqd_trn = 1 - (1 - rsqd_trn) * ((n - 1)/(n-p-1))

eval_test_df = rbind(eval_test_df, c('Ridge', format(round(mse,4),nsmall=4), format(round(rsqd,4),nsmall=4), format(round(adjrsqd,4),nsmall=4), format(round(rmse,4),nsmall=4)))
eval_train_df = rbind(eval_train_df, c('Ridge', format(round(mse_trndf,4),nsmall=4), format(round(rsqd_trn,4),nsmall=4), format(round(adjrsqd_trn,4),nsmall=4), format(round(rmse_trn,4),nsmall=4)))


#####################################################################################
#                            Elastic Net Regression                                 #
#####################################################################################

library(glmnetUtils)
```

    ## Warning: package 'glmnetUtils' was built under R version 4.1.3

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
elastic.pred_trn=predict(elastic.mod ,s=cva.min$lambdaMin ,newx=x)
elastic.pred=predict(elastic.mod ,s=cva.min$lambdaMin ,newx=xtest)
elastic.pred_coef=predict(elastic.mod ,s=cva.min$lambdaMin ,newx=xtest, type = "coef")

testMSE_ELASTIC<-mean((ytest-elastic.pred)^2)
testMSE_ELASTIC
```

    ## [1] 6.109

``` r
# Metrics RMSE; R-squared; MAE
train_score = postResample(pred = elastic.pred_trn, obs = rtrain$Life.expectancy)
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

## Train scores
rmse_trn = train_score[1]
rsqd_trn = train_score[2]
mse_trndf = rmse_trn^2
n=dim(x)[1]
p = length(coef(elastic.mod,s=bestlambda))-1
adjrsqd_trn = 1 - (1 - rsqd_trn) * ((n - 1)/(n-p-1))


eval_test_df = rbind(eval_test_df, c('ElasticNet', format(round(mse,4),nsmall=4), format(round(rsqd,4),nsmall=4), format(round(adjrsqd,4),nsmall=4), format(round(rmse,4),nsmall=4)))

eval_train_df = rbind(eval_train_df, c('ElasticNet', format(round(mse_trndf,4),nsmall=4), format(round(rsqd_trn,4),nsmall=4), format(round(adjrsqd_trn,4),nsmall=4), format(round(rmse_trn,4),nsmall=4)))

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
sm=summary(MLRT$finalModel)
mse_trn = mean(sm$residuals^2)
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

## Train scores
rmse_trn = train_score[1]
rsqd_trn = train_score[2]
mse_trndf = mse_trn
adjrsqd_trn = sm$adj.r.squared


eval_test_df = rbind(eval_test_df, c('MLR - Tamas', format(round(mse,4),nsmall=4), format(round(rsqd,4),nsmall=4), format(round(adjrsqd,4),nsmall=4), format(round(rmse,4),nsmall=4)))

eval_train_df = rbind(eval_train_df, c('MLR - Tamas', format(round(mse_trndf,4),nsmall=4), format(round(rsqd_trn,4),nsmall=4), format(round(adjrsqd_trn,4),nsmall=4), format(round(rmse_trn,4),nsmall=4)))
```

``` r
#####################################################################################
#                                      Objective 2                                  #
#                   Model with complexity (adding interaction terms)                #
#####################################################################################
#####################################################################################
#                        Tamas's  Manual MLR Interaction                            #
#####################################################################################
fit_interaction = lm(Life.expectancy ~ Continent + Status + Schooling + log.percentage.expenditure + Year + Adult.Mortality + infant.deaths + Income.composition.of.resources:Status + Schooling:Status + I(GDP^2) ,data = train)

interact_pred_train = predict(fit_interaction, train)
train_score = postResample(pred = interact_pred_train, obs = train$Life.expectancy)

interact_pred = predict(fit_interaction, test)
test_score = postResample(pred = interact_pred, obs = test$Life.expectancy)

summary(fit_interaction)
```

    ## 
    ## Call:
    ## lm(formula = Life.expectancy ~ Continent + Status + Schooling + 
    ##     log.percentage.expenditure + Year + Adult.Mortality + infant.deaths + 
    ##     Income.composition.of.resources:Status + Schooling:Status + 
    ##     I(GDP^2), data = train)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -11.921  -1.449  -0.264   1.123  10.858 
    ## 
    ## Coefficients:
    ##                                                          Estimate
    ## (Intercept)                                      10.7033082346211
    ## ContinentAmericas                                 3.2826482894549
    ## ContinentAsia                                     1.3433954944256
    ## ContinentEurope                                   2.8742205130719
    ## ContinentOceania                                  1.2158854223812
    ## StatusDeveloping                                  7.0484793649483
    ## Schooling                                         0.1659690005870
    ## log.percentage.expenditure                        0.1003444071425
    ## Year                                              0.0262331580264
    ## Adult.Mortality                                  -0.0557935517321
    ## infant.deaths                                    -0.0028297012772
    ## I(GDP^2)                                         -0.0000000000292
    ## StatusDeveloped:Income.composition.of.resources  17.9984512984908
    ## StatusDeveloping:Income.composition.of.resources  2.7437970019810
    ## StatusDeveloping:Schooling                        0.3277235556393
    ##                                                        Std. Error t value
    ## (Intercept)                                      24.3158065637234    0.44
    ## ContinentAmericas                                 0.1937051489887   16.95
    ## ContinentAsia                                     0.1799249993177    7.47
    ## ContinentEurope                                   0.2361877077023   12.17
    ## ContinentOceania                                  0.2684700565321    4.53
    ## StatusDeveloping                                  2.2570156972723    3.12
    ## Schooling                                         0.1026710494270    1.62
    ## log.percentage.expenditure                        0.0215399783688    4.66
    ## Year                                              0.0121336829563    2.16
    ## Adult.Mortality                                   0.0007051036737  -79.13
    ## infant.deaths                                     0.0004375361514   -6.47
    ## I(GDP^2)                                          0.0000000000772   -0.38
    ## StatusDeveloped:Income.composition.of.resources   3.4357348691848    5.24
    ## StatusDeveloping:Income.composition.of.resources  0.4530203089306    6.06
    ## StatusDeveloping:Schooling                        0.1063609578972    3.08
    ##                                                              Pr(>|t|)    
    ## (Intercept)                                                    0.6598    
    ## ContinentAmericas                                < 0.0000000000000002 ***
    ## ContinentAsia                                        0.00000000000011 ***
    ## ContinentEurope                                  < 0.0000000000000002 ***
    ## ContinentOceania                                     0.00000620910741 ***
    ## StatusDeveloping                                               0.0018 ** 
    ## Schooling                                                      0.1061    
    ## log.percentage.expenditure                           0.00000335360281 ***
    ## Year                                                           0.0307 *  
    ## Adult.Mortality                                  < 0.0000000000000002 ***
    ## infant.deaths                                        0.00000000011978 ***
    ## I(GDP^2)                                                       0.7057    
    ## StatusDeveloped:Income.composition.of.resources      0.00000017544447 ***
    ## StatusDeveloping:Income.composition.of.resources     0.00000000160155 ***
    ## StatusDeveloping:Schooling                                     0.0021 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.56 on 2474 degrees of freedom
    ## Multiple R-squared:  0.929,  Adjusted R-squared:  0.929 
    ## F-statistic: 2.33e+03 on 14 and 2474 DF,  p-value: <0.0000000000000002

``` r
confint(fit_interaction)
```

    ##                                                              2.5 %
    ## (Intercept)                                      -36.9781240637946
    ## ContinentAmericas                                  2.9028073443004
    ## ContinentAsia                                      0.9905763662064
    ## ContinentEurope                                    2.4110745275578
    ## ContinentOceania                                   0.6894362260060
    ## StatusDeveloping                                   2.6226446354310
    ## Schooling                                         -0.0353610552116
    ## log.percentage.expenditure                         0.0581061611025
    ## Year                                               0.0024399360787
    ## Adult.Mortality                                   -0.0571762059736
    ## infant.deaths                                     -0.0036876761227
    ## I(GDP^2)                                          -0.0000000001805
    ## StatusDeveloped:Income.composition.of.resources   11.2612386491376
    ## StatusDeveloping:Income.composition.of.resources   1.8554589107285
    ## StatusDeveloping:Schooling                         0.1191578722481
    ##                                                            97.5 %
    ## (Intercept)                                      58.3847405330369
    ## ContinentAmericas                                 3.6624892346094
    ## ContinentAsia                                     1.6962146226448
    ## ContinentEurope                                   3.3373664985860
    ## ContinentOceania                                  1.7423346187564
    ## StatusDeveloping                                 11.4743140944656
    ## Schooling                                         0.3672990563856
    ## log.percentage.expenditure                        0.1425826531826
    ## Year                                              0.0500263799741
    ## Adult.Mortality                                  -0.0544108974906
    ## infant.deaths                                    -0.0019717264317
    ## I(GDP^2)                                          0.0000000001222
    ## StatusDeveloped:Income.composition.of.resources  24.7356639478440
    ## StatusDeveloping:Income.composition.of.resources  3.6321350932335
    ## StatusDeveloping:Schooling                        0.5362892390305

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

## Train scores
sm = summary(fit_interaction)
mse_trn = mean(sm$residuals^2)
rmse_trn = train_score[1]
rsqd_trn = train_score[2]
mse_trndf = mse_trn
adjrsqd_trn = sm$adj.r.squared

eval_test_df = rbind(eval_test_df, c('MLR Interact - Tamas', format(round(mse,4),nsmall=4), format(round(rsqd,4),nsmall=4), format(round(adjrsqd,4),nsmall=4), format(round(rmse,4),nsmall=4)))

eval_train_df = rbind(eval_train_df, c('MLR Interact - Tamas', format(round(mse_trndf,4),nsmall=4), format(round(rsqd_trn,4),nsmall=4), format(round(adjrsqd_trn,4),nsmall=4), format(round(rmse_trn,4),nsmall=4)))
```

``` r
#####################################################################################
#                        Reuven's  Manual MLR Interaction                           #
#####################################################################################

# 5-fold cross validation
cv <- trainControl(
  method = "cv", 
  number = 5,
  savePredictions = TRUE
)
MLRT = train(
  Life.expectancy ~ Income.composition.of.resources + Schooling:log.percentage.expenditure  +  log.HIV.AIDS + log.GDP + BMI + Year + Adult.Mortality,
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
    ## -13.062  -1.771  -0.243   1.463  16.422 
    ## 
    ## Coefficients:
    ##                                         Estimate Std. Error t value
    ## (Intercept)                            72.485657  26.887099    2.70
    ## Income.composition.of.resources         8.086783   0.410264   19.71
    ## log.HIV.AIDS                           -0.193307   0.074332   -2.60
    ## log.GDP                                 0.299205   0.046630    6.42
    ## BMI                                     0.030037   0.003537    8.49
    ## Year                                   -0.000564   0.013407   -0.04
    ## Adult.Mortality                        -0.058634   0.001135  -51.65
    ## `Schooling:log.percentage.expenditure`  0.012255   0.001843    6.65
    ##                                                    Pr(>|t|)    
    ## (Intercept)                                          0.0071 ** 
    ## Income.composition.of.resources        < 0.0000000000000002 ***
    ## log.HIV.AIDS                                         0.0094 ** 
    ## log.GDP                                      0.000000000166 ***
    ## BMI                                    < 0.0000000000000002 ***
    ## Year                                                 0.9664    
    ## Adult.Mortality                        < 0.0000000000000002 ***
    ## `Schooling:log.percentage.expenditure`       0.000000000036 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.91 on 2481 degrees of freedom
    ## Multiple R-squared:  0.909,  Adjusted R-squared:  0.908 
    ## F-statistic: 3.52e+03 on 7 and 2481 DF,  p-value: <0.0000000000000002

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
    ## -13.062  -1.771  -0.243   1.463  16.422 
    ## 
    ## Coefficients:
    ##                                         Estimate Std. Error t value
    ## (Intercept)                            72.485657  26.887099    2.70
    ## Income.composition.of.resources         8.086783   0.410264   19.71
    ## log.HIV.AIDS                           -0.193307   0.074332   -2.60
    ## log.GDP                                 0.299205   0.046630    6.42
    ## BMI                                     0.030037   0.003537    8.49
    ## Year                                   -0.000564   0.013407   -0.04
    ## Adult.Mortality                        -0.058634   0.001135  -51.65
    ## `Schooling:log.percentage.expenditure`  0.012255   0.001843    6.65
    ##                                                    Pr(>|t|)    
    ## (Intercept)                                          0.0071 ** 
    ## Income.composition.of.resources        < 0.0000000000000002 ***
    ## log.HIV.AIDS                                         0.0094 ** 
    ## log.GDP                                      0.000000000166 ***
    ## BMI                                    < 0.0000000000000002 ***
    ## Year                                                 0.9664    
    ## Adult.Mortality                        < 0.0000000000000002 ***
    ## `Schooling:log.percentage.expenditure`       0.000000000036 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.91 on 2481 degrees of freedom
    ## Multiple R-squared:  0.909,  Adjusted R-squared:  0.908 
    ## F-statistic: 3.52e+03 on 7 and 2481 DF,  p-value: <0.0000000000000002

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
plot(test_pred ~ ytest, main = "Original vs Predicted scatter plot (Custom MLR Reuven)", xlab = 'Original observations', ylab='Predicted values')
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


## Train scores
sm = summary(MLRT$finalModel)
mse_trn = mean(sm$residuals^2)
rmse_trn = train_score[1]
rsqd_trn = train_score[2]
mse_trndf = mse_trn
adjrsqd_trn = sm$adj.r.squared

eval_test_df = rbind(eval_test_df, c('MLR Interact - Reuven', format(round(mse,4),nsmall=4), format(round(rsqd,4),nsmall=4), format(round(adjrsqd,4),nsmall=4), format(round(rmse,4),nsmall=4)))

eval_train_df = rbind(eval_train_df, c('MLR Interact - Reuven', format(round(mse_trndf,4),nsmall=4), format(round(rsqd_trn,4),nsmall=4), format(round(adjrsqd_trn,4),nsmall=4), format(round(rmse_trn,4),nsmall=4)))
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
test_custom <- LifeExp2[-index,]
```

``` r
#retrain/fit/test/predict and measure accuracy
fit.custom <- lm(Life.expectancy ~log.Adult.Mortality + infant.deaths + log.GDP + Measles + HIV_cat + Country + Country:infant.deaths +Country:log.Adult.Mortality + log.Adult.Mortality:infant.deaths:Country,data = train_custom)
#model has interactions between HIV and Measles variables and their respective
#binary categories to have a sort of conditional
#interaction between Country:infant.deaths
#check model performance on the test set and test set prediction
train_custom_pred <- predict(fit.custom,train_custom)
```

    ## Warning in predict.lm(fit.custom, train_custom): prediction from a rank-
    ## deficient fit may be misleading

``` r
test_custom_pred <- predict(fit.custom,test_custom) #rank deficient warning comes from the self-interactions (ex. HIV_cat:HIV)
```

    ## Warning in predict.lm(fit.custom, test_custom): prediction from a rank-deficient
    ## fit may be misleading

``` r
test_score = caret::postResample(pred = test_custom_pred,obs = test_custom$Life.expectancy)
train_score = caret::postResample(pred = train_custom_pred,obs = train_custom$Life.expectancy)
forecast::accuracy(test_custom_pred,test_custom$Life.expectancy)
```

    ## Registered S3 method overwritten by 'quantmod':
    ##   method            from
    ##   as.zoo.data.frame zoo

    ##               ME  RMSE    MAE     MPE MAPE
    ## Test set -0.1458 1.673 0.9427 -0.2657 1.38

``` r
##fits
residuals = resid(fit.custom)
summary(fit.custom)
```

    ## 
    ## Call:
    ## lm(formula = Life.expectancy ~ log.Adult.Mortality + infant.deaths + 
    ##     log.GDP + Measles + HIV_cat + Country + Country:infant.deaths + 
    ##     Country:log.Adult.Mortality + log.Adult.Mortality:infant.deaths:Country, 
    ##     data = train_custom)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -5.331 -0.367 -0.025  0.181  6.856 
    ## 
    ## Coefficients: (136 not defined because of singularities)
    ##                                                                                                     Estimate
    ## (Intercept)                                                                                     631.64584860
    ## log.Adult.Mortality                                                                            -103.58440281
    ## infant.deaths                                                                                    -5.35931093
    ## log.GDP                                                                                           0.00053354
    ## Measles                                                                                          -0.00000492
    ## HIV_cat                                                                                          -0.46587226
    ## CountryAlbania                                                                                 -521.44196764
    ## CountryAlgeria                                                                                 -498.20801497
    ## CountryAngola                                                                                  -286.43132858
    ## CountryAntigua and Barbuda                                                                     -501.43802090
    ## CountryArgentina                                                                               -474.64680260
    ## CountryArmenia                                                                                 -499.13724563
    ## CountryAustralia                                                                               -514.13948776
    ## CountryAustria                                                                                 -464.26294260
    ## CountryAzerbaijan                                                                              -550.94622673
    ## CountryBahamas                                                                                 -507.50608451
    ## CountryBahrain                                                                                 -538.98660256
    ## CountryBangladesh                                                                              -720.86450289
    ## CountryBarbados                                                                                -526.95404472
    ## CountryBelarus                                                                                 -552.08434686
    ## CountryBelgium                                                                                 -407.62406097
    ## CountryBelize                                                                                  -474.68081292
    ## CountryBenin                                                                                   -287.59980316
    ## CountryBhutan                                                                                  -437.94098839
    ## CountryBolivia                                                                                 -174.75686798
    ## CountryBosnia and Herzegovina                                                                  -513.12381551
    ## CountryBotswana                                                                                -429.69049504
    ## CountryBrazil                                                                                  -276.96947773
    ## CountryBrunei Darussalam                                                                       -495.00074768
    ## CountryBulgaria                                                                                -471.14665494
    ## CountryBurkina Faso                                                                           -1431.73834543
    ## CountryBurundi                                                                                  479.49312878
    ## CountryCabo Verde                                                                              -535.21362874
    ## CountryCambodia                                                                                -555.59804010
    ## CountryCameroon                                                                                -424.53308604
    ## CountryCanada                                                                                  -452.83731961
    ## CountryCentral African Republic                                                                 -77.79191146
    ## CountryChad                                                                                   -1361.70760852
    ## CountryChile                                                                                   -387.58269018
    ## CountryChina                                                                                   -575.42845877
    ## CountryColombia                                                                                -466.25544007
    ## CountryComoros                                                                                 -421.70045617
    ## CountryCongo                                                                                   -359.88682255
    ## CountryCosta Rica                                                                              -473.19250825
    ## CountryCroatia                                                                                 -508.20384356
    ## CountryCuba                                                                                    -511.47008726
    ## CountryCyprus                                                                                  -494.05213736
    ## CountryCzechia                                                                                 -508.59616797
    ## CountryDemocratic People's Republic of Korea                                                   -422.28774970
    ## CountryDemocratic Republic of the Congo                                                         319.75988347
    ## CountryDenmark                                                                                 -472.93145036
    ## CountryDjibouti                                                                               -1220.92750924
    ## CountryDominican Republic                                                                     -1629.00632165
    ## CountryEcuador                                                                                 -578.84763092
    ## CountryEgypt                                                                                   1671.23615169
    ## CountryEl Salvador                                                                             -776.39294090
    ## CountryEquatorial Guinea                                                                       -402.60010608
    ## CountryEritrea                                                                                -1076.47603071
    ## CountryEstonia                                                                                 -533.74815847
    ## CountryEthiopia                                                                                -523.88772101
    ## CountryFiji                                                                                    -498.24491146
    ## CountryFinland                                                                                 -450.08853381
    ## CountryFrance                                                                                  -493.17163233
    ## CountryGabon                                                                                   -466.66579237
    ## CountryGambia                                                                                  -173.01890835
    ## CountryGeorgia                                                                                 -629.92724447
    ## CountryGermany                                                                                  -12.38579990
    ## CountryGhana                                                                                  -2856.08752486
    ## CountryGreece                                                                                  -379.45241795
    ## CountryGrenada                                                                                 -587.21493280
    ## CountryGuatemala                                                                              -1364.97988460
    ## CountryGuinea                                                                                  -596.34477128
    ## CountryGuinea-Bissau                                                                           1491.69690686
    ## CountryGuyana                                                                                  -552.33934117
    ## CountryHaiti                                                                                   -844.76066545
    ## CountryHonduras                                                                                -465.01372147
    ## CountryHungary                                                                                 -514.77837533
    ## CountryIceland                                                                                 -539.56560224
    ## CountryIndia                                                                                   -454.68751821
    ## CountryIndonesia                                                                               -769.69442368
    ## CountryIran                                                                                    -448.78258380
    ## CountryIraq                                                                                   -2381.08824896
    ## CountryIreland                                                                                 -467.20953645
    ## CountryIsrael                                                                                  -515.16131102
    ## CountryItaly                                                                                   -534.62326626
    ## CountryIvory Coast                                                                              639.92150105
    ## CountryJamaica                                                                                 -497.14010113
    ## CountryJapan                                                                                   -547.14113973
    ## CountryJordan                                                                                  -472.69606064
    ## CountryKazakhstan                                                                              -524.21536262
    ## CountryKenya                                                                                   -603.47220306
    ## CountryKiribati                                                                                -460.26423509
    ## CountryKuwait                                                                                  -510.66829599
    ## CountryKyrgyzstan                                                                              -939.27994638
    ## CountryLao People's Democratic Republic                                                        -536.70798715
    ## CountryLatvia                                                                                  -538.05162275
    ## CountryLebanon                                                                                 -505.92823198
    ## CountryLesotho                                                                                 -219.97947881
    ## CountryLiberia                                                                                 -467.54467799
    ## CountryLibya                                                                                   1083.68168977
    ## CountryLithuania                                                                               -570.49374027
    ## CountryLuxembourg                                                                              -507.40259907
    ## CountryMadagascar                                                                             -1072.80951817
    ## CountryMalawi                                                                                  -555.11666098
    ## CountryMalaysia                                                                                -497.11541116
    ## CountryMaldives                                                                                -533.43600573
    ## CountryMali                                                                                     889.58841088
    ## CountryMalta                                                                                   -470.40697177
    ## CountryMauritania                                                                             10928.43698108
    ## CountryMauritius                                                                               -460.89735909
    ## CountryMexico                                                                                  -391.85288666
    ## CountryMicronesia (Federated States of)                                                        -467.27816596
    ## CountryMongolia                                                                                -580.41485197
    ## CountryMontenegro                                                                              -509.69997477
    ## CountryMorocco                                                                                 -543.00619317
    ## CountryMozambique                                                                               508.66450482
    ## CountryMyanmar                                                                                 -584.17362179
    ## CountryNamibia                                                                                 -733.45402514
    ## CountryNepal                                                                                   -542.38187882
    ## CountryNetherlands                                                                             -481.20903174
    ## CountryNew Zealand                                                                             -458.86459577
    ## CountryNicaragua                                                                               -322.49134270
    ## CountryNiger                                                                                   6900.37058488
    ## CountryNigeria                                                                                -2382.51591206
    ## CountryNorway                                                                                  -503.12230267
    ## CountryOman                                                                                    -496.71171391
    ## CountryPakistan                                                                               -1172.65834311
    ## CountryPanama                                                                                 -1163.64664519
    ## CountryPapua New Guinea                                                                        -805.50468083
    ## CountryParaguay                                                                                -202.20526049
    ## CountryPeru                                                                                    -579.11453950
    ## CountryPhilippines                                                                             -596.16536632
    ## CountryPoland                                                                                  -517.77253205
    ## CountryPortugal                                                                                -423.64895613
    ## CountryQatar                                                                                   -506.96912838
    ## CountryRepublic of Korea                                                                       -491.47751579
    ## CountryRepublic of Moldova                                                                     -413.23502339
    ## CountryRomania                                                                                 -149.91189448
    ## CountryRussian Federation                                                                      -220.33559049
    ## CountryRwanda                                                                                  -515.17309017
    ## CountrySaint Lucia                                                                             -491.77249645
    ## CountrySaint Vincent and the Grenadines                                                        -702.81146488
    ## CountrySamoa                                                                                   -575.81903190
    ## CountrySao Tome and Principe                                                                   -433.66640474
    ## CountrySaudi Arabia                                                                            -471.21759733
    ## CountrySenegal                                                                                 -571.20075307
    ## CountrySerbia                                                                                  -494.71168302
    ## CountrySeychelles                                                                              -507.56998252
    ## CountrySierra Leone                                                                             496.67821711
    ## CountrySingapore                                                                               -504.70330469
    ## CountrySlovakia                                                                                -508.99282084
    ## CountrySlovenia                                                                                -467.19432979
    ## CountrySolomon Islands                                                                         -526.29426312
    ## CountrySomalia                                                                                -4007.22133074
    ## CountrySouth Africa                                                                             -86.94189187
    ## CountrySouth Sudan                                                                             1164.76822203
    ## CountrySpain                                                                                   -566.33386476
    ## CountrySri Lanka                                                                               -571.92339531
    ## CountrySudan                                                                                  -1214.39190326
    ## CountrySuriname                                                                                -427.17375310
    ## CountrySwaziland                                                                               -541.15418280
    ## CountrySweden                                                                                  -533.13433809
    ## CountrySwitzerland                                                                             -535.67896941
    ## CountrySyrian Arab Republic                                                                    -515.36919548
    ## CountryTajikistan                                                                              -176.77108216
    ## CountryThailand                                                                                -523.39282753
    ## CountryThe former Yugoslav republic of Macedonia                                               -512.48796015
    ## CountryTimor-Leste                                                                             -474.53607049
    ## CountryTogo                                                                                    -245.72888372
    ## CountryTonga                                                                                   -502.98137123
    ## CountryTrinidad and Tobago                                                                     -400.61948127
    ## CountryTunisia                                                                                 -692.06046855
    ## CountryTurkey                                                                                  -269.09157179
    ## CountryTurkmenistan                                                                            -561.88222021
    ## CountryUganda                                                                                  -357.41545527
    ## CountryUkraine                                                                                 -848.82732701
    ## CountryUnited Arab Emirates                                                                    -515.91847959
    ## CountryUnited Kingdom of Great Britain and Northern Ireland                                    -663.41108985
    ## CountryUnited Republic of Tanzania                                                             1070.88128091
    ## CountryUnited States of America                                                               -1150.15825812
    ## CountryUruguay                                                                                 -505.81589374
    ## CountryUzbekistan                                                                              -552.95608989
    ## CountryVanuatu                                                                                 -500.49033548
    ## CountryVenezuela                                                                               -736.40956739
    ## CountryViet Nam                                                                                -644.81261311
    ## CountryYemen                                                                                   1222.59035220
    ## CountryZambia                                                                                  -650.92008927
    ## CountryZimbabwe                                                                                -216.66859144
    ## infant.deaths:CountryAlbania                                                                     68.36848789
    ## infant.deaths:CountryAlgeria                                                                      5.04843002
    ## infant.deaths:CountryAngola                                                                       3.30175497
    ## infant.deaths:CountryAntigua and Barbuda                                                                  NA
    ## infant.deaths:CountryArgentina                                                                    2.50757961
    ## infant.deaths:CountryArmenia                                                                              NA
    ## infant.deaths:CountryAustralia                                                                            NA
    ## infant.deaths:CountryAustria                                                                              NA
    ## infant.deaths:CountryAzerbaijan                                                                  14.04165687
    ## infant.deaths:CountryBahamas                                                                              NA
    ## infant.deaths:CountryBahrain                                                                              NA
    ## infant.deaths:CountryBangladesh                                                                   5.22045323
    ## infant.deaths:CountryBarbados                                                                             NA
    ## infant.deaths:CountryBelarus                                                                    101.83234786
    ## infant.deaths:CountryBelgium                                                                    -93.02253004
    ## infant.deaths:CountryBelize                                                                               NA
    ## infant.deaths:CountryBenin                                                                                NA
    ## infant.deaths:CountryBhutan                                                                       9.99131332
    ## infant.deaths:CountryBolivia                                                                     14.92377138
    ## infant.deaths:CountryBosnia and Herzegovina                                                               NA
    ## infant.deaths:CountryBotswana                                                                             NA
    ## infant.deaths:CountryBrazil                                                                       3.02277147
    ## infant.deaths:CountryBrunei Darussalam                                                                    NA
    ## infant.deaths:CountryBulgaria                                                                     5.26942395
    ## infant.deaths:CountryBurkina Faso                                                                23.19866761
    ## infant.deaths:CountryBurundi                                                                    -29.51960952
    ## infant.deaths:CountryCabo Verde                                                                           NA
    ## infant.deaths:CountryCambodia                                                                     7.00857720
    ## infant.deaths:CountryCameroon                                                                     4.02573411
    ## infant.deaths:CountryCanada                                                                               NA
    ## infant.deaths:CountryCentral African Republic                                                   -17.46813922
    ## infant.deaths:CountryChad                                                                        26.44769983
    ## infant.deaths:CountryChile                                                                                NA
    ## infant.deaths:CountryChina                                                                        5.39642086
    ## infant.deaths:CountryColombia                                                                     1.97999835
    ## infant.deaths:CountryComoros                                                                     -5.67816671
    ## infant.deaths:CountryCongo                                                                      -17.76428014
    ## infant.deaths:CountryCosta Rica                                                                           NA
    ## infant.deaths:CountryCroatia                                                                              NA
    ## infant.deaths:CountryCuba                                                                                 NA
    ## infant.deaths:CountryCyprus                                                                               NA
    ## infant.deaths:CountryCzechia                                                                              NA
    ## infant.deaths:CountryDemocratic People's Republic of Korea                                       -0.15944716
    ## infant.deaths:CountryDemocratic Republic of the Congo                                             2.19744272
    ## infant.deaths:CountryDenmark                                                                              NA
    ## infant.deaths:CountryDjibouti                                                                   395.59875895
    ## infant.deaths:CountryDominican Republic                                                         183.87322448
    ## infant.deaths:CountryEcuador                                                                     13.44161268
    ## infant.deaths:CountryEgypt                                                                      -25.51097872
    ## infant.deaths:CountryEl Salvador                                                                161.91103446
    ## infant.deaths:CountryEquatorial Guinea                                                                    NA
    ## infant.deaths:CountryEritrea                                                                     96.43527293
    ## infant.deaths:CountryEstonia                                                                              NA
    ## infant.deaths:CountryEthiopia                                                                     5.34104535
    ## infant.deaths:CountryFiji                                                                                 NA
    ## infant.deaths:CountryFinland                                                                              NA
    ## infant.deaths:CountryFrance                                                                       8.24664241
    ## infant.deaths:CountryGabon                                                                                NA
    ## infant.deaths:CountryGambia                                                                               NA
    ## infant.deaths:CountryGeorgia                                                                     51.88541133
    ## infant.deaths:CountryGermany                                                                   -156.68175295
    ## infant.deaths:CountryGhana                                                                       67.51635127
    ## infant.deaths:CountryGreece                                                                     -85.11921779
    ## infant.deaths:CountryGrenada                                                                              NA
    ## infant.deaths:CountryGuatemala                                                                   94.22427450
    ## infant.deaths:CountryGuinea                                                                       8.45071241
    ## infant.deaths:CountryGuinea-Bissau                                                             -467.50147517
    ## infant.deaths:CountryGuyana                                                                      28.36645257
    ## infant.deaths:CountryHaiti                                                                       22.98509918
    ## infant.deaths:CountryHonduras                                                                     7.52115788
    ## infant.deaths:CountryHungary                                                                     43.76253151
    ## infant.deaths:CountryIceland                                                                              NA
    ## infant.deaths:CountryIndia                                                                        5.39875665
    ## infant.deaths:CountryIndonesia                                                                    6.89233418
    ## infant.deaths:CountryIran                                                                         1.50686246
    ## infant.deaths:CountryIraq                                                                        67.08321186
    ## infant.deaths:CountryIreland                                                                              NA
    ## infant.deaths:CountryIsrael                                                                       5.45821024
    ## infant.deaths:CountryItaly                                                                        2.78016376
    ## infant.deaths:CountryIvory Coast                                                                -12.79877680
    ## infant.deaths:CountryJamaica                                                                              NA
    ## infant.deaths:CountryJapan                                                                       16.83171644
    ## infant.deaths:CountryJordan                                                                               NA
    ## infant.deaths:CountryKazakhstan                                                                  10.88035563
    ## infant.deaths:CountryKenya                                                                        7.12532127
    ## infant.deaths:CountryKiribati                                                                             NA
    ## infant.deaths:CountryKuwait                                                                      25.74108917
    ## infant.deaths:CountryKyrgyzstan                                                                 122.74477681
    ## infant.deaths:CountryLao People's Democratic Republic                                            14.33417320
    ## infant.deaths:CountryLatvia                                                                               NA
    ## infant.deaths:CountryLebanon                                                                              NA
    ## infant.deaths:CountryLesotho                                                                    -43.68191132
    ## infant.deaths:CountryLiberia                                                                      6.61888702
    ## infant.deaths:CountryLibya                                                                     -813.35484872
    ## infant.deaths:CountryLithuania                                                                            NA
    ## infant.deaths:CountryLuxembourg                                                                           NA
    ## infant.deaths:CountryMadagascar                                                                  24.92102113
    ## infant.deaths:CountryMalawi                                                                       7.51170179
    ## infant.deaths:CountryMalaysia                                                                    11.56131107
    ## infant.deaths:CountryMaldives                                                                             NA
    ## infant.deaths:CountryMali                                                                       -17.45373455
    ## infant.deaths:CountryMalta                                                                                NA
    ## infant.deaths:CountryMauritania                                                               -1447.05838507
    ## infant.deaths:CountryMauritius                                                                            NA
    ## infant.deaths:CountryMexico                                                                       2.26979452
    ## infant.deaths:CountryMicronesia (Federated States of)                                                     NA
    ## infant.deaths:CountryMongolia                                                                   119.41592569
    ## infant.deaths:CountryMontenegro                                                                           NA
    ## infant.deaths:CountryMorocco                                                                      4.31010226
    ## infant.deaths:CountryMozambique                                                                 -11.88850068
    ## infant.deaths:CountryMyanmar                                                                      7.25509190
    ## infant.deaths:CountryNamibia                                                                     97.94243743
    ## infant.deaths:CountryNepal                                                                        6.67667560
    ## infant.deaths:CountryNetherlands                                                                          NA
    ## infant.deaths:CountryNew Zealand                                                                          NA
    ## infant.deaths:CountryNicaragua                                                                  -53.10049535
    ## infant.deaths:CountryNiger                                                                     -149.20121828
    ## infant.deaths:CountryNigeria                                                                      9.51539565
    ## infant.deaths:CountryNorway                                                                               NA
    ## infant.deaths:CountryOman                                                                                 NA
    ## infant.deaths:CountryPakistan                                                                     7.31005059
    ## infant.deaths:CountryPanama                                                                     691.93261440
    ## infant.deaths:CountryPapua New Guinea                                                            42.28798059
    ## infant.deaths:CountryParaguay                                                                   -98.51765085
    ## infant.deaths:CountryPeru                                                                         0.42130870
    ## infant.deaths:CountryPhilippines                                                                  6.37336404
    ## infant.deaths:CountryPoland                                                                      12.28214187
    ## infant.deaths:CountryPortugal                                                                   -34.32118985
    ## infant.deaths:CountryQatar                                                                                NA
    ## infant.deaths:CountryRepublic of Korea                                                           16.17235238
    ## infant.deaths:CountryRepublic of Moldova                                                                  NA
    ## infant.deaths:CountryRomania                                                                   -174.95556667
    ## infant.deaths:CountryRussian Federation                                                         -13.83230354
    ## infant.deaths:CountryRwanda                                                                       9.12794287
    ## infant.deaths:CountrySaint Lucia                                                                          NA
    ## infant.deaths:CountrySaint Vincent and the Grenadines                                                     NA
    ## infant.deaths:CountrySamoa                                                                                NA
    ## infant.deaths:CountrySao Tome and Principe                                                                NA
    ## infant.deaths:CountrySaudi Arabia                                                                 0.44798872
    ## infant.deaths:CountrySenegal                                                                      9.36713289
    ## infant.deaths:CountrySerbia                                                                               NA
    ## infant.deaths:CountrySeychelles                                                                           NA
    ## infant.deaths:CountrySierra Leone                                                               -27.55668001
    ## infant.deaths:CountrySingapore                                                                            NA
    ## infant.deaths:CountrySlovakia                                                                             NA
    ## infant.deaths:CountrySlovenia                                                                             NA
    ## infant.deaths:CountrySolomon Islands                                                                      NA
    ## infant.deaths:CountrySomalia                                                                     76.41415632
    ## infant.deaths:CountrySouth Africa                                                                -2.74877157
    ## infant.deaths:CountrySouth Sudan                                                                -57.61787714
    ## infant.deaths:CountrySpain                                                                       49.51001572
    ## infant.deaths:CountrySri Lanka                                                                   21.72670673
    ## infant.deaths:CountrySudan                                                                       15.89092251
    ## infant.deaths:CountrySuriname                                                                             NA
    ## infant.deaths:CountrySwaziland                                                                   57.99424640
    ## infant.deaths:CountrySweden                                                                               NA
    ## infant.deaths:CountrySwitzerland                                                                          NA
    ## infant.deaths:CountrySyrian Arab Republic                                                         6.15586452
    ## infant.deaths:CountryTajikistan                                                                 -23.13338581
    ## infant.deaths:CountryThailand                                                                     8.59064778
    ## infant.deaths:CountryThe former Yugoslav republic of Macedonia                                            NA
    ## infant.deaths:CountryTimor-Leste                                                                  5.64169894
    ## infant.deaths:CountryTogo                                                                        -8.70761324
    ## infant.deaths:CountryTonga                                                                                NA
    ## infant.deaths:CountryTrinidad and Tobago                                                                  NA
    ## infant.deaths:CountryTunisia                                                                     61.52944294
    ## infant.deaths:CountryTurkey                                                                       2.33734840
    ## infant.deaths:CountryTurkmenistan                                                                17.56155229
    ## infant.deaths:CountryUganda                                                                       5.98052630
    ## infant.deaths:CountryUkraine                                                                     89.36706715
    ## infant.deaths:CountryUnited Arab Emirates                                                                 NA
    ## infant.deaths:CountryUnited Kingdom of Great Britain and Northern Ireland                        52.85479190
    ## infant.deaths:CountryUnited Republic of Tanzania                                                -12.62264810
    ## infant.deaths:CountryUnited States of America                                                    29.55390973
    ## infant.deaths:CountryUruguay                                                                      4.08606124
    ## infant.deaths:CountryUzbekistan                                                                   5.32869228
    ## infant.deaths:CountryVanuatu                                                                              NA
    ## infant.deaths:CountryVenezuela                                                                   28.69357310
    ## infant.deaths:CountryViet Nam                                                                    10.74579399
    ## infant.deaths:CountryYemen                                                                      -38.14850173
    ## infant.deaths:CountryZambia                                                                       9.91952369
    ## infant.deaths:CountryZimbabwe                                                                    -3.11396743
    ## log.Adult.Mortality:CountryAlbania                                                               96.42138806
    ## log.Adult.Mortality:CountryAlgeria                                                               90.19203577
    ## log.Adult.Mortality:CountryAngola                                                                50.55214276
    ## log.Adult.Mortality:CountryAntigua and Barbuda                                                   92.46060545
    ## log.Adult.Mortality:CountryArgentina                                                             86.62872366
    ## log.Adult.Mortality:CountryArmenia                                                               92.63845052
    ## log.Adult.Mortality:CountryAustralia                                                             96.36147414
    ## log.Adult.Mortality:CountryAustria                                                               83.89463246
    ## log.Adult.Mortality:CountryAzerbaijan                                                           102.37598421
    ## log.Adult.Mortality:CountryBahamas                                                               93.96925237
    ## log.Adult.Mortality:CountryBahrain                                                               99.69413231
    ## log.Adult.Mortality:CountryBangladesh                                                           138.60990609
    ## log.Adult.Mortality:CountryBarbados                                                              97.19800103
    ## log.Adult.Mortality:CountryBelarus                                                              102.12282763
    ## log.Adult.Mortality:CountryBelgium                                                               71.23901935
    ## log.Adult.Mortality:CountryBelize                                                                86.92358479
    ## log.Adult.Mortality:CountryBenin                                                                 76.40725658
    ## log.Adult.Mortality:CountryBhutan                                                                80.52732303
    ## log.Adult.Mortality:CountryBolivia                                                               27.40894820
    ## log.Adult.Mortality:CountryBosnia and Herzegovina                                                94.47722888
    ## log.Adult.Mortality:CountryBotswana                                                              81.27058488
    ## log.Adult.Mortality:CountryBrazil                                                                47.68552677
    ## log.Adult.Mortality:CountryBrunei Darussalam                                                     90.34018368
    ## log.Adult.Mortality:CountryBulgaria                                                              86.08303124
    ## log.Adult.Mortality:CountryBurkina Faso                                                         262.67710538
    ## log.Adult.Mortality:CountryBurundi                                                              -82.28809261
    ## log.Adult.Mortality:CountryCabo Verde                                                            98.83023535
    ## log.Adult.Mortality:CountryCambodia                                                             102.87312276
    ## log.Adult.Mortality:CountryCameroon                                                              79.88661976
    ## log.Adult.Mortality:CountryCanada                                                                83.50600588
    ## log.Adult.Mortality:CountryCentral African Republic                                              27.00214465
    ## log.Adult.Mortality:CountryChad                                                                 229.79604175
    ## log.Adult.Mortality:CountryChile                                                                 69.73221235
    ## log.Adult.Mortality:CountryChina                                                                108.50877198
    ## log.Adult.Mortality:CountryColombia                                                              85.70588775
    ## log.Adult.Mortality:CountryComoros                                                               76.58295692
    ## log.Adult.Mortality:CountryCongo                                                                 73.47701433
    ## log.Adult.Mortality:CountryCosta Rica                                                            87.37283382
    ## log.Adult.Mortality:CountryCroatia                                                               93.54544692
    ## log.Adult.Mortality:CountryCuba                                                                  95.63536506
    ## log.Adult.Mortality:CountryCyprus                                                                89.79553092
    ## log.Adult.Mortality:CountryCzechia                                                               93.62027597
    ## log.Adult.Mortality:CountryDemocratic People's Republic of Korea                                 76.19442379
    ## log.Adult.Mortality:CountryDemocratic Republic of the Congo                                     -55.80045013
    ## log.Adult.Mortality:CountryDenmark                                                               85.73199986
    ## log.Adult.Mortality:CountryDjibouti                                                             220.82869376
    ## log.Adult.Mortality:CountryDominican Republic                                                   305.44040479
    ## log.Adult.Mortality:CountryEcuador                                                              108.16053723
    ## log.Adult.Mortality:CountryEgypt                                                               -324.09055512
    ## log.Adult.Mortality:CountryEl Salvador                                                          143.52404033
    ## log.Adult.Mortality:CountryEquatorial Guinea                                                     76.66336776
    ## log.Adult.Mortality:CountryEritrea                                                              194.79974322
    ## log.Adult.Mortality:CountryEstonia                                                               99.10692179
    ## log.Adult.Mortality:CountryEthiopia                                                              97.49372438
    ## log.Adult.Mortality:CountryFiji                                                                  91.44451908
    ## log.Adult.Mortality:CountryFinland                                                               81.15636665
    ## log.Adult.Mortality:CountryFrance                                                                89.17151160
    ## log.Adult.Mortality:CountryGabon                                                                 87.43244390
    ## log.Adult.Mortality:CountryGambia                                                                35.84276491
    ## log.Adult.Mortality:CountryGeorgia                                                              118.39568924
    ## log.Adult.Mortality:CountryGermany                                                              -19.32134424
    ## log.Adult.Mortality:CountryGhana                                                                506.10951542
    ## log.Adult.Mortality:CountryGreece                                                                64.01189858
    ## log.Adult.Mortality:CountryGrenada                                                              109.27060747
    ## log.Adult.Mortality:CountryGuatemala                                                            251.50971260
    ## log.Adult.Mortality:CountryGuinea                                                               109.34080242
    ## log.Adult.Mortality:CountryGuinea-Bissau                                                       -256.67035120
    ## log.Adult.Mortality:CountryGuyana                                                               101.27390253
    ## log.Adult.Mortality:CountryHaiti                                                                154.53401578
    ## log.Adult.Mortality:CountryHonduras                                                              85.17059382
    ## log.Adult.Mortality:CountryHungary                                                               95.15993829
    ## log.Adult.Mortality:CountryIceland                                                              101.25026186
    ## log.Adult.Mortality:CountryIndia                                                                 82.36802584
    ## log.Adult.Mortality:CountryIndonesia                                                            144.39533391
    ## log.Adult.Mortality:CountryIran                                                                  80.42791459
    ## log.Adult.Mortality:CountryIraq                                                                 459.89180136
    ## log.Adult.Mortality:CountryIreland                                                               83.98274337
    ## log.Adult.Mortality:CountryIsrael                                                                95.20132148
    ## log.Adult.Mortality:CountryItaly                                                                101.29856909
    ## log.Adult.Mortality:CountryIvory Coast                                                          -93.35923993
    ## log.Adult.Mortality:CountryJamaica                                                               92.75002286
    ## log.Adult.Mortality:CountryJapan                                                                103.10846919
    ## log.Adult.Mortality:CountryJordan                                                                90.12161936
    ## log.Adult.Mortality:CountryKazakhstan                                                            96.80067579
    ## log.Adult.Mortality:CountryKenya                                                                109.56499901
    ## log.Adult.Mortality:CountryKiribati                                                              83.71959067
    ## log.Adult.Mortality:CountryKuwait                                                                93.02453886
    ## log.Adult.Mortality:CountryKyrgyzstan                                                           177.90891653
    ## log.Adult.Mortality:CountryLao People's Democratic Republic                                      97.32595970
    ## log.Adult.Mortality:CountryLatvia                                                                99.72112446
    ## log.Adult.Mortality:CountryLebanon                                                               93.62146352
    ## log.Adult.Mortality:CountryLesotho                                                               46.47648151
    ## log.Adult.Mortality:CountryLiberia                                                               86.41105762
    ## log.Adult.Mortality:CountryLibya                                                               -226.21190029
    ## log.Adult.Mortality:CountryLithuania                                                            105.82159830
    ## log.Adult.Mortality:CountryLuxembourg                                                            93.34764505
    ## log.Adult.Mortality:CountryMadagascar                                                           194.31648629
    ## log.Adult.Mortality:CountryMalawi                                                               100.25206645
    ## log.Adult.Mortality:CountryMalaysia                                                              91.17942082
    ## log.Adult.Mortality:CountryMaldives                                                              98.53799742
    ## log.Adult.Mortality:CountryMali                                                                -152.19872551
    ## log.Adult.Mortality:CountryMalta                                                                 83.96348906
    ## log.Adult.Mortality:CountryMauritania                                                         -2018.99530311
    ## log.Adult.Mortality:CountryMauritius                                                             84.27119082
    ## log.Adult.Mortality:CountryMexico                                                                70.48573099
    ## log.Adult.Mortality:CountryMicronesia (Federated States of)                                      84.98711249
    ## log.Adult.Mortality:CountryMongolia                                                             106.12435492
    ## log.Adult.Mortality:CountryMontenegro                                                            93.66800931
    ## log.Adult.Mortality:CountryMorocco                                                              103.24499332
    ## log.Adult.Mortality:CountryMozambique                                                           -72.57428196
    ## log.Adult.Mortality:CountryMyanmar                                                              107.13190762
    ## log.Adult.Mortality:CountryNamibia                                                              133.32664117
    ## log.Adult.Mortality:CountryNepal                                                                 99.77410193
    ## log.Adult.Mortality:CountryNetherlands                                                           88.53588920
    ## log.Adult.Mortality:CountryNew Zealand                                                           82.37458976
    ## log.Adult.Mortality:CountryNicaragua                                                             56.81147169
    ## log.Adult.Mortality:CountryNiger                                                              -1200.50447213
    ## log.Adult.Mortality:CountryNigeria                                                              402.87964314
    ## log.Adult.Mortality:CountryNorway                                                                92.58091768
    ## log.Adult.Mortality:CountryOman                                                                  92.02631589
    ## log.Adult.Mortality:CountryPakistan                                                             223.03795588
    ## log.Adult.Mortality:CountryPanama                                                               230.76495211
    ## log.Adult.Mortality:CountryPapua New Guinea                                                     142.61923391
    ## log.Adult.Mortality:CountryParaguay                                                              33.18107376
    ## log.Adult.Mortality:CountryPeru                                                                 109.55380339
    ## log.Adult.Mortality:CountryPhilippines                                                          110.34624607
    ## log.Adult.Mortality:CountryPoland                                                                96.12688649
    ## log.Adult.Mortality:CountryPortugal                                                              75.15106434
    ## log.Adult.Mortality:CountryQatar                                                                 92.25290351
    ## log.Adult.Mortality:CountryRepublic of Korea                                                     89.23337404
    ## log.Adult.Mortality:CountryRepublic of Moldova                                                   76.81201438
    ## log.Adult.Mortality:CountryRomania                                                               23.79677970
    ## log.Adult.Mortality:CountryRussian Federation                                                    43.99286988
    ## log.Adult.Mortality:CountryRwanda                                                                93.33028788
    ## log.Adult.Mortality:CountrySaint Lucia                                                           90.52719785
    ## log.Adult.Mortality:CountrySaint Vincent and the Grenadines                                     131.54319193
    ## log.Adult.Mortality:CountrySamoa                                                                107.19076069
    ## log.Adult.Mortality:CountrySao Tome and Principe                                                 78.88441050
    ## log.Adult.Mortality:CountrySaudi Arabia                                                          84.60003887
    ## log.Adult.Mortality:CountrySenegal                                                              106.48262048
    ## log.Adult.Mortality:CountrySerbia                                                                91.55465996
    ## log.Adult.Mortality:CountrySeychelles                                                            93.63494988
    ## log.Adult.Mortality:CountrySierra Leone                                                         -72.32214954
    ## log.Adult.Mortality:CountrySingapore                                                             92.61597992
    ## log.Adult.Mortality:CountrySlovakia                                                              93.73275033
    ## log.Adult.Mortality:CountrySlovenia                                                              85.03828044
    ## log.Adult.Mortality:CountrySolomon Islands                                                       96.37234194
    ## log.Adult.Mortality:CountrySomalia                                                              695.99244706
    ## log.Adult.Mortality:CountrySouth Africa                                                          25.07983497
    ## log.Adult.Mortality:CountrySouth Sudan                                                         -185.59570743
    ## log.Adult.Mortality:CountrySpain                                                                106.71587268
    ## log.Adult.Mortality:CountrySri Lanka                                                            106.44438273
    ## log.Adult.Mortality:CountrySudan                                                                223.42719363
    ## log.Adult.Mortality:CountrySuriname                                                              78.36063478
    ## log.Adult.Mortality:CountrySwaziland                                                             97.67714021
    ## log.Adult.Mortality:CountrySweden                                                                99.63268489
    ## log.Adult.Mortality:CountrySwitzerland                                                          100.24789509
    ## log.Adult.Mortality:CountrySyrian Arab Republic                                                  95.00845418
    ## log.Adult.Mortality:CountryTajikistan                                                            27.00988978
    ## log.Adult.Mortality:CountryThailand                                                              96.54418829
    ## log.Adult.Mortality:CountryThe former Yugoslav republic of Macedonia                             94.04771092
    ## log.Adult.Mortality:CountryTimor-Leste                                                           85.24547470
    ## log.Adult.Mortality:CountryTogo                                                                  45.76996732
    ## log.Adult.Mortality:CountryTonga                                                                 92.32352761
    ## log.Adult.Mortality:CountryTrinidad and Tobago                                                   73.12026351
    ## log.Adult.Mortality:CountryTunisia                                                              132.69979477
    ## log.Adult.Mortality:CountryTurkey                                                                41.32129398
    ## log.Adult.Mortality:CountryTurkmenistan                                                         103.77684072
    ## log.Adult.Mortality:CountryUganda                                                                64.92210985
    ## log.Adult.Mortality:CountryUkraine                                                              157.31329284
    ## log.Adult.Mortality:CountryUnited Arab Emirates                                                  95.83236531
    ## log.Adult.Mortality:CountryUnited Kingdom of Great Britain and Northern Ireland                 132.16788021
    ## log.Adult.Mortality:CountryUnited Republic of Tanzania                                         -161.43319947
    ## log.Adult.Mortality:CountryUnited States of America                                             232.30004188
    ## log.Adult.Mortality:CountryUruguay                                                               93.26414866
    ## log.Adult.Mortality:CountryUzbekistan                                                           101.97586789
    ## log.Adult.Mortality:CountryVanuatu                                                               91.74941717
    ## log.Adult.Mortality:CountryVenezuela                                                            138.88198029
    ## log.Adult.Mortality:CountryViet Nam                                                             123.20840526
    ## log.Adult.Mortality:CountryYemen                                                               -222.13112882
    ## log.Adult.Mortality:CountryZambia                                                               118.66196602
    ## log.Adult.Mortality:CountryZimbabwe                                                              46.57729827
    ## log.Adult.Mortality:infant.deaths:CountryAfghanistan                                              0.96972794
    ## log.Adult.Mortality:infant.deaths:CountryAlbania                                                -14.19192189
    ## log.Adult.Mortality:infant.deaths:CountryAlgeria                                                  0.10316274
    ## log.Adult.Mortality:infant.deaths:CountryAngola                                                   0.37344882
    ## log.Adult.Mortality:infant.deaths:CountryAntigua and Barbuda                                              NA
    ## log.Adult.Mortality:infant.deaths:CountryArgentina                                                0.58850486
    ## log.Adult.Mortality:infant.deaths:CountryArmenia                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryAustralia                                                        NA
    ## log.Adult.Mortality:infant.deaths:CountryAustria                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryAzerbaijan                                              -1.89949968
    ## log.Adult.Mortality:infant.deaths:CountryBahamas                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryBahrain                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryBangladesh                                               0.00528243
    ## log.Adult.Mortality:infant.deaths:CountryBarbados                                                         NA
    ## log.Adult.Mortality:infant.deaths:CountryBelarus                                                -18.07006845
    ## log.Adult.Mortality:infant.deaths:CountryBelgium                                                 21.92402458
    ## log.Adult.Mortality:infant.deaths:CountryBelize                                                           NA
    ## log.Adult.Mortality:infant.deaths:CountryBenin                                                            NA
    ## log.Adult.Mortality:infant.deaths:CountryBhutan                                                  -0.93469607
    ## log.Adult.Mortality:infant.deaths:CountryBolivia                                                 -1.44683237
    ## log.Adult.Mortality:infant.deaths:CountryBosnia and Herzegovina                                           NA
    ## log.Adult.Mortality:infant.deaths:CountryBotswana                                                         NA
    ## log.Adult.Mortality:infant.deaths:CountryBrazil                                                   0.46532302
    ## log.Adult.Mortality:infant.deaths:CountryBrunei Darussalam                                                NA
    ## log.Adult.Mortality:infant.deaths:CountryBulgaria                                                         NA
    ## log.Adult.Mortality:infant.deaths:CountryBurkina Faso                                            -3.32344726
    ## log.Adult.Mortality:infant.deaths:CountryBurundi                                                  6.18603999
    ## log.Adult.Mortality:infant.deaths:CountryCabo Verde                                                       NA
    ## log.Adult.Mortality:infant.deaths:CountryCambodia                                                -0.38399152
    ## log.Adult.Mortality:infant.deaths:CountryCameroon                                                 0.18765709
    ## log.Adult.Mortality:infant.deaths:CountryCanada                                                           NA
    ## log.Adult.Mortality:infant.deaths:CountryCentral African Republic                                 3.38709726
    ## log.Adult.Mortality:infant.deaths:CountryChad                                                    -3.42957567
    ## log.Adult.Mortality:infant.deaths:CountryChile                                                            NA
    ## log.Adult.Mortality:infant.deaths:CountryChina                                                   -0.01105365
    ## log.Adult.Mortality:infant.deaths:CountryColombia                                                 0.63507837
    ## log.Adult.Mortality:infant.deaths:CountryComoros                                                  2.13241258
    ## log.Adult.Mortality:infant.deaths:CountryCongo                                                    3.15672287
    ## log.Adult.Mortality:infant.deaths:CountryCosta Rica                                                       NA
    ## log.Adult.Mortality:infant.deaths:CountryCroatia                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryCuba                                                             NA
    ## log.Adult.Mortality:infant.deaths:CountryCyprus                                                           NA
    ## log.Adult.Mortality:infant.deaths:CountryCzechia                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryDemocratic People's Republic of Korea                    1.05405253
    ## log.Adult.Mortality:infant.deaths:CountryDemocratic Republic of the Congo                         0.56504233
    ## log.Adult.Mortality:infant.deaths:CountryDenmark                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryDjibouti                                               -70.11514715
    ## log.Adult.Mortality:infant.deaths:CountryDominican Republic                                     -33.64830390
    ## log.Adult.Mortality:infant.deaths:CountryEcuador                                                 -1.64761818
    ## log.Adult.Mortality:infant.deaths:CountryEgypt                                                    5.89511609
    ## log.Adult.Mortality:infant.deaths:CountryEl Salvador                                            -29.07670911
    ## log.Adult.Mortality:infant.deaths:CountryEquatorial Guinea                                                NA
    ## log.Adult.Mortality:infant.deaths:CountryEritrea                                                -16.30178671
    ## log.Adult.Mortality:infant.deaths:CountryEstonia                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryEthiopia                                                -0.00958104
    ## log.Adult.Mortality:infant.deaths:CountryFiji                                                             NA
    ## log.Adult.Mortality:infant.deaths:CountryFinland                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryFrance                                                           NA
    ## log.Adult.Mortality:infant.deaths:CountryGabon                                                            NA
    ## log.Adult.Mortality:infant.deaths:CountryGambia                                                           NA
    ## log.Adult.Mortality:infant.deaths:CountryGeorgia                                                 -9.71201557
    ## log.Adult.Mortality:infant.deaths:CountryGermany                                                 36.92697669
    ## log.Adult.Mortality:infant.deaths:CountryGhana                                                  -10.94553850
    ## log.Adult.Mortality:infant.deaths:CountryGreece                                                  20.65214677
    ## log.Adult.Mortality:infant.deaths:CountryGrenada                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryGuatemala                                              -16.45342127
    ## log.Adult.Mortality:infant.deaths:CountryGuinea                                                  -0.60565722
    ## log.Adult.Mortality:infant.deaths:CountryGuinea-Bissau                                           82.39341942
    ## log.Adult.Mortality:infant.deaths:CountryGuyana                                                  -4.20287121
    ## log.Adult.Mortality:infant.deaths:CountryHaiti                                                   -3.25164632
    ## log.Adult.Mortality:infant.deaths:CountryHonduras                                                -0.41470045
    ## log.Adult.Mortality:infant.deaths:CountryHungary                                                 -7.43611478
    ## log.Adult.Mortality:infant.deaths:CountryIceland                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryIndia                                                   -0.00717479
    ## log.Adult.Mortality:infant.deaths:CountryIndonesia                                               -0.30238444
    ## log.Adult.Mortality:infant.deaths:CountryIran                                                     0.81315487
    ## log.Adult.Mortality:infant.deaths:CountryIraq                                                   -12.07971442
    ## log.Adult.Mortality:infant.deaths:CountryIreland                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryIsrael                                                           NA
    ## log.Adult.Mortality:infant.deaths:CountryItaly                                                            NA
    ## log.Adult.Mortality:infant.deaths:CountryIvory Coast                                              2.92282993
    ## log.Adult.Mortality:infant.deaths:CountryJamaica                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryJapan                                                   -2.75162330
    ## log.Adult.Mortality:infant.deaths:CountryJordan                                                           NA
    ## log.Adult.Mortality:infant.deaths:CountryKazakhstan                                              -1.07355015
    ## log.Adult.Mortality:infant.deaths:CountryKenya                                                   -0.31401298
    ## log.Adult.Mortality:infant.deaths:CountryKiribati                                                         NA
    ## log.Adult.Mortality:infant.deaths:CountryKuwait                                                  -4.62216413
    ## log.Adult.Mortality:infant.deaths:CountryKyrgyzstan                                             -22.96595841
    ## log.Adult.Mortality:infant.deaths:CountryLao People's Democratic Republic                        -1.60827861
    ## log.Adult.Mortality:infant.deaths:CountryLatvia                                                           NA
    ## log.Adult.Mortality:infant.deaths:CountryLebanon                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryLesotho                                                  7.66257438
    ## log.Adult.Mortality:infant.deaths:CountryLiberia                                                 -0.37389070
    ## log.Adult.Mortality:infant.deaths:CountryLibya                                                  164.35921713
    ## log.Adult.Mortality:infant.deaths:CountryLithuania                                                        NA
    ## log.Adult.Mortality:infant.deaths:CountryLuxembourg                                                       NA
    ## log.Adult.Mortality:infant.deaths:CountryMadagascar                                              -3.51512765
    ## log.Adult.Mortality:infant.deaths:CountryMalawi                                                  -0.37994535
    ## log.Adult.Mortality:infant.deaths:CountryMalaysia                                                -1.25909138
    ## log.Adult.Mortality:infant.deaths:CountryMaldives                                                         NA
    ## log.Adult.Mortality:infant.deaths:CountryMali                                                     3.97499067
    ## log.Adult.Mortality:infant.deaths:CountryMalta                                                            NA
    ## log.Adult.Mortality:infant.deaths:CountryMauritania                                             268.18610275
    ## log.Adult.Mortality:infant.deaths:CountryMauritius                                                        NA
    ## log.Adult.Mortality:infant.deaths:CountryMexico                                                   0.61987017
    ## log.Adult.Mortality:infant.deaths:CountryMicronesia (Federated States of)                                 NA
    ## log.Adult.Mortality:infant.deaths:CountryMongolia                                               -20.57697815
    ## log.Adult.Mortality:infant.deaths:CountryMontenegro                                                       NA
    ## log.Adult.Mortality:infant.deaths:CountryMorocco                                                  0.07243786
    ## log.Adult.Mortality:infant.deaths:CountryMozambique                                               2.80235385
    ## log.Adult.Mortality:infant.deaths:CountryMyanmar                                                 -0.35517240
    ## log.Adult.Mortality:infant.deaths:CountryNamibia                                                -16.37345896
    ## log.Adult.Mortality:infant.deaths:CountryNepal                                                   -0.26348015
    ## log.Adult.Mortality:infant.deaths:CountryNetherlands                                                      NA
    ## log.Adult.Mortality:infant.deaths:CountryNew Zealand                                                      NA
    ## log.Adult.Mortality:infant.deaths:CountryNicaragua                                               11.64387398
    ## log.Adult.Mortality:infant.deaths:CountryNiger                                                   26.99993614
    ## log.Adult.Mortality:infant.deaths:CountryNigeria                                                 -0.69098379
    ## log.Adult.Mortality:infant.deaths:CountryNorway                                                           NA
    ## log.Adult.Mortality:infant.deaths:CountryOman                                                             NA
    ## log.Adult.Mortality:infant.deaths:CountryPakistan                                                -0.38344683
    ## log.Adult.Mortality:infant.deaths:CountryPanama                                                -143.53985084
    ## log.Adult.Mortality:infant.deaths:CountryPapua New Guinea                                        -6.34005269
    ## log.Adult.Mortality:infant.deaths:CountryParaguay                                                20.50736285
    ## log.Adult.Mortality:infant.deaths:CountryPeru                                                     0.86203501
    ## log.Adult.Mortality:infant.deaths:CountryPhilippines                                             -0.20213250
    ## log.Adult.Mortality:infant.deaths:CountryPoland                                                  -1.55502672
    ## log.Adult.Mortality:infant.deaths:CountryPortugal                                                 8.92552738
    ## log.Adult.Mortality:infant.deaths:CountryQatar                                                            NA
    ## log.Adult.Mortality:infant.deaths:CountryRepublic of Korea                                       -2.10246175
    ## log.Adult.Mortality:infant.deaths:CountryRepublic of Moldova                                              NA
    ## log.Adult.Mortality:infant.deaths:CountryRomania                                                 35.18343805
    ## log.Adult.Mortality:infant.deaths:CountryRussian Federation                                       3.31328995
    ## log.Adult.Mortality:infant.deaths:CountryRwanda                                                  -0.63919176
    ## log.Adult.Mortality:infant.deaths:CountrySaint Lucia                                                      NA
    ## log.Adult.Mortality:infant.deaths:CountrySaint Vincent and the Grenadines                                 NA
    ## log.Adult.Mortality:infant.deaths:CountrySamoa                                                            NA
    ## log.Adult.Mortality:infant.deaths:CountrySao Tome and Principe                                            NA
    ## log.Adult.Mortality:infant.deaths:CountrySaudi Arabia                                             1.07752978
    ## log.Adult.Mortality:infant.deaths:CountrySenegal                                                 -0.84966189
    ## log.Adult.Mortality:infant.deaths:CountrySerbia                                                           NA
    ## log.Adult.Mortality:infant.deaths:CountrySeychelles                                                       NA
    ## log.Adult.Mortality:infant.deaths:CountrySierra Leone                                             5.34226400
    ## log.Adult.Mortality:infant.deaths:CountrySingapore                                                        NA
    ## log.Adult.Mortality:infant.deaths:CountrySlovakia                                                         NA
    ## log.Adult.Mortality:infant.deaths:CountrySlovenia                                                         NA
    ## log.Adult.Mortality:infant.deaths:CountrySolomon Islands                                                  NA
    ## log.Adult.Mortality:infant.deaths:CountrySomalia                                                -12.27477438
    ## log.Adult.Mortality:infant.deaths:CountrySouth Africa                                             1.29566684
    ## log.Adult.Mortality:infant.deaths:CountrySouth Sudan                                             10.45151891
    ## log.Adult.Mortality:infant.deaths:CountrySpain                                                   -9.73627241
    ## log.Adult.Mortality:infant.deaths:CountrySri Lanka                                               -3.29152376
    ## log.Adult.Mortality:infant.deaths:CountrySudan                                                   -1.95218560
    ## log.Adult.Mortality:infant.deaths:CountrySuriname                                                         NA
    ## log.Adult.Mortality:infant.deaths:CountrySwaziland                                               -8.43561261
    ## log.Adult.Mortality:infant.deaths:CountrySweden                                                           NA
    ## log.Adult.Mortality:infant.deaths:CountrySwitzerland                                                      NA
    ## log.Adult.Mortality:infant.deaths:CountrySyrian Arab Republic                                    -0.20831632
    ## log.Adult.Mortality:infant.deaths:CountryTajikistan                                               5.58477796
    ## log.Adult.Mortality:infant.deaths:CountryThailand                                                -0.60054319
    ## log.Adult.Mortality:infant.deaths:CountryThe former Yugoslav republic of Macedonia                        NA
    ## log.Adult.Mortality:infant.deaths:CountryTimor-Leste                                              0.26802094
    ## log.Adult.Mortality:infant.deaths:CountryTogo                                                     2.48857359
    ## log.Adult.Mortality:infant.deaths:CountryTonga                                                            NA
    ## log.Adult.Mortality:infant.deaths:CountryTrinidad and Tobago                                              NA
    ## log.Adult.Mortality:infant.deaths:CountryTunisia                                                -12.12442511
    ## log.Adult.Mortality:infant.deaths:CountryTurkey                                                   0.70762603
    ## log.Adult.Mortality:infant.deaths:CountryTurkmenistan                                            -2.46099893
    ## log.Adult.Mortality:infant.deaths:CountryUganda                                                  -0.07613872
    ## log.Adult.Mortality:infant.deaths:CountryUkraine                                                -15.54077232
    ## log.Adult.Mortality:infant.deaths:CountryUnited Arab Emirates                                             NA
    ## log.Adult.Mortality:infant.deaths:CountryUnited Kingdom of Great Britain and Northern Ireland   -11.63598001
    ## log.Adult.Mortality:infant.deaths:CountryUnited Republic of Tanzania                              2.89478474
    ## log.Adult.Mortality:infant.deaths:CountryUnited States of America                                -5.21405632
    ## log.Adult.Mortality:infant.deaths:CountryUruguay                                                  0.17596747
    ## log.Adult.Mortality:infant.deaths:CountryUzbekistan                                              -0.01364923
    ## log.Adult.Mortality:infant.deaths:CountryVanuatu                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryVenezuela                                               -4.61018368
    ## log.Adult.Mortality:infant.deaths:CountryViet Nam                                                -1.15537792
    ## log.Adult.Mortality:infant.deaths:CountryYemen                                                    7.90984748
    ## log.Adult.Mortality:infant.deaths:CountryZambia                                                  -0.83135070
    ## log.Adult.Mortality:infant.deaths:CountryZimbabwe                                                 1.31460873
    ##                                                                                                   Std. Error
    ## (Intercept)                                                                                     491.32599535
    ## log.Adult.Mortality                                                                              90.97122131
    ## infant.deaths                                                                                     5.02418445
    ## log.GDP                                                                                           0.02840994
    ## Measles                                                                                           0.00000467
    ## HIV_cat                                                                                           0.27809682
    ## CountryAlbania                                                                                  502.80365189
    ## CountryAlgeria                                                                                  772.75571244
    ## CountryAngola                                                                                   663.96345350
    ## CountryAntigua and Barbuda                                                                      491.81554289
    ## CountryArgentina                                                                                573.77443936
    ## CountryArmenia                                                                                  487.20325511
    ## CountryAustralia                                                                                487.14002130
    ## CountryAustria                                                                                  491.56113783
    ## CountryAzerbaijan                                                                               509.92570672
    ## CountryBahamas                                                                                  491.71392704
    ## CountryBahrain                                                                                  491.39604589
    ## CountryBangladesh                                                                               631.54357156
    ## CountryBarbados                                                                                 491.81990049
    ## CountryBelarus                                                                                  491.81309180
    ## CountryBelgium                                                                                  491.92780430
    ## CountryBelize                                                                                   493.65488703
    ## CountryBenin                                                                                    369.41333994
    ## CountryBhutan                                                                                   513.01787105
    ## CountryBolivia                                                                                  509.35909343
    ## CountryBosnia and Herzegovina                                                                   491.71690645
    ## CountryBotswana                                                                                 481.44705384
    ## CountryBrazil                                                                                   539.15907203
    ## CountryBrunei Darussalam                                                                        492.98703064
    ## CountryBulgaria                                                                                 492.64959059
    ## CountryBurkina Faso                                                                             894.52441941
    ## CountryBurundi                                                                                  838.13232860
    ## CountryCabo Verde                                                                               491.75853048
    ## CountryCambodia                                                                                 493.85795285
    ## CountryCameroon                                                                                1247.42919413
    ## CountryCanada                                                                                   483.03307268
    ## CountryCentral African Republic                                                                 871.55188496
    ## CountryChad                                                                                   25127.67253127
    ## CountryChile                                                                                    482.61165461
    ## CountryChina                                                                                    513.83082956
    ## CountryColombia                                                                                 597.37061361
    ## CountryComoros                                                                                  502.11397635
    ## CountryCongo                                                                                    586.75350169
    ## CountryCosta Rica                                                                               489.42008256
    ## CountryCroatia                                                                                  491.67551639
    ## CountryCuba                                                                                     486.75798652
    ## CountryCyprus                                                                                   491.61885102
    ## CountryCzechia                                                                                  491.50918920
    ## CountryDemocratic People's Republic of Korea                                                    493.71234320
    ## CountryDemocratic Republic of the Congo                                                        2716.81720074
    ## CountryDenmark                                                                                  491.47151938
    ## CountryDjibouti                                                                                 549.11557313
    ## CountryDominican Republic                                                                      1298.34159812
    ## CountryEcuador                                                                                  522.32101791
    ## CountryEgypt                                                                                   1379.58846251
    ## CountryEl Salvador                                                                              516.47358848
    ## CountryEquatorial Guinea                                                                        477.81002486
    ## CountryEritrea                                                                                  555.85263556
    ## CountryEstonia                                                                                  491.39629768
    ## CountryEthiopia                                                                                 496.61534847
    ## CountryFiji                                                                                     492.66215779
    ## CountryFinland                                                                                  491.52220029
    ## CountryFrance                                                                                   491.66588884
    ## CountryGabon                                                                                    481.70342525
    ## CountryGambia                                                                                   478.90450276
    ## CountryGeorgia                                                                                  503.95241250
    ## CountryGermany                                                                                  516.00148336
    ## CountryGhana                                                                                   1536.42146381
    ## CountryGreece                                                                                   492.13106564
    ## CountryGrenada                                                                                  491.79466087
    ## CountryGuatemala                                                                                519.17375671
    ## CountryGuinea                                                                                   623.56477729
    ## CountryGuinea-Bissau                                                                            922.17940417
    ## CountryGuyana                                                                                   532.99064307
    ## CountryHaiti                                                                                    520.57118632
    ## CountryHonduras                                                                                 534.17678574
    ## CountryHungary                                                                                  493.70599512
    ## CountryIceland                                                                                  491.49431269
    ## CountryIndia                                                                                    587.90827269
    ## CountryIndonesia                                                                                699.35483223
    ## CountryIran                                                                                     495.63106876
    ## CountryIraq                                                                                     915.84721339
    ## CountryIreland                                                                                  491.49512138
    ## CountryIsrael                                                                                   491.49595785
    ## CountryItaly                                                                                    491.56191664
    ## CountryIvory Coast                                                                             1583.25573212
    ## CountryJamaica                                                                                  486.78094540
    ## CountryJapan                                                                                    499.95390164
    ## CountryJordan                                                                                   472.47810146
    ## CountryKazakhstan                                                                               506.23306476
    ## CountryKenya                                                                                    504.77514602
    ## CountryKiribati                                                                                 496.35120404
    ## CountryKuwait                                                                                   493.03161737
    ## CountryKyrgyzstan                                                                               603.04035446
    ## CountryLao People's Democratic Republic                                                         528.36637451
    ## CountryLatvia                                                                                   491.51767334
    ## CountryLebanon                                                                                  487.24515704
    ## CountryLesotho                                                                                  565.77304733
    ## CountryLiberia                                                                                  503.73682500
    ## CountryLibya                                                                                    794.42714578
    ## CountryLithuania                                                                                491.60687454
    ## CountryLuxembourg                                                                               491.44034237
    ## CountryMadagascar                                                                               614.10434975
    ## CountryMalawi                                                                                   494.76281685
    ## CountryMalaysia                                                                                 585.18143431
    ## CountryMaldives                                                                                 491.35584975
    ## CountryMali                                                                                    1886.46226432
    ## CountryMalta                                                                                    491.73786177
    ## CountryMauritania                                                                             10618.45687302
    ## CountryMauritius                                                                                492.68015809
    ## CountryMexico                                                                                   627.66567527
    ## CountryMicronesia (Federated States of)                                                         493.04061203
    ## CountryMongolia                                                                                 515.50664348
    ## CountryMontenegro                                                                               491.61378726
    ## CountryMorocco                                                                                  494.79580980
    ## CountryMozambique                                                                               648.25016982
    ## CountryMyanmar                                                                                  521.68221587
    ## CountryNamibia                                                                                 1105.38671231
    ## CountryNepal                                                                                    498.08496302
    ## CountryNetherlands                                                                              486.55251826
    ## CountryNew Zealand                                                                              491.78253573
    ## CountryNicaragua                                                                                506.94629833
    ## CountryNiger                                                                                   1947.36048230
    ## CountryNigeria                                                                                 1200.10450493
    ## CountryNorway                                                                                   491.53418138
    ## CountryOman                                                                                     486.65636153
    ## CountryPakistan                                                                                2416.58733312
    ## CountryPanama                                                                                  2329.90824402
    ## CountryPapua New Guinea                                                                        2337.75753908
    ## CountryParaguay                                                                                 532.12020263
    ## CountryPeru                                                                                     518.90996434
    ## CountryPhilippines                                                                             1023.65052708
    ## CountryPoland                                                                                   545.41658822
    ## CountryPortugal                                                                                 491.65195717
    ## CountryQatar                                                                                    492.45833871
    ## CountryRepublic of Korea                                                                        493.10902144
    ## CountryRepublic of Moldova                                                                      486.74916210
    ## CountryRomania                                                                                  506.84658566
    ## CountryRussian Federation                                                                       520.54708260
    ## CountryRwanda                                                                                   492.88224653
    ## CountrySaint Lucia                                                                              492.01352137
    ## CountrySaint Vincent and the Grenadines                                                         492.28833655
    ## CountrySamoa                                                                                    491.45427514
    ## CountrySao Tome and Principe                                                                    492.27102483
    ## CountrySaudi Arabia                                                                             554.17056295
    ## CountrySenegal                                                                                  637.39723599
    ## CountrySerbia                                                                                   486.99452273
    ## CountrySeychelles                                                                               494.35666643
    ## CountrySierra Leone                                                                             646.05921685
    ## CountrySingapore                                                                                491.47577142
    ## CountrySlovakia                                                                                 491.57610171
    ## CountrySlovenia                                                                                 491.40319627
    ## CountrySolomon Islands                                                                          491.51213926
    ## CountrySomalia                                                                                 2564.51874413
    ## CountrySouth Africa                                                                             535.74369120
    ## CountrySouth Sudan                                                                             2044.45602817
    ## CountrySpain                                                                                    520.56041087
    ## CountrySri Lanka                                                                                517.61638342
    ## CountrySudan                                                                                    726.48367253
    ## CountrySuriname                                                                                 492.10232474
    ## CountrySwaziland                                                                                517.55772904
    ## CountrySweden                                                                                   491.58086136
    ## CountrySwitzerland                                                                              491.47555429
    ## CountrySyrian Arab Republic                                                                     494.98362300
    ## CountryTajikistan                                                                               641.74587169
    ## CountryThailand                                                                                 502.18319422
    ## CountryThe former Yugoslav republic of Macedonia                                                491.69453728
    ## CountryTimor-Leste                                                                              517.13428995
    ## CountryTogo                                                                                     796.52083596
    ## CountryTonga                                                                                    493.20293453
    ## CountryTrinidad and Tobago                                                                      492.51824664
    ## CountryTunisia                                                                                  826.56995704
    ## CountryTurkey                                                                                   562.29408222
    ## CountryTurkmenistan                                                                             632.57747807
    ## CountryUganda                                                                                   502.16057059
    ## CountryUkraine                                                                                  517.89361945
    ## CountryUnited Arab Emirates                                                                     486.62744799
    ## CountryUnited Kingdom of Great Britain and Northern Ireland                                     611.01547058
    ## CountryUnited Republic of Tanzania                                                              765.30173469
    ## CountryUnited States of America                                                                1310.03306424
    ## CountryUruguay                                                                                  522.20967547
    ## CountryUzbekistan                                                                               503.44017176
    ## CountryVanuatu                                                                                  491.64788830
    ## CountryVenezuela                                                                               5991.21251871
    ## CountryViet Nam                                                                                2425.98621881
    ## CountryYemen                                                                                   1432.61131517
    ## CountryZambia                                                                                   582.58607833
    ## CountryZimbabwe                                                                                 505.20332498
    ## infant.deaths:CountryAlbania                                                                    147.69292447
    ## infant.deaths:CountryAlgeria                                                                     29.35036031
    ## infant.deaths:CountryAngola                                                                       6.17919436
    ## infant.deaths:CountryAntigua and Barbuda                                                                  NA
    ## infant.deaths:CountryArgentina                                                                   25.29432627
    ## infant.deaths:CountryArmenia                                                                              NA
    ## infant.deaths:CountryAustralia                                                                            NA
    ## infant.deaths:CountryAustria                                                                              NA
    ## infant.deaths:CountryAzerbaijan                                                                  27.07027281
    ## infant.deaths:CountryBahamas                                                                              NA
    ## infant.deaths:CountryBahrain                                                                              NA
    ## infant.deaths:CountryBangladesh                                                                   5.05820405
    ## infant.deaths:CountryBarbados                                                                             NA
    ## infant.deaths:CountryBelarus                                                                     60.64682751
    ## infant.deaths:CountryBelgium                                                                     81.47290795
    ## infant.deaths:CountryBelize                                                                               NA
    ## infant.deaths:CountryBenin                                                                                NA
    ## infant.deaths:CountryBhutan                                                                     149.85275416
    ## infant.deaths:CountryBolivia                                                                     23.42245788
    ## infant.deaths:CountryBosnia and Herzegovina                                                               NA
    ## infant.deaths:CountryBotswana                                                                             NA
    ## infant.deaths:CountryBrazil                                                                       5.25465576
    ## infant.deaths:CountryBrunei Darussalam                                                                    NA
    ## infant.deaths:CountryBulgaria                                                                     5.27597941
    ## infant.deaths:CountryBurkina Faso                                                                15.50793182
    ## infant.deaths:CountryBurundi                                                                     29.86363570
    ## infant.deaths:CountryCabo Verde                                                                           NA
    ## infant.deaths:CountryCambodia                                                                     6.65978052
    ## infant.deaths:CountryCameroon                                                                    19.13498924
    ## infant.deaths:CountryCanada                                                                               NA
    ## infant.deaths:CountryCentral African Republic                                                    43.73310173
    ## infant.deaths:CountryChad                                                                       546.76960577
    ## infant.deaths:CountryChile                                                                                NA
    ## infant.deaths:CountryChina                                                                        5.02771479
    ## infant.deaths:CountryColombia                                                                    19.49840134
    ## infant.deaths:CountryComoros                                                                     74.00728387
    ## infant.deaths:CountryCongo                                                                       47.16297362
    ## infant.deaths:CountryCosta Rica                                                                           NA
    ## infant.deaths:CountryCroatia                                                                              NA
    ## infant.deaths:CountryCuba                                                                                 NA
    ## infant.deaths:CountryCyprus                                                                               NA
    ## infant.deaths:CountryCzechia                                                                              NA
    ## infant.deaths:CountryDemocratic People's Republic of Korea                                        8.20002373
    ## infant.deaths:CountryDemocratic Republic of the Congo                                            12.36957870
    ## infant.deaths:CountryDenmark                                                                              NA
    ## infant.deaths:CountryDjibouti                                                                   179.80151451
    ## infant.deaths:CountryDominican Republic                                                         198.84816561
    ## infant.deaths:CountryEcuador                                                                     25.98341820
    ## infant.deaths:CountryEgypt                                                                       23.74754398
    ## infant.deaths:CountryEl Salvador                                                                 74.11034365
    ## infant.deaths:CountryEquatorial Guinea                                                                    NA
    ## infant.deaths:CountryEritrea                                                                     37.25338223
    ## infant.deaths:CountryEstonia                                                                              NA
    ## infant.deaths:CountryEthiopia                                                                     5.07375281
    ## infant.deaths:CountryFiji                                                                                 NA
    ## infant.deaths:CountryFinland                                                                              NA
    ## infant.deaths:CountryFrance                                                                       5.30439288
    ## infant.deaths:CountryGabon                                                                                NA
    ## infant.deaths:CountryGambia                                                                               NA
    ## infant.deaths:CountryGeorgia                                                                     68.79395950
    ## infant.deaths:CountryGermany                                                                     62.81711136
    ## infant.deaths:CountryGhana                                                                       35.03061988
    ## infant.deaths:CountryGreece                                                                     208.98545255
    ## infant.deaths:CountryGrenada                                                                              NA
    ## infant.deaths:CountryGuatemala                                                                   16.65412534
    ## infant.deaths:CountryGuinea                                                                      13.75041197
    ## infant.deaths:CountryGuinea-Bissau                                                              177.78426480
    ## infant.deaths:CountryGuyana                                                                     219.64266786
    ## infant.deaths:CountryHaiti                                                                        8.42682864
    ## infant.deaths:CountryHonduras                                                                    35.56559160
    ## infant.deaths:CountryHungary                                                                     75.66351361
    ## infant.deaths:CountryIceland                                                                              NA
    ## infant.deaths:CountryIndia                                                                        5.02747179
    ## infant.deaths:CountryIndonesia                                                                    5.82080107
    ## infant.deaths:CountryIran                                                                         6.06837010
    ## infant.deaths:CountryIraq                                                                        25.34351218
    ## infant.deaths:CountryIreland                                                                              NA
    ## infant.deaths:CountryIsrael                                                                       5.24989978
    ## infant.deaths:CountryItaly                                                                        5.28215790
    ## infant.deaths:CountryIvory Coast                                                                 24.77379972
    ## infant.deaths:CountryJamaica                                                                              NA
    ## infant.deaths:CountryJapan                                                                       41.60096195
    ## infant.deaths:CountryJordan                                                                               NA
    ## infant.deaths:CountryKazakhstan                                                                  14.53255308
    ## infant.deaths:CountryKenya                                                                        5.45678788
    ## infant.deaths:CountryKiribati                                                                             NA
    ## infant.deaths:CountryKuwait                                                                     121.46106541
    ## infant.deaths:CountryKyrgyzstan                                                                  92.57307282
    ## infant.deaths:CountryLao People's Democratic Republic                                            16.71821948
    ## infant.deaths:CountryLatvia                                                                               NA
    ## infant.deaths:CountryLebanon                                                                              NA
    ## infant.deaths:CountryLesotho                                                                     61.66007824
    ## infant.deaths:CountryLiberia                                                                     12.00098006
    ## infant.deaths:CountryLibya                                                                      313.05083550
    ## infant.deaths:CountryLithuania                                                                            NA
    ## infant.deaths:CountryLuxembourg                                                                           NA
    ## infant.deaths:CountryMadagascar                                                                  17.41131538
    ## infant.deaths:CountryMalawi                                                                       5.43357536
    ## infant.deaths:CountryMalaysia                                                                    86.42932086
    ## infant.deaths:CountryMaldives                                                                             NA
    ## infant.deaths:CountryMali                                                                        29.70136046
    ## infant.deaths:CountryMalta                                                                                NA
    ## infant.deaths:CountryMauritania                                                                1326.22841302
    ## infant.deaths:CountryMauritius                                                                            NA
    ## infant.deaths:CountryMexico                                                                      11.77775594
    ## infant.deaths:CountryMicronesia (Federated States of)                                                     NA
    ## infant.deaths:CountryMongolia                                                                   103.55406846
    ## infant.deaths:CountryMontenegro                                                                           NA
    ## infant.deaths:CountryMorocco                                                                      5.78493361
    ## infant.deaths:CountryMozambique                                                                   8.24346367
    ## infant.deaths:CountryMyanmar                                                                      5.92469902
    ## infant.deaths:CountryNamibia                                                                    330.24410978
    ## infant.deaths:CountryNepal                                                                        5.65002977
    ## infant.deaths:CountryNetherlands                                                                          NA
    ## infant.deaths:CountryNew Zealand                                                                          NA
    ## infant.deaths:CountryNicaragua                                                                   33.79394970
    ## infant.deaths:CountryNiger                                                                       41.35955201
    ## infant.deaths:CountryNigeria                                                                      5.39019516
    ## infant.deaths:CountryNorway                                                                               NA
    ## infant.deaths:CountryOman                                                                                 NA
    ## infant.deaths:CountryPakistan                                                                     8.19497052
    ## infant.deaths:CountryPanama                                                                    2273.45777991
    ## infant.deaths:CountryPapua New Guinea                                                           208.99856049
    ## infant.deaths:CountryParaguay                                                                    62.82780750
    ## infant.deaths:CountryPeru                                                                        17.62993749
    ## infant.deaths:CountryPhilippines                                                                 16.26945287
    ## infant.deaths:CountryPoland                                                                     114.69928474
    ## infant.deaths:CountryPortugal                                                                   320.73309945
    ## infant.deaths:CountryQatar                                                                                NA
    ## infant.deaths:CountryRepublic of Korea                                                           14.79980938
    ## infant.deaths:CountryRepublic of Moldova                                                                  NA
    ## infant.deaths:CountryRomania                                                                     52.77794056
    ## infant.deaths:CountryRussian Federation                                                          14.05937618
    ## infant.deaths:CountryRwanda                                                                       5.35233658
    ## infant.deaths:CountrySaint Lucia                                                                          NA
    ## infant.deaths:CountrySaint Vincent and the Grenadines                                                     NA
    ## infant.deaths:CountrySamoa                                                                                NA
    ## infant.deaths:CountrySao Tome and Principe                                                                NA
    ## infant.deaths:CountrySaudi Arabia                                                                29.27239004
    ## infant.deaths:CountrySenegal                                                                     22.60735509
    ## infant.deaths:CountrySerbia                                                                               NA
    ## infant.deaths:CountrySeychelles                                                                           NA
    ## infant.deaths:CountrySierra Leone                                                                15.64675232
    ## infant.deaths:CountrySingapore                                                                            NA
    ## infant.deaths:CountrySlovakia                                                                             NA
    ## infant.deaths:CountrySlovenia                                                                             NA
    ## infant.deaths:CountrySolomon Islands                                                                      NA
    ## infant.deaths:CountrySomalia                                                                     50.51682784
    ## infant.deaths:CountrySouth Africa                                                                 6.78520830
    ## infant.deaths:CountrySouth Sudan                                                                 75.63114217
    ## infant.deaths:CountrySpain                                                                       90.93298815
    ## infant.deaths:CountrySri Lanka                                                                   39.79430470
    ## infant.deaths:CountrySudan                                                                        9.46559357
    ## infant.deaths:CountrySuriname                                                                             NA
    ## infant.deaths:CountrySwaziland                                                                   62.86487357
    ## infant.deaths:CountrySweden                                                                               NA
    ## infant.deaths:CountrySwitzerland                                                                          NA
    ## infant.deaths:CountrySyrian Arab Republic                                                         9.96176326
    ## infant.deaths:CountryTajikistan                                                                  41.68776007
    ## infant.deaths:CountryThailand                                                                    10.57988481
    ## infant.deaths:CountryThe former Yugoslav republic of Macedonia                                            NA
    ## infant.deaths:CountryTimor-Leste                                                                 66.91435384
    ## infant.deaths:CountryTogo                                                                        45.96257772
    ## infant.deaths:CountryTonga                                                                                NA
    ## infant.deaths:CountryTrinidad and Tobago                                                                  NA
    ## infant.deaths:CountryTunisia                                                                    215.99192208
    ## infant.deaths:CountryTurkey                                                                      11.29005488
    ## infant.deaths:CountryTurkmenistan                                                                61.25159637
    ## infant.deaths:CountryUganda                                                                       5.33103107
    ## infant.deaths:CountryUkraine                                                                     34.93424129
    ## infant.deaths:CountryUnited Arab Emirates                                                                 NA
    ## infant.deaths:CountryUnited Kingdom of Great Britain and Northern Ireland                        94.77767251
    ## infant.deaths:CountryUnited Republic of Tanzania                                                  8.62405230
    ## infant.deaths:CountryUnited States of America                                                    46.81335007
    ## infant.deaths:CountryUruguay                                                                    187.14834553
    ## infant.deaths:CountryUzbekistan                                                                   7.40406679
    ## infant.deaths:CountryVanuatu                                                                              NA
    ## infant.deaths:CountryVenezuela                                                                  660.00527894
    ## infant.deaths:CountryViet Nam                                                                    84.36949134
    ## infant.deaths:CountryYemen                                                                       37.79445864
    ## infant.deaths:CountryZambia                                                                      12.47533349
    ## infant.deaths:CountryZimbabwe                                                                     6.89556933
    ## log.Adult.Mortality:CountryAlbania                                                               93.93055587
    ## log.Adult.Mortality:CountryAlgeria                                                              154.63980647
    ## log.Adult.Mortality:CountryAngola                                                               123.93151425
    ## log.Adult.Mortality:CountryAntigua and Barbuda                                                   91.07874765
    ## log.Adult.Mortality:CountryArgentina                                                            110.49572687
    ## log.Adult.Mortality:CountryArmenia                                                               91.15924154
    ## log.Adult.Mortality:CountryAustralia                                                             91.20968096
    ## log.Adult.Mortality:CountryAustria                                                               91.03821148
    ## log.Adult.Mortality:CountryAzerbaijan                                                            94.92531125
    ## log.Adult.Mortality:CountryBahamas                                                               91.04933238
    ## log.Adult.Mortality:CountryBahrain                                                               90.99118576
    ## log.Adult.Mortality:CountryBangladesh                                                           123.38400782
    ## log.Adult.Mortality:CountryBarbados                                                              91.09101903
    ## log.Adult.Mortality:CountryBelarus                                                               91.06539804
    ## log.Adult.Mortality:CountryBelgium                                                               91.13875218
    ## log.Adult.Mortality:CountryBelize                                                                91.42874872
    ## log.Adult.Mortality:CountryBenin                                                                 91.20191880
    ## log.Adult.Mortality:CountryBhutan                                                                95.00942116
    ## log.Adult.Mortality:CountryBolivia                                                               94.43142140
    ## log.Adult.Mortality:CountryBosnia and Herzegovina                                                91.06804655
    ## log.Adult.Mortality:CountryBotswana                                                              90.97884985
    ## log.Adult.Mortality:CountryBrazil                                                               101.79611122
    ## log.Adult.Mortality:CountryBrunei Darussalam                                                     91.40368151
    ## log.Adult.Mortality:CountryBulgaria                                                              91.26657931
    ## log.Adult.Mortality:CountryBurkina Faso                                                         162.94072031
    ## log.Adult.Mortality:CountryBurundi                                                              148.40805869
    ## log.Adult.Mortality:CountryCabo Verde                                                            91.06601944
    ## log.Adult.Mortality:CountryCambodia                                                              91.44819810
    ## log.Adult.Mortality:CountryCameroon                                                             215.90034170
    ## log.Adult.Mortality:CountryCanada                                                                91.43782621
    ## log.Adult.Mortality:CountryCentral African Republic                                             148.62498373
    ## log.Adult.Mortality:CountryChad                                                                4173.25934504
    ## log.Adult.Mortality:CountryChile                                                                 91.28362528
    ## log.Adult.Mortality:CountryChina                                                                 97.45642426
    ## log.Adult.Mortality:CountryColombia                                                             114.16454157
    ## log.Adult.Mortality:CountryComoros                                                               92.88082989
    ## log.Adult.Mortality:CountryCongo                                                                105.29232850
    ## log.Adult.Mortality:CountryCosta Rica                                                            91.74384226
    ## log.Adult.Mortality:CountryCroatia                                                               91.05563539
    ## log.Adult.Mortality:CountryCuba                                                                  91.07008776
    ## log.Adult.Mortality:CountryCyprus                                                                91.06073215
    ## log.Adult.Mortality:CountryCzechia                                                               91.01722586
    ## log.Adult.Mortality:CountryDemocratic People's Republic of Korea                                 91.48293227
    ## log.Adult.Mortality:CountryDemocratic Republic of the Congo                                     469.86388031
    ## log.Adult.Mortality:CountryDenmark                                                               91.01084314
    ## log.Adult.Mortality:CountryDjibouti                                                             100.79814092
    ## log.Adult.Mortality:CountryDominican Republic                                                   242.24046058
    ## log.Adult.Mortality:CountryEcuador                                                               97.88612797
    ## log.Adult.Mortality:CountryEgypt                                                                267.04119811
    ## log.Adult.Mortality:CountryEl Salvador                                                           95.73102103
    ## log.Adult.Mortality:CountryEquatorial Guinea                                                     91.18330662
    ## log.Adult.Mortality:CountryEritrea                                                              102.16977171
    ## log.Adult.Mortality:CountryEstonia                                                               90.98585932
    ## log.Adult.Mortality:CountryEthiopia                                                              91.75500976
    ## log.Adult.Mortality:CountryFiji                                                                  91.22470054
    ## log.Adult.Mortality:CountryFinland                                                               91.02381926
    ## log.Adult.Mortality:CountryFrance                                                                91.08557129
    ## log.Adult.Mortality:CountryGabon                                                                 91.02209666
    ## log.Adult.Mortality:CountryGambia                                                                91.37809358
    ## log.Adult.Mortality:CountryGeorgia                                                               93.69980304
    ## log.Adult.Mortality:CountryGermany                                                               97.87197532
    ## log.Adult.Mortality:CountryGhana                                                                280.61498532
    ## log.Adult.Mortality:CountryGreece                                                                91.20564911
    ## log.Adult.Mortality:CountryGrenada                                                               91.06878671
    ## log.Adult.Mortality:CountryGuatemala                                                             96.30136266
    ## log.Adult.Mortality:CountryGuinea                                                               112.93416633
    ## log.Adult.Mortality:CountryGuinea-Bissau                                                        164.63536308
    ## log.Adult.Mortality:CountryGuyana                                                                98.22509199
    ## log.Adult.Mortality:CountryHaiti                                                                 96.40813150
    ## log.Adult.Mortality:CountryHonduras                                                             100.22617711
    ## log.Adult.Mortality:CountryHungary                                                               91.48778599
    ## log.Adult.Mortality:CountryIceland                                                               91.02559064
    ## log.Adult.Mortality:CountryIndia                                                                111.01691447
    ## log.Adult.Mortality:CountryIndonesia                                                            132.05964650
    ## log.Adult.Mortality:CountryIran                                                                  91.89417215
    ## log.Adult.Mortality:CountryIraq                                                                 178.69529067
    ## log.Adult.Mortality:CountryIreland                                                               91.02051051
    ## log.Adult.Mortality:CountryIsrael                                                                91.02635765
    ## log.Adult.Mortality:CountryItaly                                                                 91.05840099
    ## log.Adult.Mortality:CountryIvory Coast                                                          263.78542883
    ## log.Adult.Mortality:CountryJamaica                                                               91.06146411
    ## log.Adult.Mortality:CountryJapan                                                                 93.67746553
    ## log.Adult.Mortality:CountryJordan                                                                91.19885618
    ## log.Adult.Mortality:CountryKazakhstan                                                            93.90248215
    ## log.Adult.Mortality:CountryKenya                                                                 92.87720197
    ## log.Adult.Mortality:CountryKiribati                                                              91.92084481
    ## log.Adult.Mortality:CountryKuwait                                                                91.42999035
    ## log.Adult.Mortality:CountryKyrgyzstan                                                           112.97801357
    ## log.Adult.Mortality:CountryLao People's Democratic Republic                                      98.24580810
    ## log.Adult.Mortality:CountryLatvia                                                                91.00874595
    ## log.Adult.Mortality:CountryLebanon                                                               91.19094131
    ## log.Adult.Mortality:CountryLesotho                                                              101.41877243
    ## log.Adult.Mortality:CountryLiberia                                                               93.05038967
    ## log.Adult.Mortality:CountryLibya                                                                154.80452234
    ## log.Adult.Mortality:CountryLithuania                                                             91.02510987
    ## log.Adult.Mortality:CountryLuxembourg                                                            91.00475437
    ## log.Adult.Mortality:CountryMadagascar                                                           110.92818696
    ## log.Adult.Mortality:CountryMalawi                                                                91.45051744
    ## log.Adult.Mortality:CountryMalaysia                                                             111.71657339
    ## log.Adult.Mortality:CountryMaldives                                                              90.97951067
    ## log.Adult.Mortality:CountryMali                                                                 338.05707654
    ## log.Adult.Mortality:CountryMalta                                                                 91.10198904
    ## log.Adult.Mortality:CountryMauritania                                                          1955.81036059
    ## log.Adult.Mortality:CountryMauritius                                                             91.25400150
    ## log.Adult.Mortality:CountryMexico                                                               120.96673791
    ## log.Adult.Mortality:CountryMicronesia (Federated States of)                                      91.31794407
    ## log.Adult.Mortality:CountryMongolia                                                              95.37080237
    ## log.Adult.Mortality:CountryMontenegro                                                            91.03919526
    ## log.Adult.Mortality:CountryMorocco                                                               91.97224004
    ## log.Adult.Mortality:CountryMozambique                                                           114.61196732
    ## log.Adult.Mortality:CountryMyanmar                                                               96.63154655
    ## log.Adult.Mortality:CountryNamibia                                                              196.82671735
    ## log.Adult.Mortality:CountryNepal                                                                 92.43183131
    ## log.Adult.Mortality:CountryNetherlands                                                           91.02893362
    ## log.Adult.Mortality:CountryNew Zealand                                                           91.10356014
    ## log.Adult.Mortality:CountryNicaragua                                                             94.31459264
    ## log.Adult.Mortality:CountryNiger                                                                341.91623859
    ## log.Adult.Mortality:CountryNigeria                                                              207.47314992
    ## log.Adult.Mortality:CountryNorway                                                                91.03409067
    ## log.Adult.Mortality:CountryOman                                                                  91.04245180
    ## log.Adult.Mortality:CountryPakistan                                                             467.61755430
    ## log.Adult.Mortality:CountryPanama                                                               484.47413149
    ## log.Adult.Mortality:CountryPapua New Guinea                                                     429.20277549
    ## log.Adult.Mortality:CountryParaguay                                                              99.56732638
    ## log.Adult.Mortality:CountryPeru                                                                  97.69605161
    ## log.Adult.Mortality:CountryPhilippines                                                          191.30995142
    ## log.Adult.Mortality:CountryPoland                                                               102.69217857
    ## log.Adult.Mortality:CountryPortugal                                                              91.05924074
    ## log.Adult.Mortality:CountryQatar                                                                 91.31783079
    ## log.Adult.Mortality:CountryRepublic of Korea                                                     91.54632652
    ## log.Adult.Mortality:CountryRepublic of Moldova                                                   91.04428905
    ## log.Adult.Mortality:CountryRomania                                                               94.19790360
    ## log.Adult.Mortality:CountryRussian Federation                                                    95.73730285
    ## log.Adult.Mortality:CountryRwanda                                                                91.31205514
    ## log.Adult.Mortality:CountrySaint Lucia                                                           91.11569243
    ## log.Adult.Mortality:CountrySaint Vincent and the Grenadines                                      91.16484179
    ## log.Adult.Mortality:CountrySamoa                                                                 90.99925399
    ## log.Adult.Mortality:CountrySao Tome and Principe                                                 91.14916996
    ## log.Adult.Mortality:CountrySaudi Arabia                                                         107.00804139
    ## log.Adult.Mortality:CountrySenegal                                                              116.42913543
    ## log.Adult.Mortality:CountrySerbia                                                                91.12042911
    ## log.Adult.Mortality:CountrySeychelles                                                            91.57555994
    ## log.Adult.Mortality:CountrySierra Leone                                                         114.25901968
    ## log.Adult.Mortality:CountrySingapore                                                             91.01718921
    ## log.Adult.Mortality:CountrySlovakia                                                              91.02831295
    ## log.Adult.Mortality:CountrySlovenia                                                              90.99116849
    ## log.Adult.Mortality:CountrySolomon Islands                                                       91.00818528
    ## log.Adult.Mortality:CountrySomalia                                                              439.32449427
    ## log.Adult.Mortality:CountrySouth Africa                                                          97.69679960
    ## log.Adult.Mortality:CountrySouth Sudan                                                          346.32127901
    ## log.Adult.Mortality:CountrySpain                                                                100.17998095
    ## log.Adult.Mortality:CountrySri Lanka                                                             96.78875947
    ## log.Adult.Mortality:CountrySudan                                                                134.43330718
    ## log.Adult.Mortality:CountrySuriname                                                              91.11977194
    ## log.Adult.Mortality:CountrySwaziland                                                             94.73232043
    ## log.Adult.Mortality:CountrySweden                                                                91.05135600
    ## log.Adult.Mortality:CountrySwitzerland                                                           91.01877869
    ## log.Adult.Mortality:CountrySyrian Arab Republic                                                  91.78261521
    ## log.Adult.Mortality:CountryTajikistan                                                           121.59524923
    ## log.Adult.Mortality:CountryThailand                                                              93.33911034
    ## log.Adult.Mortality:CountryThe former Yugoslav republic of Macedonia                             91.06131609
    ## log.Adult.Mortality:CountryTimor-Leste                                                           95.96439899
    ## log.Adult.Mortality:CountryTogo                                                                 142.02742313
    ## log.Adult.Mortality:CountryTonga                                                                 91.37851933
    ## log.Adult.Mortality:CountryTrinidad and Tobago                                                   91.20607060
    ## log.Adult.Mortality:CountryTunisia                                                              168.61553170
    ## log.Adult.Mortality:CountryTurkey                                                               110.74358552
    ## log.Adult.Mortality:CountryTurkmenistan                                                         117.57075473
    ## log.Adult.Mortality:CountryUganda                                                                92.63513888
    ## log.Adult.Mortality:CountryUkraine                                                               95.78242590
    ## log.Adult.Mortality:CountryUnited Arab Emirates                                                  91.04346077
    ## log.Adult.Mortality:CountryUnited Kingdom of Great Britain and Northern Ireland                 124.62492730
    ## log.Adult.Mortality:CountryUnited Republic of Tanzania                                          131.10496457
    ## log.Adult.Mortality:CountryUnited States of America                                             274.20712645
    ## log.Adult.Mortality:CountryUruguay                                                               98.27238956
    ## log.Adult.Mortality:CountryUzbekistan                                                            93.54485114
    ## log.Adult.Mortality:CountryVanuatu                                                               91.03930669
    ## log.Adult.Mortality:CountryVenezuela                                                           1162.27075190
    ## log.Adult.Mortality:CountryViet Nam                                                             490.69456246
    ## log.Adult.Mortality:CountryYemen                                                                260.04674324
    ## log.Adult.Mortality:CountryZambia                                                               102.94475906
    ## log.Adult.Mortality:CountryZimbabwe                                                              92.91224809
    ## log.Adult.Mortality:infant.deaths:CountryAfghanistan                                              0.93337894
    ## log.Adult.Mortality:infant.deaths:CountryAlbania                                                 32.10704189
    ## log.Adult.Mortality:infant.deaths:CountryAlgeria                                                  6.06622659
    ## log.Adult.Mortality:infant.deaths:CountryAngola                                                   0.69381649
    ## log.Adult.Mortality:infant.deaths:CountryAntigua and Barbuda                                              NA
    ## log.Adult.Mortality:infant.deaths:CountryArgentina                                                5.22398459
    ## log.Adult.Mortality:infant.deaths:CountryArmenia                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryAustralia                                                        NA
    ## log.Adult.Mortality:infant.deaths:CountryAustria                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryAzerbaijan                                               5.26872861
    ## log.Adult.Mortality:infant.deaths:CountryBahamas                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryBahrain                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryBangladesh                                               0.11803489
    ## log.Adult.Mortality:infant.deaths:CountryBarbados                                                         NA
    ## log.Adult.Mortality:infant.deaths:CountryBelarus                                                 11.02891224
    ## log.Adult.Mortality:infant.deaths:CountryBelgium                                                 17.88187002
    ## log.Adult.Mortality:infant.deaths:CountryBelize                                                           NA
    ## log.Adult.Mortality:infant.deaths:CountryBenin                                                            NA
    ## log.Adult.Mortality:infant.deaths:CountryBhutan                                                  27.78445300
    ## log.Adult.Mortality:infant.deaths:CountryBolivia                                                  4.03490821
    ## log.Adult.Mortality:infant.deaths:CountryBosnia and Herzegovina                                           NA
    ## log.Adult.Mortality:infant.deaths:CountryBotswana                                                         NA
    ## log.Adult.Mortality:infant.deaths:CountryBrazil                                                   0.28937245
    ## log.Adult.Mortality:infant.deaths:CountryBrunei Darussalam                                                NA
    ## log.Adult.Mortality:infant.deaths:CountryBulgaria                                                         NA
    ## log.Adult.Mortality:infant.deaths:CountryBurkina Faso                                             2.65610591
    ## log.Adult.Mortality:infant.deaths:CountryBurundi                                                  5.05965533
    ## log.Adult.Mortality:infant.deaths:CountryCabo Verde                                                       NA
    ## log.Adult.Mortality:infant.deaths:CountryCambodia                                                 0.76037498
    ## log.Adult.Mortality:infant.deaths:CountryCameroon                                                 3.15278621
    ## log.Adult.Mortality:infant.deaths:CountryCanada                                                           NA
    ## log.Adult.Mortality:infant.deaths:CountryCentral African Republic                                 7.08505350
    ## log.Adult.Mortality:infant.deaths:CountryChad                                                    90.80212381
    ## log.Adult.Mortality:infant.deaths:CountryChile                                                            NA
    ## log.Adult.Mortality:infant.deaths:CountryChina                                                    0.04198259
    ## log.Adult.Mortality:infant.deaths:CountryColombia                                                 3.82565104
    ## log.Adult.Mortality:infant.deaths:CountryComoros                                                 13.32496632
    ## log.Adult.Mortality:infant.deaths:CountryCongo                                                    7.71798705
    ## log.Adult.Mortality:infant.deaths:CountryCosta Rica                                                       NA
    ## log.Adult.Mortality:infant.deaths:CountryCroatia                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryCuba                                                             NA
    ## log.Adult.Mortality:infant.deaths:CountryCyprus                                                           NA
    ## log.Adult.Mortality:infant.deaths:CountryCzechia                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryDemocratic People's Republic of Korea                    1.23337009
    ## log.Adult.Mortality:infant.deaths:CountryDemocratic Republic of the Congo                         1.94994627
    ## log.Adult.Mortality:infant.deaths:CountryDenmark                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryDjibouti                                                31.46377167
    ## log.Adult.Mortality:infant.deaths:CountryDominican Republic                                      37.11807953
    ## log.Adult.Mortality:infant.deaths:CountryEcuador                                                  5.09879571
    ## log.Adult.Mortality:infant.deaths:CountryEgypt                                                    4.52071171
    ## log.Adult.Mortality:infant.deaths:CountryEl Salvador                                             13.73541827
    ## log.Adult.Mortality:infant.deaths:CountryEquatorial Guinea                                                NA
    ## log.Adult.Mortality:infant.deaths:CountryEritrea                                                  6.60548142
    ## log.Adult.Mortality:infant.deaths:CountryEstonia                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryEthiopia                                                 0.11076952
    ## log.Adult.Mortality:infant.deaths:CountryFiji                                                             NA
    ## log.Adult.Mortality:infant.deaths:CountryFinland                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryFrance                                                           NA
    ## log.Adult.Mortality:infant.deaths:CountryGabon                                                            NA
    ## log.Adult.Mortality:infant.deaths:CountryGambia                                                           NA
    ## log.Adult.Mortality:infant.deaths:CountryGeorgia                                                 13.82503561
    ## log.Adult.Mortality:infant.deaths:CountryGermany                                                 14.23680131
    ## log.Adult.Mortality:infant.deaths:CountryGhana                                                    6.32197246
    ## log.Adult.Mortality:infant.deaths:CountryGreece                                                  47.59358054
    ## log.Adult.Mortality:infant.deaths:CountryGrenada                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryGuatemala                                                2.92784224
    ## log.Adult.Mortality:infant.deaths:CountryGuinea                                                   2.22014590
    ## log.Adult.Mortality:infant.deaths:CountryGuinea-Bissau                                           31.22424563
    ## log.Adult.Mortality:infant.deaths:CountryGuyana                                                  39.35825780
    ## log.Adult.Mortality:infant.deaths:CountryHaiti                                                    1.27589376
    ## log.Adult.Mortality:infant.deaths:CountryHonduras                                                 6.97290669
    ## log.Adult.Mortality:infant.deaths:CountryHungary                                                 14.78277193
    ## log.Adult.Mortality:infant.deaths:CountryIceland                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryIndia                                                    0.03343861
    ## log.Adult.Mortality:infant.deaths:CountryIndonesia                                                0.56519343
    ## log.Adult.Mortality:infant.deaths:CountryIran                                                     0.67052524
    ## log.Adult.Mortality:infant.deaths:CountryIraq                                                     4.94170356
    ## log.Adult.Mortality:infant.deaths:CountryIreland                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryIsrael                                                           NA
    ## log.Adult.Mortality:infant.deaths:CountryItaly                                                            NA
    ## log.Adult.Mortality:infant.deaths:CountryIvory Coast                                              3.98626355
    ## log.Adult.Mortality:infant.deaths:CountryJamaica                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryJapan                                                    9.79634422
    ## log.Adult.Mortality:infant.deaths:CountryJordan                                                           NA
    ## log.Adult.Mortality:infant.deaths:CountryKazakhstan                                               2.56775778
    ## log.Adult.Mortality:infant.deaths:CountryKenya                                                    0.33710345
    ## log.Adult.Mortality:infant.deaths:CountryKiribati                                                         NA
    ## log.Adult.Mortality:infant.deaths:CountryKuwait                                                  27.32101133
    ## log.Adult.Mortality:infant.deaths:CountryKyrgyzstan                                              17.65532792
    ## log.Adult.Mortality:infant.deaths:CountryLao People's Democratic Republic                         2.90281736
    ## log.Adult.Mortality:infant.deaths:CountryLatvia                                                           NA
    ## log.Adult.Mortality:infant.deaths:CountryLebanon                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryLesotho                                                  9.79823162
    ## log.Adult.Mortality:infant.deaths:CountryLiberia                                                  1.90354179
    ## log.Adult.Mortality:infant.deaths:CountryLibya                                                   62.80915433
    ## log.Adult.Mortality:infant.deaths:CountryLithuania                                                        NA
    ## log.Adult.Mortality:infant.deaths:CountryLuxembourg                                                       NA
    ## log.Adult.Mortality:infant.deaths:CountryMadagascar                                               2.90483214
    ## log.Adult.Mortality:infant.deaths:CountryMalawi                                                   0.32828803
    ## log.Adult.Mortality:infant.deaths:CountryMalaysia                                                17.59833376
    ## log.Adult.Mortality:infant.deaths:CountryMaldives                                                         NA
    ## log.Adult.Mortality:infant.deaths:CountryMali                                                     5.23435273
    ## log.Adult.Mortality:infant.deaths:CountryMalta                                                            NA
    ## log.Adult.Mortality:infant.deaths:CountryMauritania                                             244.27446496
    ## log.Adult.Mortality:infant.deaths:CountryMauritius                                                        NA
    ## log.Adult.Mortality:infant.deaths:CountryMexico                                                   2.17023696
    ## log.Adult.Mortality:infant.deaths:CountryMicronesia (Federated States of)                                 NA
    ## log.Adult.Mortality:infant.deaths:CountryMongolia                                                18.81219097
    ## log.Adult.Mortality:infant.deaths:CountryMontenegro                                                       NA
    ## log.Adult.Mortality:infant.deaths:CountryMorocco                                                  0.57590125
    ## log.Adult.Mortality:infant.deaths:CountryMozambique                                               1.07494031
    ## log.Adult.Mortality:infant.deaths:CountryMyanmar                                                  0.58100016
    ## log.Adult.Mortality:infant.deaths:CountryNamibia                                                 58.20307429
    ## log.Adult.Mortality:infant.deaths:CountryNepal                                                    0.46605166
    ## log.Adult.Mortality:infant.deaths:CountryNetherlands                                                      NA
    ## log.Adult.Mortality:infant.deaths:CountryNew Zealand                                                      NA
    ## log.Adult.Mortality:infant.deaths:CountryNicaragua                                                6.55142513
    ## log.Adult.Mortality:infant.deaths:CountryNiger                                                    7.18192823
    ## log.Adult.Mortality:infant.deaths:CountryNigeria                                                  0.32775059
    ## log.Adult.Mortality:infant.deaths:CountryNorway                                                           NA
    ## log.Adult.Mortality:infant.deaths:CountryOman                                                             NA
    ## log.Adult.Mortality:infant.deaths:CountryPakistan                                                 1.25496848
    ## log.Adult.Mortality:infant.deaths:CountryPanama                                                 474.99804763
    ## log.Adult.Mortality:infant.deaths:CountryPapua New Guinea                                        38.33723686
    ## log.Adult.Mortality:infant.deaths:CountryParaguay                                                12.34007804
    ## log.Adult.Mortality:infant.deaths:CountryPeru                                                     3.24587349
    ## log.Adult.Mortality:infant.deaths:CountryPhilippines                                              2.89169688
    ## log.Adult.Mortality:infant.deaths:CountryPoland                                                  23.03064927
    ## log.Adult.Mortality:infant.deaths:CountryPortugal                                                68.44154121
    ## log.Adult.Mortality:infant.deaths:CountryQatar                                                            NA
    ## log.Adult.Mortality:infant.deaths:CountryRepublic of Korea                                        2.92701271
    ## log.Adult.Mortality:infant.deaths:CountryRepublic of Moldova                                              NA
    ## log.Adult.Mortality:infant.deaths:CountryRomania                                                 10.23427195
    ## log.Adult.Mortality:infant.deaths:CountryRussian Federation                                       2.27355983
    ## log.Adult.Mortality:infant.deaths:CountryRwanda                                                   0.27852130
    ## log.Adult.Mortality:infant.deaths:CountrySaint Lucia                                                      NA
    ## log.Adult.Mortality:infant.deaths:CountrySaint Vincent and the Grenadines                                 NA
    ## log.Adult.Mortality:infant.deaths:CountrySamoa                                                            NA
    ## log.Adult.Mortality:infant.deaths:CountrySao Tome and Principe                                            NA
    ## log.Adult.Mortality:infant.deaths:CountrySaudi Arabia                                             6.25488449
    ## log.Adult.Mortality:infant.deaths:CountrySenegal                                                  3.94621678
    ## log.Adult.Mortality:infant.deaths:CountrySerbia                                                           NA
    ## log.Adult.Mortality:infant.deaths:CountrySeychelles                                                       NA
    ## log.Adult.Mortality:infant.deaths:CountrySierra Leone                                             2.43716310
    ## log.Adult.Mortality:infant.deaths:CountrySingapore                                                        NA
    ## log.Adult.Mortality:infant.deaths:CountrySlovakia                                                         NA
    ## log.Adult.Mortality:infant.deaths:CountrySlovenia                                                         NA
    ## log.Adult.Mortality:infant.deaths:CountrySolomon Islands                                                  NA
    ## log.Adult.Mortality:infant.deaths:CountrySomalia                                                  8.58503963
    ## log.Adult.Mortality:infant.deaths:CountrySouth Africa                                             0.75838315
    ## log.Adult.Mortality:infant.deaths:CountrySouth Sudan                                             12.70519080
    ## log.Adult.Mortality:infant.deaths:CountrySpain                                                   22.04239219
    ## log.Adult.Mortality:infant.deaths:CountrySri Lanka                                                7.95952533
    ## log.Adult.Mortality:infant.deaths:CountrySudan                                                    1.44326564
    ## log.Adult.Mortality:infant.deaths:CountrySuriname                                                         NA
    ## log.Adult.Mortality:infant.deaths:CountrySwaziland                                               10.08524008
    ## log.Adult.Mortality:infant.deaths:CountrySweden                                                           NA
    ## log.Adult.Mortality:infant.deaths:CountrySwitzerland                                                      NA
    ## log.Adult.Mortality:infant.deaths:CountrySyrian Arab Republic                                     1.75776427
    ## log.Adult.Mortality:infant.deaths:CountryTajikistan                                               8.08438453
    ## log.Adult.Mortality:infant.deaths:CountryThailand                                                 1.72506330
    ## log.Adult.Mortality:infant.deaths:CountryThe former Yugoslav republic of Macedonia                        NA
    ## log.Adult.Mortality:infant.deaths:CountryTimor-Leste                                             12.42599499
    ## log.Adult.Mortality:infant.deaths:CountryTogo                                                     7.93348150
    ## log.Adult.Mortality:infant.deaths:CountryTonga                                                            NA
    ## log.Adult.Mortality:infant.deaths:CountryTrinidad and Tobago                                              NA
    ## log.Adult.Mortality:infant.deaths:CountryTunisia                                                 46.07959538
    ## log.Adult.Mortality:infant.deaths:CountryTurkey                                                   1.85695698
    ## log.Adult.Mortality:infant.deaths:CountryTurkmenistan                                            11.42269686
    ## log.Adult.Mortality:infant.deaths:CountryUganda                                                   0.26662013
    ## log.Adult.Mortality:infant.deaths:CountryUkraine                                                  6.30959940
    ## log.Adult.Mortality:infant.deaths:CountryUnited Arab Emirates                                             NA
    ## log.Adult.Mortality:infant.deaths:CountryUnited Kingdom of Great Britain and Northern Ireland    22.13446509
    ## log.Adult.Mortality:infant.deaths:CountryUnited Republic of Tanzania                              1.13115761
    ## log.Adult.Mortality:infant.deaths:CountryUnited States of America                                 9.90454646
    ## log.Adult.Mortality:infant.deaths:CountryUruguay                                                 39.24629535
    ## log.Adult.Mortality:infant.deaths:CountryUzbekistan                                               1.05782436
    ## log.Adult.Mortality:infant.deaths:CountryVanuatu                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryVenezuela                                              128.06404883
    ## log.Adult.Mortality:infant.deaths:CountryViet Nam                                                17.09325813
    ## log.Adult.Mortality:infant.deaths:CountryYemen                                                    6.77996244
    ## log.Adult.Mortality:infant.deaths:CountryZambia                                                   1.76722859
    ## log.Adult.Mortality:infant.deaths:CountryZimbabwe                                                 0.75670554
    ##                                                                                               t value
    ## (Intercept)                                                                                      1.29
    ## log.Adult.Mortality                                                                             -1.14
    ## infant.deaths                                                                                   -1.07
    ## log.GDP                                                                                          0.02
    ## Measles                                                                                         -1.05
    ## HIV_cat                                                                                         -1.68
    ## CountryAlbania                                                                                  -1.04
    ## CountryAlgeria                                                                                  -0.64
    ## CountryAngola                                                                                   -0.43
    ## CountryAntigua and Barbuda                                                                      -1.02
    ## CountryArgentina                                                                                -0.83
    ## CountryArmenia                                                                                  -1.02
    ## CountryAustralia                                                                                -1.06
    ## CountryAustria                                                                                  -0.94
    ## CountryAzerbaijan                                                                               -1.08
    ## CountryBahamas                                                                                  -1.03
    ## CountryBahrain                                                                                  -1.10
    ## CountryBangladesh                                                                               -1.14
    ## CountryBarbados                                                                                 -1.07
    ## CountryBelarus                                                                                  -1.12
    ## CountryBelgium                                                                                  -0.83
    ## CountryBelize                                                                                   -0.96
    ## CountryBenin                                                                                    -0.78
    ## CountryBhutan                                                                                   -0.85
    ## CountryBolivia                                                                                  -0.34
    ## CountryBosnia and Herzegovina                                                                   -1.04
    ## CountryBotswana                                                                                 -0.89
    ## CountryBrazil                                                                                   -0.51
    ## CountryBrunei Darussalam                                                                        -1.00
    ## CountryBulgaria                                                                                 -0.96
    ## CountryBurkina Faso                                                                             -1.60
    ## CountryBurundi                                                                                   0.57
    ## CountryCabo Verde                                                                               -1.09
    ## CountryCambodia                                                                                 -1.13
    ## CountryCameroon                                                                                 -0.34
    ## CountryCanada                                                                                   -0.94
    ## CountryCentral African Republic                                                                 -0.09
    ## CountryChad                                                                                     -0.05
    ## CountryChile                                                                                    -0.80
    ## CountryChina                                                                                    -1.12
    ## CountryColombia                                                                                 -0.78
    ## CountryComoros                                                                                  -0.84
    ## CountryCongo                                                                                    -0.61
    ## CountryCosta Rica                                                                               -0.97
    ## CountryCroatia                                                                                  -1.03
    ## CountryCuba                                                                                     -1.05
    ## CountryCyprus                                                                                   -1.00
    ## CountryCzechia                                                                                  -1.03
    ## CountryDemocratic People's Republic of Korea                                                    -0.86
    ## CountryDemocratic Republic of the Congo                                                          0.12
    ## CountryDenmark                                                                                  -0.96
    ## CountryDjibouti                                                                                 -2.22
    ## CountryDominican Republic                                                                       -1.25
    ## CountryEcuador                                                                                  -1.11
    ## CountryEgypt                                                                                     1.21
    ## CountryEl Salvador                                                                              -1.50
    ## CountryEquatorial Guinea                                                                        -0.84
    ## CountryEritrea                                                                                  -1.94
    ## CountryEstonia                                                                                  -1.09
    ## CountryEthiopia                                                                                 -1.05
    ## CountryFiji                                                                                     -1.01
    ## CountryFinland                                                                                  -0.92
    ## CountryFrance                                                                                   -1.00
    ## CountryGabon                                                                                    -0.97
    ## CountryGambia                                                                                   -0.36
    ## CountryGeorgia                                                                                  -1.25
    ## CountryGermany                                                                                  -0.02
    ## CountryGhana                                                                                    -1.86
    ## CountryGreece                                                                                   -0.77
    ## CountryGrenada                                                                                  -1.19
    ## CountryGuatemala                                                                                -2.63
    ## CountryGuinea                                                                                   -0.96
    ## CountryGuinea-Bissau                                                                             1.62
    ## CountryGuyana                                                                                   -1.04
    ## CountryHaiti                                                                                    -1.62
    ## CountryHonduras                                                                                 -0.87
    ## CountryHungary                                                                                  -1.04
    ## CountryIceland                                                                                  -1.10
    ## CountryIndia                                                                                    -0.77
    ## CountryIndonesia                                                                                -1.10
    ## CountryIran                                                                                     -0.91
    ## CountryIraq                                                                                     -2.60
    ## CountryIreland                                                                                  -0.95
    ## CountryIsrael                                                                                   -1.05
    ## CountryItaly                                                                                    -1.09
    ## CountryIvory Coast                                                                               0.40
    ## CountryJamaica                                                                                  -1.02
    ## CountryJapan                                                                                    -1.09
    ## CountryJordan                                                                                   -1.00
    ## CountryKazakhstan                                                                               -1.04
    ## CountryKenya                                                                                    -1.20
    ## CountryKiribati                                                                                 -0.93
    ## CountryKuwait                                                                                   -1.04
    ## CountryKyrgyzstan                                                                               -1.56
    ## CountryLao People's Democratic Republic                                                         -1.02
    ## CountryLatvia                                                                                   -1.09
    ## CountryLebanon                                                                                  -1.04
    ## CountryLesotho                                                                                  -0.39
    ## CountryLiberia                                                                                  -0.93
    ## CountryLibya                                                                                     1.36
    ## CountryLithuania                                                                                -1.16
    ## CountryLuxembourg                                                                               -1.03
    ## CountryMadagascar                                                                               -1.75
    ## CountryMalawi                                                                                   -1.12
    ## CountryMalaysia                                                                                 -0.85
    ## CountryMaldives                                                                                 -1.09
    ## CountryMali                                                                                      0.47
    ## CountryMalta                                                                                    -0.96
    ## CountryMauritania                                                                                1.03
    ## CountryMauritius                                                                                -0.94
    ## CountryMexico                                                                                   -0.62
    ## CountryMicronesia (Federated States of)                                                         -0.95
    ## CountryMongolia                                                                                 -1.13
    ## CountryMontenegro                                                                               -1.04
    ## CountryMorocco                                                                                  -1.10
    ## CountryMozambique                                                                                0.78
    ## CountryMyanmar                                                                                  -1.12
    ## CountryNamibia                                                                                  -0.66
    ## CountryNepal                                                                                    -1.09
    ## CountryNetherlands                                                                              -0.99
    ## CountryNew Zealand                                                                              -0.93
    ## CountryNicaragua                                                                                -0.64
    ## CountryNiger                                                                                     3.54
    ## CountryNigeria                                                                                  -1.99
    ## CountryNorway                                                                                   -1.02
    ## CountryOman                                                                                     -1.02
    ## CountryPakistan                                                                                 -0.49
    ## CountryPanama                                                                                   -0.50
    ## CountryPapua New Guinea                                                                         -0.34
    ## CountryParaguay                                                                                 -0.38
    ## CountryPeru                                                                                     -1.12
    ## CountryPhilippines                                                                              -0.58
    ## CountryPoland                                                                                   -0.95
    ## CountryPortugal                                                                                 -0.86
    ## CountryQatar                                                                                    -1.03
    ## CountryRepublic of Korea                                                                        -1.00
    ## CountryRepublic of Moldova                                                                      -0.85
    ## CountryRomania                                                                                  -0.30
    ## CountryRussian Federation                                                                       -0.42
    ## CountryRwanda                                                                                   -1.05
    ## CountrySaint Lucia                                                                              -1.00
    ## CountrySaint Vincent and the Grenadines                                                         -1.43
    ## CountrySamoa                                                                                    -1.17
    ## CountrySao Tome and Principe                                                                    -0.88
    ## CountrySaudi Arabia                                                                             -0.85
    ## CountrySenegal                                                                                  -0.90
    ## CountrySerbia                                                                                   -1.02
    ## CountrySeychelles                                                                               -1.03
    ## CountrySierra Leone                                                                              0.77
    ## CountrySingapore                                                                                -1.03
    ## CountrySlovakia                                                                                 -1.04
    ## CountrySlovenia                                                                                 -0.95
    ## CountrySolomon Islands                                                                          -1.07
    ## CountrySomalia                                                                                  -1.56
    ## CountrySouth Africa                                                                             -0.16
    ## CountrySouth Sudan                                                                               0.57
    ## CountrySpain                                                                                    -1.09
    ## CountrySri Lanka                                                                                -1.10
    ## CountrySudan                                                                                    -1.67
    ## CountrySuriname                                                                                 -0.87
    ## CountrySwaziland                                                                                -1.05
    ## CountrySweden                                                                                   -1.08
    ## CountrySwitzerland                                                                              -1.09
    ## CountrySyrian Arab Republic                                                                     -1.04
    ## CountryTajikistan                                                                               -0.28
    ## CountryThailand                                                                                 -1.04
    ## CountryThe former Yugoslav republic of Macedonia                                                -1.04
    ## CountryTimor-Leste                                                                              -0.92
    ## CountryTogo                                                                                     -0.31
    ## CountryTonga                                                                                    -1.02
    ## CountryTrinidad and Tobago                                                                      -0.81
    ## CountryTunisia                                                                                  -0.84
    ## CountryTurkey                                                                                   -0.48
    ## CountryTurkmenistan                                                                             -0.89
    ## CountryUganda                                                                                   -0.71
    ## CountryUkraine                                                                                  -1.64
    ## CountryUnited Arab Emirates                                                                     -1.06
    ## CountryUnited Kingdom of Great Britain and Northern Ireland                                     -1.09
    ## CountryUnited Republic of Tanzania                                                               1.40
    ## CountryUnited States of America                                                                 -0.88
    ## CountryUruguay                                                                                  -0.97
    ## CountryUzbekistan                                                                               -1.10
    ## CountryVanuatu                                                                                  -1.02
    ## CountryVenezuela                                                                                -0.12
    ## CountryViet Nam                                                                                 -0.27
    ## CountryYemen                                                                                     0.85
    ## CountryZambia                                                                                   -1.12
    ## CountryZimbabwe                                                                                 -0.43
    ## infant.deaths:CountryAlbania                                                                     0.46
    ## infant.deaths:CountryAlgeria                                                                     0.17
    ## infant.deaths:CountryAngola                                                                      0.53
    ## infant.deaths:CountryAntigua and Barbuda                                                           NA
    ## infant.deaths:CountryArgentina                                                                   0.10
    ## infant.deaths:CountryArmenia                                                                       NA
    ## infant.deaths:CountryAustralia                                                                     NA
    ## infant.deaths:CountryAustria                                                                       NA
    ## infant.deaths:CountryAzerbaijan                                                                  0.52
    ## infant.deaths:CountryBahamas                                                                       NA
    ## infant.deaths:CountryBahrain                                                                       NA
    ## infant.deaths:CountryBangladesh                                                                  1.03
    ## infant.deaths:CountryBarbados                                                                      NA
    ## infant.deaths:CountryBelarus                                                                     1.68
    ## infant.deaths:CountryBelgium                                                                    -1.14
    ## infant.deaths:CountryBelize                                                                        NA
    ## infant.deaths:CountryBenin                                                                         NA
    ## infant.deaths:CountryBhutan                                                                      0.07
    ## infant.deaths:CountryBolivia                                                                     0.64
    ## infant.deaths:CountryBosnia and Herzegovina                                                        NA
    ## infant.deaths:CountryBotswana                                                                      NA
    ## infant.deaths:CountryBrazil                                                                      0.58
    ## infant.deaths:CountryBrunei Darussalam                                                             NA
    ## infant.deaths:CountryBulgaria                                                                    1.00
    ## infant.deaths:CountryBurkina Faso                                                                1.50
    ## infant.deaths:CountryBurundi                                                                    -0.99
    ## infant.deaths:CountryCabo Verde                                                                    NA
    ## infant.deaths:CountryCambodia                                                                    1.05
    ## infant.deaths:CountryCameroon                                                                    0.21
    ## infant.deaths:CountryCanada                                                                        NA
    ## infant.deaths:CountryCentral African Republic                                                   -0.40
    ## infant.deaths:CountryChad                                                                        0.05
    ## infant.deaths:CountryChile                                                                         NA
    ## infant.deaths:CountryChina                                                                       1.07
    ## infant.deaths:CountryColombia                                                                    0.10
    ## infant.deaths:CountryComoros                                                                    -0.08
    ## infant.deaths:CountryCongo                                                                      -0.38
    ## infant.deaths:CountryCosta Rica                                                                    NA
    ## infant.deaths:CountryCroatia                                                                       NA
    ## infant.deaths:CountryCuba                                                                          NA
    ## infant.deaths:CountryCyprus                                                                        NA
    ## infant.deaths:CountryCzechia                                                                       NA
    ## infant.deaths:CountryDemocratic People's Republic of Korea                                      -0.02
    ## infant.deaths:CountryDemocratic Republic of the Congo                                            0.18
    ## infant.deaths:CountryDenmark                                                                       NA
    ## infant.deaths:CountryDjibouti                                                                    2.20
    ## infant.deaths:CountryDominican Republic                                                          0.92
    ## infant.deaths:CountryEcuador                                                                     0.52
    ## infant.deaths:CountryEgypt                                                                      -1.07
    ## infant.deaths:CountryEl Salvador                                                                 2.18
    ## infant.deaths:CountryEquatorial Guinea                                                             NA
    ## infant.deaths:CountryEritrea                                                                     2.59
    ## infant.deaths:CountryEstonia                                                                       NA
    ## infant.deaths:CountryEthiopia                                                                    1.05
    ## infant.deaths:CountryFiji                                                                          NA
    ## infant.deaths:CountryFinland                                                                       NA
    ## infant.deaths:CountryFrance                                                                      1.55
    ## infant.deaths:CountryGabon                                                                         NA
    ## infant.deaths:CountryGambia                                                                        NA
    ## infant.deaths:CountryGeorgia                                                                     0.75
    ## infant.deaths:CountryGermany                                                                    -2.49
    ## infant.deaths:CountryGhana                                                                       1.93
    ## infant.deaths:CountryGreece                                                                     -0.41
    ## infant.deaths:CountryGrenada                                                                       NA
    ## infant.deaths:CountryGuatemala                                                                   5.66
    ## infant.deaths:CountryGuinea                                                                      0.61
    ## infant.deaths:CountryGuinea-Bissau                                                              -2.63
    ## infant.deaths:CountryGuyana                                                                      0.13
    ## infant.deaths:CountryHaiti                                                                       2.73
    ## infant.deaths:CountryHonduras                                                                    0.21
    ## infant.deaths:CountryHungary                                                                     0.58
    ## infant.deaths:CountryIceland                                                                       NA
    ## infant.deaths:CountryIndia                                                                       1.07
    ## infant.deaths:CountryIndonesia                                                                   1.18
    ## infant.deaths:CountryIran                                                                        0.25
    ## infant.deaths:CountryIraq                                                                        2.65
    ## infant.deaths:CountryIreland                                                                       NA
    ## infant.deaths:CountryIsrael                                                                      1.04
    ## infant.deaths:CountryItaly                                                                       0.53
    ## infant.deaths:CountryIvory Coast                                                                -0.52
    ## infant.deaths:CountryJamaica                                                                       NA
    ## infant.deaths:CountryJapan                                                                       0.40
    ## infant.deaths:CountryJordan                                                                        NA
    ## infant.deaths:CountryKazakhstan                                                                  0.75
    ## infant.deaths:CountryKenya                                                                       1.31
    ## infant.deaths:CountryKiribati                                                                      NA
    ## infant.deaths:CountryKuwait                                                                      0.21
    ## infant.deaths:CountryKyrgyzstan                                                                  1.33
    ## infant.deaths:CountryLao People's Democratic Republic                                            0.86
    ## infant.deaths:CountryLatvia                                                                        NA
    ## infant.deaths:CountryLebanon                                                                       NA
    ## infant.deaths:CountryLesotho                                                                    -0.71
    ## infant.deaths:CountryLiberia                                                                     0.55
    ## infant.deaths:CountryLibya                                                                      -2.60
    ## infant.deaths:CountryLithuania                                                                     NA
    ## infant.deaths:CountryLuxembourg                                                                    NA
    ## infant.deaths:CountryMadagascar                                                                  1.43
    ## infant.deaths:CountryMalawi                                                                      1.38
    ## infant.deaths:CountryMalaysia                                                                    0.13
    ## infant.deaths:CountryMaldives                                                                      NA
    ## infant.deaths:CountryMali                                                                       -0.59
    ## infant.deaths:CountryMalta                                                                         NA
    ## infant.deaths:CountryMauritania                                                                 -1.09
    ## infant.deaths:CountryMauritius                                                                     NA
    ## infant.deaths:CountryMexico                                                                      0.19
    ## infant.deaths:CountryMicronesia (Federated States of)                                              NA
    ## infant.deaths:CountryMongolia                                                                    1.15
    ## infant.deaths:CountryMontenegro                                                                    NA
    ## infant.deaths:CountryMorocco                                                                     0.75
    ## infant.deaths:CountryMozambique                                                                 -1.44
    ## infant.deaths:CountryMyanmar                                                                     1.22
    ## infant.deaths:CountryNamibia                                                                     0.30
    ## infant.deaths:CountryNepal                                                                       1.18
    ## infant.deaths:CountryNetherlands                                                                   NA
    ## infant.deaths:CountryNew Zealand                                                                   NA
    ## infant.deaths:CountryNicaragua                                                                  -1.57
    ## infant.deaths:CountryNiger                                                                      -3.61
    ## infant.deaths:CountryNigeria                                                                     1.77
    ## infant.deaths:CountryNorway                                                                        NA
    ## infant.deaths:CountryOman                                                                          NA
    ## infant.deaths:CountryPakistan                                                                    0.89
    ## infant.deaths:CountryPanama                                                                      0.30
    ## infant.deaths:CountryPapua New Guinea                                                            0.20
    ## infant.deaths:CountryParaguay                                                                   -1.57
    ## infant.deaths:CountryPeru                                                                        0.02
    ## infant.deaths:CountryPhilippines                                                                 0.39
    ## infant.deaths:CountryPoland                                                                      0.11
    ## infant.deaths:CountryPortugal                                                                   -0.11
    ## infant.deaths:CountryQatar                                                                         NA
    ## infant.deaths:CountryRepublic of Korea                                                           1.09
    ## infant.deaths:CountryRepublic of Moldova                                                           NA
    ## infant.deaths:CountryRomania                                                                    -3.31
    ## infant.deaths:CountryRussian Federation                                                         -0.98
    ## infant.deaths:CountryRwanda                                                                      1.71
    ## infant.deaths:CountrySaint Lucia                                                                   NA
    ## infant.deaths:CountrySaint Vincent and the Grenadines                                              NA
    ## infant.deaths:CountrySamoa                                                                         NA
    ## infant.deaths:CountrySao Tome and Principe                                                         NA
    ## infant.deaths:CountrySaudi Arabia                                                                0.02
    ## infant.deaths:CountrySenegal                                                                     0.41
    ## infant.deaths:CountrySerbia                                                                        NA
    ## infant.deaths:CountrySeychelles                                                                    NA
    ## infant.deaths:CountrySierra Leone                                                               -1.76
    ## infant.deaths:CountrySingapore                                                                     NA
    ## infant.deaths:CountrySlovakia                                                                      NA
    ## infant.deaths:CountrySlovenia                                                                      NA
    ## infant.deaths:CountrySolomon Islands                                                               NA
    ## infant.deaths:CountrySomalia                                                                     1.51
    ## infant.deaths:CountrySouth Africa                                                               -0.41
    ## infant.deaths:CountrySouth Sudan                                                                -0.76
    ## infant.deaths:CountrySpain                                                                       0.54
    ## infant.deaths:CountrySri Lanka                                                                   0.55
    ## infant.deaths:CountrySudan                                                                       1.68
    ## infant.deaths:CountrySuriname                                                                      NA
    ## infant.deaths:CountrySwaziland                                                                   0.92
    ## infant.deaths:CountrySweden                                                                        NA
    ## infant.deaths:CountrySwitzerland                                                                   NA
    ## infant.deaths:CountrySyrian Arab Republic                                                        0.62
    ## infant.deaths:CountryTajikistan                                                                 -0.55
    ## infant.deaths:CountryThailand                                                                    0.81
    ## infant.deaths:CountryThe former Yugoslav republic of Macedonia                                     NA
    ## infant.deaths:CountryTimor-Leste                                                                 0.08
    ## infant.deaths:CountryTogo                                                                       -0.19
    ## infant.deaths:CountryTonga                                                                         NA
    ## infant.deaths:CountryTrinidad and Tobago                                                           NA
    ## infant.deaths:CountryTunisia                                                                     0.28
    ## infant.deaths:CountryTurkey                                                                      0.21
    ## infant.deaths:CountryTurkmenistan                                                                0.29
    ## infant.deaths:CountryUganda                                                                      1.12
    ## infant.deaths:CountryUkraine                                                                     2.56
    ## infant.deaths:CountryUnited Arab Emirates                                                          NA
    ## infant.deaths:CountryUnited Kingdom of Great Britain and Northern Ireland                        0.56
    ## infant.deaths:CountryUnited Republic of Tanzania                                                -1.46
    ## infant.deaths:CountryUnited States of America                                                    0.63
    ## infant.deaths:CountryUruguay                                                                     0.02
    ## infant.deaths:CountryUzbekistan                                                                  0.72
    ## infant.deaths:CountryVanuatu                                                                       NA
    ## infant.deaths:CountryVenezuela                                                                   0.04
    ## infant.deaths:CountryViet Nam                                                                    0.13
    ## infant.deaths:CountryYemen                                                                      -1.01
    ## infant.deaths:CountryZambia                                                                      0.80
    ## infant.deaths:CountryZimbabwe                                                                   -0.45
    ## log.Adult.Mortality:CountryAlbania                                                               1.03
    ## log.Adult.Mortality:CountryAlgeria                                                               0.58
    ## log.Adult.Mortality:CountryAngola                                                                0.41
    ## log.Adult.Mortality:CountryAntigua and Barbuda                                                   1.02
    ## log.Adult.Mortality:CountryArgentina                                                             0.78
    ## log.Adult.Mortality:CountryArmenia                                                               1.02
    ## log.Adult.Mortality:CountryAustralia                                                             1.06
    ## log.Adult.Mortality:CountryAustria                                                               0.92
    ## log.Adult.Mortality:CountryAzerbaijan                                                            1.08
    ## log.Adult.Mortality:CountryBahamas                                                               1.03
    ## log.Adult.Mortality:CountryBahrain                                                               1.10
    ## log.Adult.Mortality:CountryBangladesh                                                            1.12
    ## log.Adult.Mortality:CountryBarbados                                                              1.07
    ## log.Adult.Mortality:CountryBelarus                                                               1.12
    ## log.Adult.Mortality:CountryBelgium                                                               0.78
    ## log.Adult.Mortality:CountryBelize                                                                0.95
    ## log.Adult.Mortality:CountryBenin                                                                 0.84
    ## log.Adult.Mortality:CountryBhutan                                                                0.85
    ## log.Adult.Mortality:CountryBolivia                                                               0.29
    ## log.Adult.Mortality:CountryBosnia and Herzegovina                                                1.04
    ## log.Adult.Mortality:CountryBotswana                                                              0.89
    ## log.Adult.Mortality:CountryBrazil                                                                0.47
    ## log.Adult.Mortality:CountryBrunei Darussalam                                                     0.99
    ## log.Adult.Mortality:CountryBulgaria                                                              0.94
    ## log.Adult.Mortality:CountryBurkina Faso                                                          1.61
    ## log.Adult.Mortality:CountryBurundi                                                              -0.55
    ## log.Adult.Mortality:CountryCabo Verde                                                            1.09
    ## log.Adult.Mortality:CountryCambodia                                                              1.12
    ## log.Adult.Mortality:CountryCameroon                                                              0.37
    ## log.Adult.Mortality:CountryCanada                                                                0.91
    ## log.Adult.Mortality:CountryCentral African Republic                                              0.18
    ## log.Adult.Mortality:CountryChad                                                                  0.06
    ## log.Adult.Mortality:CountryChile                                                                 0.76
    ## log.Adult.Mortality:CountryChina                                                                 1.11
    ## log.Adult.Mortality:CountryColombia                                                              0.75
    ## log.Adult.Mortality:CountryComoros                                                               0.82
    ## log.Adult.Mortality:CountryCongo                                                                 0.70
    ## log.Adult.Mortality:CountryCosta Rica                                                            0.95
    ## log.Adult.Mortality:CountryCroatia                                                               1.03
    ## log.Adult.Mortality:CountryCuba                                                                  1.05
    ## log.Adult.Mortality:CountryCyprus                                                                0.99
    ## log.Adult.Mortality:CountryCzechia                                                               1.03
    ## log.Adult.Mortality:CountryDemocratic People's Republic of Korea                                 0.83
    ## log.Adult.Mortality:CountryDemocratic Republic of the Congo                                     -0.12
    ## log.Adult.Mortality:CountryDenmark                                                               0.94
    ## log.Adult.Mortality:CountryDjibouti                                                              2.19
    ## log.Adult.Mortality:CountryDominican Republic                                                    1.26
    ## log.Adult.Mortality:CountryEcuador                                                               1.10
    ## log.Adult.Mortality:CountryEgypt                                                                -1.21
    ## log.Adult.Mortality:CountryEl Salvador                                                           1.50
    ## log.Adult.Mortality:CountryEquatorial Guinea                                                     0.84
    ## log.Adult.Mortality:CountryEritrea                                                               1.91
    ## log.Adult.Mortality:CountryEstonia                                                               1.09
    ## log.Adult.Mortality:CountryEthiopia                                                              1.06
    ## log.Adult.Mortality:CountryFiji                                                                  1.00
    ## log.Adult.Mortality:CountryFinland                                                               0.89
    ## log.Adult.Mortality:CountryFrance                                                                0.98
    ## log.Adult.Mortality:CountryGabon                                                                 0.96
    ## log.Adult.Mortality:CountryGambia                                                                0.39
    ## log.Adult.Mortality:CountryGeorgia                                                               1.26
    ## log.Adult.Mortality:CountryGermany                                                              -0.20
    ## log.Adult.Mortality:CountryGhana                                                                 1.80
    ## log.Adult.Mortality:CountryGreece                                                                0.70
    ## log.Adult.Mortality:CountryGrenada                                                               1.20
    ## log.Adult.Mortality:CountryGuatemala                                                             2.61
    ## log.Adult.Mortality:CountryGuinea                                                                0.97
    ## log.Adult.Mortality:CountryGuinea-Bissau                                                        -1.56
    ## log.Adult.Mortality:CountryGuyana                                                                1.03
    ## log.Adult.Mortality:CountryHaiti                                                                 1.60
    ## log.Adult.Mortality:CountryHonduras                                                              0.85
    ## log.Adult.Mortality:CountryHungary                                                               1.04
    ## log.Adult.Mortality:CountryIceland                                                               1.11
    ## log.Adult.Mortality:CountryIndia                                                                 0.74
    ## log.Adult.Mortality:CountryIndonesia                                                             1.09
    ## log.Adult.Mortality:CountryIran                                                                  0.88
    ## log.Adult.Mortality:CountryIraq                                                                  2.57
    ## log.Adult.Mortality:CountryIreland                                                               0.92
    ## log.Adult.Mortality:CountryIsrael                                                                1.05
    ## log.Adult.Mortality:CountryItaly                                                                 1.11
    ## log.Adult.Mortality:CountryIvory Coast                                                          -0.35
    ## log.Adult.Mortality:CountryJamaica                                                               1.02
    ## log.Adult.Mortality:CountryJapan                                                                 1.10
    ## log.Adult.Mortality:CountryJordan                                                                0.99
    ## log.Adult.Mortality:CountryKazakhstan                                                            1.03
    ## log.Adult.Mortality:CountryKenya                                                                 1.18
    ## log.Adult.Mortality:CountryKiribati                                                              0.91
    ## log.Adult.Mortality:CountryKuwait                                                                1.02
    ## log.Adult.Mortality:CountryKyrgyzstan                                                            1.57
    ## log.Adult.Mortality:CountryLao People's Democratic Republic                                      0.99
    ## log.Adult.Mortality:CountryLatvia                                                                1.10
    ## log.Adult.Mortality:CountryLebanon                                                               1.03
    ## log.Adult.Mortality:CountryLesotho                                                               0.46
    ## log.Adult.Mortality:CountryLiberia                                                               0.93
    ## log.Adult.Mortality:CountryLibya                                                                -1.46
    ## log.Adult.Mortality:CountryLithuania                                                             1.16
    ## log.Adult.Mortality:CountryLuxembourg                                                            1.03
    ## log.Adult.Mortality:CountryMadagascar                                                            1.75
    ## log.Adult.Mortality:CountryMalawi                                                                1.10
    ## log.Adult.Mortality:CountryMalaysia                                                              0.82
    ## log.Adult.Mortality:CountryMaldives                                                              1.08
    ## log.Adult.Mortality:CountryMali                                                                 -0.45
    ## log.Adult.Mortality:CountryMalta                                                                 0.92
    ## log.Adult.Mortality:CountryMauritania                                                           -1.03
    ## log.Adult.Mortality:CountryMauritius                                                             0.92
    ## log.Adult.Mortality:CountryMexico                                                                0.58
    ## log.Adult.Mortality:CountryMicronesia (Federated States of)                                      0.93
    ## log.Adult.Mortality:CountryMongolia                                                              1.11
    ## log.Adult.Mortality:CountryMontenegro                                                            1.03
    ## log.Adult.Mortality:CountryMorocco                                                               1.12
    ## log.Adult.Mortality:CountryMozambique                                                           -0.63
    ## log.Adult.Mortality:CountryMyanmar                                                               1.11
    ## log.Adult.Mortality:CountryNamibia                                                               0.68
    ## log.Adult.Mortality:CountryNepal                                                                 1.08
    ## log.Adult.Mortality:CountryNetherlands                                                           0.97
    ## log.Adult.Mortality:CountryNew Zealand                                                           0.90
    ## log.Adult.Mortality:CountryNicaragua                                                             0.60
    ## log.Adult.Mortality:CountryNiger                                                                -3.51
    ## log.Adult.Mortality:CountryNigeria                                                               1.94
    ## log.Adult.Mortality:CountryNorway                                                                1.02
    ## log.Adult.Mortality:CountryOman                                                                  1.01
    ## log.Adult.Mortality:CountryPakistan                                                              0.48
    ## log.Adult.Mortality:CountryPanama                                                                0.48
    ## log.Adult.Mortality:CountryPapua New Guinea                                                      0.33
    ## log.Adult.Mortality:CountryParaguay                                                              0.33
    ## log.Adult.Mortality:CountryPeru                                                                  1.12
    ## log.Adult.Mortality:CountryPhilippines                                                           0.58
    ## log.Adult.Mortality:CountryPoland                                                                0.94
    ## log.Adult.Mortality:CountryPortugal                                                              0.83
    ## log.Adult.Mortality:CountryQatar                                                                 1.01
    ## log.Adult.Mortality:CountryRepublic of Korea                                                     0.97
    ## log.Adult.Mortality:CountryRepublic of Moldova                                                   0.84
    ## log.Adult.Mortality:CountryRomania                                                               0.25
    ## log.Adult.Mortality:CountryRussian Federation                                                    0.46
    ## log.Adult.Mortality:CountryRwanda                                                                1.02
    ## log.Adult.Mortality:CountrySaint Lucia                                                           0.99
    ## log.Adult.Mortality:CountrySaint Vincent and the Grenadines                                      1.44
    ## log.Adult.Mortality:CountrySamoa                                                                 1.18
    ## log.Adult.Mortality:CountrySao Tome and Principe                                                 0.87
    ## log.Adult.Mortality:CountrySaudi Arabia                                                          0.79
    ## log.Adult.Mortality:CountrySenegal                                                               0.91
    ## log.Adult.Mortality:CountrySerbia                                                                1.00
    ## log.Adult.Mortality:CountrySeychelles                                                            1.02
    ## log.Adult.Mortality:CountrySierra Leone                                                         -0.63
    ## log.Adult.Mortality:CountrySingapore                                                             1.02
    ## log.Adult.Mortality:CountrySlovakia                                                              1.03
    ## log.Adult.Mortality:CountrySlovenia                                                              0.93
    ## log.Adult.Mortality:CountrySolomon Islands                                                       1.06
    ## log.Adult.Mortality:CountrySomalia                                                               1.58
    ## log.Adult.Mortality:CountrySouth Africa                                                          0.26
    ## log.Adult.Mortality:CountrySouth Sudan                                                          -0.54
    ## log.Adult.Mortality:CountrySpain                                                                 1.07
    ## log.Adult.Mortality:CountrySri Lanka                                                             1.10
    ## log.Adult.Mortality:CountrySudan                                                                 1.66
    ## log.Adult.Mortality:CountrySuriname                                                              0.86
    ## log.Adult.Mortality:CountrySwaziland                                                             1.03
    ## log.Adult.Mortality:CountrySweden                                                                1.09
    ## log.Adult.Mortality:CountrySwitzerland                                                           1.10
    ## log.Adult.Mortality:CountrySyrian Arab Republic                                                  1.04
    ## log.Adult.Mortality:CountryTajikistan                                                            0.22
    ## log.Adult.Mortality:CountryThailand                                                              1.03
    ## log.Adult.Mortality:CountryThe former Yugoslav republic of Macedonia                             1.03
    ## log.Adult.Mortality:CountryTimor-Leste                                                           0.89
    ## log.Adult.Mortality:CountryTogo                                                                  0.32
    ## log.Adult.Mortality:CountryTonga                                                                 1.01
    ## log.Adult.Mortality:CountryTrinidad and Tobago                                                   0.80
    ## log.Adult.Mortality:CountryTunisia                                                               0.79
    ## log.Adult.Mortality:CountryTurkey                                                                0.37
    ## log.Adult.Mortality:CountryTurkmenistan                                                          0.88
    ## log.Adult.Mortality:CountryUganda                                                                0.70
    ## log.Adult.Mortality:CountryUkraine                                                               1.64
    ## log.Adult.Mortality:CountryUnited Arab Emirates                                                  1.05
    ## log.Adult.Mortality:CountryUnited Kingdom of Great Britain and Northern Ireland                  1.06
    ## log.Adult.Mortality:CountryUnited Republic of Tanzania                                          -1.23
    ## log.Adult.Mortality:CountryUnited States of America                                              0.85
    ## log.Adult.Mortality:CountryUruguay                                                               0.95
    ## log.Adult.Mortality:CountryUzbekistan                                                            1.09
    ## log.Adult.Mortality:CountryVanuatu                                                               1.01
    ## log.Adult.Mortality:CountryVenezuela                                                             0.12
    ## log.Adult.Mortality:CountryViet Nam                                                              0.25
    ## log.Adult.Mortality:CountryYemen                                                                -0.85
    ## log.Adult.Mortality:CountryZambia                                                                1.15
    ## log.Adult.Mortality:CountryZimbabwe                                                              0.50
    ## log.Adult.Mortality:infant.deaths:CountryAfghanistan                                             1.04
    ## log.Adult.Mortality:infant.deaths:CountryAlbania                                                -0.44
    ## log.Adult.Mortality:infant.deaths:CountryAlgeria                                                 0.02
    ## log.Adult.Mortality:infant.deaths:CountryAngola                                                  0.54
    ## log.Adult.Mortality:infant.deaths:CountryAntigua and Barbuda                                       NA
    ## log.Adult.Mortality:infant.deaths:CountryArgentina                                               0.11
    ## log.Adult.Mortality:infant.deaths:CountryArmenia                                                   NA
    ## log.Adult.Mortality:infant.deaths:CountryAustralia                                                 NA
    ## log.Adult.Mortality:infant.deaths:CountryAustria                                                   NA
    ## log.Adult.Mortality:infant.deaths:CountryAzerbaijan                                             -0.36
    ## log.Adult.Mortality:infant.deaths:CountryBahamas                                                   NA
    ## log.Adult.Mortality:infant.deaths:CountryBahrain                                                   NA
    ## log.Adult.Mortality:infant.deaths:CountryBangladesh                                              0.04
    ## log.Adult.Mortality:infant.deaths:CountryBarbados                                                  NA
    ## log.Adult.Mortality:infant.deaths:CountryBelarus                                                -1.64
    ## log.Adult.Mortality:infant.deaths:CountryBelgium                                                 1.23
    ## log.Adult.Mortality:infant.deaths:CountryBelize                                                    NA
    ## log.Adult.Mortality:infant.deaths:CountryBenin                                                     NA
    ## log.Adult.Mortality:infant.deaths:CountryBhutan                                                 -0.03
    ## log.Adult.Mortality:infant.deaths:CountryBolivia                                                -0.36
    ## log.Adult.Mortality:infant.deaths:CountryBosnia and Herzegovina                                    NA
    ## log.Adult.Mortality:infant.deaths:CountryBotswana                                                  NA
    ## log.Adult.Mortality:infant.deaths:CountryBrazil                                                  1.61
    ## log.Adult.Mortality:infant.deaths:CountryBrunei Darussalam                                         NA
    ## log.Adult.Mortality:infant.deaths:CountryBulgaria                                                  NA
    ## log.Adult.Mortality:infant.deaths:CountryBurkina Faso                                           -1.25
    ## log.Adult.Mortality:infant.deaths:CountryBurundi                                                 1.22
    ## log.Adult.Mortality:infant.deaths:CountryCabo Verde                                                NA
    ## log.Adult.Mortality:infant.deaths:CountryCambodia                                               -0.51
    ## log.Adult.Mortality:infant.deaths:CountryCameroon                                                0.06
    ## log.Adult.Mortality:infant.deaths:CountryCanada                                                    NA
    ## log.Adult.Mortality:infant.deaths:CountryCentral African Republic                                0.48
    ## log.Adult.Mortality:infant.deaths:CountryChad                                                   -0.04
    ## log.Adult.Mortality:infant.deaths:CountryChile                                                     NA
    ## log.Adult.Mortality:infant.deaths:CountryChina                                                  -0.26
    ## log.Adult.Mortality:infant.deaths:CountryColombia                                                0.17
    ## log.Adult.Mortality:infant.deaths:CountryComoros                                                 0.16
    ## log.Adult.Mortality:infant.deaths:CountryCongo                                                   0.41
    ## log.Adult.Mortality:infant.deaths:CountryCosta Rica                                                NA
    ## log.Adult.Mortality:infant.deaths:CountryCroatia                                                   NA
    ## log.Adult.Mortality:infant.deaths:CountryCuba                                                      NA
    ## log.Adult.Mortality:infant.deaths:CountryCyprus                                                    NA
    ## log.Adult.Mortality:infant.deaths:CountryCzechia                                                   NA
    ## log.Adult.Mortality:infant.deaths:CountryDemocratic People's Republic of Korea                   0.85
    ## log.Adult.Mortality:infant.deaths:CountryDemocratic Republic of the Congo                        0.29
    ## log.Adult.Mortality:infant.deaths:CountryDenmark                                                   NA
    ## log.Adult.Mortality:infant.deaths:CountryDjibouti                                               -2.23
    ## log.Adult.Mortality:infant.deaths:CountryDominican Republic                                     -0.91
    ## log.Adult.Mortality:infant.deaths:CountryEcuador                                                -0.32
    ## log.Adult.Mortality:infant.deaths:CountryEgypt                                                   1.30
    ## log.Adult.Mortality:infant.deaths:CountryEl Salvador                                            -2.12
    ## log.Adult.Mortality:infant.deaths:CountryEquatorial Guinea                                         NA
    ## log.Adult.Mortality:infant.deaths:CountryEritrea                                                -2.47
    ## log.Adult.Mortality:infant.deaths:CountryEstonia                                                   NA
    ## log.Adult.Mortality:infant.deaths:CountryEthiopia                                               -0.09
    ## log.Adult.Mortality:infant.deaths:CountryFiji                                                      NA
    ## log.Adult.Mortality:infant.deaths:CountryFinland                                                   NA
    ## log.Adult.Mortality:infant.deaths:CountryFrance                                                    NA
    ## log.Adult.Mortality:infant.deaths:CountryGabon                                                     NA
    ## log.Adult.Mortality:infant.deaths:CountryGambia                                                    NA
    ## log.Adult.Mortality:infant.deaths:CountryGeorgia                                                -0.70
    ## log.Adult.Mortality:infant.deaths:CountryGermany                                                 2.59
    ## log.Adult.Mortality:infant.deaths:CountryGhana                                                  -1.73
    ## log.Adult.Mortality:infant.deaths:CountryGreece                                                  0.43
    ## log.Adult.Mortality:infant.deaths:CountryGrenada                                                   NA
    ## log.Adult.Mortality:infant.deaths:CountryGuatemala                                              -5.62
    ## log.Adult.Mortality:infant.deaths:CountryGuinea                                                 -0.27
    ## log.Adult.Mortality:infant.deaths:CountryGuinea-Bissau                                           2.64
    ## log.Adult.Mortality:infant.deaths:CountryGuyana                                                 -0.11
    ## log.Adult.Mortality:infant.deaths:CountryHaiti                                                  -2.55
    ## log.Adult.Mortality:infant.deaths:CountryHonduras                                               -0.06
    ## log.Adult.Mortality:infant.deaths:CountryHungary                                                -0.50
    ## log.Adult.Mortality:infant.deaths:CountryIceland                                                   NA
    ## log.Adult.Mortality:infant.deaths:CountryIndia                                                  -0.21
    ## log.Adult.Mortality:infant.deaths:CountryIndonesia                                              -0.54
    ## log.Adult.Mortality:infant.deaths:CountryIran                                                    1.21
    ## log.Adult.Mortality:infant.deaths:CountryIraq                                                   -2.44
    ## log.Adult.Mortality:infant.deaths:CountryIreland                                                   NA
    ## log.Adult.Mortality:infant.deaths:CountryIsrael                                                    NA
    ## log.Adult.Mortality:infant.deaths:CountryItaly                                                     NA
    ## log.Adult.Mortality:infant.deaths:CountryIvory Coast                                             0.73
    ## log.Adult.Mortality:infant.deaths:CountryJamaica                                                   NA
    ## log.Adult.Mortality:infant.deaths:CountryJapan                                                  -0.28
    ## log.Adult.Mortality:infant.deaths:CountryJordan                                                    NA
    ## log.Adult.Mortality:infant.deaths:CountryKazakhstan                                             -0.42
    ## log.Adult.Mortality:infant.deaths:CountryKenya                                                  -0.93
    ## log.Adult.Mortality:infant.deaths:CountryKiribati                                                  NA
    ## log.Adult.Mortality:infant.deaths:CountryKuwait                                                 -0.17
    ## log.Adult.Mortality:infant.deaths:CountryKyrgyzstan                                             -1.30
    ## log.Adult.Mortality:infant.deaths:CountryLao People's Democratic Republic                       -0.55
    ## log.Adult.Mortality:infant.deaths:CountryLatvia                                                    NA
    ## log.Adult.Mortality:infant.deaths:CountryLebanon                                                   NA
    ## log.Adult.Mortality:infant.deaths:CountryLesotho                                                 0.78
    ## log.Adult.Mortality:infant.deaths:CountryLiberia                                                -0.20
    ## log.Adult.Mortality:infant.deaths:CountryLibya                                                   2.62
    ## log.Adult.Mortality:infant.deaths:CountryLithuania                                                 NA
    ## log.Adult.Mortality:infant.deaths:CountryLuxembourg                                                NA
    ## log.Adult.Mortality:infant.deaths:CountryMadagascar                                             -1.21
    ## log.Adult.Mortality:infant.deaths:CountryMalawi                                                 -1.16
    ## log.Adult.Mortality:infant.deaths:CountryMalaysia                                               -0.07
    ## log.Adult.Mortality:infant.deaths:CountryMaldives                                                  NA
    ## log.Adult.Mortality:infant.deaths:CountryMali                                                    0.76
    ## log.Adult.Mortality:infant.deaths:CountryMalta                                                     NA
    ## log.Adult.Mortality:infant.deaths:CountryMauritania                                              1.10
    ## log.Adult.Mortality:infant.deaths:CountryMauritius                                                 NA
    ## log.Adult.Mortality:infant.deaths:CountryMexico                                                  0.29
    ## log.Adult.Mortality:infant.deaths:CountryMicronesia (Federated States of)                          NA
    ## log.Adult.Mortality:infant.deaths:CountryMongolia                                               -1.09
    ## log.Adult.Mortality:infant.deaths:CountryMontenegro                                                NA
    ## log.Adult.Mortality:infant.deaths:CountryMorocco                                                 0.13
    ## log.Adult.Mortality:infant.deaths:CountryMozambique                                              2.61
    ## log.Adult.Mortality:infant.deaths:CountryMyanmar                                                -0.61
    ## log.Adult.Mortality:infant.deaths:CountryNamibia                                                -0.28
    ## log.Adult.Mortality:infant.deaths:CountryNepal                                                  -0.57
    ## log.Adult.Mortality:infant.deaths:CountryNetherlands                                               NA
    ## log.Adult.Mortality:infant.deaths:CountryNew Zealand                                               NA
    ## log.Adult.Mortality:infant.deaths:CountryNicaragua                                               1.78
    ## log.Adult.Mortality:infant.deaths:CountryNiger                                                   3.76
    ## log.Adult.Mortality:infant.deaths:CountryNigeria                                                -2.11
    ## log.Adult.Mortality:infant.deaths:CountryNorway                                                    NA
    ## log.Adult.Mortality:infant.deaths:CountryOman                                                      NA
    ## log.Adult.Mortality:infant.deaths:CountryPakistan                                               -0.31
    ## log.Adult.Mortality:infant.deaths:CountryPanama                                                 -0.30
    ## log.Adult.Mortality:infant.deaths:CountryPapua New Guinea                                       -0.17
    ## log.Adult.Mortality:infant.deaths:CountryParaguay                                                1.66
    ## log.Adult.Mortality:infant.deaths:CountryPeru                                                    0.27
    ## log.Adult.Mortality:infant.deaths:CountryPhilippines                                            -0.07
    ## log.Adult.Mortality:infant.deaths:CountryPoland                                                 -0.07
    ## log.Adult.Mortality:infant.deaths:CountryPortugal                                                0.13
    ## log.Adult.Mortality:infant.deaths:CountryQatar                                                     NA
    ## log.Adult.Mortality:infant.deaths:CountryRepublic of Korea                                      -0.72
    ## log.Adult.Mortality:infant.deaths:CountryRepublic of Moldova                                       NA
    ## log.Adult.Mortality:infant.deaths:CountryRomania                                                 3.44
    ## log.Adult.Mortality:infant.deaths:CountryRussian Federation                                      1.46
    ## log.Adult.Mortality:infant.deaths:CountryRwanda                                                 -2.29
    ## log.Adult.Mortality:infant.deaths:CountrySaint Lucia                                               NA
    ## log.Adult.Mortality:infant.deaths:CountrySaint Vincent and the Grenadines                          NA
    ## log.Adult.Mortality:infant.deaths:CountrySamoa                                                     NA
    ## log.Adult.Mortality:infant.deaths:CountrySao Tome and Principe                                     NA
    ## log.Adult.Mortality:infant.deaths:CountrySaudi Arabia                                            0.17
    ## log.Adult.Mortality:infant.deaths:CountrySenegal                                                -0.22
    ## log.Adult.Mortality:infant.deaths:CountrySerbia                                                    NA
    ## log.Adult.Mortality:infant.deaths:CountrySeychelles                                                NA
    ## log.Adult.Mortality:infant.deaths:CountrySierra Leone                                            2.19
    ## log.Adult.Mortality:infant.deaths:CountrySingapore                                                 NA
    ## log.Adult.Mortality:infant.deaths:CountrySlovakia                                                  NA
    ## log.Adult.Mortality:infant.deaths:CountrySlovenia                                                  NA
    ## log.Adult.Mortality:infant.deaths:CountrySolomon Islands                                           NA
    ## log.Adult.Mortality:infant.deaths:CountrySomalia                                                -1.43
    ## log.Adult.Mortality:infant.deaths:CountrySouth Africa                                            1.71
    ## log.Adult.Mortality:infant.deaths:CountrySouth Sudan                                             0.82
    ## log.Adult.Mortality:infant.deaths:CountrySpain                                                  -0.44
    ## log.Adult.Mortality:infant.deaths:CountrySri Lanka                                              -0.41
    ## log.Adult.Mortality:infant.deaths:CountrySudan                                                  -1.35
    ## log.Adult.Mortality:infant.deaths:CountrySuriname                                                  NA
    ## log.Adult.Mortality:infant.deaths:CountrySwaziland                                              -0.84
    ## log.Adult.Mortality:infant.deaths:CountrySweden                                                    NA
    ## log.Adult.Mortality:infant.deaths:CountrySwitzerland                                               NA
    ## log.Adult.Mortality:infant.deaths:CountrySyrian Arab Republic                                   -0.12
    ## log.Adult.Mortality:infant.deaths:CountryTajikistan                                              0.69
    ## log.Adult.Mortality:infant.deaths:CountryThailand                                               -0.35
    ## log.Adult.Mortality:infant.deaths:CountryThe former Yugoslav republic of Macedonia                 NA
    ## log.Adult.Mortality:infant.deaths:CountryTimor-Leste                                             0.02
    ## log.Adult.Mortality:infant.deaths:CountryTogo                                                    0.31
    ## log.Adult.Mortality:infant.deaths:CountryTonga                                                     NA
    ## log.Adult.Mortality:infant.deaths:CountryTrinidad and Tobago                                       NA
    ## log.Adult.Mortality:infant.deaths:CountryTunisia                                                -0.26
    ## log.Adult.Mortality:infant.deaths:CountryTurkey                                                  0.38
    ## log.Adult.Mortality:infant.deaths:CountryTurkmenistan                                           -0.22
    ## log.Adult.Mortality:infant.deaths:CountryUganda                                                 -0.29
    ## log.Adult.Mortality:infant.deaths:CountryUkraine                                                -2.46
    ## log.Adult.Mortality:infant.deaths:CountryUnited Arab Emirates                                      NA
    ## log.Adult.Mortality:infant.deaths:CountryUnited Kingdom of Great Britain and Northern Ireland   -0.53
    ## log.Adult.Mortality:infant.deaths:CountryUnited Republic of Tanzania                             2.56
    ## log.Adult.Mortality:infant.deaths:CountryUnited States of America                               -0.53
    ## log.Adult.Mortality:infant.deaths:CountryUruguay                                                 0.00
    ## log.Adult.Mortality:infant.deaths:CountryUzbekistan                                             -0.01
    ## log.Adult.Mortality:infant.deaths:CountryVanuatu                                                   NA
    ## log.Adult.Mortality:infant.deaths:CountryVenezuela                                              -0.04
    ## log.Adult.Mortality:infant.deaths:CountryViet Nam                                               -0.07
    ## log.Adult.Mortality:infant.deaths:CountryYemen                                                   1.17
    ## log.Adult.Mortality:infant.deaths:CountryZambia                                                 -0.47
    ## log.Adult.Mortality:infant.deaths:CountryZimbabwe                                                1.74
    ##                                                                                                  Pr(>|t|)
    ## (Intercept)                                                                                       0.19874
    ## log.Adult.Mortality                                                                               0.25499
    ## infant.deaths                                                                                     0.28624
    ## log.GDP                                                                                           0.98502
    ## Measles                                                                                           0.29195
    ## HIV_cat                                                                                           0.09406
    ## CountryAlbania                                                                                    0.29984
    ## CountryAlgeria                                                                                    0.51919
    ## CountryAngola                                                                                     0.66623
    ## CountryAntigua and Barbuda                                                                        0.30807
    ## CountryArgentina                                                                                  0.40821
    ## CountryArmenia                                                                                    0.30573
    ## CountryAustralia                                                                                  0.29137
    ## CountryAustria                                                                                    0.34505
    ## CountryAzerbaijan                                                                                 0.28008
    ## CountryBahamas                                                                                    0.30215
    ## CountryBahrain                                                                                    0.27285
    ## CountryBangladesh                                                                                 0.25383
    ## CountryBarbados                                                                                   0.28411
    ## CountryBelarus                                                                                    0.26177
    ## CountryBelgium                                                                                    0.40742
    ## CountryBelize                                                                                     0.33639
    ## CountryBenin                                                                                      0.43635
    ## CountryBhutan                                                                                     0.39340
    ## CountryBolivia                                                                                    0.73157
    ## CountryBosnia and Herzegovina                                                                     0.29683
    ## CountryBotswana                                                                                   0.37224
    ## CountryBrazil                                                                                     0.60752
    ## CountryBrunei Darussalam                                                                          0.31547
    ## CountryBulgaria                                                                                   0.33902
    ## CountryBurkina Faso                                                                               0.10964
    ## CountryBurundi                                                                                    0.56732
    ## CountryCabo Verde                                                                                 0.27657
    ## CountryCambodia                                                                                   0.26073
    ## CountryCameroon                                                                                   0.73365
    ## CountryCanada                                                                                     0.34863
    ## CountryCentral African Republic                                                                   0.92889
    ## CountryChad                                                                                       0.95679
    ## CountryChile                                                                                      0.42202
    ## CountryChina                                                                                      0.26291
    ## CountryColombia                                                                                   0.43519
    ## CountryComoros                                                                                    0.40110
    ## CountryCongo                                                                                      0.53972
    ## CountryCosta Rica                                                                                 0.33375
    ## CountryCroatia                                                                                    0.30145
    ## CountryCuba                                                                                       0.29350
    ## CountryCyprus                                                                                     0.31505
    ## CountryCzechia                                                                                    0.30091
    ## CountryDemocratic People's Republic of Korea                                                      0.39248
    ## CountryDemocratic Republic of the Congo                                                           0.90632
    ## CountryDenmark                                                                                    0.33603
    ## CountryDjibouti                                                                                   0.02630
    ## CountryDominican Republic                                                                         0.20975
    ## CountryEcuador                                                                                    0.26791
    ## CountryEgypt                                                                                      0.22589
    ## CountryEl Salvador                                                                                0.13294
    ## CountryEquatorial Guinea                                                                          0.39956
    ## CountryEritrea                                                                                    0.05294
    ## CountryEstonia                                                                                    0.27753
    ## CountryEthiopia                                                                                   0.29160
    ## CountryFiji                                                                                       0.31199
    ## CountryFinland                                                                                    0.35994
    ## CountryFrance                                                                                     0.31596
    ## CountryGabon                                                                                      0.33278
    ## CountryGambia                                                                                     0.71793
    ## CountryGeorgia                                                                                    0.21146
    ## CountryGermany                                                                                    0.98085
    ## CountryGhana                                                                                      0.06319
    ## CountryGreece                                                                                     0.44078
    ## CountryGrenada                                                                                    0.23262
    ## CountryGuatemala                                                                                  0.00863
    ## CountryGuinea                                                                                     0.33902
    ## CountryGuinea-Bissau                                                                              0.10592
    ## CountryGuyana                                                                                     0.30019
    ## CountryHaiti                                                                                      0.10481
    ## CountryHonduras                                                                                   0.38412
    ## CountryHungary                                                                                    0.29723
    ## CountryIceland                                                                                    0.27243
    ## CountryIndia                                                                                      0.43938
    ## CountryIndonesia                                                                                  0.27122
    ## CountryIran                                                                                       0.36533
    ## CountryIraq                                                                                       0.00940
    ## CountryIreland                                                                                    0.34193
    ## CountryIsrael                                                                                     0.29470
    ## CountryItaly                                                                                      0.27691
    ## CountryIvory Coast                                                                                0.68613
    ## CountryJamaica                                                                                    0.30725
    ## CountryJapan                                                                                      0.27393
    ## CountryJordan                                                                                     0.31722
    ## CountryKazakhstan                                                                                 0.30056
    ## CountryKenya                                                                                      0.23203
    ## CountryKiribati                                                                                   0.35389
    ## CountryKuwait                                                                                     0.30044
    ## CountryKyrgyzstan                                                                                 0.11950
    ## CountryLao People's Democratic Republic                                                           0.30986
    ## CountryLatvia                                                                                     0.27380
    ## CountryLebanon                                                                                    0.29924
    ## CountryLesotho                                                                                    0.69746
    ## CountryLiberia                                                                                    0.35345
    ## CountryLibya                                                                                      0.17270
    ## CountryLithuania                                                                                  0.24601
    ## CountryLuxembourg                                                                                 0.30198
    ## CountryMadagascar                                                                                 0.08081
    ## CountryMalawi                                                                                     0.26201
    ## CountryMalaysia                                                                                   0.39571
    ## CountryMaldives                                                                                   0.27778
    ## CountryMali                                                                                       0.63729
    ## CountryMalta                                                                                      0.33888
    ## CountryMauritania                                                                                 0.30352
    ## CountryMauritius                                                                                  0.34966
    ## CountryMexico                                                                                     0.53250
    ## CountryMicronesia (Federated States of)                                                           0.34338
    ## CountryMongolia                                                                                   0.26035
    ## CountryMontenegro                                                                                 0.29997
    ## CountryMorocco                                                                                    0.27259
    ## CountryMozambique                                                                                 0.43274
    ## CountryMyanmar                                                                                    0.26295
    ## CountryNamibia                                                                                    0.50707
    ## CountryNepal                                                                                      0.27632
    ## CountryNetherlands                                                                                0.32278
    ## CountryNew Zealand                                                                                0.35091
    ## CountryNicaragua                                                                                  0.52476
    ## CountryNiger                                                                                      0.00040
    ## CountryNigeria                                                                                    0.04726
    ## CountryNorway                                                                                     0.30617
    ## CountryOman                                                                                       0.30755
    ## CountryPakistan                                                                                   0.62755
    ## CountryPanama                                                                                     0.61753
    ## CountryPapua New Guinea                                                                           0.73046
    ## CountryParaguay                                                                                   0.70399
    ## CountryPeru                                                                                       0.26455
    ## CountryPhilippines                                                                                0.56037
    ## CountryPoland                                                                                     0.34258
    ## CountryPortugal                                                                                   0.38897
    ## CountryQatar                                                                                      0.30339
    ## CountryRepublic of Korea                                                                          0.31904
    ## CountryRepublic of Moldova                                                                        0.39601
    ## CountryRomania                                                                                    0.76744
    ## CountryRussian Federation                                                                         0.67214
    ## CountryRwanda                                                                                     0.29605
    ## CountrySaint Lucia                                                                                0.31768
    ## CountrySaint Vincent and the Grenadines                                                           0.15356
    ## CountrySamoa                                                                                      0.24148
    ## CountrySao Tome and Principe                                                                      0.37846
    ## CountrySaudi Arabia                                                                               0.39526
    ## CountrySenegal                                                                                    0.37029
    ## CountrySerbia                                                                                     0.30983
    ## CountrySeychelles                                                                                 0.30468
    ## CountrySierra Leone                                                                               0.44212
    ## CountrySingapore                                                                                  0.30459
    ## CountrySlovakia                                                                                   0.30060
    ## CountrySlovenia                                                                                   0.34186
    ## CountrySolomon Islands                                                                            0.28441
    ## CountrySomalia                                                                                    0.11832
    ## CountrySouth Africa                                                                               0.87110
    ## CountrySouth Sudan                                                                                0.56894
    ## CountrySpain                                                                                      0.27676
    ## CountrySri Lanka                                                                                  0.26934
    ## CountrySudan                                                                                      0.09477
    ## CountrySuriname                                                                                   0.38547
    ## CountrySwaziland                                                                                  0.29588
    ## CountrySweden                                                                                     0.27827
    ## CountrySwitzerland                                                                                0.27588
    ## CountrySyrian Arab Republic                                                                       0.29792
    ## CountryTajikistan                                                                                 0.78300
    ## CountryThailand                                                                                   0.29744
    ## CountryThe former Yugoslav republic of Macedonia                                                  0.29741
    ## CountryTimor-Leste                                                                                0.35893
    ## CountryTogo                                                                                       0.75773
    ## CountryTonga                                                                                      0.30794
    ## CountryTrinidad and Tobago                                                                        0.41609
    ## CountryTunisia                                                                                    0.40255
    ## CountryTurkey                                                                                     0.63231
    ## CountryTurkmenistan                                                                               0.37452
    ## CountryUganda                                                                                     0.47670
    ## CountryUkraine                                                                                    0.10138
    ## CountryUnited Arab Emirates                                                                       0.28919
    ## CountryUnited Kingdom of Great Britain and Northern Ireland                                       0.27773
    ## CountryUnited Republic of Tanzania                                                                0.16189
    ## CountryUnited States of America                                                                   0.38008
    ## CountryUruguay                                                                                    0.33287
    ## CountryUzbekistan                                                                                 0.27219
    ## CountryVanuatu                                                                                    0.30882
    ## CountryVenezuela                                                                                  0.90219
    ## CountryViet Nam                                                                                   0.79043
    ## CountryYemen                                                                                      0.39355
    ## CountryZambia                                                                                     0.26401
    ## CountryZimbabwe                                                                                   0.66806
    ## infant.deaths:CountryAlbania                                                                      0.64348
    ## infant.deaths:CountryAlgeria                                                                      0.86345
    ## infant.deaths:CountryAngola                                                                       0.59317
    ## infant.deaths:CountryAntigua and Barbuda                                                               NA
    ## infant.deaths:CountryArgentina                                                                    0.92104
    ## infant.deaths:CountryArmenia                                                                           NA
    ## infant.deaths:CountryAustralia                                                                         NA
    ## infant.deaths:CountryAustria                                                                           NA
    ## infant.deaths:CountryAzerbaijan                                                                   0.60402
    ## infant.deaths:CountryBahamas                                                                           NA
    ## infant.deaths:CountryBahrain                                                                           NA
    ## infant.deaths:CountryBangladesh                                                                   0.30217
    ## infant.deaths:CountryBarbados                                                                          NA
    ## infant.deaths:CountryBelarus                                                                      0.09330
    ## infant.deaths:CountryBelgium                                                                      0.25370
    ## infant.deaths:CountryBelize                                                                            NA
    ## infant.deaths:CountryBenin                                                                             NA
    ## infant.deaths:CountryBhutan                                                                       0.94685
    ## infant.deaths:CountryBolivia                                                                      0.52410
    ## infant.deaths:CountryBosnia and Herzegovina                                                            NA
    ## infant.deaths:CountryBotswana                                                                          NA
    ## infant.deaths:CountryBrazil                                                                       0.56519
    ## infant.deaths:CountryBrunei Darussalam                                                                 NA
    ## infant.deaths:CountryBulgaria                                                                     0.31804
    ## infant.deaths:CountryBurkina Faso                                                                 0.13484
    ## infant.deaths:CountryBurundi                                                                      0.32304
    ## infant.deaths:CountryCabo Verde                                                                        NA
    ## infant.deaths:CountryCambodia                                                                     0.29276
    ## infant.deaths:CountryCameroon                                                                     0.83339
    ## infant.deaths:CountryCanada                                                                            NA
    ## infant.deaths:CountryCentral African Republic                                                     0.68962
    ## infant.deaths:CountryChad                                                                         0.96143
    ## infant.deaths:CountryChile                                                                             NA
    ## infant.deaths:CountryChina                                                                        0.28326
    ## infant.deaths:CountryColombia                                                                     0.91913
    ## infant.deaths:CountryComoros                                                                      0.93885
    ## infant.deaths:CountryCongo                                                                        0.70647
    ## infant.deaths:CountryCosta Rica                                                                        NA
    ## infant.deaths:CountryCroatia                                                                           NA
    ## infant.deaths:CountryCuba                                                                              NA
    ## infant.deaths:CountryCyprus                                                                            NA
    ## infant.deaths:CountryCzechia                                                                           NA
    ## infant.deaths:CountryDemocratic People's Republic of Korea                                        0.98449
    ## infant.deaths:CountryDemocratic Republic of the Congo                                             0.85902
    ## infant.deaths:CountryDenmark                                                                           NA
    ## infant.deaths:CountryDjibouti                                                                     0.02791
    ## infant.deaths:CountryDominican Republic                                                           0.35524
    ## infant.deaths:CountryEcuador                                                                      0.60500
    ## infant.deaths:CountryEgypt                                                                        0.28284
    ## infant.deaths:CountryEl Salvador                                                                  0.02903
    ## infant.deaths:CountryEquatorial Guinea                                                                 NA
    ## infant.deaths:CountryEritrea                                                                      0.00971
    ## infant.deaths:CountryEstonia                                                                           NA
    ## infant.deaths:CountryEthiopia                                                                     0.29262
    ## infant.deaths:CountryFiji                                                                              NA
    ## infant.deaths:CountryFinland                                                                           NA
    ## infant.deaths:CountryFrance                                                                       0.12019
    ## infant.deaths:CountryGabon                                                                             NA
    ## infant.deaths:CountryGambia                                                                            NA
    ## infant.deaths:CountryGeorgia                                                                      0.45081
    ## infant.deaths:CountryGermany                                                                      0.01271
    ## infant.deaths:CountryGhana                                                                        0.05409
    ## infant.deaths:CountryGreece                                                                       0.68384
    ## infant.deaths:CountryGrenada                                                                           NA
    ## infant.deaths:CountryGuatemala                                                                0.000000018
    ## infant.deaths:CountryGuinea                                                                       0.53891
    ## infant.deaths:CountryGuinea-Bissau                                                                0.00862
    ## infant.deaths:CountryGuyana                                                                       0.89725
    ## infant.deaths:CountryHaiti                                                                        0.00644
    ## infant.deaths:CountryHonduras                                                                     0.83254
    ## infant.deaths:CountryHungary                                                                      0.56307
    ## infant.deaths:CountryIceland                                                                           NA
    ## infant.deaths:CountryIndia                                                                        0.28303
    ## infant.deaths:CountryIndonesia                                                                    0.23653
    ## infant.deaths:CountryIran                                                                         0.80392
    ## infant.deaths:CountryIraq                                                                         0.00819
    ## infant.deaths:CountryIreland                                                                           NA
    ## infant.deaths:CountryIsrael                                                                       0.29862
    ## infant.deaths:CountryItaly                                                                        0.59872
    ## infant.deaths:CountryIvory Coast                                                                  0.60548
    ## infant.deaths:CountryJamaica                                                                           NA
    ## infant.deaths:CountryJapan                                                                        0.68582
    ## infant.deaths:CountryJordan                                                                            NA
    ## infant.deaths:CountryKazakhstan                                                                   0.45414
    ## infant.deaths:CountryKenya                                                                        0.19179
    ## infant.deaths:CountryKiribati                                                                          NA
    ## infant.deaths:CountryKuwait                                                                       0.83219
    ## infant.deaths:CountryKyrgyzstan                                                                   0.18503
    ## infant.deaths:CountryLao People's Democratic Republic                                             0.39133
    ## infant.deaths:CountryLatvia                                                                            NA
    ## infant.deaths:CountryLebanon                                                                           NA
    ## infant.deaths:CountryLesotho                                                                      0.47877
    ## infant.deaths:CountryLiberia                                                                      0.58134
    ## infant.deaths:CountryLibya                                                                        0.00945
    ## infant.deaths:CountryLithuania                                                                         NA
    ## infant.deaths:CountryLuxembourg                                                                        NA
    ## infant.deaths:CountryMadagascar                                                                   0.15251
    ## infant.deaths:CountryMalawi                                                                       0.16699
    ## infant.deaths:CountryMalaysia                                                                     0.89360
    ## infant.deaths:CountryMaldives                                                                          NA
    ## infant.deaths:CountryMali                                                                         0.55684
    ## infant.deaths:CountryMalta                                                                             NA
    ## infant.deaths:CountryMauritania                                                                   0.27536
    ## infant.deaths:CountryMauritius                                                                         NA
    ## infant.deaths:CountryMexico                                                                       0.84720
    ## infant.deaths:CountryMicronesia (Federated States of)                                                  NA
    ## infant.deaths:CountryMongolia                                                                     0.24898
    ## infant.deaths:CountryMontenegro                                                                        NA
    ## infant.deaths:CountryMorocco                                                                      0.45633
    ## infant.deaths:CountryMozambique                                                                   0.14942
    ## infant.deaths:CountryMyanmar                                                                      0.22090
    ## infant.deaths:CountryNamibia                                                                      0.76682
    ## infant.deaths:CountryNepal                                                                        0.23747
    ## infant.deaths:CountryNetherlands                                                                       NA
    ## infant.deaths:CountryNew Zealand                                                                       NA
    ## infant.deaths:CountryNicaragua                                                                    0.11628
    ## infant.deaths:CountryNiger                                                                        0.00032
    ## infant.deaths:CountryNigeria                                                                      0.07767
    ## infant.deaths:CountryNorway                                                                            NA
    ## infant.deaths:CountryOman                                                                              NA
    ## infant.deaths:CountryPakistan                                                                     0.37250
    ## infant.deaths:CountryPanama                                                                       0.76089
    ## infant.deaths:CountryPapua New Guinea                                                             0.83968
    ## infant.deaths:CountryParaguay                                                                     0.11703
    ## infant.deaths:CountryPeru                                                                         0.98094
    ## infant.deaths:CountryPhilippines                                                                  0.69530
    ## infant.deaths:CountryPoland                                                                       0.91474
    ## infant.deaths:CountryPortugal                                                                     0.91479
    ## infant.deaths:CountryQatar                                                                             NA
    ## infant.deaths:CountryRepublic of Korea                                                            0.27465
    ## infant.deaths:CountryRepublic of Moldova                                                               NA
    ## infant.deaths:CountryRomania                                                                      0.00093
    ## infant.deaths:CountryRussian Federation                                                           0.32532
    ## infant.deaths:CountryRwanda                                                                       0.08828
    ## infant.deaths:CountrySaint Lucia                                                                       NA
    ## infant.deaths:CountrySaint Vincent and the Grenadines                                                  NA
    ## infant.deaths:CountrySamoa                                                                             NA
    ## infant.deaths:CountrySao Tome and Principe                                                             NA
    ## infant.deaths:CountrySaudi Arabia                                                                 0.98779
    ## infant.deaths:CountrySenegal                                                                      0.67867
    ## infant.deaths:CountrySerbia                                                                            NA
    ## infant.deaths:CountrySeychelles                                                                        NA
    ## infant.deaths:CountrySierra Leone                                                                 0.07837
    ## infant.deaths:CountrySingapore                                                                         NA
    ## infant.deaths:CountrySlovakia                                                                          NA
    ## infant.deaths:CountrySlovenia                                                                          NA
    ## infant.deaths:CountrySolomon Islands                                                                   NA
    ## infant.deaths:CountrySomalia                                                                      0.13054
    ## infant.deaths:CountrySouth Africa                                                                 0.68544
    ## infant.deaths:CountrySouth Sudan                                                                  0.44626
    ## infant.deaths:CountrySpain                                                                        0.58618
    ## infant.deaths:CountrySri Lanka                                                                    0.58515
    ## infant.deaths:CountrySudan                                                                        0.09335
    ## infant.deaths:CountrySuriname                                                                          NA
    ## infant.deaths:CountrySwaziland                                                                    0.35637
    ## infant.deaths:CountrySweden                                                                            NA
    ## infant.deaths:CountrySwitzerland                                                                       NA
    ## infant.deaths:CountrySyrian Arab Republic                                                         0.53668
    ## infant.deaths:CountryTajikistan                                                                   0.57901
    ## infant.deaths:CountryThailand                                                                     0.41691
    ## infant.deaths:CountryThe former Yugoslav republic of Macedonia                                         NA
    ## infant.deaths:CountryTimor-Leste                                                                  0.93282
    ## infant.deaths:CountryTogo                                                                         0.84976
    ## infant.deaths:CountryTonga                                                                             NA
    ## infant.deaths:CountryTrinidad and Tobago                                                               NA
    ## infant.deaths:CountryTunisia                                                                      0.77578
    ## infant.deaths:CountryTurkey                                                                       0.83601
    ## infant.deaths:CountryTurkmenistan                                                                 0.77436
    ## infant.deaths:CountryUganda                                                                       0.26208
    ## infant.deaths:CountryUkraine                                                                      0.01060
    ## infant.deaths:CountryUnited Arab Emirates                                                              NA
    ## infant.deaths:CountryUnited Kingdom of Great Britain and Northern Ireland                         0.57713
    ## infant.deaths:CountryUnited Republic of Tanzania                                                  0.14345
    ## infant.deaths:CountryUnited States of America                                                     0.52791
    ## infant.deaths:CountryUruguay                                                                      0.98258
    ## infant.deaths:CountryUzbekistan                                                                   0.47180
    ## infant.deaths:CountryVanuatu                                                                           NA
    ## infant.deaths:CountryVenezuela                                                                    0.96533
    ## infant.deaths:CountryViet Nam                                                                     0.89866
    ## infant.deaths:CountryYemen                                                                        0.31293
    ## infant.deaths:CountryZambia                                                                       0.42664
    ## infant.deaths:CountryZimbabwe                                                                     0.65162
    ## log.Adult.Mortality:CountryAlbania                                                                0.30478
    ## log.Adult.Mortality:CountryAlgeria                                                                0.55980
    ## log.Adult.Mortality:CountryAngola                                                                 0.68339
    ## log.Adult.Mortality:CountryAntigua and Barbuda                                                    0.31015
    ## log.Adult.Mortality:CountryArgentina                                                              0.43314
    ## log.Adult.Mortality:CountryArmenia                                                                0.30965
    ## log.Adult.Mortality:CountryAustralia                                                              0.29088
    ## log.Adult.Mortality:CountryAustria                                                                0.35689
    ## log.Adult.Mortality:CountryAzerbaijan                                                             0.28095
    ## log.Adult.Mortality:CountryBahamas                                                                0.30217
    ## log.Adult.Mortality:CountryBahrain                                                                0.27337
    ## log.Adult.Mortality:CountryBangladesh                                                             0.26141
    ## log.Adult.Mortality:CountryBarbados                                                               0.28609
    ## log.Adult.Mortality:CountryBelarus                                                                0.26225
    ## log.Adult.Mortality:CountryBelgium                                                                0.43452
    ## log.Adult.Mortality:CountryBelize                                                                 0.34187
    ## log.Adult.Mortality:CountryBenin                                                                  0.40226
    ## log.Adult.Mortality:CountryBhutan                                                                 0.39678
    ## log.Adult.Mortality:CountryBolivia                                                                0.77165
    ## log.Adult.Mortality:CountryBosnia and Herzegovina                                                 0.29967
    ## log.Adult.Mortality:CountryBotswana                                                               0.37182
    ## log.Adult.Mortality:CountryBrazil                                                                 0.63952
    ## log.Adult.Mortality:CountryBrunei Darussalam                                                      0.32310
    ## log.Adult.Mortality:CountryBulgaria                                                               0.34570
    ## log.Adult.Mortality:CountryBurkina Faso                                                           0.10711
    ## log.Adult.Mortality:CountryBurundi                                                                0.57932
    ## log.Adult.Mortality:CountryCabo Verde                                                             0.27795
    ## log.Adult.Mortality:CountryCambodia                                                               0.26076
    ## log.Adult.Mortality:CountryCameroon                                                               0.71141
    ## log.Adult.Mortality:CountryCanada                                                                 0.36123
    ## log.Adult.Mortality:CountryCentral African Republic                                               0.85585
    ## log.Adult.Mortality:CountryChad                                                                   0.95609
    ## log.Adult.Mortality:CountryChile                                                                  0.44502
    ## log.Adult.Mortality:CountryChina                                                                  0.26567
    ## log.Adult.Mortality:CountryColombia                                                               0.45291
    ## log.Adult.Mortality:CountryComoros                                                                0.40974
    ## log.Adult.Mortality:CountryCongo                                                                  0.48536
    ## log.Adult.Mortality:CountryCosta Rica                                                             0.34104
    ## log.Adult.Mortality:CountryCroatia                                                                0.30439
    ## log.Adult.Mortality:CountryCuba                                                                   0.29379
    ## log.Adult.Mortality:CountryCyprus                                                                 0.32421
    ## log.Adult.Mortality:CountryCzechia                                                                0.30380
    ## log.Adult.Mortality:CountryDemocratic People's Republic of Korea                                  0.40502
    ## log.Adult.Mortality:CountryDemocratic Republic of the Congo                                       0.90548
    ## log.Adult.Mortality:CountryDenmark                                                                0.34631
    ## log.Adult.Mortality:CountryDjibouti                                                               0.02859
    ## log.Adult.Mortality:CountryDominican Republic                                                     0.20750
    ## log.Adult.Mortality:CountryEcuador                                                                0.26932
    ## log.Adult.Mortality:CountryEgypt                                                                  0.22504
    ## log.Adult.Mortality:CountryEl Salvador                                                            0.13398
    ## log.Adult.Mortality:CountryEquatorial Guinea                                                      0.40059
    ## log.Adult.Mortality:CountryEritrea                                                                0.05672
    ## log.Adult.Mortality:CountryEstonia                                                                0.27618
    ## log.Adult.Mortality:CountryEthiopia                                                               0.28812
    ## log.Adult.Mortality:CountryFiji                                                                   0.31627
    ## log.Adult.Mortality:CountryFinland                                                                0.37272
    ## log.Adult.Mortality:CountryFrance                                                                 0.32771
    ## log.Adult.Mortality:CountryGabon                                                                  0.33689
    ## log.Adult.Mortality:CountryGambia                                                                 0.69492
    ## log.Adult.Mortality:CountryGeorgia                                                                0.20654
    ## log.Adult.Mortality:CountryGermany                                                                0.84352
    ## log.Adult.Mortality:CountryGhana                                                                  0.07146
    ## log.Adult.Mortality:CountryGreece                                                                 0.48286
    ## log.Adult.Mortality:CountryGrenada                                                                0.23034
    ## log.Adult.Mortality:CountryGuatemala                                                              0.00908
    ## log.Adult.Mortality:CountryGuinea                                                                 0.33308
    ## log.Adult.Mortality:CountryGuinea-Bissau                                                          0.11916
    ## log.Adult.Mortality:CountryGuyana                                                                 0.30265
    ## log.Adult.Mortality:CountryHaiti                                                                  0.10912
    ## log.Adult.Mortality:CountryHonduras                                                               0.39555
    ## log.Adult.Mortality:CountryHungary                                                                0.29841
    ## log.Adult.Mortality:CountryIceland                                                                0.26614
    ## log.Adult.Mortality:CountryIndia                                                                  0.45822
    ## log.Adult.Mortality:CountryIndonesia                                                              0.27435
    ## log.Adult.Mortality:CountryIran                                                                   0.38156
    ## log.Adult.Mortality:CountryIraq                                                                   0.01014
    ## log.Adult.Mortality:CountryIreland                                                                0.35629
    ## log.Adult.Mortality:CountryIsrael                                                                 0.29576
    ## log.Adult.Mortality:CountryItaly                                                                  0.26608
    ## log.Adult.Mortality:CountryIvory Coast                                                            0.72344
    ## log.Adult.Mortality:CountryJamaica                                                                0.30855
    ## log.Adult.Mortality:CountryJapan                                                                  0.27118
    ## log.Adult.Mortality:CountryJordan                                                                 0.32319
    ## log.Adult.Mortality:CountryKazakhstan                                                             0.30274
    ## log.Adult.Mortality:CountryKenya                                                                  0.23828
    ## log.Adult.Mortality:CountryKiribati                                                               0.36253
    ## log.Adult.Mortality:CountryKuwait                                                                 0.30907
    ## log.Adult.Mortality:CountryKyrgyzstan                                                             0.11549
    ## log.Adult.Mortality:CountryLao People's Democratic Republic                                       0.32199
    ## log.Adult.Mortality:CountryLatvia                                                                 0.27334
    ## log.Adult.Mortality:CountryLebanon                                                                0.30472
    ## log.Adult.Mortality:CountryLesotho                                                                0.64682
    ## log.Adult.Mortality:CountryLiberia                                                                0.35319
    ## log.Adult.Mortality:CountryLibya                                                                  0.14411
    ## log.Adult.Mortality:CountryLithuania                                                              0.24516
    ## log.Adult.Mortality:CountryLuxembourg                                                             0.30514
    ## log.Adult.Mortality:CountryMadagascar                                                             0.07998
    ## log.Adult.Mortality:CountryMalawi                                                                 0.27311
    ## log.Adult.Mortality:CountryMalaysia                                                               0.41451
    ## log.Adult.Mortality:CountryMaldives                                                               0.27891
    ## log.Adult.Mortality:CountryMali                                                                   0.65261
    ## log.Adult.Mortality:CountryMalta                                                                  0.35683
    ## log.Adult.Mortality:CountryMauritania                                                             0.30206
    ## log.Adult.Mortality:CountryMauritius                                                              0.35588
    ## log.Adult.Mortality:CountryMexico                                                                 0.56017
    ## log.Adult.Mortality:CountryMicronesia (Federated States of)                                       0.35214
    ## log.Adult.Mortality:CountryMongolia                                                               0.26596
    ## log.Adult.Mortality:CountryMontenegro                                                             0.30367
    ## log.Adult.Mortality:CountryMorocco                                                                0.26176
    ## log.Adult.Mortality:CountryMozambique                                                             0.52667
    ## log.Adult.Mortality:CountryMyanmar                                                                0.26772
    ## log.Adult.Mortality:CountryNamibia                                                                0.49825
    ## log.Adult.Mortality:CountryNepal                                                                  0.28053
    ## log.Adult.Mortality:CountryNetherlands                                                            0.33087
    ## log.Adult.Mortality:CountryNew Zealand                                                            0.36601
    ## log.Adult.Mortality:CountryNicaragua                                                              0.54701
    ## log.Adult.Mortality:CountryNiger                                                                  0.00046
    ## log.Adult.Mortality:CountryNigeria                                                                0.05230
    ## log.Adult.Mortality:CountryNorway                                                                 0.30929
    ## log.Adult.Mortality:CountryOman                                                                   0.31224
    ## log.Adult.Mortality:CountryPakistan                                                               0.63344
    ## log.Adult.Mortality:CountryPanama                                                                 0.63390
    ## log.Adult.Mortality:CountryPapua New Guinea                                                       0.73971
    ## log.Adult.Mortality:CountryParaguay                                                               0.73898
    ## log.Adult.Mortality:CountryPeru                                                                   0.26227
    ## log.Adult.Mortality:CountryPhilippines                                                            0.56415
    ## log.Adult.Mortality:CountryPoland                                                                 0.34936
    ## log.Adult.Mortality:CountryPortugal                                                               0.40931
    ## log.Adult.Mortality:CountryQatar                                                                  0.31251
    ## log.Adult.Mortality:CountryRepublic of Korea                                                      0.32982
    ## log.Adult.Mortality:CountryRepublic of Moldova                                                    0.39896
    ## log.Adult.Mortality:CountryRomania                                                                0.80059
    ## log.Adult.Mortality:CountryRussian Federation                                                     0.64592
    ## log.Adult.Mortality:CountryRwanda                                                                 0.30686
    ## log.Adult.Mortality:CountrySaint Lucia                                                            0.32057
    ## log.Adult.Mortality:CountrySaint Vincent and the Grenadines                                       0.14921
    ## log.Adult.Mortality:CountrySamoa                                                                  0.23897
    ## log.Adult.Mortality:CountrySao Tome and Principe                                                  0.38691
    ## log.Adult.Mortality:CountrySaudi Arabia                                                           0.42928
    ## log.Adult.Mortality:CountrySenegal                                                                0.36053
    ## log.Adult.Mortality:CountrySerbia                                                                 0.31514
    ## log.Adult.Mortality:CountrySeychelles                                                             0.30668
    ## log.Adult.Mortality:CountrySierra Leone                                                           0.52683
    ## log.Adult.Mortality:CountrySingapore                                                              0.30901
    ## log.Adult.Mortality:CountrySlovakia                                                               0.30328
    ## log.Adult.Mortality:CountrySlovenia                                                               0.35013
    ## log.Adult.Mortality:CountrySolomon Islands                                                        0.28976
    ## log.Adult.Mortality:CountrySomalia                                                                0.11331
    ## log.Adult.Mortality:CountrySouth Africa                                                           0.79743
    ## log.Adult.Mortality:CountrySouth Sudan                                                            0.59209
    ## log.Adult.Mortality:CountrySpain                                                                  0.28690
    ## log.Adult.Mortality:CountrySri Lanka                                                              0.27158
    ## log.Adult.Mortality:CountrySudan                                                                  0.09668
    ## log.Adult.Mortality:CountrySuriname                                                               0.38991
    ## log.Adult.Mortality:CountrySwaziland                                                              0.30263
    ## log.Adult.Mortality:CountrySweden                                                                 0.27399
    ## log.Adult.Mortality:CountrySwitzerland                                                            0.27086
    ## log.Adult.Mortality:CountrySyrian Arab Republic                                                   0.30073
    ## log.Adult.Mortality:CountryTajikistan                                                             0.82424
    ## log.Adult.Mortality:CountryThailand                                                               0.30111
    ## log.Adult.Mortality:CountryThe former Yugoslav republic of Macedonia                              0.30183
    ## log.Adult.Mortality:CountryTimor-Leste                                                            0.37449
    ## log.Adult.Mortality:CountryTogo                                                                   0.74729
    ## log.Adult.Mortality:CountryTonga                                                                  0.31246
    ## log.Adult.Mortality:CountryTrinidad and Tobago                                                    0.42283
    ## log.Adult.Mortality:CountryTunisia                                                                0.43138
    ## log.Adult.Mortality:CountryTurkey                                                                 0.70910
    ## log.Adult.Mortality:CountryTurkmenistan                                                           0.37752
    ## log.Adult.Mortality:CountryUganda                                                                 0.48349
    ## log.Adult.Mortality:CountryUkraine                                                                0.10067
    ## log.Adult.Mortality:CountryUnited Arab Emirates                                                   0.29266
    ## log.Adult.Mortality:CountryUnited Kingdom of Great Britain and Northern Ireland                   0.28904
    ## log.Adult.Mortality:CountryUnited Republic of Tanzania                                            0.21835
    ## log.Adult.Mortality:CountryUnited States of America                                               0.39701
    ## log.Adult.Mortality:CountryUruguay                                                                0.34272
    ## log.Adult.Mortality:CountryUzbekistan                                                             0.27580
    ## log.Adult.Mortality:CountryVanuatu                                                                0.31368
    ## log.Adult.Mortality:CountryVenezuela                                                              0.90490
    ## log.Adult.Mortality:CountryViet Nam                                                               0.80177
    ## log.Adult.Mortality:CountryYemen                                                                  0.39310
    ## log.Adult.Mortality:CountryZambia                                                                 0.24919
    ## log.Adult.Mortality:CountryZimbabwe                                                               0.61622
    ## log.Adult.Mortality:infant.deaths:CountryAfghanistan                                              0.29896
    ## log.Adult.Mortality:infant.deaths:CountryAlbania                                                  0.65853
    ## log.Adult.Mortality:infant.deaths:CountryAlgeria                                                  0.98643
    ## log.Adult.Mortality:infant.deaths:CountryAngola                                                   0.59047
    ## log.Adult.Mortality:infant.deaths:CountryAntigua and Barbuda                                           NA
    ## log.Adult.Mortality:infant.deaths:CountryArgentina                                                0.91032
    ## log.Adult.Mortality:infant.deaths:CountryArmenia                                                       NA
    ## log.Adult.Mortality:infant.deaths:CountryAustralia                                                     NA
    ## log.Adult.Mortality:infant.deaths:CountryAustria                                                       NA
    ## log.Adult.Mortality:infant.deaths:CountryAzerbaijan                                               0.71850
    ## log.Adult.Mortality:infant.deaths:CountryBahamas                                                       NA
    ## log.Adult.Mortality:infant.deaths:CountryBahrain                                                       NA
    ## log.Adult.Mortality:infant.deaths:CountryBangladesh                                               0.96431
    ## log.Adult.Mortality:infant.deaths:CountryBarbados                                                      NA
    ## log.Adult.Mortality:infant.deaths:CountryBelarus                                                  0.10150
    ## log.Adult.Mortality:infant.deaths:CountryBelgium                                                  0.22033
    ## log.Adult.Mortality:infant.deaths:CountryBelize                                                        NA
    ## log.Adult.Mortality:infant.deaths:CountryBenin                                                         NA
    ## log.Adult.Mortality:infant.deaths:CountryBhutan                                                   0.97317
    ## log.Adult.Mortality:infant.deaths:CountryBolivia                                                  0.71995
    ## log.Adult.Mortality:infant.deaths:CountryBosnia and Herzegovina                                        NA
    ## log.Adult.Mortality:infant.deaths:CountryBotswana                                                      NA
    ## log.Adult.Mortality:infant.deaths:CountryBrazil                                                   0.10799
    ## log.Adult.Mortality:infant.deaths:CountryBrunei Darussalam                                             NA
    ## log.Adult.Mortality:infant.deaths:CountryBulgaria                                                      NA
    ## log.Adult.Mortality:infant.deaths:CountryBurkina Faso                                             0.21100
    ## log.Adult.Mortality:infant.deaths:CountryBurundi                                                  0.22163
    ## log.Adult.Mortality:infant.deaths:CountryCabo Verde                                                    NA
    ## log.Adult.Mortality:infant.deaths:CountryCambodia                                                 0.61362
    ## log.Adult.Mortality:infant.deaths:CountryCameroon                                                 0.95254
    ## log.Adult.Mortality:infant.deaths:CountryCanada                                                        NA
    ## log.Adult.Mortality:infant.deaths:CountryCentral African Republic                                 0.63266
    ## log.Adult.Mortality:infant.deaths:CountryChad                                                     0.96988
    ## log.Adult.Mortality:infant.deaths:CountryChile                                                         NA
    ## log.Adult.Mortality:infant.deaths:CountryChina                                                    0.79235
    ## log.Adult.Mortality:infant.deaths:CountryColombia                                                 0.86817
    ## log.Adult.Mortality:infant.deaths:CountryComoros                                                  0.87287
    ## log.Adult.Mortality:infant.deaths:CountryCongo                                                    0.68258
    ## log.Adult.Mortality:infant.deaths:CountryCosta Rica                                                    NA
    ## log.Adult.Mortality:infant.deaths:CountryCroatia                                                       NA
    ## log.Adult.Mortality:infant.deaths:CountryCuba                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryCyprus                                                        NA
    ## log.Adult.Mortality:infant.deaths:CountryCzechia                                                       NA
    ## log.Adult.Mortality:infant.deaths:CountryDemocratic People's Republic of Korea                    0.39287
    ## log.Adult.Mortality:infant.deaths:CountryDemocratic Republic of the Congo                         0.77202
    ## log.Adult.Mortality:infant.deaths:CountryDenmark                                                       NA
    ## log.Adult.Mortality:infant.deaths:CountryDjibouti                                                 0.02597
    ## log.Adult.Mortality:infant.deaths:CountryDominican Republic                                       0.36478
    ## log.Adult.Mortality:infant.deaths:CountryEcuador                                                  0.74663
    ## log.Adult.Mortality:infant.deaths:CountryEgypt                                                    0.19238
    ## log.Adult.Mortality:infant.deaths:CountryEl Salvador                                              0.03440
    ## log.Adult.Mortality:infant.deaths:CountryEquatorial Guinea                                             NA
    ## log.Adult.Mortality:infant.deaths:CountryEritrea                                                  0.01368
    ## log.Adult.Mortality:infant.deaths:CountryEstonia                                                       NA
    ## log.Adult.Mortality:infant.deaths:CountryEthiopia                                                 0.93108
    ## log.Adult.Mortality:infant.deaths:CountryFiji                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryFinland                                                       NA
    ## log.Adult.Mortality:infant.deaths:CountryFrance                                                        NA
    ## log.Adult.Mortality:infant.deaths:CountryGabon                                                         NA
    ## log.Adult.Mortality:infant.deaths:CountryGambia                                                        NA
    ## log.Adult.Mortality:infant.deaths:CountryGeorgia                                                  0.48246
    ## log.Adult.Mortality:infant.deaths:CountryGermany                                                  0.00957
    ## log.Adult.Mortality:infant.deaths:CountryGhana                                                    0.08355
    ## log.Adult.Mortality:infant.deaths:CountryGreece                                                   0.66439
    ## log.Adult.Mortality:infant.deaths:CountryGrenada                                                       NA
    ## log.Adult.Mortality:infant.deaths:CountryGuatemala                                            0.000000022
    ## log.Adult.Mortality:infant.deaths:CountryGuinea                                                   0.78504
    ## log.Adult.Mortality:infant.deaths:CountryGuinea-Bissau                                            0.00839
    ## log.Adult.Mortality:infant.deaths:CountryGuyana                                                   0.91497
    ## log.Adult.Mortality:infant.deaths:CountryHaiti                                                    0.01090
    ## log.Adult.Mortality:infant.deaths:CountryHonduras                                                 0.95258
    ## log.Adult.Mortality:infant.deaths:CountryHungary                                                  0.61500
    ## log.Adult.Mortality:infant.deaths:CountryIceland                                                       NA
    ## log.Adult.Mortality:infant.deaths:CountryIndia                                                    0.83013
    ## log.Adult.Mortality:infant.deaths:CountryIndonesia                                                0.59271
    ## log.Adult.Mortality:infant.deaths:CountryIran                                                     0.22539
    ## log.Adult.Mortality:infant.deaths:CountryIraq                                                     0.01460
    ## log.Adult.Mortality:infant.deaths:CountryIreland                                                       NA
    ## log.Adult.Mortality:infant.deaths:CountryIsrael                                                        NA
    ## log.Adult.Mortality:infant.deaths:CountryItaly                                                         NA
    ## log.Adult.Mortality:infant.deaths:CountryIvory Coast                                              0.46351
    ## log.Adult.Mortality:infant.deaths:CountryJamaica                                                       NA
    ## log.Adult.Mortality:infant.deaths:CountryJapan                                                    0.77883
    ## log.Adult.Mortality:infant.deaths:CountryJordan                                                        NA
    ## log.Adult.Mortality:infant.deaths:CountryKazakhstan                                               0.67593
    ## log.Adult.Mortality:infant.deaths:CountryKenya                                                    0.35171
    ## log.Adult.Mortality:infant.deaths:CountryKiribati                                                      NA
    ## log.Adult.Mortality:infant.deaths:CountryKuwait                                                   0.86567
    ## log.Adult.Mortality:infant.deaths:CountryKyrgyzstan                                               0.19349
    ## log.Adult.Mortality:infant.deaths:CountryLao People's Democratic Republic                         0.57962
    ## log.Adult.Mortality:infant.deaths:CountryLatvia                                                        NA
    ## log.Adult.Mortality:infant.deaths:CountryLebanon                                                       NA
    ## log.Adult.Mortality:infant.deaths:CountryLesotho                                                  0.43429
    ## log.Adult.Mortality:infant.deaths:CountryLiberia                                                  0.84430
    ## log.Adult.Mortality:infant.deaths:CountryLibya                                                    0.00895
    ## log.Adult.Mortality:infant.deaths:CountryLithuania                                                     NA
    ## log.Adult.Mortality:infant.deaths:CountryLuxembourg                                                    NA
    ## log.Adult.Mortality:infant.deaths:CountryMadagascar                                               0.22639
    ## log.Adult.Mortality:infant.deaths:CountryMalawi                                                   0.24727
    ## log.Adult.Mortality:infant.deaths:CountryMalaysia                                                 0.94297
    ## log.Adult.Mortality:infant.deaths:CountryMaldives                                                      NA
    ## log.Adult.Mortality:infant.deaths:CountryMali                                                     0.44771
    ## log.Adult.Mortality:infant.deaths:CountryMalta                                                         NA
    ## log.Adult.Mortality:infant.deaths:CountryMauritania                                               0.27239
    ## log.Adult.Mortality:infant.deaths:CountryMauritius                                                     NA
    ## log.Adult.Mortality:infant.deaths:CountryMexico                                                   0.77520
    ## log.Adult.Mortality:infant.deaths:CountryMicronesia (Federated States of)                              NA
    ## log.Adult.Mortality:infant.deaths:CountryMongolia                                                 0.27418
    ## log.Adult.Mortality:infant.deaths:CountryMontenegro                                                    NA
    ## log.Adult.Mortality:infant.deaths:CountryMorocco                                                  0.89992
    ## log.Adult.Mortality:infant.deaths:CountryMozambique                                               0.00921
    ## log.Adult.Mortality:infant.deaths:CountryMyanmar                                                  0.54107
    ## log.Adult.Mortality:infant.deaths:CountryNamibia                                                  0.77850
    ## log.Adult.Mortality:infant.deaths:CountryNepal                                                    0.57191
    ## log.Adult.Mortality:infant.deaths:CountryNetherlands                                                   NA
    ## log.Adult.Mortality:infant.deaths:CountryNew Zealand                                                   NA
    ## log.Adult.Mortality:infant.deaths:CountryNicaragua                                                0.07568
    ## log.Adult.Mortality:infant.deaths:CountryNiger                                                    0.00018
    ## log.Adult.Mortality:infant.deaths:CountryNigeria                                                  0.03514
    ## log.Adult.Mortality:infant.deaths:CountryNorway                                                        NA
    ## log.Adult.Mortality:infant.deaths:CountryOman                                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryPakistan                                                 0.75999
    ## log.Adult.Mortality:infant.deaths:CountryPanama                                                   0.76254
    ## log.Adult.Mortality:infant.deaths:CountryPapua New Guinea                                         0.86867
    ## log.Adult.Mortality:infant.deaths:CountryParaguay                                                 0.09671
    ## log.Adult.Mortality:infant.deaths:CountryPeru                                                     0.79059
    ## log.Adult.Mortality:infant.deaths:CountryPhilippines                                              0.94428
    ## log.Adult.Mortality:infant.deaths:CountryPoland                                                   0.94617
    ## log.Adult.Mortality:infant.deaths:CountryPortugal                                                 0.89626
    ## log.Adult.Mortality:infant.deaths:CountryQatar                                                         NA
    ## log.Adult.Mortality:infant.deaths:CountryRepublic of Korea                                        0.47266
    ## log.Adult.Mortality:infant.deaths:CountryRepublic of Moldova                                           NA
    ## log.Adult.Mortality:infant.deaths:CountryRomania                                                  0.00060
    ## log.Adult.Mortality:infant.deaths:CountryRussian Federation                                       0.14520
    ## log.Adult.Mortality:infant.deaths:CountryRwanda                                                   0.02185
    ## log.Adult.Mortality:infant.deaths:CountrySaint Lucia                                                   NA
    ## log.Adult.Mortality:infant.deaths:CountrySaint Vincent and the Grenadines                              NA
    ## log.Adult.Mortality:infant.deaths:CountrySamoa                                                         NA
    ## log.Adult.Mortality:infant.deaths:CountrySao Tome and Principe                                         NA
    ## log.Adult.Mortality:infant.deaths:CountrySaudi Arabia                                             0.86324
    ## log.Adult.Mortality:infant.deaths:CountrySenegal                                                  0.82955
    ## log.Adult.Mortality:infant.deaths:CountrySerbia                                                        NA
    ## log.Adult.Mortality:infant.deaths:CountrySeychelles                                                    NA
    ## log.Adult.Mortality:infant.deaths:CountrySierra Leone                                             0.02850
    ## log.Adult.Mortality:infant.deaths:CountrySingapore                                                     NA
    ## log.Adult.Mortality:infant.deaths:CountrySlovakia                                                      NA
    ## log.Adult.Mortality:infant.deaths:CountrySlovenia                                                      NA
    ## log.Adult.Mortality:infant.deaths:CountrySolomon Islands                                               NA
    ## log.Adult.Mortality:infant.deaths:CountrySomalia                                                  0.15294
    ## log.Adult.Mortality:infant.deaths:CountrySouth Africa                                             0.08772
    ## log.Adult.Mortality:infant.deaths:CountrySouth Sudan                                              0.41083
    ## log.Adult.Mortality:infant.deaths:CountrySpain                                                    0.65875
    ## log.Adult.Mortality:infant.deaths:CountrySri Lanka                                                0.67926
    ## log.Adult.Mortality:infant.deaths:CountrySudan                                                    0.17634
    ## log.Adult.Mortality:infant.deaths:CountrySuriname                                                      NA
    ## log.Adult.Mortality:infant.deaths:CountrySwaziland                                                0.40302
    ## log.Adult.Mortality:infant.deaths:CountrySweden                                                        NA
    ## log.Adult.Mortality:infant.deaths:CountrySwitzerland                                                   NA
    ## log.Adult.Mortality:infant.deaths:CountrySyrian Arab Republic                                     0.90567
    ## log.Adult.Mortality:infant.deaths:CountryTajikistan                                               0.48977
    ## log.Adult.Mortality:infant.deaths:CountryThailand                                                 0.72778
    ## log.Adult.Mortality:infant.deaths:CountryThe former Yugoslav republic of Macedonia                     NA
    ## log.Adult.Mortality:infant.deaths:CountryTimor-Leste                                              0.98279
    ## log.Adult.Mortality:infant.deaths:CountryTogo                                                     0.75380
    ## log.Adult.Mortality:infant.deaths:CountryTonga                                                         NA
    ## log.Adult.Mortality:infant.deaths:CountryTrinidad and Tobago                                           NA
    ## log.Adult.Mortality:infant.deaths:CountryTunisia                                                  0.79249
    ## log.Adult.Mortality:infant.deaths:CountryTurkey                                                   0.70320
    ## log.Adult.Mortality:infant.deaths:CountryTurkmenistan                                             0.82944
    ## log.Adult.Mortality:infant.deaths:CountryUganda                                                   0.77524
    ## log.Adult.Mortality:infant.deaths:CountryUkraine                                                  0.01387
    ## log.Adult.Mortality:infant.deaths:CountryUnited Arab Emirates                                          NA
    ## log.Adult.Mortality:infant.deaths:CountryUnited Kingdom of Great Britain and Northern Ireland     0.59916
    ## log.Adult.Mortality:infant.deaths:CountryUnited Republic of Tanzania                              0.01057
    ## log.Adult.Mortality:infant.deaths:CountryUnited States of America                                 0.59865
    ## log.Adult.Mortality:infant.deaths:CountryUruguay                                                  0.99642
    ## log.Adult.Mortality:infant.deaths:CountryUzbekistan                                               0.98971
    ## log.Adult.Mortality:infant.deaths:CountryVanuatu                                                       NA
    ## log.Adult.Mortality:infant.deaths:CountryVenezuela                                                0.97129
    ## log.Adult.Mortality:infant.deaths:CountryViet Nam                                                 0.94612
    ## log.Adult.Mortality:infant.deaths:CountryYemen                                                    0.24350
    ## log.Adult.Mortality:infant.deaths:CountryZambia                                                   0.63810
    ## log.Adult.Mortality:infant.deaths:CountryZimbabwe                                                 0.08250
    ##                                                                                                  
    ## (Intercept)                                                                                      
    ## log.Adult.Mortality                                                                              
    ## infant.deaths                                                                                    
    ## log.GDP                                                                                          
    ## Measles                                                                                          
    ## HIV_cat                                                                                       .  
    ## CountryAlbania                                                                                   
    ## CountryAlgeria                                                                                   
    ## CountryAngola                                                                                    
    ## CountryAntigua and Barbuda                                                                       
    ## CountryArgentina                                                                                 
    ## CountryArmenia                                                                                   
    ## CountryAustralia                                                                                 
    ## CountryAustria                                                                                   
    ## CountryAzerbaijan                                                                                
    ## CountryBahamas                                                                                   
    ## CountryBahrain                                                                                   
    ## CountryBangladesh                                                                                
    ## CountryBarbados                                                                                  
    ## CountryBelarus                                                                                   
    ## CountryBelgium                                                                                   
    ## CountryBelize                                                                                    
    ## CountryBenin                                                                                     
    ## CountryBhutan                                                                                    
    ## CountryBolivia                                                                                   
    ## CountryBosnia and Herzegovina                                                                    
    ## CountryBotswana                                                                                  
    ## CountryBrazil                                                                                    
    ## CountryBrunei Darussalam                                                                         
    ## CountryBulgaria                                                                                  
    ## CountryBurkina Faso                                                                              
    ## CountryBurundi                                                                                   
    ## CountryCabo Verde                                                                                
    ## CountryCambodia                                                                                  
    ## CountryCameroon                                                                                  
    ## CountryCanada                                                                                    
    ## CountryCentral African Republic                                                                  
    ## CountryChad                                                                                      
    ## CountryChile                                                                                     
    ## CountryChina                                                                                     
    ## CountryColombia                                                                                  
    ## CountryComoros                                                                                   
    ## CountryCongo                                                                                     
    ## CountryCosta Rica                                                                                
    ## CountryCroatia                                                                                   
    ## CountryCuba                                                                                      
    ## CountryCyprus                                                                                    
    ## CountryCzechia                                                                                   
    ## CountryDemocratic People's Republic of Korea                                                     
    ## CountryDemocratic Republic of the Congo                                                          
    ## CountryDenmark                                                                                   
    ## CountryDjibouti                                                                               *  
    ## CountryDominican Republic                                                                        
    ## CountryEcuador                                                                                   
    ## CountryEgypt                                                                                     
    ## CountryEl Salvador                                                                               
    ## CountryEquatorial Guinea                                                                         
    ## CountryEritrea                                                                                .  
    ## CountryEstonia                                                                                   
    ## CountryEthiopia                                                                                  
    ## CountryFiji                                                                                      
    ## CountryFinland                                                                                   
    ## CountryFrance                                                                                    
    ## CountryGabon                                                                                     
    ## CountryGambia                                                                                    
    ## CountryGeorgia                                                                                   
    ## CountryGermany                                                                                   
    ## CountryGhana                                                                                  .  
    ## CountryGreece                                                                                    
    ## CountryGrenada                                                                                   
    ## CountryGuatemala                                                                              ** 
    ## CountryGuinea                                                                                    
    ## CountryGuinea-Bissau                                                                             
    ## CountryGuyana                                                                                    
    ## CountryHaiti                                                                                     
    ## CountryHonduras                                                                                  
    ## CountryHungary                                                                                   
    ## CountryIceland                                                                                   
    ## CountryIndia                                                                                     
    ## CountryIndonesia                                                                                 
    ## CountryIran                                                                                      
    ## CountryIraq                                                                                   ** 
    ## CountryIreland                                                                                   
    ## CountryIsrael                                                                                    
    ## CountryItaly                                                                                     
    ## CountryIvory Coast                                                                               
    ## CountryJamaica                                                                                   
    ## CountryJapan                                                                                     
    ## CountryJordan                                                                                    
    ## CountryKazakhstan                                                                                
    ## CountryKenya                                                                                     
    ## CountryKiribati                                                                                  
    ## CountryKuwait                                                                                    
    ## CountryKyrgyzstan                                                                                
    ## CountryLao People's Democratic Republic                                                          
    ## CountryLatvia                                                                                    
    ## CountryLebanon                                                                                   
    ## CountryLesotho                                                                                   
    ## CountryLiberia                                                                                   
    ## CountryLibya                                                                                     
    ## CountryLithuania                                                                                 
    ## CountryLuxembourg                                                                                
    ## CountryMadagascar                                                                             .  
    ## CountryMalawi                                                                                    
    ## CountryMalaysia                                                                                  
    ## CountryMaldives                                                                                  
    ## CountryMali                                                                                      
    ## CountryMalta                                                                                     
    ## CountryMauritania                                                                                
    ## CountryMauritius                                                                                 
    ## CountryMexico                                                                                    
    ## CountryMicronesia (Federated States of)                                                          
    ## CountryMongolia                                                                                  
    ## CountryMontenegro                                                                                
    ## CountryMorocco                                                                                   
    ## CountryMozambique                                                                                
    ## CountryMyanmar                                                                                   
    ## CountryNamibia                                                                                   
    ## CountryNepal                                                                                     
    ## CountryNetherlands                                                                               
    ## CountryNew Zealand                                                                               
    ## CountryNicaragua                                                                                 
    ## CountryNiger                                                                                  ***
    ## CountryNigeria                                                                                *  
    ## CountryNorway                                                                                    
    ## CountryOman                                                                                      
    ## CountryPakistan                                                                                  
    ## CountryPanama                                                                                    
    ## CountryPapua New Guinea                                                                          
    ## CountryParaguay                                                                                  
    ## CountryPeru                                                                                      
    ## CountryPhilippines                                                                               
    ## CountryPoland                                                                                    
    ## CountryPortugal                                                                                  
    ## CountryQatar                                                                                     
    ## CountryRepublic of Korea                                                                         
    ## CountryRepublic of Moldova                                                                       
    ## CountryRomania                                                                                   
    ## CountryRussian Federation                                                                        
    ## CountryRwanda                                                                                    
    ## CountrySaint Lucia                                                                               
    ## CountrySaint Vincent and the Grenadines                                                          
    ## CountrySamoa                                                                                     
    ## CountrySao Tome and Principe                                                                     
    ## CountrySaudi Arabia                                                                              
    ## CountrySenegal                                                                                   
    ## CountrySerbia                                                                                    
    ## CountrySeychelles                                                                                
    ## CountrySierra Leone                                                                              
    ## CountrySingapore                                                                                 
    ## CountrySlovakia                                                                                  
    ## CountrySlovenia                                                                                  
    ## CountrySolomon Islands                                                                           
    ## CountrySomalia                                                                                   
    ## CountrySouth Africa                                                                              
    ## CountrySouth Sudan                                                                               
    ## CountrySpain                                                                                     
    ## CountrySri Lanka                                                                                 
    ## CountrySudan                                                                                  .  
    ## CountrySuriname                                                                                  
    ## CountrySwaziland                                                                                 
    ## CountrySweden                                                                                    
    ## CountrySwitzerland                                                                               
    ## CountrySyrian Arab Republic                                                                      
    ## CountryTajikistan                                                                                
    ## CountryThailand                                                                                  
    ## CountryThe former Yugoslav republic of Macedonia                                                 
    ## CountryTimor-Leste                                                                               
    ## CountryTogo                                                                                      
    ## CountryTonga                                                                                     
    ## CountryTrinidad and Tobago                                                                       
    ## CountryTunisia                                                                                   
    ## CountryTurkey                                                                                    
    ## CountryTurkmenistan                                                                              
    ## CountryUganda                                                                                    
    ## CountryUkraine                                                                                   
    ## CountryUnited Arab Emirates                                                                      
    ## CountryUnited Kingdom of Great Britain and Northern Ireland                                      
    ## CountryUnited Republic of Tanzania                                                               
    ## CountryUnited States of America                                                                  
    ## CountryUruguay                                                                                   
    ## CountryUzbekistan                                                                                
    ## CountryVanuatu                                                                                   
    ## CountryVenezuela                                                                                 
    ## CountryViet Nam                                                                                  
    ## CountryYemen                                                                                     
    ## CountryZambia                                                                                    
    ## CountryZimbabwe                                                                                  
    ## infant.deaths:CountryAlbania                                                                     
    ## infant.deaths:CountryAlgeria                                                                     
    ## infant.deaths:CountryAngola                                                                      
    ## infant.deaths:CountryAntigua and Barbuda                                                         
    ## infant.deaths:CountryArgentina                                                                   
    ## infant.deaths:CountryArmenia                                                                     
    ## infant.deaths:CountryAustralia                                                                   
    ## infant.deaths:CountryAustria                                                                     
    ## infant.deaths:CountryAzerbaijan                                                                  
    ## infant.deaths:CountryBahamas                                                                     
    ## infant.deaths:CountryBahrain                                                                     
    ## infant.deaths:CountryBangladesh                                                                  
    ## infant.deaths:CountryBarbados                                                                    
    ## infant.deaths:CountryBelarus                                                                  .  
    ## infant.deaths:CountryBelgium                                                                     
    ## infant.deaths:CountryBelize                                                                      
    ## infant.deaths:CountryBenin                                                                       
    ## infant.deaths:CountryBhutan                                                                      
    ## infant.deaths:CountryBolivia                                                                     
    ## infant.deaths:CountryBosnia and Herzegovina                                                      
    ## infant.deaths:CountryBotswana                                                                    
    ## infant.deaths:CountryBrazil                                                                      
    ## infant.deaths:CountryBrunei Darussalam                                                           
    ## infant.deaths:CountryBulgaria                                                                    
    ## infant.deaths:CountryBurkina Faso                                                                
    ## infant.deaths:CountryBurundi                                                                     
    ## infant.deaths:CountryCabo Verde                                                                  
    ## infant.deaths:CountryCambodia                                                                    
    ## infant.deaths:CountryCameroon                                                                    
    ## infant.deaths:CountryCanada                                                                      
    ## infant.deaths:CountryCentral African Republic                                                    
    ## infant.deaths:CountryChad                                                                        
    ## infant.deaths:CountryChile                                                                       
    ## infant.deaths:CountryChina                                                                       
    ## infant.deaths:CountryColombia                                                                    
    ## infant.deaths:CountryComoros                                                                     
    ## infant.deaths:CountryCongo                                                                       
    ## infant.deaths:CountryCosta Rica                                                                  
    ## infant.deaths:CountryCroatia                                                                     
    ## infant.deaths:CountryCuba                                                                        
    ## infant.deaths:CountryCyprus                                                                      
    ## infant.deaths:CountryCzechia                                                                     
    ## infant.deaths:CountryDemocratic People's Republic of Korea                                       
    ## infant.deaths:CountryDemocratic Republic of the Congo                                            
    ## infant.deaths:CountryDenmark                                                                     
    ## infant.deaths:CountryDjibouti                                                                 *  
    ## infant.deaths:CountryDominican Republic                                                          
    ## infant.deaths:CountryEcuador                                                                     
    ## infant.deaths:CountryEgypt                                                                       
    ## infant.deaths:CountryEl Salvador                                                              *  
    ## infant.deaths:CountryEquatorial Guinea                                                           
    ## infant.deaths:CountryEritrea                                                                  ** 
    ## infant.deaths:CountryEstonia                                                                     
    ## infant.deaths:CountryEthiopia                                                                    
    ## infant.deaths:CountryFiji                                                                        
    ## infant.deaths:CountryFinland                                                                     
    ## infant.deaths:CountryFrance                                                                      
    ## infant.deaths:CountryGabon                                                                       
    ## infant.deaths:CountryGambia                                                                      
    ## infant.deaths:CountryGeorgia                                                                     
    ## infant.deaths:CountryGermany                                                                  *  
    ## infant.deaths:CountryGhana                                                                    .  
    ## infant.deaths:CountryGreece                                                                      
    ## infant.deaths:CountryGrenada                                                                     
    ## infant.deaths:CountryGuatemala                                                                ***
    ## infant.deaths:CountryGuinea                                                                      
    ## infant.deaths:CountryGuinea-Bissau                                                            ** 
    ## infant.deaths:CountryGuyana                                                                      
    ## infant.deaths:CountryHaiti                                                                    ** 
    ## infant.deaths:CountryHonduras                                                                    
    ## infant.deaths:CountryHungary                                                                     
    ## infant.deaths:CountryIceland                                                                     
    ## infant.deaths:CountryIndia                                                                       
    ## infant.deaths:CountryIndonesia                                                                   
    ## infant.deaths:CountryIran                                                                        
    ## infant.deaths:CountryIraq                                                                     ** 
    ## infant.deaths:CountryIreland                                                                     
    ## infant.deaths:CountryIsrael                                                                      
    ## infant.deaths:CountryItaly                                                                       
    ## infant.deaths:CountryIvory Coast                                                                 
    ## infant.deaths:CountryJamaica                                                                     
    ## infant.deaths:CountryJapan                                                                       
    ## infant.deaths:CountryJordan                                                                      
    ## infant.deaths:CountryKazakhstan                                                                  
    ## infant.deaths:CountryKenya                                                                       
    ## infant.deaths:CountryKiribati                                                                    
    ## infant.deaths:CountryKuwait                                                                      
    ## infant.deaths:CountryKyrgyzstan                                                                  
    ## infant.deaths:CountryLao People's Democratic Republic                                            
    ## infant.deaths:CountryLatvia                                                                      
    ## infant.deaths:CountryLebanon                                                                     
    ## infant.deaths:CountryLesotho                                                                     
    ## infant.deaths:CountryLiberia                                                                     
    ## infant.deaths:CountryLibya                                                                    ** 
    ## infant.deaths:CountryLithuania                                                                   
    ## infant.deaths:CountryLuxembourg                                                                  
    ## infant.deaths:CountryMadagascar                                                                  
    ## infant.deaths:CountryMalawi                                                                      
    ## infant.deaths:CountryMalaysia                                                                    
    ## infant.deaths:CountryMaldives                                                                    
    ## infant.deaths:CountryMali                                                                        
    ## infant.deaths:CountryMalta                                                                       
    ## infant.deaths:CountryMauritania                                                                  
    ## infant.deaths:CountryMauritius                                                                   
    ## infant.deaths:CountryMexico                                                                      
    ## infant.deaths:CountryMicronesia (Federated States of)                                            
    ## infant.deaths:CountryMongolia                                                                    
    ## infant.deaths:CountryMontenegro                                                                  
    ## infant.deaths:CountryMorocco                                                                     
    ## infant.deaths:CountryMozambique                                                                  
    ## infant.deaths:CountryMyanmar                                                                     
    ## infant.deaths:CountryNamibia                                                                     
    ## infant.deaths:CountryNepal                                                                       
    ## infant.deaths:CountryNetherlands                                                                 
    ## infant.deaths:CountryNew Zealand                                                                 
    ## infant.deaths:CountryNicaragua                                                                   
    ## infant.deaths:CountryNiger                                                                    ***
    ## infant.deaths:CountryNigeria                                                                  .  
    ## infant.deaths:CountryNorway                                                                      
    ## infant.deaths:CountryOman                                                                        
    ## infant.deaths:CountryPakistan                                                                    
    ## infant.deaths:CountryPanama                                                                      
    ## infant.deaths:CountryPapua New Guinea                                                            
    ## infant.deaths:CountryParaguay                                                                    
    ## infant.deaths:CountryPeru                                                                        
    ## infant.deaths:CountryPhilippines                                                                 
    ## infant.deaths:CountryPoland                                                                      
    ## infant.deaths:CountryPortugal                                                                    
    ## infant.deaths:CountryQatar                                                                       
    ## infant.deaths:CountryRepublic of Korea                                                           
    ## infant.deaths:CountryRepublic of Moldova                                                         
    ## infant.deaths:CountryRomania                                                                  ***
    ## infant.deaths:CountryRussian Federation                                                          
    ## infant.deaths:CountryRwanda                                                                   .  
    ## infant.deaths:CountrySaint Lucia                                                                 
    ## infant.deaths:CountrySaint Vincent and the Grenadines                                            
    ## infant.deaths:CountrySamoa                                                                       
    ## infant.deaths:CountrySao Tome and Principe                                                       
    ## infant.deaths:CountrySaudi Arabia                                                                
    ## infant.deaths:CountrySenegal                                                                     
    ## infant.deaths:CountrySerbia                                                                      
    ## infant.deaths:CountrySeychelles                                                                  
    ## infant.deaths:CountrySierra Leone                                                             .  
    ## infant.deaths:CountrySingapore                                                                   
    ## infant.deaths:CountrySlovakia                                                                    
    ## infant.deaths:CountrySlovenia                                                                    
    ## infant.deaths:CountrySolomon Islands                                                             
    ## infant.deaths:CountrySomalia                                                                     
    ## infant.deaths:CountrySouth Africa                                                                
    ## infant.deaths:CountrySouth Sudan                                                                 
    ## infant.deaths:CountrySpain                                                                       
    ## infant.deaths:CountrySri Lanka                                                                   
    ## infant.deaths:CountrySudan                                                                    .  
    ## infant.deaths:CountrySuriname                                                                    
    ## infant.deaths:CountrySwaziland                                                                   
    ## infant.deaths:CountrySweden                                                                      
    ## infant.deaths:CountrySwitzerland                                                                 
    ## infant.deaths:CountrySyrian Arab Republic                                                        
    ## infant.deaths:CountryTajikistan                                                                  
    ## infant.deaths:CountryThailand                                                                    
    ## infant.deaths:CountryThe former Yugoslav republic of Macedonia                                   
    ## infant.deaths:CountryTimor-Leste                                                                 
    ## infant.deaths:CountryTogo                                                                        
    ## infant.deaths:CountryTonga                                                                       
    ## infant.deaths:CountryTrinidad and Tobago                                                         
    ## infant.deaths:CountryTunisia                                                                     
    ## infant.deaths:CountryTurkey                                                                      
    ## infant.deaths:CountryTurkmenistan                                                                
    ## infant.deaths:CountryUganda                                                                      
    ## infant.deaths:CountryUkraine                                                                  *  
    ## infant.deaths:CountryUnited Arab Emirates                                                        
    ## infant.deaths:CountryUnited Kingdom of Great Britain and Northern Ireland                        
    ## infant.deaths:CountryUnited Republic of Tanzania                                                 
    ## infant.deaths:CountryUnited States of America                                                    
    ## infant.deaths:CountryUruguay                                                                     
    ## infant.deaths:CountryUzbekistan                                                                  
    ## infant.deaths:CountryVanuatu                                                                     
    ## infant.deaths:CountryVenezuela                                                                   
    ## infant.deaths:CountryViet Nam                                                                    
    ## infant.deaths:CountryYemen                                                                       
    ## infant.deaths:CountryZambia                                                                      
    ## infant.deaths:CountryZimbabwe                                                                    
    ## log.Adult.Mortality:CountryAlbania                                                               
    ## log.Adult.Mortality:CountryAlgeria                                                               
    ## log.Adult.Mortality:CountryAngola                                                                
    ## log.Adult.Mortality:CountryAntigua and Barbuda                                                   
    ## log.Adult.Mortality:CountryArgentina                                                             
    ## log.Adult.Mortality:CountryArmenia                                                               
    ## log.Adult.Mortality:CountryAustralia                                                             
    ## log.Adult.Mortality:CountryAustria                                                               
    ## log.Adult.Mortality:CountryAzerbaijan                                                            
    ## log.Adult.Mortality:CountryBahamas                                                               
    ## log.Adult.Mortality:CountryBahrain                                                               
    ## log.Adult.Mortality:CountryBangladesh                                                            
    ## log.Adult.Mortality:CountryBarbados                                                              
    ## log.Adult.Mortality:CountryBelarus                                                               
    ## log.Adult.Mortality:CountryBelgium                                                               
    ## log.Adult.Mortality:CountryBelize                                                                
    ## log.Adult.Mortality:CountryBenin                                                                 
    ## log.Adult.Mortality:CountryBhutan                                                                
    ## log.Adult.Mortality:CountryBolivia                                                               
    ## log.Adult.Mortality:CountryBosnia and Herzegovina                                                
    ## log.Adult.Mortality:CountryBotswana                                                              
    ## log.Adult.Mortality:CountryBrazil                                                                
    ## log.Adult.Mortality:CountryBrunei Darussalam                                                     
    ## log.Adult.Mortality:CountryBulgaria                                                              
    ## log.Adult.Mortality:CountryBurkina Faso                                                          
    ## log.Adult.Mortality:CountryBurundi                                                               
    ## log.Adult.Mortality:CountryCabo Verde                                                            
    ## log.Adult.Mortality:CountryCambodia                                                              
    ## log.Adult.Mortality:CountryCameroon                                                              
    ## log.Adult.Mortality:CountryCanada                                                                
    ## log.Adult.Mortality:CountryCentral African Republic                                              
    ## log.Adult.Mortality:CountryChad                                                                  
    ## log.Adult.Mortality:CountryChile                                                                 
    ## log.Adult.Mortality:CountryChina                                                                 
    ## log.Adult.Mortality:CountryColombia                                                              
    ## log.Adult.Mortality:CountryComoros                                                               
    ## log.Adult.Mortality:CountryCongo                                                                 
    ## log.Adult.Mortality:CountryCosta Rica                                                            
    ## log.Adult.Mortality:CountryCroatia                                                               
    ## log.Adult.Mortality:CountryCuba                                                                  
    ## log.Adult.Mortality:CountryCyprus                                                                
    ## log.Adult.Mortality:CountryCzechia                                                               
    ## log.Adult.Mortality:CountryDemocratic People's Republic of Korea                                 
    ## log.Adult.Mortality:CountryDemocratic Republic of the Congo                                      
    ## log.Adult.Mortality:CountryDenmark                                                               
    ## log.Adult.Mortality:CountryDjibouti                                                           *  
    ## log.Adult.Mortality:CountryDominican Republic                                                    
    ## log.Adult.Mortality:CountryEcuador                                                               
    ## log.Adult.Mortality:CountryEgypt                                                                 
    ## log.Adult.Mortality:CountryEl Salvador                                                           
    ## log.Adult.Mortality:CountryEquatorial Guinea                                                     
    ## log.Adult.Mortality:CountryEritrea                                                            .  
    ## log.Adult.Mortality:CountryEstonia                                                               
    ## log.Adult.Mortality:CountryEthiopia                                                              
    ## log.Adult.Mortality:CountryFiji                                                                  
    ## log.Adult.Mortality:CountryFinland                                                               
    ## log.Adult.Mortality:CountryFrance                                                                
    ## log.Adult.Mortality:CountryGabon                                                                 
    ## log.Adult.Mortality:CountryGambia                                                                
    ## log.Adult.Mortality:CountryGeorgia                                                               
    ## log.Adult.Mortality:CountryGermany                                                               
    ## log.Adult.Mortality:CountryGhana                                                              .  
    ## log.Adult.Mortality:CountryGreece                                                                
    ## log.Adult.Mortality:CountryGrenada                                                               
    ## log.Adult.Mortality:CountryGuatemala                                                          ** 
    ## log.Adult.Mortality:CountryGuinea                                                                
    ## log.Adult.Mortality:CountryGuinea-Bissau                                                         
    ## log.Adult.Mortality:CountryGuyana                                                                
    ## log.Adult.Mortality:CountryHaiti                                                                 
    ## log.Adult.Mortality:CountryHonduras                                                              
    ## log.Adult.Mortality:CountryHungary                                                               
    ## log.Adult.Mortality:CountryIceland                                                               
    ## log.Adult.Mortality:CountryIndia                                                                 
    ## log.Adult.Mortality:CountryIndonesia                                                             
    ## log.Adult.Mortality:CountryIran                                                                  
    ## log.Adult.Mortality:CountryIraq                                                               *  
    ## log.Adult.Mortality:CountryIreland                                                               
    ## log.Adult.Mortality:CountryIsrael                                                                
    ## log.Adult.Mortality:CountryItaly                                                                 
    ## log.Adult.Mortality:CountryIvory Coast                                                           
    ## log.Adult.Mortality:CountryJamaica                                                               
    ## log.Adult.Mortality:CountryJapan                                                                 
    ## log.Adult.Mortality:CountryJordan                                                                
    ## log.Adult.Mortality:CountryKazakhstan                                                            
    ## log.Adult.Mortality:CountryKenya                                                                 
    ## log.Adult.Mortality:CountryKiribati                                                              
    ## log.Adult.Mortality:CountryKuwait                                                                
    ## log.Adult.Mortality:CountryKyrgyzstan                                                            
    ## log.Adult.Mortality:CountryLao People's Democratic Republic                                      
    ## log.Adult.Mortality:CountryLatvia                                                                
    ## log.Adult.Mortality:CountryLebanon                                                               
    ## log.Adult.Mortality:CountryLesotho                                                               
    ## log.Adult.Mortality:CountryLiberia                                                               
    ## log.Adult.Mortality:CountryLibya                                                                 
    ## log.Adult.Mortality:CountryLithuania                                                             
    ## log.Adult.Mortality:CountryLuxembourg                                                            
    ## log.Adult.Mortality:CountryMadagascar                                                         .  
    ## log.Adult.Mortality:CountryMalawi                                                                
    ## log.Adult.Mortality:CountryMalaysia                                                              
    ## log.Adult.Mortality:CountryMaldives                                                              
    ## log.Adult.Mortality:CountryMali                                                                  
    ## log.Adult.Mortality:CountryMalta                                                                 
    ## log.Adult.Mortality:CountryMauritania                                                            
    ## log.Adult.Mortality:CountryMauritius                                                             
    ## log.Adult.Mortality:CountryMexico                                                                
    ## log.Adult.Mortality:CountryMicronesia (Federated States of)                                      
    ## log.Adult.Mortality:CountryMongolia                                                              
    ## log.Adult.Mortality:CountryMontenegro                                                            
    ## log.Adult.Mortality:CountryMorocco                                                               
    ## log.Adult.Mortality:CountryMozambique                                                            
    ## log.Adult.Mortality:CountryMyanmar                                                               
    ## log.Adult.Mortality:CountryNamibia                                                               
    ## log.Adult.Mortality:CountryNepal                                                                 
    ## log.Adult.Mortality:CountryNetherlands                                                           
    ## log.Adult.Mortality:CountryNew Zealand                                                           
    ## log.Adult.Mortality:CountryNicaragua                                                             
    ## log.Adult.Mortality:CountryNiger                                                              ***
    ## log.Adult.Mortality:CountryNigeria                                                            .  
    ## log.Adult.Mortality:CountryNorway                                                                
    ## log.Adult.Mortality:CountryOman                                                                  
    ## log.Adult.Mortality:CountryPakistan                                                              
    ## log.Adult.Mortality:CountryPanama                                                                
    ## log.Adult.Mortality:CountryPapua New Guinea                                                      
    ## log.Adult.Mortality:CountryParaguay                                                              
    ## log.Adult.Mortality:CountryPeru                                                                  
    ## log.Adult.Mortality:CountryPhilippines                                                           
    ## log.Adult.Mortality:CountryPoland                                                                
    ## log.Adult.Mortality:CountryPortugal                                                              
    ## log.Adult.Mortality:CountryQatar                                                                 
    ## log.Adult.Mortality:CountryRepublic of Korea                                                     
    ## log.Adult.Mortality:CountryRepublic of Moldova                                                   
    ## log.Adult.Mortality:CountryRomania                                                               
    ## log.Adult.Mortality:CountryRussian Federation                                                    
    ## log.Adult.Mortality:CountryRwanda                                                                
    ## log.Adult.Mortality:CountrySaint Lucia                                                           
    ## log.Adult.Mortality:CountrySaint Vincent and the Grenadines                                      
    ## log.Adult.Mortality:CountrySamoa                                                                 
    ## log.Adult.Mortality:CountrySao Tome and Principe                                                 
    ## log.Adult.Mortality:CountrySaudi Arabia                                                          
    ## log.Adult.Mortality:CountrySenegal                                                               
    ## log.Adult.Mortality:CountrySerbia                                                                
    ## log.Adult.Mortality:CountrySeychelles                                                            
    ## log.Adult.Mortality:CountrySierra Leone                                                          
    ## log.Adult.Mortality:CountrySingapore                                                             
    ## log.Adult.Mortality:CountrySlovakia                                                              
    ## log.Adult.Mortality:CountrySlovenia                                                              
    ## log.Adult.Mortality:CountrySolomon Islands                                                       
    ## log.Adult.Mortality:CountrySomalia                                                               
    ## log.Adult.Mortality:CountrySouth Africa                                                          
    ## log.Adult.Mortality:CountrySouth Sudan                                                           
    ## log.Adult.Mortality:CountrySpain                                                                 
    ## log.Adult.Mortality:CountrySri Lanka                                                             
    ## log.Adult.Mortality:CountrySudan                                                              .  
    ## log.Adult.Mortality:CountrySuriname                                                              
    ## log.Adult.Mortality:CountrySwaziland                                                             
    ## log.Adult.Mortality:CountrySweden                                                                
    ## log.Adult.Mortality:CountrySwitzerland                                                           
    ## log.Adult.Mortality:CountrySyrian Arab Republic                                                  
    ## log.Adult.Mortality:CountryTajikistan                                                            
    ## log.Adult.Mortality:CountryThailand                                                              
    ## log.Adult.Mortality:CountryThe former Yugoslav republic of Macedonia                             
    ## log.Adult.Mortality:CountryTimor-Leste                                                           
    ## log.Adult.Mortality:CountryTogo                                                                  
    ## log.Adult.Mortality:CountryTonga                                                                 
    ## log.Adult.Mortality:CountryTrinidad and Tobago                                                   
    ## log.Adult.Mortality:CountryTunisia                                                               
    ## log.Adult.Mortality:CountryTurkey                                                                
    ## log.Adult.Mortality:CountryTurkmenistan                                                          
    ## log.Adult.Mortality:CountryUganda                                                                
    ## log.Adult.Mortality:CountryUkraine                                                               
    ## log.Adult.Mortality:CountryUnited Arab Emirates                                                  
    ## log.Adult.Mortality:CountryUnited Kingdom of Great Britain and Northern Ireland                  
    ## log.Adult.Mortality:CountryUnited Republic of Tanzania                                           
    ## log.Adult.Mortality:CountryUnited States of America                                              
    ## log.Adult.Mortality:CountryUruguay                                                               
    ## log.Adult.Mortality:CountryUzbekistan                                                            
    ## log.Adult.Mortality:CountryVanuatu                                                               
    ## log.Adult.Mortality:CountryVenezuela                                                             
    ## log.Adult.Mortality:CountryViet Nam                                                              
    ## log.Adult.Mortality:CountryYemen                                                                 
    ## log.Adult.Mortality:CountryZambia                                                                
    ## log.Adult.Mortality:CountryZimbabwe                                                              
    ## log.Adult.Mortality:infant.deaths:CountryAfghanistan                                             
    ## log.Adult.Mortality:infant.deaths:CountryAlbania                                                 
    ## log.Adult.Mortality:infant.deaths:CountryAlgeria                                                 
    ## log.Adult.Mortality:infant.deaths:CountryAngola                                                  
    ## log.Adult.Mortality:infant.deaths:CountryAntigua and Barbuda                                     
    ## log.Adult.Mortality:infant.deaths:CountryArgentina                                               
    ## log.Adult.Mortality:infant.deaths:CountryArmenia                                                 
    ## log.Adult.Mortality:infant.deaths:CountryAustralia                                               
    ## log.Adult.Mortality:infant.deaths:CountryAustria                                                 
    ## log.Adult.Mortality:infant.deaths:CountryAzerbaijan                                              
    ## log.Adult.Mortality:infant.deaths:CountryBahamas                                                 
    ## log.Adult.Mortality:infant.deaths:CountryBahrain                                                 
    ## log.Adult.Mortality:infant.deaths:CountryBangladesh                                              
    ## log.Adult.Mortality:infant.deaths:CountryBarbados                                                
    ## log.Adult.Mortality:infant.deaths:CountryBelarus                                                 
    ## log.Adult.Mortality:infant.deaths:CountryBelgium                                                 
    ## log.Adult.Mortality:infant.deaths:CountryBelize                                                  
    ## log.Adult.Mortality:infant.deaths:CountryBenin                                                   
    ## log.Adult.Mortality:infant.deaths:CountryBhutan                                                  
    ## log.Adult.Mortality:infant.deaths:CountryBolivia                                                 
    ## log.Adult.Mortality:infant.deaths:CountryBosnia and Herzegovina                                  
    ## log.Adult.Mortality:infant.deaths:CountryBotswana                                                
    ## log.Adult.Mortality:infant.deaths:CountryBrazil                                                  
    ## log.Adult.Mortality:infant.deaths:CountryBrunei Darussalam                                       
    ## log.Adult.Mortality:infant.deaths:CountryBulgaria                                                
    ## log.Adult.Mortality:infant.deaths:CountryBurkina Faso                                            
    ## log.Adult.Mortality:infant.deaths:CountryBurundi                                                 
    ## log.Adult.Mortality:infant.deaths:CountryCabo Verde                                              
    ## log.Adult.Mortality:infant.deaths:CountryCambodia                                                
    ## log.Adult.Mortality:infant.deaths:CountryCameroon                                                
    ## log.Adult.Mortality:infant.deaths:CountryCanada                                                  
    ## log.Adult.Mortality:infant.deaths:CountryCentral African Republic                                
    ## log.Adult.Mortality:infant.deaths:CountryChad                                                    
    ## log.Adult.Mortality:infant.deaths:CountryChile                                                   
    ## log.Adult.Mortality:infant.deaths:CountryChina                                                   
    ## log.Adult.Mortality:infant.deaths:CountryColombia                                                
    ## log.Adult.Mortality:infant.deaths:CountryComoros                                                 
    ## log.Adult.Mortality:infant.deaths:CountryCongo                                                   
    ## log.Adult.Mortality:infant.deaths:CountryCosta Rica                                              
    ## log.Adult.Mortality:infant.deaths:CountryCroatia                                                 
    ## log.Adult.Mortality:infant.deaths:CountryCuba                                                    
    ## log.Adult.Mortality:infant.deaths:CountryCyprus                                                  
    ## log.Adult.Mortality:infant.deaths:CountryCzechia                                                 
    ## log.Adult.Mortality:infant.deaths:CountryDemocratic People's Republic of Korea                   
    ## log.Adult.Mortality:infant.deaths:CountryDemocratic Republic of the Congo                        
    ## log.Adult.Mortality:infant.deaths:CountryDenmark                                                 
    ## log.Adult.Mortality:infant.deaths:CountryDjibouti                                             *  
    ## log.Adult.Mortality:infant.deaths:CountryDominican Republic                                      
    ## log.Adult.Mortality:infant.deaths:CountryEcuador                                                 
    ## log.Adult.Mortality:infant.deaths:CountryEgypt                                                   
    ## log.Adult.Mortality:infant.deaths:CountryEl Salvador                                          *  
    ## log.Adult.Mortality:infant.deaths:CountryEquatorial Guinea                                       
    ## log.Adult.Mortality:infant.deaths:CountryEritrea                                              *  
    ## log.Adult.Mortality:infant.deaths:CountryEstonia                                                 
    ## log.Adult.Mortality:infant.deaths:CountryEthiopia                                                
    ## log.Adult.Mortality:infant.deaths:CountryFiji                                                    
    ## log.Adult.Mortality:infant.deaths:CountryFinland                                                 
    ## log.Adult.Mortality:infant.deaths:CountryFrance                                                  
    ## log.Adult.Mortality:infant.deaths:CountryGabon                                                   
    ## log.Adult.Mortality:infant.deaths:CountryGambia                                                  
    ## log.Adult.Mortality:infant.deaths:CountryGeorgia                                                 
    ## log.Adult.Mortality:infant.deaths:CountryGermany                                              ** 
    ## log.Adult.Mortality:infant.deaths:CountryGhana                                                .  
    ## log.Adult.Mortality:infant.deaths:CountryGreece                                                  
    ## log.Adult.Mortality:infant.deaths:CountryGrenada                                                 
    ## log.Adult.Mortality:infant.deaths:CountryGuatemala                                            ***
    ## log.Adult.Mortality:infant.deaths:CountryGuinea                                                  
    ## log.Adult.Mortality:infant.deaths:CountryGuinea-Bissau                                        ** 
    ## log.Adult.Mortality:infant.deaths:CountryGuyana                                                  
    ## log.Adult.Mortality:infant.deaths:CountryHaiti                                                *  
    ## log.Adult.Mortality:infant.deaths:CountryHonduras                                                
    ## log.Adult.Mortality:infant.deaths:CountryHungary                                                 
    ## log.Adult.Mortality:infant.deaths:CountryIceland                                                 
    ## log.Adult.Mortality:infant.deaths:CountryIndia                                                   
    ## log.Adult.Mortality:infant.deaths:CountryIndonesia                                               
    ## log.Adult.Mortality:infant.deaths:CountryIran                                                    
    ## log.Adult.Mortality:infant.deaths:CountryIraq                                                 *  
    ## log.Adult.Mortality:infant.deaths:CountryIreland                                                 
    ## log.Adult.Mortality:infant.deaths:CountryIsrael                                                  
    ## log.Adult.Mortality:infant.deaths:CountryItaly                                                   
    ## log.Adult.Mortality:infant.deaths:CountryIvory Coast                                             
    ## log.Adult.Mortality:infant.deaths:CountryJamaica                                                 
    ## log.Adult.Mortality:infant.deaths:CountryJapan                                                   
    ## log.Adult.Mortality:infant.deaths:CountryJordan                                                  
    ## log.Adult.Mortality:infant.deaths:CountryKazakhstan                                              
    ## log.Adult.Mortality:infant.deaths:CountryKenya                                                   
    ## log.Adult.Mortality:infant.deaths:CountryKiribati                                                
    ## log.Adult.Mortality:infant.deaths:CountryKuwait                                                  
    ## log.Adult.Mortality:infant.deaths:CountryKyrgyzstan                                              
    ## log.Adult.Mortality:infant.deaths:CountryLao People's Democratic Republic                        
    ## log.Adult.Mortality:infant.deaths:CountryLatvia                                                  
    ## log.Adult.Mortality:infant.deaths:CountryLebanon                                                 
    ## log.Adult.Mortality:infant.deaths:CountryLesotho                                                 
    ## log.Adult.Mortality:infant.deaths:CountryLiberia                                                 
    ## log.Adult.Mortality:infant.deaths:CountryLibya                                                ** 
    ## log.Adult.Mortality:infant.deaths:CountryLithuania                                               
    ## log.Adult.Mortality:infant.deaths:CountryLuxembourg                                              
    ## log.Adult.Mortality:infant.deaths:CountryMadagascar                                              
    ## log.Adult.Mortality:infant.deaths:CountryMalawi                                                  
    ## log.Adult.Mortality:infant.deaths:CountryMalaysia                                                
    ## log.Adult.Mortality:infant.deaths:CountryMaldives                                                
    ## log.Adult.Mortality:infant.deaths:CountryMali                                                    
    ## log.Adult.Mortality:infant.deaths:CountryMalta                                                   
    ## log.Adult.Mortality:infant.deaths:CountryMauritania                                              
    ## log.Adult.Mortality:infant.deaths:CountryMauritius                                               
    ## log.Adult.Mortality:infant.deaths:CountryMexico                                                  
    ## log.Adult.Mortality:infant.deaths:CountryMicronesia (Federated States of)                        
    ## log.Adult.Mortality:infant.deaths:CountryMongolia                                                
    ## log.Adult.Mortality:infant.deaths:CountryMontenegro                                              
    ## log.Adult.Mortality:infant.deaths:CountryMorocco                                                 
    ## log.Adult.Mortality:infant.deaths:CountryMozambique                                           ** 
    ## log.Adult.Mortality:infant.deaths:CountryMyanmar                                                 
    ## log.Adult.Mortality:infant.deaths:CountryNamibia                                                 
    ## log.Adult.Mortality:infant.deaths:CountryNepal                                                   
    ## log.Adult.Mortality:infant.deaths:CountryNetherlands                                             
    ## log.Adult.Mortality:infant.deaths:CountryNew Zealand                                             
    ## log.Adult.Mortality:infant.deaths:CountryNicaragua                                            .  
    ## log.Adult.Mortality:infant.deaths:CountryNiger                                                ***
    ## log.Adult.Mortality:infant.deaths:CountryNigeria                                              *  
    ## log.Adult.Mortality:infant.deaths:CountryNorway                                                  
    ## log.Adult.Mortality:infant.deaths:CountryOman                                                    
    ## log.Adult.Mortality:infant.deaths:CountryPakistan                                                
    ## log.Adult.Mortality:infant.deaths:CountryPanama                                                  
    ## log.Adult.Mortality:infant.deaths:CountryPapua New Guinea                                        
    ## log.Adult.Mortality:infant.deaths:CountryParaguay                                             .  
    ## log.Adult.Mortality:infant.deaths:CountryPeru                                                    
    ## log.Adult.Mortality:infant.deaths:CountryPhilippines                                             
    ## log.Adult.Mortality:infant.deaths:CountryPoland                                                  
    ## log.Adult.Mortality:infant.deaths:CountryPortugal                                                
    ## log.Adult.Mortality:infant.deaths:CountryQatar                                                   
    ## log.Adult.Mortality:infant.deaths:CountryRepublic of Korea                                       
    ## log.Adult.Mortality:infant.deaths:CountryRepublic of Moldova                                     
    ## log.Adult.Mortality:infant.deaths:CountryRomania                                              ***
    ## log.Adult.Mortality:infant.deaths:CountryRussian Federation                                      
    ## log.Adult.Mortality:infant.deaths:CountryRwanda                                               *  
    ## log.Adult.Mortality:infant.deaths:CountrySaint Lucia                                             
    ## log.Adult.Mortality:infant.deaths:CountrySaint Vincent and the Grenadines                        
    ## log.Adult.Mortality:infant.deaths:CountrySamoa                                                   
    ## log.Adult.Mortality:infant.deaths:CountrySao Tome and Principe                                   
    ## log.Adult.Mortality:infant.deaths:CountrySaudi Arabia                                            
    ## log.Adult.Mortality:infant.deaths:CountrySenegal                                                 
    ## log.Adult.Mortality:infant.deaths:CountrySerbia                                                  
    ## log.Adult.Mortality:infant.deaths:CountrySeychelles                                              
    ## log.Adult.Mortality:infant.deaths:CountrySierra Leone                                         *  
    ## log.Adult.Mortality:infant.deaths:CountrySingapore                                               
    ## log.Adult.Mortality:infant.deaths:CountrySlovakia                                                
    ## log.Adult.Mortality:infant.deaths:CountrySlovenia                                                
    ## log.Adult.Mortality:infant.deaths:CountrySolomon Islands                                         
    ## log.Adult.Mortality:infant.deaths:CountrySomalia                                                 
    ## log.Adult.Mortality:infant.deaths:CountrySouth Africa                                         .  
    ## log.Adult.Mortality:infant.deaths:CountrySouth Sudan                                             
    ## log.Adult.Mortality:infant.deaths:CountrySpain                                                   
    ## log.Adult.Mortality:infant.deaths:CountrySri Lanka                                               
    ## log.Adult.Mortality:infant.deaths:CountrySudan                                                   
    ## log.Adult.Mortality:infant.deaths:CountrySuriname                                                
    ## log.Adult.Mortality:infant.deaths:CountrySwaziland                                               
    ## log.Adult.Mortality:infant.deaths:CountrySweden                                                  
    ## log.Adult.Mortality:infant.deaths:CountrySwitzerland                                             
    ## log.Adult.Mortality:infant.deaths:CountrySyrian Arab Republic                                    
    ## log.Adult.Mortality:infant.deaths:CountryTajikistan                                              
    ## log.Adult.Mortality:infant.deaths:CountryThailand                                                
    ## log.Adult.Mortality:infant.deaths:CountryThe former Yugoslav republic of Macedonia               
    ## log.Adult.Mortality:infant.deaths:CountryTimor-Leste                                             
    ## log.Adult.Mortality:infant.deaths:CountryTogo                                                    
    ## log.Adult.Mortality:infant.deaths:CountryTonga                                                   
    ## log.Adult.Mortality:infant.deaths:CountryTrinidad and Tobago                                     
    ## log.Adult.Mortality:infant.deaths:CountryTunisia                                                 
    ## log.Adult.Mortality:infant.deaths:CountryTurkey                                                  
    ## log.Adult.Mortality:infant.deaths:CountryTurkmenistan                                            
    ## log.Adult.Mortality:infant.deaths:CountryUganda                                                  
    ## log.Adult.Mortality:infant.deaths:CountryUkraine                                              *  
    ## log.Adult.Mortality:infant.deaths:CountryUnited Arab Emirates                                    
    ## log.Adult.Mortality:infant.deaths:CountryUnited Kingdom of Great Britain and Northern Ireland    
    ## log.Adult.Mortality:infant.deaths:CountryUnited Republic of Tanzania                          *  
    ## log.Adult.Mortality:infant.deaths:CountryUnited States of America                                
    ## log.Adult.Mortality:infant.deaths:CountryUruguay                                                 
    ## log.Adult.Mortality:infant.deaths:CountryUzbekistan                                              
    ## log.Adult.Mortality:infant.deaths:CountryVanuatu                                                 
    ## log.Adult.Mortality:infant.deaths:CountryVenezuela                                               
    ## log.Adult.Mortality:infant.deaths:CountryViet Nam                                                
    ## log.Adult.Mortality:infant.deaths:CountryYemen                                                   
    ## log.Adult.Mortality:infant.deaths:CountryZambia                                                  
    ## log.Adult.Mortality:infant.deaths:CountryZimbabwe                                             .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.39 on 1890 degrees of freedom
    ## Multiple R-squared:  0.984,  Adjusted R-squared:  0.979 
    ## F-statistic:  192 on 598 and 1890 DF,  p-value: <0.0000000000000002

``` r
hist(residuals, main ="Histogram of Residuals")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/run%20custom%20model-1.png)<!-- -->

``` r
plot(residuals, main="Residuals Plot")
abline(h=0, col="blue")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/run%20custom%20model-2.png)<!-- -->

``` r
plot(fit.custom,which=2)
```

    ## Warning: not plotting observations with leverage one:
    ##   106, 363, 421, 461, 712, 898, 1300, 1398, 1979, 2072, 2319

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/run%20custom%20model-3.png)<!-- -->

``` r
plot(fit.custom, which =4)
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/run%20custom%20model-4.png)<!-- -->

``` r
#### Scatter plot
plot(test_pred ~ ytest, main = "Original vs Predicted scatter plot (Custom MLR Miguel)", xlab = 'Original observations', ylab='Predicted values')
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/run%20custom%20model-5.png)<!-- -->

``` r
#### Model test scores
rmse = test_score[1]
rsqd = test_score[2]
mse = rmse^2
n=dim(xtest)[1]
p = length(fit.custom$coefficients)-1
adjrsqd = 1 - (1 - rsqd) * ((n - 1)/(n-p-1))

## Train scores
sm=summary(fit.custom)
rmse_trn = train_score[1]
rsqd_trn = train_score[2]
mse_trn = rmse_trn^2
mse_trndf = mse_trn
adjrsqd_trn = sm$adj.r.squared

eval_test_df = rbind(eval_test_df, c('MLR Interact - Miguel', format(round(mse,4),nsmall=4), format(round(rsqd,4),nsmall=4), format(round(adjrsqd,4),nsmall=4), format(round(rmse,4),nsmall=4)))

eval_train_df = rbind(eval_train_df, c('MLR Interact - Miguel', format(round(mse_trndf,4),nsmall=4), format(round(rsqd_trn,4),nsmall=4), format(round(adjrsqd_trn,4),nsmall=4), format(round(rmse_trn,4),nsmall=4)))
```

### Visualize ‘k’ and the most important features

## KNN without cross validation

## KNN regression Observations:

KNN is a non-parametric regression or classification algorithm. In our
case we used the regression version. The algorithm uses similaroty

# Training set scores

``` r
knitr::kable(eval_train_df, "html")
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
Rsquared
</td>
<td style="text-align:left;">
LASSO
</td>
<td style="text-align:left;">
6.1668
</td>
<td style="text-align:left;">
0.9332
</td>
<td style="text-align:left;">
0.9326
</td>
<td style="text-align:left;">
2.4833
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
6.2796
</td>
<td style="text-align:left;">
0.9320
</td>
<td style="text-align:left;">
0.9316
</td>
<td style="text-align:left;">
2.5059
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
6.2796
</td>
<td style="text-align:left;">
0.9320
</td>
<td style="text-align:left;">
0.9316
</td>
<td style="text-align:left;">
2.5059
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
6.8023
</td>
<td style="text-align:left;">
0.9272
</td>
<td style="text-align:left;">
0.9265
</td>
<td style="text-align:left;">
2.6081
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
6.1723
</td>
<td style="text-align:left;">
0.9331
</td>
<td style="text-align:left;">
0.9325
</td>
<td style="text-align:left;">
2.4844
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
6.5733
</td>
<td style="text-align:left;">
0.9288
</td>
<td style="text-align:left;">
0.9285
</td>
<td style="text-align:left;">
2.5639
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
6.5135
</td>
<td style="text-align:left;">
0.9294
</td>
<td style="text-align:left;">
0.9290
</td>
<td style="text-align:left;">
2.5522
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
8.4399
</td>
<td style="text-align:left;">
0.9085
</td>
<td style="text-align:left;">
0.9083
</td>
<td style="text-align:left;">
2.9052
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
0.9787
</td>
<td style="text-align:left;">
1.2074
</td>
</tr>
</tbody>
</table>

# Test set scores

``` r
knitr::kable(eval_test_df, "html")
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
6.3827
</td>
<td style="text-align:left;">
0.9218
</td>
<td style="text-align:left;">
0.9192
</td>
<td style="text-align:left;">
2.5264
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
8.3690
</td>
<td style="text-align:left;">
0.8989
</td>
<td style="text-align:left;">
0.8968
</td>
<td style="text-align:left;">
2.8929
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
2.7993
</td>
<td style="text-align:left;">
0.9705
</td>
<td style="text-align:left;">
1.0436
</td>
<td style="text-align:left;">
1.6731
</td>
</tr>
</tbody>
</table>
