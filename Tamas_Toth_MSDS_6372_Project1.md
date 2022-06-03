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
Brazil
</td>
<td style="text-align:right;">
2005
</td>
<td style="text-align:left;">
Developing
</td>
<td style="text-align:right;">
72.7
</td>
<td style="text-align:right;">
163
</td>
<td style="text-align:right;">
75
</td>
<td style="text-align:right;">
6.97
</td>
<td style="text-align:right;">
23.76
</td>
<td style="text-align:right;">
98
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
47.8
</td>
<td style="text-align:right;">
85
</td>
<td style="text-align:right;">
99
</td>
<td style="text-align:right;">
8.27
</td>
<td style="text-align:right;">
99
</td>
<td style="text-align:right;">
0.1
</td>
<td style="text-align:right;">
477.2
</td>
<td style="text-align:right;">
186917361
</td>
<td style="text-align:right;">
3.2
</td>
<td style="text-align:right;">
3.1
</td>
<td style="text-align:right;">
0.694
</td>
<td style="text-align:right;">
13.8
</td>
</tr>
<tr>
<td style="text-align:left;">
Switzerland
</td>
<td style="text-align:right;">
2005
</td>
<td style="text-align:left;">
Developed
</td>
<td style="text-align:right;">
81.1
</td>
<td style="text-align:right;">
66
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
10.15
</td>
<td style="text-align:right;">
10055.35
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
60
</td>
<td style="text-align:right;">
53.2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
94
</td>
<td style="text-align:right;">
1.86
</td>
<td style="text-align:right;">
94
</td>
<td style="text-align:right;">
0.1
</td>
<td style="text-align:right;">
54797.5
</td>
<td style="text-align:right;">
7437115
</td>
<td style="text-align:right;">
0.6
</td>
<td style="text-align:right;">
0.4
</td>
<td style="text-align:right;">
0.899
</td>
<td style="text-align:right;">
15.2
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:right;">
2012
</td>
<td style="text-align:left;">
Developed
</td>
<td style="text-align:right;">
82.0
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
7.49
</td>
<td style="text-align:right;">
4793.90
</td>
<td style="text-align:right;">
96
</td>
<td style="text-align:right;">
376
</td>
<td style="text-align:right;">
62.1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
96
</td>
<td style="text-align:right;">
9.28
</td>
<td style="text-align:right;">
97
</td>
<td style="text-align:right;">
0.1
</td>
<td style="text-align:right;">
34814.1
</td>
<td style="text-align:right;">
59539717
</td>
<td style="text-align:right;">
0.5
</td>
<td style="text-align:right;">
0.6
</td>
<td style="text-align:right;">
0.877
</td>
<td style="text-align:right;">
16.6
</td>
</tr>
<tr>
<td style="text-align:left;">
Samoa
</td>
<td style="text-align:right;">
2015
</td>
<td style="text-align:left;">
Developing
</td>
<td style="text-align:right;">
74.0
</td>
<td style="text-align:right;">
125
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
59
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
74.7
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
61
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
66
</td>
<td style="text-align:right;">
0.1
</td>
<td style="text-align:right;">
4149.4
</td>
<td style="text-align:right;">
193759
</td>
<td style="text-align:right;">
0.2
</td>
<td style="text-align:right;">
0.1
</td>
<td style="text-align:right;">
0.702
</td>
<td style="text-align:right;">
12.9
</td>
</tr>
<tr>
<td style="text-align:left;">
Thailand
</td>
<td style="text-align:right;">
2006
</td>
<td style="text-align:left;">
Developing
</td>
<td style="text-align:right;">
73.0
</td>
<td style="text-align:right;">
168
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
6.18
</td>
<td style="text-align:right;">
433.92
</td>
<td style="text-align:right;">
96
</td>
<td style="text-align:right;">
3588
</td>
<td style="text-align:right;">
23.6
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
98
</td>
<td style="text-align:right;">
3.49
</td>
<td style="text-align:right;">
98
</td>
<td style="text-align:right;">
0.5
</td>
<td style="text-align:right;">
3369.0
</td>
<td style="text-align:right;">
65824164
</td>
<td style="text-align:right;">
8.7
</td>
<td style="text-align:right;">
8.9
</td>
<td style="text-align:right;">
0.686
</td>
<td style="text-align:right;">
12.4
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
#drop new variables
drop = c("GDP2","Schooling2","comp2", "Population2")
LifeExp <- LifeExp[,!colnames(LifeExp) %in% drop]
LifeExpKNN = LifeExp
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
\## Overall life expectancy over time

``` r
#####################################################################################
#                          Overall life expectancy over time                        #
#####################################################################################

mean_LifeExp = LifeExp %>% group_by(Year) %>% summarise_at(vars(Life.expectancy), list(meanle = mean))
mean_LifeExp_reg = LifeExp %>% group_by(Year, Region) %>% summarise_at(vars(Life.expectancy), list(meanle = mean))

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

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-15-1.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-15-2.png" angle=90 style="display: block; margin: auto;" />

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

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-16-1.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-16-2.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-16-3.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-16-4.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-16-5.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-16-6.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-16-7.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-16-8.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-16-9.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-16-10.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-16-11.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-16-12.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-16-13.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-16-14.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-16-15.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-16-16.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-16-17.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-16-18.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-16-19.png" angle=90 style="display: block; margin: auto;" />

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

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-17-1.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-17-2.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-17-3.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-17-4.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-17-5.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-17-6.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-17-7.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-17-8.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-17-9.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-17-10.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-17-11.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-17-12.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-17-13.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-17-14.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-17-15.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-17-16.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-17-17.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-17-18.png" angle=90 style="display: block; margin: auto;" /><img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-17-19.png" angle=90 style="display: block; margin: auto;" />
\## Overall life expectancy over time

``` r
#####################################################################################
#                          Overall life expectancy over time                        #
#####################################################################################

mean_LifeExp = LifeExp %>% group_by(Year) %>% summarise_at(vars(Life.expectancy), list(meanle = mean))
mean_LifeExp_reg = LifeExp %>% group_by(Year, Region) %>% summarise_at(vars(Life.expectancy), list(meanle = mean))

#Overall life expectancy
ggplot(data=mean_LifeExp, aes(x=Year, y=meanle)) +
  geom_line()+
  geom_point() +
   ggtitle("Mean life expactancy by year") +
  theme(plot.title = element_text(hjust = 0.5))+
   xlab("Year") + ylab("Average Life Expectancy")
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-18-1.png" angle=90 style="display: block; margin: auto;" />

``` r
#Overall life expectancy by Region
ggplot(data=mean_LifeExp_reg, aes(x=Year, y=meanle, group = Region)) +
  geom_line(aes(color=Region))+
  geom_point(aes(color=Region)) +
  ggtitle("Mean life expactancy by year by region") +
theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Year") + ylab("Average Life Expectancy")
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-18-2.png" angle=90 style="display: block; margin: auto;" />

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

drop = c('Country')#,'HIV.AIDS', 'percentage.expenditure')
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
    ##  3 Adult.Mortality     2489     0 1.65e2 1.25e2 2.51e+0 155     1.16       1.66 
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
    ##  3 Adult.Mortality            0.916 4.81e-35   2489
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

#####################################################################################
#                                       Lasso                                       #
#####################################################################################
library(glmnet)
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

    ## [1] 9.56

``` r
coef(lasso.mod,s=bestlambda)
```

    ## 49 x 1 sparse Matrix of class "dgCMatrix"
    ##                                               s1
    ## (Intercept)                     -120.14327346318
    ## Year                               0.08750529491
    ## StatusDeveloping                  -0.94632375192
    ## Adult.Mortality                   -0.01112238578
    ## infant.deaths                      .            
    ## Alcohol                           -0.03079196830
    ## percentage.expenditure             0.00004106166
    ## Hepatitis.B                        0.00000078399
    ## Measles                           -0.00001786203
    ## BMI                                0.00616366644
    ## under.five.deaths                 -0.00334059322
    ## Polio                              0.02102923336
    ## Total.expenditure                 -0.01929565325
    ## Diphtheria                         0.02205563345
    ## HIV.AIDS                          -0.14729822668
    ## GDP                                .            
    ## Population                         0.00000000372
    ## thinness..1.19.years               .            
    ## thinness.5.9.years                -0.04223255378
    ## Income.composition.of.resources    4.34871417174
    ## Schooling                          0.49399422121
    ## RegionAustralia and New Zealand    4.19352305715
    ## RegionCanada                       3.49560169034
    ## RegionCaribbean                    1.36797908786
    ## RegionCentral Africa              -0.95436529961
    ## RegionCentral Asia                -3.36511760547
    ## RegionCentral Europe               1.80754934977
    ## RegionEastern Africa               .            
    ## RegionEastern Europe               .            
    ## RegionMashriq                      0.18153042423
    ## RegionMeso-America                 1.85640652781
    ## RegionNorth Africa                 0.87052427119
    ## RegionNW Pacific and East Asia     .            
    ## RegionSouth America                .            
    ## RegionSouth Asia                  -0.34653278637
    ## RegionSouth Pacific                .            
    ## RegionSoutheast Asia               .            
    ## RegionSouthern Africa             -1.89355097364
    ## RegionUS                          -0.45974958846
    ## RegionWestern Africa              -2.73932758290
    ## RegionWestern Europe               5.06571615217
    ## RegionWestern Indian Ocean        -0.22048042099
    ## log.HIV.AIDS                      -1.60288207592
    ## log.GDP                            0.11070598425
    ## log.percentage.expenditure         0.12191491203
    ## ContinentAmericas                  2.49409765176
    ## ContinentAsia                      1.44743164878
    ## ContinentEurope                    .            
    ## ContinentOceania                   .

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
    ##   3.0919   0.8832   2.3310

``` r
##### Fit Linear Model based on LASSO regularization without factors to measure VIF####
fit.lasso.lm = lm(Life.expectancy ~ Year + Adult.Mortality + Alcohol + log.percentage.expenditure + Hepatitis.B + Measles + BMI + under.five.deaths + Polio + Total.expenditure + Diphtheria + log.HIV.AIDS + Population + thinness.5.9.years + Income.composition.of.resources + Schooling + log.GDP, data = train)
summary(fit.lasso.lm)
```

    ## 
    ## Call:
    ## lm(formula = Life.expectancy ~ Year + Adult.Mortality + Alcohol + 
    ##     log.percentage.expenditure + Hepatitis.B + Measles + BMI + 
    ##     under.five.deaths + Polio + Total.expenditure + Diphtheria + 
    ##     log.HIV.AIDS + Population + thinness.5.9.years + Income.composition.of.resources + 
    ##     Schooling + log.GDP, data = train)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -18.869  -2.023  -0.043   1.966  16.346 
    ## 
    ## Coefficients:
    ##                                         Estimate       Std. Error t value
    ## (Intercept)                     -65.816869664578  34.568489373985   -1.90
    ## Year                              0.059213650381   0.017286016352    3.43
    ## Adult.Mortality                  -0.014947631507   0.000785990378  -19.02
    ## Alcohol                           0.114313628030   0.024244207641    4.72
    ## log.percentage.expenditure        0.200532992556   0.033167260950    6.05
    ## Hepatitis.B                      -0.005040358472   0.003759889757   -1.34
    ## Measles                          -0.000027043682   0.000007612304   -3.55
    ## BMI                               0.010138195221   0.004828773006    2.10
    ## under.five.deaths                -0.002844508349   0.000702763055   -4.05
    ## Polio                             0.016561770548   0.004389774026    3.77
    ## Total.expenditure                 0.032548163639   0.032382693943    1.01
    ## Diphtheria                        0.023298365019   0.004729613285    4.93
    ## log.HIV.AIDS                     -2.409272907985   0.068385919287  -35.23
    ## Population                        0.000000003919   0.000000000765    5.13
    ## thinness.5.9.years               -0.092905779016   0.022832000254   -4.07
    ## Income.composition.of.resources   6.006774382857   0.631112751439    9.52
    ## Schooling                         0.468967192787   0.043943061997   10.67
    ## log.GDP                           0.267173823302   0.060277170365    4.43
    ##                                             Pr(>|t|)    
    ## (Intercept)                                  0.05703 .  
    ## Year                                         0.00062 ***
    ## Adult.Mortality                 < 0.0000000000000002 ***
    ## Alcohol                                 0.0000025504 ***
    ## log.percentage.expenditure              0.0000000017 ***
    ## Hepatitis.B                                  0.18019    
    ## Measles                                      0.00039 ***
    ## BMI                                          0.03587 *  
    ## under.five.deaths                       0.0000533344 ***
    ## Polio                                        0.00017 ***
    ## Total.expenditure                            0.31494    
    ## Diphtheria                              0.0000008945 ***
    ## log.HIV.AIDS                    < 0.0000000000000002 ***
    ## Population                              0.0000003188 ***
    ## thinness.5.9.years                      0.0000486747 ***
    ## Income.composition.of.resources < 0.0000000000000002 ***
    ## Schooling                       < 0.0000000000000002 ***
    ## log.GDP                                 0.0000097249 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.65 on 2471 degrees of freedom
    ## Multiple R-squared:  0.857,  Adjusted R-squared:  0.856 
    ## F-statistic:  868 on 17 and 2471 DF,  p-value: <0.0000000000000002

``` r
### Visualize VIF
fit.lasso.lm_VIF = vif(fit.lasso.lm)
barplot(fit.lasso.lm_VIF, main = 'VIF Values', horiz = TRUE, col="blue", xlim = c(0,12))
abline(v=10, col="red")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-4.png)<!-- -->

``` r
######################################################################################################
#Tamas I believe that with the new data set there is no colinearity based on VIF unless we go with 4 #
# I'm commenting out the next section of code for now                                                #
######################################################################################################


# We can see that LASSO did not remove infant.deaths or under.five.deaths as those are perfectly correlated. Let's remove the one with the smallest coefficient (infant.death).

##### Fit Linear Model based on LASSO regularization without factors to measure VIF####
#fit.lasso.lm2 = lm(Life.expectancy ~ Year + Adult.Mortality + Alcohol + Hepatitis.B + Measles + BMI + under.five.deaths + Polio + Total.expenditure + Diphtheria + thinness.5.9.years + Income.composition.of.resources + Schooling + log.HIV.AIDS + log.GDP + log.percentage.expenditure, data = train)
#summary(fit.lasso.lm2)

### re-run Visualize VIF
#fit.lasso.lm_VIF2 = vif(fit.lasso.lm2)
#barplot(fit.lasso.lm_VIF2, main = 'VIF Values', horiz = TRUE, col="blue", xlim = c(0,12))
#abline(v=10, col="red")

##### Fit Linear Model based on LASSO regularization and removed multicollinearity####

##### Continent and Region are correlated, dropped Continent for now, because the lasso coef was closer to 0###

fit.lasso.lm3 = lm(Life.expectancy ~ Year + Status + Adult.Mortality + Alcohol + log.percentage.expenditure + Hepatitis.B + Measles + BMI + under.five.deaths + Polio + Total.expenditure + Diphtheria + log.HIV.AIDS + Population + thinness.5.9.years + Income.composition.of.resources + Schooling + Region + log.GDP, data = train)

#### Hypothesis testing ####
summary(fit.lasso.lm3)
```

    ## 
    ## Call:
    ## lm(formula = Life.expectancy ~ Year + Status + Adult.Mortality + 
    ##     Alcohol + log.percentage.expenditure + Hepatitis.B + Measles + 
    ##     BMI + under.five.deaths + Polio + Total.expenditure + Diphtheria + 
    ##     log.HIV.AIDS + Population + thinness.5.9.years + Income.composition.of.resources + 
    ##     Schooling + Region + log.GDP, data = train)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -22.869  -1.771  -0.131   1.744  13.539 
    ## 
    ## Coefficients:
    ##                                          Estimate        Std. Error t value
    ## (Intercept)                     -158.518141271197   32.256218233928   -4.91
    ## Year                               0.107192375808    0.016191817726    6.62
    ## StatusDeveloping                  -0.982396312017    0.316224032880   -3.11
    ## Adult.Mortality                   -0.011607620459    0.000733370080  -15.83
    ## Alcohol                           -0.085416957638    0.028166237860   -3.03
    ## log.percentage.expenditure         0.139464108677    0.031483008520    4.43
    ## Hepatitis.B                        0.001300631368    0.003500240494    0.37
    ## Measles                           -0.000018869502    0.000006882617   -2.74
    ## BMI                                0.004166567053    0.004973746437    0.84
    ## under.five.deaths                 -0.003230876609    0.000662746115   -4.87
    ## Polio                              0.020009346889    0.003947823309    5.07
    ## Total.expenditure                 -0.046788699417    0.032771545941   -1.43
    ## Diphtheria                         0.021798719709    0.004300703992    5.07
    ## log.HIV.AIDS                      -1.928057600167    0.100287433983  -19.23
    ## Population                         0.000000003842    0.000000000767    5.01
    ## thinness.5.9.years                -0.038372298937    0.028565663934   -1.34
    ## Income.composition.of.resources    4.462168222963    0.591340160357    7.55
    ## Schooling                          0.451031129200    0.042270421830   10.67
    ## RegionAustralia and New Zealand    4.059784446712    0.914320028060    4.44
    ## RegionCanada                       5.711923573954    1.023759209028    5.58
    ## RegionCaribbean                    3.312337176041    0.523162904267    6.33
    ## RegionCentral Africa              -1.776454253756    0.595331457536   -2.98
    ## RegionCentral Asia                -3.008543156088    0.585485933254   -5.14
    ## RegionCentral Europe               1.298366208665    0.524247266394    2.48
    ## RegionEastern Africa              -0.756149823605    0.623967226208   -1.21
    ## RegionEastern Europe              -0.093339039134    0.583120160348   -0.16
    ## RegionMashriq                      0.470067651943    0.554696021886    0.85
    ## RegionMeso-America                 3.615843206096    0.546870369209    6.61
    ## RegionNorth Africa                -0.227061402146    0.513523490805   -0.44
    ## RegionNW Pacific and East Asia     0.505237103571    0.632239949647    0.80
    ## RegionSouth America                1.745711725045    0.522250010747    3.34
    ## RegionSouth Asia                  -0.309558588540    0.560764248895   -0.55
    ## RegionSouth Pacific               -1.125515132316    0.546171760555   -2.06
    ## RegionSoutheast Asia               0.340871540898    0.488903198757    0.70
    ## RegionSouthern Africa             -3.892959693272    0.675133901725   -5.77
    ## RegionUS                           1.410211257693    1.138763643111    1.24
    ## RegionWestern Africa              -3.473772477607    0.563851688343   -6.16
    ## RegionWestern Europe               4.699230811539    0.557054689372    8.44
    ## RegionWestern Indian Ocean        -1.380873681729    0.622072961442   -2.22
    ## log.GDP                            0.120531331302    0.058932618337    2.05
    ##                                             Pr(>|t|)    
    ## (Intercept)                        0.000000949662338 ***
    ## Year                               0.000000000043938 ***
    ## StatusDeveloping                             0.00191 ** 
    ## Adult.Mortality                 < 0.0000000000000002 ***
    ## Alcohol                                      0.00245 ** 
    ## log.percentage.expenditure         0.000009845681775 ***
    ## Hepatitis.B                                  0.71024    
    ## Measles                                      0.00616 ** 
    ## BMI                                          0.40227    
    ## under.five.deaths                  0.000001157819982 ***
    ## Polio                              0.000000431026503 ***
    ## Total.expenditure                            0.15350    
    ## Diphtheria                         0.000000430602347 ***
    ## log.HIV.AIDS                    < 0.0000000000000002 ***
    ## Population                         0.000000592223096 ***
    ## thinness.5.9.years                           0.17930    
    ## Income.composition.of.resources    0.000000000000063 ***
    ## Schooling                       < 0.0000000000000002 ***
    ## RegionAustralia and New Zealand    0.000009385241543 ***
    ## RegionCanada                       0.000000026794682 ***
    ## RegionCaribbean                    0.000000000288111 ***
    ## RegionCentral Africa                         0.00287 ** 
    ## RegionCentral Asia                 0.000000298728457 ***
    ## RegionCentral Europe                         0.01333 *  
    ## RegionEastern Africa                         0.22569    
    ## RegionEastern Europe                         0.87284    
    ## RegionMashriq                                0.39684    
    ## RegionMeso-America                 0.000000000046420 ***
    ## RegionNorth Africa                           0.65841    
    ## RegionNW Pacific and East Asia               0.42430    
    ## RegionSouth America                          0.00084 ***
    ## RegionSouth Asia                             0.58098    
    ## RegionSouth Pacific                          0.03943 *  
    ## RegionSoutheast Asia                         0.48573    
    ## RegionSouthern Africa              0.000000009127720 ***
    ## RegionUS                                     0.21570    
    ## RegionWestern Africa               0.000000000843629 ***
    ## RegionWestern Europe            < 0.0000000000000002 ***
    ## RegionWestern Indian Ocean                   0.02652 *  
    ## log.GDP                                      0.04094 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.25 on 2449 degrees of freedom
    ## Multiple R-squared:  0.887,  Adjusted R-squared:  0.885 
    ## F-statistic:  493 on 39 and 2449 DF,  p-value: <0.0000000000000002

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
    ##   3.2282   0.8871   2.3780

``` r
postResample(pred = test_pred, obs = test$Life.expectancy)
```

    ##     RMSE Rsquared      MAE 
    ##   3.1113   0.8824   2.3810

``` r
sm = summary(fit.lasso.lm3)
mse = mean(sm$residuals^2)

### Checking Multiple Liner Regression model assumptions
confint(fit.lasso.lm3)
```

    ##                                             2.5 %           97.5 %
    ## (Intercept)                     -221.770428082547 -95.265854459848
    ## Year                               0.075441304079   0.138943447536
    ## StatusDeveloping                  -1.602490492907  -0.362302131127
    ## Adult.Mortality                   -0.013045710142  -0.010169530777
    ## Alcohol                           -0.140649066456  -0.030184848821
    ## log.percentage.expenditure         0.077728034405   0.201200182948
    ## Hepatitis.B                       -0.005563106161   0.008164368897
    ## Measles                           -0.000032365854  -0.000005373150
    ## BMI                               -0.005586617089   0.013919751196
    ## under.five.deaths                 -0.004530477418  -0.001931275799
    ## Polio                              0.012267929391   0.027750764387
    ## Total.expenditure                 -0.111051509399   0.017474110565
    ## Diphtheria                         0.013365326792   0.030232112626
    ## log.HIV.AIDS                      -2.124714551452  -1.731400648882
    ## Population                         0.000000002337   0.000000005346
    ## thinness.5.9.years                -0.094387655573   0.017643057698
    ## Income.composition.of.resources    3.302589714574   5.621746731352
    ## Schooling                          0.368141658840   0.533920599560
    ## RegionAustralia and New Zealand    2.266864017143   5.852704876281
    ## RegionCanada                       3.704400229290   7.719446918619
    ## RegionCaribbean                    2.286449707469   4.338224644614
    ## RegionCentral Africa              -2.943859428994  -0.609049078519
    ## RegionCentral Asia                -4.156641916632  -1.860444395545
    ## RegionCentral Europe               0.270352378478   2.326380038852
    ## RegionEastern Africa              -1.979707826182   0.467408178972
    ## RegionEastern Europe              -1.236798677221   1.050120598953
    ## RegionMashriq                     -0.617654151493   1.557789455379
    ## RegionMeso-America                 2.543466984216   4.688219427976
    ## RegionNorth Africa                -1.234046625559   0.779923821268
    ## RegionNW Pacific and East Asia    -0.734543156428   1.745017363571
    ## RegionSouth America                0.721614379615   2.769809070475
    ## RegionSouth Asia                  -1.409179779326   0.790062602245
    ## RegionSouth Pacific               -2.196521429345  -0.054508835288
    ## RegionSoutheast Asia              -0.617834936300   1.299578018096
    ## RegionSouthern Africa             -5.216852124750  -2.569067261795
    ## RegionUS                          -0.822828091248   3.643250606634
    ## RegionWestern Africa              -4.579447930677  -2.368097024537
    ## RegionWestern Europe               3.606883818899   5.791577804178
    ## RegionWestern Indian Ocean        -2.600717157779  -0.161030205678
    ## log.GDP                            0.004968407881   0.236094254723

``` r
hist(residuals, main = "Histogram of Residuals")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-5.png)<!-- -->

``` r
plot(residuals, main = "Residuals plot") 
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
#####################################################################################
#                                  Forward Selection                                #
#####################################################################################
library(leaps)
mlr.fwd=regsubsets(Life.expectancy~.,data=train,method="forward",nvmax=44)
```

    ## Warning in leaps.setup(x, y, wt = wt, nbest = nbest, nvmax = nvmax, force.in =
    ## force.in, : 3 linear dependencies found

    ## Reordering variables and trying again:

``` r
testASE<-c()
for (i in 1:44){
  predictions = predict.regsubsets(object=mlr.fwd,newdata=test,id=i) 
  testASE[i] = mean((test$Life.expectancy-predictions)^2)
}
par(mfrow=c(1,1))
plot(1:44,testASE,type="l",xlab="# of predictors",ylab="test vs train ASE")
index<-which(testASE==min(testASE))
points(index,testASE[index],col="red",pch=10)
rss<-summary(mlr.fwd)$rss
lines(1:45,rss/dim(train)[1],lty=3,col="blue")  
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-9.png)<!-- -->

``` r
mlr.fwd.final=regsubsets(Life.expectancy~.,data=LifeExp,method="forward",nvmax=29)
```

    ## Warning in leaps.setup(x, y, wt = wt, nbest = nbest, nvmax = nvmax, force.in =
    ## force.in, : 3 linear dependencies found

    ## Reordering variables and trying again:

``` r
coef(mlr.fwd.final,29)
```

    ##                     (Intercept)                            Year 
    ##               -127.908493368914                  0.091274334529 
    ##                StatusDeveloping                 Adult.Mortality 
    ##                 -0.930348336007                 -0.010529755464 
    ##                         Alcohol                         Measles 
    ##                 -0.052985867888                 -0.000019235711 
    ##               under.five.deaths                           Polio 
    ##                 -0.003597935171                  0.021059641698 
    ##                      Diphtheria                        HIV.AIDS 
    ##                  0.020544905673                 -0.134491381357 
    ##                      Population              thinness.5.9.years 
    ##                  0.000000004138                 -0.075021425467 
    ## Income.composition.of.resources                       Schooling 
    ##                  4.404625784218                  0.488791773413 
    ## RegionAustralia and New Zealand                    RegionCanada 
    ##                  4.389381627531                  2.417687476177 
    ##            RegionCentral Africa              RegionCentral Asia 
    ##                 -0.854655075325                 -3.352281595088 
    ##            RegionCentral Europe              RegionNorth Africa 
    ##                  2.133936855638                  1.091403645714 
    ##             RegionSouth America           RegionSouthern Africa 
    ##                 -1.662071000598                 -2.080372230434 
    ##                        RegionUS            RegionWestern Africa 
    ##                 -2.682942123018                 -2.530676410145 
    ##            RegionWestern Europe                    log.HIV.AIDS 
    ##                  5.401462746037                 -1.607077118451 
    ##                         log.GDP      log.percentage.expenditure 
    ##                  0.166899529709                  0.119892288665 
    ##                   ContinentAsia               ContinentAmericas 
    ##                  1.775117393340                  4.293057384888

``` r
summary(mlr.fwd.final)
```

    ## Subset selection object
    ## Call: regsubsets.formula(Life.expectancy ~ ., data = LifeExp, method = "forward", 
    ##     nvmax = 29)
    ## 48 Variables  (and intercept)
    ##                                 Forced in Forced out
    ## Year                                FALSE      FALSE
    ## StatusDeveloping                    FALSE      FALSE
    ## Adult.Mortality                     FALSE      FALSE
    ## infant.deaths                       FALSE      FALSE
    ## Alcohol                             FALSE      FALSE
    ## percentage.expenditure              FALSE      FALSE
    ## Hepatitis.B                         FALSE      FALSE
    ## Measles                             FALSE      FALSE
    ## BMI                                 FALSE      FALSE
    ## under.five.deaths                   FALSE      FALSE
    ## Polio                               FALSE      FALSE
    ## Total.expenditure                   FALSE      FALSE
    ## Diphtheria                          FALSE      FALSE
    ## HIV.AIDS                            FALSE      FALSE
    ## GDP                                 FALSE      FALSE
    ## Population                          FALSE      FALSE
    ## thinness..1.19.years                FALSE      FALSE
    ## thinness.5.9.years                  FALSE      FALSE
    ## Income.composition.of.resources     FALSE      FALSE
    ## Schooling                           FALSE      FALSE
    ## RegionAustralia and New Zealand     FALSE      FALSE
    ## RegionCanada                        FALSE      FALSE
    ## RegionCaribbean                     FALSE      FALSE
    ## RegionCentral Africa                FALSE      FALSE
    ## RegionCentral Asia                  FALSE      FALSE
    ## RegionCentral Europe                FALSE      FALSE
    ## RegionEastern Africa                FALSE      FALSE
    ## RegionEastern Europe                FALSE      FALSE
    ## RegionMashriq                       FALSE      FALSE
    ## RegionMeso-America                  FALSE      FALSE
    ## RegionNorth Africa                  FALSE      FALSE
    ## RegionNW Pacific and East Asia      FALSE      FALSE
    ## RegionSouth America                 FALSE      FALSE
    ## RegionSouth Asia                    FALSE      FALSE
    ## RegionSouth Pacific                 FALSE      FALSE
    ## RegionSoutheast Asia                FALSE      FALSE
    ## RegionSouthern Africa               FALSE      FALSE
    ## RegionUS                            FALSE      FALSE
    ## RegionWestern Africa                FALSE      FALSE
    ## RegionWestern Europe                FALSE      FALSE
    ## RegionWestern Indian Ocean          FALSE      FALSE
    ## log.HIV.AIDS                        FALSE      FALSE
    ## log.GDP                             FALSE      FALSE
    ## log.percentage.expenditure          FALSE      FALSE
    ## ContinentAsia                       FALSE      FALSE
    ## ContinentAmericas                   FALSE      FALSE
    ## ContinentEurope                     FALSE      FALSE
    ## ContinentOceania                    FALSE      FALSE
    ## 1 subsets of each size up to 30
    ## Selection Algorithm: forward
    ##           Year StatusDeveloping Adult.Mortality infant.deaths Alcohol
    ## 1  ( 1 )  " "  " "              " "             " "           " "    
    ## 2  ( 1 )  " "  " "              " "             " "           " "    
    ## 3  ( 1 )  " "  " "              "*"             " "           " "    
    ## 4  ( 1 )  " "  " "              "*"             " "           " "    
    ## 5  ( 1 )  " "  " "              "*"             " "           " "    
    ## 6  ( 1 )  " "  " "              "*"             " "           " "    
    ## 7  ( 1 )  " "  " "              "*"             " "           " "    
    ## 8  ( 1 )  " "  " "              "*"             " "           " "    
    ## 9  ( 1 )  " "  " "              "*"             " "           " "    
    ## 10  ( 1 ) " "  " "              "*"             " "           " "    
    ## 11  ( 1 ) " "  " "              "*"             " "           " "    
    ## 12  ( 1 ) " "  " "              "*"             " "           " "    
    ## 13  ( 1 ) " "  "*"              "*"             " "           " "    
    ## 14  ( 1 ) " "  "*"              "*"             " "           " "    
    ## 15  ( 1 ) " "  "*"              "*"             " "           " "    
    ## 16  ( 1 ) " "  "*"              "*"             " "           " "    
    ## 17  ( 1 ) "*"  "*"              "*"             " "           " "    
    ## 18  ( 1 ) "*"  "*"              "*"             " "           " "    
    ## 19  ( 1 ) "*"  "*"              "*"             " "           " "    
    ## 20  ( 1 ) "*"  "*"              "*"             " "           " "    
    ## 21  ( 1 ) "*"  "*"              "*"             " "           " "    
    ## 22  ( 1 ) "*"  "*"              "*"             " "           " "    
    ## 23  ( 1 ) "*"  "*"              "*"             " "           " "    
    ## 24  ( 1 ) "*"  "*"              "*"             " "           " "    
    ## 25  ( 1 ) "*"  "*"              "*"             " "           " "    
    ## 26  ( 1 ) "*"  "*"              "*"             " "           " "    
    ## 27  ( 1 ) "*"  "*"              "*"             " "           " "    
    ## 28  ( 1 ) "*"  "*"              "*"             " "           " "    
    ## 29  ( 1 ) "*"  "*"              "*"             " "           "*"    
    ## 30  ( 1 ) "*"  "*"              "*"             "*"           "*"    
    ##           percentage.expenditure Hepatitis.B Measles BMI under.five.deaths
    ## 1  ( 1 )  " "                    " "         " "     " " " "              
    ## 2  ( 1 )  " "                    " "         " "     " " " "              
    ## 3  ( 1 )  " "                    " "         " "     " " " "              
    ## 4  ( 1 )  " "                    " "         " "     " " " "              
    ## 5  ( 1 )  " "                    " "         " "     " " " "              
    ## 6  ( 1 )  " "                    " "         " "     " " " "              
    ## 7  ( 1 )  " "                    " "         " "     " " " "              
    ## 8  ( 1 )  " "                    " "         " "     " " " "              
    ## 9  ( 1 )  " "                    " "         " "     " " " "              
    ## 10  ( 1 ) " "                    " "         " "     " " " "              
    ## 11  ( 1 ) " "                    " "         " "     " " " "              
    ## 12  ( 1 ) " "                    " "         " "     " " " "              
    ## 13  ( 1 ) " "                    " "         " "     " " " "              
    ## 14  ( 1 ) " "                    " "         " "     " " " "              
    ## 15  ( 1 ) " "                    " "         " "     " " "*"              
    ## 16  ( 1 ) " "                    " "         " "     " " "*"              
    ## 17  ( 1 ) " "                    " "         " "     " " "*"              
    ## 18  ( 1 ) " "                    " "         " "     " " "*"              
    ## 19  ( 1 ) " "                    " "         " "     " " "*"              
    ## 20  ( 1 ) " "                    " "         " "     " " "*"              
    ## 21  ( 1 ) " "                    " "         " "     " " "*"              
    ## 22  ( 1 ) " "                    " "         " "     " " "*"              
    ## 23  ( 1 ) " "                    " "         " "     " " "*"              
    ## 24  ( 1 ) " "                    " "         " "     " " "*"              
    ## 25  ( 1 ) " "                    " "         "*"     " " "*"              
    ## 26  ( 1 ) " "                    " "         "*"     " " "*"              
    ## 27  ( 1 ) " "                    " "         "*"     " " "*"              
    ## 28  ( 1 ) " "                    " "         "*"     " " "*"              
    ## 29  ( 1 ) " "                    " "         "*"     " " "*"              
    ## 30  ( 1 ) " "                    " "         "*"     " " "*"              
    ##           Polio Total.expenditure Diphtheria HIV.AIDS GDP Population
    ## 1  ( 1 )  " "   " "               " "        " "      " " " "       
    ## 2  ( 1 )  " "   " "               " "        " "      " " " "       
    ## 3  ( 1 )  " "   " "               " "        " "      " " " "       
    ## 4  ( 1 )  " "   " "               " "        " "      " " " "       
    ## 5  ( 1 )  " "   " "               " "        " "      " " " "       
    ## 6  ( 1 )  " "   " "               " "        " "      " " " "       
    ## 7  ( 1 )  " "   " "               "*"        " "      " " " "       
    ## 8  ( 1 )  " "   " "               "*"        " "      " " " "       
    ## 9  ( 1 )  " "   " "               "*"        " "      " " " "       
    ## 10  ( 1 ) " "   " "               "*"        "*"      " " " "       
    ## 11  ( 1 ) " "   " "               "*"        "*"      " " " "       
    ## 12  ( 1 ) "*"   " "               "*"        "*"      " " " "       
    ## 13  ( 1 ) "*"   " "               "*"        "*"      " " " "       
    ## 14  ( 1 ) "*"   " "               "*"        "*"      " " " "       
    ## 15  ( 1 ) "*"   " "               "*"        "*"      " " " "       
    ## 16  ( 1 ) "*"   " "               "*"        "*"      " " " "       
    ## 17  ( 1 ) "*"   " "               "*"        "*"      " " " "       
    ## 18  ( 1 ) "*"   " "               "*"        "*"      " " " "       
    ## 19  ( 1 ) "*"   " "               "*"        "*"      " " " "       
    ## 20  ( 1 ) "*"   " "               "*"        "*"      " " " "       
    ## 21  ( 1 ) "*"   " "               "*"        "*"      " " "*"       
    ## 22  ( 1 ) "*"   " "               "*"        "*"      " " "*"       
    ## 23  ( 1 ) "*"   " "               "*"        "*"      " " "*"       
    ## 24  ( 1 ) "*"   " "               "*"        "*"      " " "*"       
    ## 25  ( 1 ) "*"   " "               "*"        "*"      " " "*"       
    ## 26  ( 1 ) "*"   " "               "*"        "*"      " " "*"       
    ## 27  ( 1 ) "*"   " "               "*"        "*"      " " "*"       
    ## 28  ( 1 ) "*"   " "               "*"        "*"      " " "*"       
    ## 29  ( 1 ) "*"   " "               "*"        "*"      " " "*"       
    ## 30  ( 1 ) "*"   " "               "*"        "*"      " " "*"       
    ##           thinness..1.19.years thinness.5.9.years
    ## 1  ( 1 )  " "                  " "               
    ## 2  ( 1 )  " "                  " "               
    ## 3  ( 1 )  " "                  " "               
    ## 4  ( 1 )  " "                  " "               
    ## 5  ( 1 )  " "                  " "               
    ## 6  ( 1 )  " "                  " "               
    ## 7  ( 1 )  " "                  " "               
    ## 8  ( 1 )  " "                  " "               
    ## 9  ( 1 )  " "                  " "               
    ## 10  ( 1 ) " "                  " "               
    ## 11  ( 1 ) " "                  " "               
    ## 12  ( 1 ) " "                  " "               
    ## 13  ( 1 ) " "                  " "               
    ## 14  ( 1 ) " "                  " "               
    ## 15  ( 1 ) " "                  " "               
    ## 16  ( 1 ) " "                  " "               
    ## 17  ( 1 ) " "                  " "               
    ## 18  ( 1 ) " "                  " "               
    ## 19  ( 1 ) " "                  " "               
    ## 20  ( 1 ) " "                  " "               
    ## 21  ( 1 ) " "                  " "               
    ## 22  ( 1 ) " "                  " "               
    ## 23  ( 1 ) " "                  " "               
    ## 24  ( 1 ) " "                  "*"               
    ## 25  ( 1 ) " "                  "*"               
    ## 26  ( 1 ) " "                  "*"               
    ## 27  ( 1 ) " "                  "*"               
    ## 28  ( 1 ) " "                  "*"               
    ## 29  ( 1 ) " "                  "*"               
    ## 30  ( 1 ) " "                  "*"               
    ##           Income.composition.of.resources Schooling
    ## 1  ( 1 )  " "                             " "      
    ## 2  ( 1 )  " "                             "*"      
    ## 3  ( 1 )  " "                             "*"      
    ## 4  ( 1 )  " "                             "*"      
    ## 5  ( 1 )  " "                             "*"      
    ## 6  ( 1 )  "*"                             "*"      
    ## 7  ( 1 )  "*"                             "*"      
    ## 8  ( 1 )  "*"                             "*"      
    ## 9  ( 1 )  "*"                             "*"      
    ## 10  ( 1 ) "*"                             "*"      
    ## 11  ( 1 ) "*"                             "*"      
    ## 12  ( 1 ) "*"                             "*"      
    ## 13  ( 1 ) "*"                             "*"      
    ## 14  ( 1 ) "*"                             "*"      
    ## 15  ( 1 ) "*"                             "*"      
    ## 16  ( 1 ) "*"                             "*"      
    ## 17  ( 1 ) "*"                             "*"      
    ## 18  ( 1 ) "*"                             "*"      
    ## 19  ( 1 ) "*"                             "*"      
    ## 20  ( 1 ) "*"                             "*"      
    ## 21  ( 1 ) "*"                             "*"      
    ## 22  ( 1 ) "*"                             "*"      
    ## 23  ( 1 ) "*"                             "*"      
    ## 24  ( 1 ) "*"                             "*"      
    ## 25  ( 1 ) "*"                             "*"      
    ## 26  ( 1 ) "*"                             "*"      
    ## 27  ( 1 ) "*"                             "*"      
    ## 28  ( 1 ) "*"                             "*"      
    ## 29  ( 1 ) "*"                             "*"      
    ## 30  ( 1 ) "*"                             "*"      
    ##           RegionAustralia and New Zealand RegionCanada RegionCaribbean
    ## 1  ( 1 )  " "                             " "          " "            
    ## 2  ( 1 )  " "                             " "          " "            
    ## 3  ( 1 )  " "                             " "          " "            
    ## 4  ( 1 )  " "                             " "          " "            
    ## 5  ( 1 )  " "                             " "          " "            
    ## 6  ( 1 )  " "                             " "          " "            
    ## 7  ( 1 )  " "                             " "          " "            
    ## 8  ( 1 )  " "                             " "          " "            
    ## 9  ( 1 )  " "                             " "          " "            
    ## 10  ( 1 ) " "                             " "          " "            
    ## 11  ( 1 ) " "                             " "          " "            
    ## 12  ( 1 ) " "                             " "          " "            
    ## 13  ( 1 ) " "                             " "          " "            
    ## 14  ( 1 ) " "                             " "          " "            
    ## 15  ( 1 ) " "                             " "          " "            
    ## 16  ( 1 ) " "                             " "          " "            
    ## 17  ( 1 ) " "                             " "          " "            
    ## 18  ( 1 ) " "                             " "          " "            
    ## 19  ( 1 ) " "                             " "          " "            
    ## 20  ( 1 ) "*"                             " "          " "            
    ## 21  ( 1 ) "*"                             " "          " "            
    ## 22  ( 1 ) "*"                             " "          " "            
    ## 23  ( 1 ) "*"                             " "          " "            
    ## 24  ( 1 ) "*"                             " "          " "            
    ## 25  ( 1 ) "*"                             " "          " "            
    ## 26  ( 1 ) "*"                             "*"          " "            
    ## 27  ( 1 ) "*"                             "*"          " "            
    ## 28  ( 1 ) "*"                             "*"          " "            
    ## 29  ( 1 ) "*"                             "*"          " "            
    ## 30  ( 1 ) "*"                             "*"          " "            
    ##           RegionCentral Africa RegionCentral Asia RegionCentral Europe
    ## 1  ( 1 )  " "                  " "                " "                 
    ## 2  ( 1 )  " "                  " "                " "                 
    ## 3  ( 1 )  " "                  " "                " "                 
    ## 4  ( 1 )  " "                  " "                " "                 
    ## 5  ( 1 )  " "                  " "                " "                 
    ## 6  ( 1 )  " "                  " "                " "                 
    ## 7  ( 1 )  " "                  " "                " "                 
    ## 8  ( 1 )  " "                  " "                " "                 
    ## 9  ( 1 )  " "                  " "                " "                 
    ## 10  ( 1 ) " "                  " "                " "                 
    ## 11  ( 1 ) " "                  "*"                " "                 
    ## 12  ( 1 ) " "                  "*"                " "                 
    ## 13  ( 1 ) " "                  "*"                " "                 
    ## 14  ( 1 ) " "                  "*"                " "                 
    ## 15  ( 1 ) " "                  "*"                " "                 
    ## 16  ( 1 ) " "                  "*"                " "                 
    ## 17  ( 1 ) " "                  "*"                " "                 
    ## 18  ( 1 ) " "                  "*"                " "                 
    ## 19  ( 1 ) " "                  "*"                "*"                 
    ## 20  ( 1 ) " "                  "*"                "*"                 
    ## 21  ( 1 ) " "                  "*"                "*"                 
    ## 22  ( 1 ) " "                  "*"                "*"                 
    ## 23  ( 1 ) " "                  "*"                "*"                 
    ## 24  ( 1 ) " "                  "*"                "*"                 
    ## 25  ( 1 ) " "                  "*"                "*"                 
    ## 26  ( 1 ) " "                  "*"                "*"                 
    ## 27  ( 1 ) " "                  "*"                "*"                 
    ## 28  ( 1 ) "*"                  "*"                "*"                 
    ## 29  ( 1 ) "*"                  "*"                "*"                 
    ## 30  ( 1 ) "*"                  "*"                "*"                 
    ##           RegionEastern Africa RegionEastern Europe RegionMashriq
    ## 1  ( 1 )  " "                  " "                  " "          
    ## 2  ( 1 )  " "                  " "                  " "          
    ## 3  ( 1 )  " "                  " "                  " "          
    ## 4  ( 1 )  " "                  " "                  " "          
    ## 5  ( 1 )  " "                  " "                  " "          
    ## 6  ( 1 )  " "                  " "                  " "          
    ## 7  ( 1 )  " "                  " "                  " "          
    ## 8  ( 1 )  " "                  " "                  " "          
    ## 9  ( 1 )  " "                  " "                  " "          
    ## 10  ( 1 ) " "                  " "                  " "          
    ## 11  ( 1 ) " "                  " "                  " "          
    ## 12  ( 1 ) " "                  " "                  " "          
    ## 13  ( 1 ) " "                  " "                  " "          
    ## 14  ( 1 ) " "                  " "                  " "          
    ## 15  ( 1 ) " "                  " "                  " "          
    ## 16  ( 1 ) " "                  " "                  " "          
    ## 17  ( 1 ) " "                  " "                  " "          
    ## 18  ( 1 ) " "                  " "                  " "          
    ## 19  ( 1 ) " "                  " "                  " "          
    ## 20  ( 1 ) " "                  " "                  " "          
    ## 21  ( 1 ) " "                  " "                  " "          
    ## 22  ( 1 ) " "                  " "                  " "          
    ## 23  ( 1 ) " "                  " "                  " "          
    ## 24  ( 1 ) " "                  " "                  " "          
    ## 25  ( 1 ) " "                  " "                  " "          
    ## 26  ( 1 ) " "                  " "                  " "          
    ## 27  ( 1 ) " "                  " "                  " "          
    ## 28  ( 1 ) " "                  " "                  " "          
    ## 29  ( 1 ) " "                  " "                  " "          
    ## 30  ( 1 ) " "                  " "                  " "          
    ##           RegionMeso-America RegionNorth Africa RegionNW Pacific and East Asia
    ## 1  ( 1 )  " "                " "                " "                           
    ## 2  ( 1 )  " "                " "                " "                           
    ## 3  ( 1 )  " "                " "                " "                           
    ## 4  ( 1 )  " "                " "                " "                           
    ## 5  ( 1 )  " "                " "                " "                           
    ## 6  ( 1 )  " "                " "                " "                           
    ## 7  ( 1 )  " "                " "                " "                           
    ## 8  ( 1 )  " "                " "                " "                           
    ## 9  ( 1 )  " "                " "                " "                           
    ## 10  ( 1 ) " "                " "                " "                           
    ## 11  ( 1 ) " "                " "                " "                           
    ## 12  ( 1 ) " "                " "                " "                           
    ## 13  ( 1 ) " "                " "                " "                           
    ## 14  ( 1 ) " "                " "                " "                           
    ## 15  ( 1 ) " "                " "                " "                           
    ## 16  ( 1 ) " "                " "                " "                           
    ## 17  ( 1 ) " "                " "                " "                           
    ## 18  ( 1 ) " "                " "                " "                           
    ## 19  ( 1 ) " "                " "                " "                           
    ## 20  ( 1 ) " "                " "                " "                           
    ## 21  ( 1 ) " "                " "                " "                           
    ## 22  ( 1 ) " "                " "                " "                           
    ## 23  ( 1 ) " "                "*"                " "                           
    ## 24  ( 1 ) " "                "*"                " "                           
    ## 25  ( 1 ) " "                "*"                " "                           
    ## 26  ( 1 ) " "                "*"                " "                           
    ## 27  ( 1 ) " "                "*"                " "                           
    ## 28  ( 1 ) " "                "*"                " "                           
    ## 29  ( 1 ) " "                "*"                " "                           
    ## 30  ( 1 ) " "                "*"                " "                           
    ##           RegionSouth America RegionSouth Asia RegionSouth Pacific
    ## 1  ( 1 )  " "                 " "              " "                
    ## 2  ( 1 )  " "                 " "              " "                
    ## 3  ( 1 )  " "                 " "              " "                
    ## 4  ( 1 )  " "                 " "              " "                
    ## 5  ( 1 )  " "                 " "              " "                
    ## 6  ( 1 )  " "                 " "              " "                
    ## 7  ( 1 )  " "                 " "              " "                
    ## 8  ( 1 )  " "                 " "              " "                
    ## 9  ( 1 )  " "                 " "              " "                
    ## 10  ( 1 ) " "                 " "              " "                
    ## 11  ( 1 ) " "                 " "              " "                
    ## 12  ( 1 ) " "                 " "              " "                
    ## 13  ( 1 ) " "                 " "              " "                
    ## 14  ( 1 ) "*"                 " "              " "                
    ## 15  ( 1 ) "*"                 " "              " "                
    ## 16  ( 1 ) "*"                 " "              " "                
    ## 17  ( 1 ) "*"                 " "              " "                
    ## 18  ( 1 ) "*"                 " "              " "                
    ## 19  ( 1 ) "*"                 " "              " "                
    ## 20  ( 1 ) "*"                 " "              " "                
    ## 21  ( 1 ) "*"                 " "              " "                
    ## 22  ( 1 ) "*"                 " "              " "                
    ## 23  ( 1 ) "*"                 " "              " "                
    ## 24  ( 1 ) "*"                 " "              " "                
    ## 25  ( 1 ) "*"                 " "              " "                
    ## 26  ( 1 ) "*"                 " "              " "                
    ## 27  ( 1 ) "*"                 " "              " "                
    ## 28  ( 1 ) "*"                 " "              " "                
    ## 29  ( 1 ) "*"                 " "              " "                
    ## 30  ( 1 ) "*"                 " "              " "                
    ##           RegionSoutheast Asia RegionSouthern Africa RegionUS
    ## 1  ( 1 )  " "                  " "                   " "     
    ## 2  ( 1 )  " "                  " "                   " "     
    ## 3  ( 1 )  " "                  " "                   " "     
    ## 4  ( 1 )  " "                  " "                   " "     
    ## 5  ( 1 )  " "                  " "                   " "     
    ## 6  ( 1 )  " "                  " "                   " "     
    ## 7  ( 1 )  " "                  " "                   " "     
    ## 8  ( 1 )  " "                  " "                   " "     
    ## 9  ( 1 )  " "                  " "                   " "     
    ## 10  ( 1 ) " "                  " "                   " "     
    ## 11  ( 1 ) " "                  " "                   " "     
    ## 12  ( 1 ) " "                  " "                   " "     
    ## 13  ( 1 ) " "                  " "                   " "     
    ## 14  ( 1 ) " "                  " "                   " "     
    ## 15  ( 1 ) " "                  " "                   " "     
    ## 16  ( 1 ) " "                  " "                   " "     
    ## 17  ( 1 ) " "                  " "                   " "     
    ## 18  ( 1 ) " "                  " "                   " "     
    ## 19  ( 1 ) " "                  " "                   " "     
    ## 20  ( 1 ) " "                  " "                   " "     
    ## 21  ( 1 ) " "                  " "                   " "     
    ## 22  ( 1 ) " "                  "*"                   " "     
    ## 23  ( 1 ) " "                  "*"                   " "     
    ## 24  ( 1 ) " "                  "*"                   " "     
    ## 25  ( 1 ) " "                  "*"                   " "     
    ## 26  ( 1 ) " "                  "*"                   " "     
    ## 27  ( 1 ) " "                  "*"                   "*"     
    ## 28  ( 1 ) " "                  "*"                   "*"     
    ## 29  ( 1 ) " "                  "*"                   "*"     
    ## 30  ( 1 ) " "                  "*"                   "*"     
    ##           RegionWestern Africa RegionWestern Europe RegionWestern Indian Ocean
    ## 1  ( 1 )  " "                  " "                  " "                       
    ## 2  ( 1 )  " "                  " "                  " "                       
    ## 3  ( 1 )  " "                  " "                  " "                       
    ## 4  ( 1 )  " "                  "*"                  " "                       
    ## 5  ( 1 )  " "                  "*"                  " "                       
    ## 6  ( 1 )  " "                  "*"                  " "                       
    ## 7  ( 1 )  " "                  "*"                  " "                       
    ## 8  ( 1 )  " "                  "*"                  " "                       
    ## 9  ( 1 )  "*"                  "*"                  " "                       
    ## 10  ( 1 ) "*"                  "*"                  " "                       
    ## 11  ( 1 ) "*"                  "*"                  " "                       
    ## 12  ( 1 ) "*"                  "*"                  " "                       
    ## 13  ( 1 ) "*"                  "*"                  " "                       
    ## 14  ( 1 ) "*"                  "*"                  " "                       
    ## 15  ( 1 ) "*"                  "*"                  " "                       
    ## 16  ( 1 ) "*"                  "*"                  " "                       
    ## 17  ( 1 ) "*"                  "*"                  " "                       
    ## 18  ( 1 ) "*"                  "*"                  " "                       
    ## 19  ( 1 ) "*"                  "*"                  " "                       
    ## 20  ( 1 ) "*"                  "*"                  " "                       
    ## 21  ( 1 ) "*"                  "*"                  " "                       
    ## 22  ( 1 ) "*"                  "*"                  " "                       
    ## 23  ( 1 ) "*"                  "*"                  " "                       
    ## 24  ( 1 ) "*"                  "*"                  " "                       
    ## 25  ( 1 ) "*"                  "*"                  " "                       
    ## 26  ( 1 ) "*"                  "*"                  " "                       
    ## 27  ( 1 ) "*"                  "*"                  " "                       
    ## 28  ( 1 ) "*"                  "*"                  " "                       
    ## 29  ( 1 ) "*"                  "*"                  " "                       
    ## 30  ( 1 ) "*"                  "*"                  " "                       
    ##           log.HIV.AIDS log.GDP log.percentage.expenditure ContinentAmericas
    ## 1  ( 1 )  "*"          " "     " "                        " "              
    ## 2  ( 1 )  "*"          " "     " "                        " "              
    ## 3  ( 1 )  "*"          " "     " "                        " "              
    ## 4  ( 1 )  "*"          " "     " "                        " "              
    ## 5  ( 1 )  "*"          " "     " "                        "*"              
    ## 6  ( 1 )  "*"          " "     " "                        "*"              
    ## 7  ( 1 )  "*"          " "     " "                        "*"              
    ## 8  ( 1 )  "*"          "*"     " "                        "*"              
    ## 9  ( 1 )  "*"          "*"     " "                        "*"              
    ## 10  ( 1 ) "*"          "*"     " "                        "*"              
    ## 11  ( 1 ) "*"          "*"     " "                        "*"              
    ## 12  ( 1 ) "*"          "*"     " "                        "*"              
    ## 13  ( 1 ) "*"          "*"     " "                        "*"              
    ## 14  ( 1 ) "*"          "*"     " "                        "*"              
    ## 15  ( 1 ) "*"          "*"     " "                        "*"              
    ## 16  ( 1 ) "*"          "*"     " "                        "*"              
    ## 17  ( 1 ) "*"          "*"     " "                        "*"              
    ## 18  ( 1 ) "*"          "*"     "*"                        "*"              
    ## 19  ( 1 ) "*"          "*"     "*"                        "*"              
    ## 20  ( 1 ) "*"          "*"     "*"                        "*"              
    ## 21  ( 1 ) "*"          "*"     "*"                        "*"              
    ## 22  ( 1 ) "*"          "*"     "*"                        "*"              
    ## 23  ( 1 ) "*"          "*"     "*"                        "*"              
    ## 24  ( 1 ) "*"          "*"     "*"                        "*"              
    ## 25  ( 1 ) "*"          "*"     "*"                        "*"              
    ## 26  ( 1 ) "*"          "*"     "*"                        "*"              
    ## 27  ( 1 ) "*"          "*"     "*"                        "*"              
    ## 28  ( 1 ) "*"          "*"     "*"                        "*"              
    ## 29  ( 1 ) "*"          "*"     "*"                        "*"              
    ## 30  ( 1 ) "*"          "*"     "*"                        "*"              
    ##           ContinentAsia ContinentEurope ContinentOceania
    ## 1  ( 1 )  " "           " "             " "             
    ## 2  ( 1 )  " "           " "             " "             
    ## 3  ( 1 )  " "           " "             " "             
    ## 4  ( 1 )  " "           " "             " "             
    ## 5  ( 1 )  " "           " "             " "             
    ## 6  ( 1 )  " "           " "             " "             
    ## 7  ( 1 )  " "           " "             " "             
    ## 8  ( 1 )  " "           " "             " "             
    ## 9  ( 1 )  " "           " "             " "             
    ## 10  ( 1 ) " "           " "             " "             
    ## 11  ( 1 ) " "           " "             " "             
    ## 12  ( 1 ) " "           " "             " "             
    ## 13  ( 1 ) " "           " "             " "             
    ## 14  ( 1 ) " "           " "             " "             
    ## 15  ( 1 ) " "           " "             " "             
    ## 16  ( 1 ) "*"           " "             " "             
    ## 17  ( 1 ) "*"           " "             " "             
    ## 18  ( 1 ) "*"           " "             " "             
    ## 19  ( 1 ) "*"           " "             " "             
    ## 20  ( 1 ) "*"           " "             " "             
    ## 21  ( 1 ) "*"           " "             " "             
    ## 22  ( 1 ) "*"           " "             " "             
    ## 23  ( 1 ) "*"           " "             " "             
    ## 24  ( 1 ) "*"           " "             " "             
    ## 25  ( 1 ) "*"           " "             " "             
    ## 26  ( 1 ) "*"           " "             " "             
    ## 27  ( 1 ) "*"           " "             " "             
    ## 28  ( 1 ) "*"           " "             " "             
    ## 29  ( 1 ) "*"           " "             " "             
    ## 30  ( 1 ) "*"           " "             " "

``` r
# Metrics RMSE; R-squared; MAE
postResample(pred = predictions, obs = ytest)
```

    ##     RMSE Rsquared      MAE 
    ##    3.096    0.883    2.341

``` r
##### Fit Linear Model based on FOrward Selection without factors to measure VIF####
fit.fwd.lm = lm(Life.expectancy ~ Year + Adult.Mortality + Alcohol + Measles + under.five.deaths + Polio + Diphtheria + Population + thinness.5.9.years + Income.composition.of.resources + Schooling + log.HIV.AIDS + log.GDP + log.percentage.expenditure, data = train)

### Visualize VIF
fit.fwd.lm_VIF = vif(fit.fwd.lm)
barplot(fit.fwd.lm_VIF, main = 'VIF Values', horiz = TRUE, col="blue", xlim = c(0,12))
abline(v=10, col="red")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-10.png)<!-- -->

``` r
############################################################################
# same as before, no longer have perfect correlation for num vars          #
# will comment out second model for now but leave in just in case          #
############################################################################


# We can see that Forward Selection did not remove infant.deaths or under.five.deaths as those are perfectly correlated. Let's remove the one with the smallest coefficient (under.five.deaths).

##### Fit Linear Model based on LASSO regularization without factors to measure VIF####
#fit.fwd.lm2 = lm(Life.expectancy ~ Year + Adult.Mortality + Alcohol + Hepatitis.B + Measles + BMI + Polio + Total.expenditure + Diphtheria + thinness.5.9.years + Income.composition.of.resources + Schooling + log.HIV.AIDS + log.GDP + log.percentage.expenditure, data = train)
#summary(fit.fwd.lm2)

### re-run Visualize VIF
#fit.fwd.lm2_VIF = vif(fit.fwd.lm2)
#barplot(fit.fwd.lm2_VIF, main = 'Re-test of VIF Values', horiz = TRUE, col="blue", xlim = c(0,12))
#abline(v=10, col="red")

##### Fit Linear Model based on Forward Selection regularization and removed multicollinearity####
fit.fwd.lm3 = lm(Life.expectancy ~ Year + Status + Adult.Mortality + Alcohol + Measles + under.five.deaths + Polio + Diphtheria + Population + thinness.5.9.years + Income.composition.of.resources + Schooling + Region + log.HIV.AIDS + log.GDP + log.percentage.expenditure, data = train)

#### Hypothesis testing ####
summary(fit.fwd.lm3)
```

    ## 
    ## Call:
    ## lm(formula = Life.expectancy ~ Year + Status + Adult.Mortality + 
    ##     Alcohol + Measles + under.five.deaths + Polio + Diphtheria + 
    ##     Population + thinness.5.9.years + Income.composition.of.resources + 
    ##     Schooling + Region + log.HIV.AIDS + log.GDP + log.percentage.expenditure, 
    ##     data = train)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -23.038  -1.773  -0.125   1.764  13.488 
    ## 
    ## Coefficients:
    ##                                          Estimate        Std. Error t value
    ## (Intercept)                     -157.018608364632   32.116140118509   -4.89
    ## Year                               0.106462509869    0.016113870124    6.61
    ## StatusDeveloping                  -0.971546560452    0.315114615674   -3.08
    ## Adult.Mortality                   -0.011604113567    0.000733140271  -15.83
    ## Alcohol                           -0.086214139210    0.028161491372   -3.06
    ## Measles                           -0.000018704873    0.000006877154   -2.72
    ## under.five.deaths                 -0.003204332038    0.000658506642   -4.87
    ## Polio                              0.020171299878    0.003914086249    5.15
    ## Diphtheria                         0.022139392280    0.003997652494    5.54
    ## Population                         0.000000003833    0.000000000767    5.00
    ## thinness.5.9.years                -0.041226124408    0.028406692657   -1.45
    ## Income.composition.of.resources    4.546752400730    0.586948732060    7.75
    ## Schooling                          0.451132646775    0.041962475889   10.75
    ## RegionAustralia and New Zealand    3.817335927764    0.899364756081    4.24
    ## RegionCanada                       5.444339457973    0.991387448194    5.49
    ## RegionCaribbean                    3.182629937136    0.517501837771    6.15
    ## RegionCentral Africa              -1.936386735055    0.580940878771   -3.33
    ## RegionCentral Asia                -3.106743457254    0.581544007044   -5.34
    ## RegionCentral Europe               1.153165178120    0.514466943984    2.24
    ## RegionEastern Africa              -0.979919729088    0.602738994239   -1.63
    ## RegionEastern Europe              -0.243487392141    0.575466431282   -0.42
    ## RegionMashriq                      0.396540308341    0.551780112739    0.72
    ## RegionMeso-America                 3.425899968914    0.533547148381    6.42
    ## RegionNorth Africa                -0.277748790000    0.511208320649   -0.54
    ## RegionNW Pacific and East Asia     0.316258922436    0.618084349620    0.51
    ## RegionSouth America                1.588714061541    0.513147960860    3.10
    ## RegionSouth Asia                  -0.481007842515    0.548421694445   -0.88
    ## RegionSouth Pacific               -1.230375494596    0.539483480004   -2.28
    ## RegionSoutheast Asia               0.226917426000    0.470339729323    0.48
    ## RegionSouthern Africa             -4.129662026770    0.656758646668   -6.29
    ## RegionUS                           0.826329920118    1.064585265715    0.78
    ## RegionWestern Africa              -3.673911814292    0.545466932507   -6.74
    ## RegionWestern Europe               4.502873401318    0.541337809675    8.32
    ## RegionWestern Indian Ocean        -1.519471204773    0.608936964882   -2.50
    ## log.HIV.AIDS                      -1.924819420445    0.100082372384  -19.23
    ## log.GDP                            0.124637228587    0.058808797644    2.12
    ## log.percentage.expenditure         0.138154975567    0.031470920602    4.39
    ##                                             Pr(>|t|)    
    ## (Intercept)                        0.000001078547642 ***
    ## Year                               0.000000000047974 ***
    ## StatusDeveloping                             0.00207 ** 
    ## Adult.Mortality                 < 0.0000000000000002 ***
    ## Alcohol                                      0.00223 ** 
    ## Measles                                      0.00658 ** 
    ## under.five.deaths                  0.000001210704402 ***
    ## Polio                              0.000000276032266 ***
    ## Diphtheria                         0.000000033835557 ***
    ## Population                         0.000000621379600 ***
    ## thinness.5.9.years                           0.14683    
    ## Income.composition.of.resources    0.000000000000014 ***
    ## Schooling                       < 0.0000000000000002 ***
    ## RegionAustralia and New Zealand    0.000022725028453 ***
    ## RegionCanada                       0.000000043923323 ***
    ## RegionCaribbean                    0.000000000902042 ***
    ## RegionCentral Africa                         0.00087 ***
    ## RegionCentral Asia                 0.000000100265214 ***
    ## RegionCentral Europe                         0.02508 *  
    ## RegionEastern Africa                         0.10413    
    ## RegionEastern Europe                         0.67225    
    ## RegionMashriq                                0.47242    
    ## RegionMeso-America                 0.000000000162037 ***
    ## RegionNorth Africa                           0.58696    
    ## RegionNW Pacific and East Asia               0.60892    
    ## RegionSouth America                          0.00198 ** 
    ## RegionSouth Asia                             0.38053    
    ## RegionSouth Pacific                          0.02265 *  
    ## RegionSoutheast Asia                         0.62953    
    ## RegionSouthern Africa              0.000000000379637 ***
    ## RegionUS                                     0.43771    
    ## RegionWestern Africa               0.000000000020303 ***
    ## RegionWestern Europe            < 0.0000000000000002 ***
    ## RegionWestern Indian Ocean                   0.01265 *  
    ## log.HIV.AIDS                    < 0.0000000000000002 ***
    ## log.GDP                                      0.03416 *  
    ## log.percentage.expenditure         0.000011819589863 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.25 on 2452 degrees of freedom
    ## Multiple R-squared:  0.887,  Adjusted R-squared:  0.885 
    ## F-statistic:  534 on 36 and 2452 DF,  p-value: <0.0000000000000002

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
    ##   3.2301   0.8869   2.3796

``` r
postResample(pred = test_pred, obs = test$Life.expectancy)
```

    ##     RMSE Rsquared      MAE 
    ##   3.1119   0.8824   2.3782

``` r
sm = summary(fit.fwd.lm3)
mse = mean(sm$residuals^2)

### Checking Multiple Liner Regression model assumptions
confint(fit.fwd.lm3)
```

    ##                                             2.5 %           97.5 %
    ## (Intercept)                     -219.996173259724 -94.041043469540
    ## Year                               0.074864307293   0.138060712445
    ## StatusDeveloping                  -1.589464874570  -0.353628246334
    ## Adult.Mortality                   -0.013041751739  -0.010166475394
    ## Alcohol                           -0.141436907073  -0.030991371347
    ## Measles                           -0.000032190504  -0.000005219242
    ## under.five.deaths                 -0.004495618745  -0.001913045332
    ## Polio                              0.012496043143   0.027846556612
    ## Diphtheria                         0.014300267826   0.029978516734
    ## Population                         0.000000002329   0.000000005337
    ## thinness.5.9.years                -0.096929715302   0.014477466485
    ## Income.composition.of.resources    3.395785885857   5.697718915604
    ## Schooling                          0.368847087592   0.533418205958
    ## RegionAustralia and New Zealand    2.053742853334   5.580929002195
    ## RegionCanada                       3.500296147525   7.388382768420
    ## RegionCaribbean                    2.167844055293   4.197415818978
    ## RegionCentral Africa              -3.075572258535  -0.797201211576
    ## RegionCentral Asia                -4.247111674226  -1.966375240281
    ## RegionCentral Europe               0.144330516434   2.161999839806
    ## RegionEastern Africa              -2.161849873383   0.202010415208
    ## RegionEastern Europe              -1.371937896692   0.884963112411
    ## RegionMashriq                     -0.685462937680   1.478543554361
    ## RegionMeso-America                 2.379650325036   4.472149612792
    ## RegionNorth Africa                -1.280193513113   0.724695933114
    ## RegionNW Pacific and East Asia    -0.895762419332   1.528280264204
    ## RegionSouth America                0.582465835989   2.594962287093
    ## RegionSouth Asia                  -1.556425458839   0.594409773810
    ## RegionSouth Pacific               -2.288265880751  -0.172485108442
    ## RegionSoutheast Asia              -0.695386771107   1.149221623107
    ## RegionSouthern Africa             -5.417521032812  -2.841803020727
    ## RegionUS                          -1.261249328526   2.913909168762
    ## RegionWestern Africa              -4.743535343562  -2.604288285021
    ## RegionWestern Europe               3.441346800887   5.564400001748
    ## RegionWestern Indian Ocean        -2.713555147656  -0.325387261891
    ## log.HIV.AIDS                      -2.121074140919  -1.728564699971
    ## log.GDP                            0.009317179030   0.239957278144
    ## log.percentage.expenditure         0.076442642229   0.199867308905

``` r
hist(residuals, main = "Histogram of Residuals")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-11.png)<!-- -->

``` r
plot(residuals, main = "Residuals plot") 
abline(h=0, col="blue")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-12.png)<!-- -->

``` r
plot(fit.fwd.lm3, which = 2)
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-13.png)<!-- -->

``` r
plot(fit.fwd.lm3, which = 4)
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-14.png)<!-- -->

``` r
#####################################################################################
#                               Backward Elimination                                #
#####################################################################################

mlr.bck=regsubsets(Life.expectancy~.,data=train,method="backward",nvmax=44)
```

    ## Warning in leaps.setup(x, y, wt = wt, nbest = nbest, nvmax = nvmax, force.in =
    ## force.in, : 3 linear dependencies found

    ## Reordering variables and trying again:

``` r
testASE<-c()
for (i in 1:44){

  predictions = predict.regsubsets(object=mlr.bck,newdata=test,id=i) 
  testASE[i] = mean((test$Life.expectancy-predictions)^2)
}
par(mfrow=c(1,1))

plot(1:44,testASE,type="l",xlab="# of predictors",ylab="test vs train ASE")
index<-which(testASE==min(testASE))
points(index,testASE[index],col="red",pch=10)
rss<-summary(mlr.bck)$rss
lines(1:45,rss/dim(train)[1],lty=3,col="blue")  
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-15.png)<!-- -->

``` r
mlr.bck.final=regsubsets(Life.expectancy~.,data=LifeExp,method="backward",nvmax=44)
```

    ## Warning in leaps.setup(x, y, wt = wt, nbest = nbest, nvmax = nvmax, force.in =
    ## force.in, : 3 linear dependencies found

    ## Reordering variables and trying again:

``` r
coef(mlr.bck.final,42)
```

    ##                     (Intercept)                            Year 
    ##               -128.126465484744                  0.092223100625 
    ##                StatusDeveloping                 Adult.Mortality 
    ##                 -0.808600106896                 -0.010777494540 
    ##                   infant.deaths                         Alcohol 
    ##                  0.013772325686                 -0.073782934323 
    ##          percentage.expenditure                         Measles 
    ##                 -0.000003835046                 -0.000016555755 
    ##                             BMI               under.five.deaths 
    ##                  0.004781492524                 -0.013223134165 
    ##                           Polio               Total.expenditure 
    ##                  0.020543927510                 -0.038881958753 
    ##                      Diphtheria                        HIV.AIDS 
    ##                  0.020890019530                 -0.132417039867 
    ##                             GDP                      Population 
    ##                  0.000007876767                  0.000000003002 
    ##              thinness.5.9.years Income.composition.of.resources 
    ##                 -0.045711953110                  4.492831876467 
    ##                       Schooling RegionAustralia and New Zealand 
    ##                  0.475168294378                  3.326096325960 
    ##                    RegionCanada                 RegionCaribbean 
    ##                  5.374147723064                  2.837750045050 
    ##            RegionCentral Africa              RegionCentral Asia 
    ##                 -2.373678088153                 -3.045579914013 
    ##            RegionCentral Europe            RegionEastern Africa 
    ##                  1.121304461375                 -1.469632292864 
    ##            RegionEastern Europe                   RegionMashriq 
    ##                 -0.573587794888                  0.334456289277 
    ##              RegionMeso-America              RegionNorth Africa 
    ##                  3.080159159729                 -0.544283451323 
    ##  RegionNW Pacific and East Asia             RegionSouth America 
    ##                  0.841043134511                  1.310642676690 
    ##                RegionSouth Asia             RegionSouth Pacific 
    ##                 -0.179296416553                 -1.311761372508 
    ##           RegionSouthern Africa                        RegionUS 
    ##                 -3.473880619932                  0.872321790270 
    ##            RegionWestern Africa            RegionWestern Europe 
    ##                 -3.953085133659                  4.307473415575 
    ##      RegionWestern Indian Ocean                    log.HIV.AIDS 
    ##                 -1.529166050938                 -1.606371320339 
    ##                         log.GDP      log.percentage.expenditure 
    ##                  0.131374259649                  0.123756858635 
    ##               ContinentAmericas 
    ##                  0.000000000000

``` r
# Metrics RMSE; R-squared; MAE
postResample(pred = predictions, obs = ytest)
```

    ##     RMSE Rsquared      MAE 
    ##   3.1186   0.8814   2.3798

``` r
##different result now, will repeat steps from earlier model

fit.bck.lm <- lm(Life.expectancy ~Year + Adult.Mortality + infant.deaths + Alcohol + percentage.expenditure + Measles + BMI + under.five.deaths + Polio + Total.expenditure + Diphtheria + Population + thinness.5.9.years + Income.composition.of.resources + Schooling + log.HIV.AIDS + log.GDP,data = train)

### Visualize VIF
fit.bck.lm_VIF = vif(fit.bck.lm)
barplot(fit.bck.lm_VIF, main = 'VIF Values', horiz = TRUE, col="blue", xlim = c(0,12))
abline(v=10, col="red")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-16.png)<!-- -->

``` r
#two are correlated and giving a high VIF it's under.five and infant.deaths
fit.bck.lm2 <- lm(Life.expectancy ~Year + Adult.Mortality + infant.deaths + Alcohol + percentage.expenditure + Measles + BMI +  Polio + Total.expenditure + Diphtheria + Population + thinness.5.9.years + Income.composition.of.resources + Schooling + log.HIV.AIDS + log.GDP,data = train)

fit.bck.lm2_VIF <- vif(fit.bck.lm2)
barplot(fit.bck.lm2_VIF, main = 'VIF Values', horiz = TRUE, col="blue", xlim = c(0,12))
abline(v=10, col="red")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-17.png)<!-- -->

``` r
##fit model with categorical variables and removed colinearity
fit.bck.lm3 <- lm(Life.expectancy ~Year + Status + Adult.Mortality + infant.deaths + Alcohol + percentage.expenditure + Measles + BMI +  Polio + Total.expenditure + Diphtheria + Population + thinness.5.9.years + Income.composition.of.resources + Schooling + log.HIV.AIDS + log.GDP + Region,data = train)


#### Hypothesis testing ####
summary(fit.bck.lm3)
```

    ## 
    ## Call:
    ## lm(formula = Life.expectancy ~ Year + Status + Adult.Mortality + 
    ##     infant.deaths + Alcohol + percentage.expenditure + Measles + 
    ##     BMI + Polio + Total.expenditure + Diphtheria + Population + 
    ##     thinness.5.9.years + Income.composition.of.resources + Schooling + 
    ##     log.HIV.AIDS + log.GDP + Region, data = train)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -22.669  -1.788  -0.125   1.696  13.259 
    ## 
    ## Coefficients:
    ##                                          Estimate        Std. Error t value
    ## (Intercept)                     -130.036755977454   31.621942481064   -4.11
    ## Year                               0.092878751186    0.015869195534    5.85
    ## StatusDeveloping                  -0.960683180074    0.318531975161   -3.02
    ## Adult.Mortality                   -0.011811740262    0.000733598784  -16.10
    ## infant.deaths                     -0.004634786727    0.000940118055   -4.93
    ## Alcohol                           -0.095313591898    0.028171577569   -3.38
    ## percentage.expenditure             0.000093082758    0.000046142508    2.02
    ## Measles                           -0.000020619731    0.000006868822   -3.00
    ## BMI                                0.003765772273    0.004983150775    0.76
    ## Polio                              0.020013973806    0.003928041130    5.10
    ## Total.expenditure                 -0.046319148758    0.032875503203   -1.41
    ## Diphtheria                         0.023242294106    0.004008161339    5.80
    ## Population                         0.000000004263    0.000000000802    5.31
    ## thinness.5.9.years                -0.042346800766    0.028651255912   -1.48
    ## Income.composition.of.resources    4.501613346003    0.592354010665    7.60
    ## Schooling                          0.470844968217    0.042203829703   11.16
    ## log.HIV.AIDS                      -1.891720803314    0.099860011121  -18.94
    ## log.GDP                            0.196245183454    0.056897075712    3.45
    ## RegionAustralia and New Zealand    3.959060793102    0.923104356326    4.29
    ## RegionCanada                       5.562558299765    1.012762633052    5.49
    ## RegionCaribbean                    3.280478163470    0.524670206038    6.25
    ## RegionCentral Africa              -1.787346522772    0.597247172972   -2.99
    ## RegionCentral Asia                -3.004172703727    0.587824998763   -5.11
    ## RegionCentral Europe               1.353135412524    0.525680755304    2.57
    ## RegionEastern Africa              -0.730802568292    0.625190953472   -1.17
    ## RegionEastern Europe              -0.025739909099    0.584583579617   -0.04
    ## RegionMashriq                      0.526716698948    0.556237133582    0.95
    ## RegionMeso-America                 3.814528016621    0.546846531487    6.98
    ## RegionNorth Africa                -0.209887664501    0.515034183495   -0.41
    ## RegionNW Pacific and East Asia     0.308290837495    0.634195688978    0.49
    ## RegionSouth America                1.798684144026    0.523762736848    3.43
    ## RegionSouth Asia                  -0.179063805291    0.562780857000   -0.32
    ## RegionSouth Pacific               -1.031270753128    0.546714454069   -1.89
    ## RegionSoutheast Asia               0.341799817573    0.490544471992    0.70
    ## RegionSouthern Africa             -3.854560863132    0.676993828236   -5.69
    ## RegionUS                           0.552454839286    1.123290942973    0.49
    ## RegionWestern Africa              -3.449951187040    0.565005197989   -6.11
    ## RegionWestern Europe               4.641766946363    0.563005751015    8.24
    ## RegionWestern Indian Ocean        -1.221591424236    0.622766661157   -1.96
    ##                                             Pr(>|t|)    
    ## (Intercept)                      0.00004047634847432 ***
    ## Year                             0.00000000548091985 ***
    ## StatusDeveloping                             0.00259 ** 
    ## Adult.Mortality                 < 0.0000000000000002 ***
    ## infant.deaths                    0.00000087727205956 ***
    ## Alcohol                                      0.00073 ***
    ## percentage.expenditure                       0.04377 *  
    ## Measles                                      0.00271 ** 
    ## BMI                                          0.44990    
    ## Polio                            0.00000037502752355 ***
    ## Total.expenditure                            0.15898    
    ## Diphtheria                       0.00000000754128740 ***
    ## Population                       0.00000011726772024 ***
    ## thinness.5.9.years                           0.13953    
    ## Income.composition.of.resources  0.00000000000004203 ***
    ## Schooling                       < 0.0000000000000002 ***
    ## log.HIV.AIDS                    < 0.0000000000000002 ***
    ## log.GDP                                      0.00057 ***
    ## RegionAustralia and New Zealand  0.00001865568025393 ***
    ## RegionCanada                     0.00000004372487367 ***
    ## RegionCaribbean                  0.00000000047516465 ***
    ## RegionCentral Africa                         0.00279 ** 
    ## RegionCentral Asia               0.00000034581541548 ***
    ## RegionCentral Europe                         0.01011 *  
    ## RegionEastern Africa                         0.24255    
    ## RegionEastern Europe                         0.96488    
    ## RegionMashriq                                0.34377    
    ## RegionMeso-America               0.00000000000390640 ***
    ## RegionNorth Africa                           0.68366    
    ## RegionNW Pacific and East Asia               0.62693    
    ## RegionSouth America                          0.00060 ***
    ## RegionSouth Asia                             0.75038    
    ## RegionSouth Pacific                          0.05937 .  
    ## RegionSoutheast Asia                         0.48601    
    ## RegionSouthern Africa            0.00000001392049825 ***
    ## RegionUS                                     0.62289    
    ## RegionWestern Africa             0.00000000118403070 ***
    ## RegionWestern Europe             0.00000000000000027 ***
    ## RegionWestern Indian Ocean                   0.04993 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.26 on 2450 degrees of freedom
    ## Multiple R-squared:  0.886,  Adjusted R-squared:  0.885 
    ## F-statistic:  503 on 38 and 2450 DF,  p-value: <0.0000000000000002

``` r
# At alpha = 0.05 the following variables are not significant therefore don't contribute to the model performance:
# BMI, Total.expenditure, thinness.5.9.years

# Predicting
train_pred = predict(fit.bck.lm3, train)
test_pred = predict(fit.bck.lm3, test)

# Scoring the final model on Training and Test set
residuals = resid(fit.bck.lm3)
postResample(pred = train_pred, obs = train$Life.expectancy)
```

    ##     RMSE Rsquared      MAE 
    ##   3.2392   0.8863   2.3912

``` r
postResample(pred = test_pred, obs = test$Life.expectancy)
```

    ##     RMSE Rsquared      MAE 
    ##   3.1073   0.8825   2.3793

``` r
sm = summary(fit.bck.lm3)
mse = mean(sm$residuals^2)

### Checking Multiple Liner Regression model assumptions
confint(fit.bck.lm3)
```

    ##                                            2.5 %           97.5 %
    ## (Intercept)                     -192.04525794015 -68.028254014755
    ## Year                               0.06176032628   0.123997176090
    ## StatusDeveloping                  -1.58530295537  -0.336063404776
    ## Adult.Mortality                   -0.01325027813  -0.010373202397
    ## infant.deaths                     -0.00647829499  -0.002791278464
    ## Alcohol                           -0.15055616038  -0.040071023419
    ## percentage.expenditure             0.00000260040   0.000183565112
    ## Measles                           -0.00003408903  -0.000007150433
    ## BMI                               -0.00600585117   0.013537395721
    ## Polio                              0.01231134939   0.027716598218
    ## Total.expenditure                 -0.11078579897   0.018147501451
    ## Diphtheria                         0.01538255935   0.031102028858
    ## Population                         0.00000000269   0.000000005836
    ## thinness.5.9.years                -0.09852998621   0.013836384677
    ## Income.composition.of.resources    3.34004697939   5.663179712614
    ## Schooling                          0.38808609727   0.553603839168
    ## log.HIV.AIDS                      -2.08753956743  -1.695902039198
    ## log.GDP                            0.08467384551   0.307816521397
    ## RegionAustralia and New Zealand    2.14891524859   5.769206337617
    ## RegionCanada                       3.57659890600   7.548517693529
    ## RegionCaribbean                    2.25163518457   4.309321142371
    ## RegionCentral Africa              -2.95850805150  -0.616184994045
    ## RegionCentral Asia                -4.15685798267  -1.851487424783
    ## RegionCentral Europe               0.32231081449   2.383960010555
    ## RegionEastern Africa              -1.95675997077   0.495154834187
    ## RegionEastern Europe              -1.17206898315   1.120589164954
    ## RegionMashriq                     -0.56402690133   1.617460299220
    ## RegionMeso-America                 2.74219875534   4.886857277902
    ## RegionNorth Africa                -1.21983505142   0.800059722420
    ## RegionNW Pacific and East Asia    -0.93532424555   1.551905920543
    ## RegionSouth America                0.77162065116   2.825747636892
    ## RegionSouth Asia                  -1.28263920698   0.924511596402
    ## RegionSouth Pacific               -2.10334101948   0.040799513221
    ## RegionSoutheast Asia              -0.62012489254   1.303724527687
    ## RegionSouthern Africa             -5.18210021819  -2.527021508076
    ## RegionUS                          -1.65024313487   2.755152813445
    ## RegionWestern Africa              -4.55788837178  -2.342014002301
    ## RegionWestern Europe               3.53775054264   5.745783350091
    ## RegionWestern Indian Ocean        -2.44279495257  -0.000387895903

``` r
hist(residuals, main = "Histogram of Residuals")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-18.png)<!-- -->

``` r
plot(residuals, main = "Residuals plot") 
abline(h=0, col="blue")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-19.png)<!-- -->

``` r
plot(fit.fwd.lm3, which = 2)
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-20.png)<!-- -->

``` r
plot(fit.fwd.lm3, which = 4)
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-21.png)<!-- -->

``` r
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

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-22.png)<!-- -->

``` r
bestlambda = cv.out$lambda.min  #Optimal penalty parameter.  You can make this call visually.
ridge.pred=predict(ridge.mod ,s=bestlambda ,newx=xtest)

testMSE_RIDGE<-mean((ytest-ridge.pred)^2)
testMSE_RIDGE
```

    ## [1] 9.551

``` r
coef(ridge.mod,s=bestlambda)
```

    ## 49 x 1 sparse Matrix of class "dgCMatrix"
    ##                                                s1
    ## (Intercept)                     -113.505283526518
    ## Year                               0.084482098182
    ## StatusDeveloping                  -1.049422516436
    ## Adult.Mortality                   -0.011074829979
    ## infant.deaths                     -0.001429935031
    ## Alcohol                           -0.021392856140
    ## percentage.expenditure             0.000043300288
    ## Hepatitis.B                        0.002217291759
    ## Measles                           -0.000018264657
    ## BMI                                0.011116190220
    ## under.five.deaths                 -0.002019179454
    ## Polio                              0.021943934825
    ## Total.expenditure                 -0.004393876540
    ## Diphtheria                         0.022409524045
    ## HIV.AIDS                          -0.176041095211
    ## GDP                                0.000003316440
    ## Population                         0.000000003517
    ## thinness..1.19.years              -0.026949927676
    ## thinness.5.9.years                -0.051480604337
    ## Income.composition.of.resources    4.691737371805
    ## Schooling                          0.448993253457
    ## RegionAustralia and New Zealand    3.418351198710
    ## RegionCanada                       3.889401156043
    ## RegionCaribbean                    1.651822713120
    ## RegionCentral Africa              -1.880887400303
    ## RegionCentral Asia                -3.256078437365
    ## RegionCentral Europe               0.985617418695
    ## RegionEastern Africa              -0.952752443385
    ## RegionEastern Europe              -0.567790521093
    ## RegionMashriq                      0.235354536949
    ## RegionMeso-America                 2.103694137141
    ## RegionNorth Africa                 0.475123720708
    ## RegionNW Pacific and East Asia     0.079798381911
    ## RegionSouth America                0.441277208937
    ## RegionSouth Asia                   0.079796063679
    ## RegionSouth Pacific               -0.529329847487
    ## RegionSoutheast Asia               0.085223568531
    ## RegionSouthern Africa             -2.945926895819
    ## RegionUS                          -0.586151023753
    ## RegionWestern Africa              -3.461866442718
    ## RegionWestern Europe               3.828777096802
    ## RegionWestern Indian Ocean        -0.419943994191
    ## log.HIV.AIDS                      -1.251469746511
    ## log.GDP                            0.168934798768
    ## log.percentage.expenditure         0.115162163493
    ## ContinentAmericas                  1.519656801404
    ## ContinentAsia                      1.227495154882
    ## ContinentEurope                    0.395467161155
    ## ContinentOceania                   0.116166434544

``` r
# Metrics RMSE; R-squared; MAE
postResample(pred = ridge.pred, obs = ytest)
```

    ##     RMSE Rsquared      MAE 
    ##   3.0905   0.8828   2.3275

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

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-23.png)<!-- -->

``` r
alpha = cva.out$alpha
mse = sapply(cva.out$modlist, function(mod) {min(mod$cvm)})
lambdaMin <- sapply(cva.out$modlist, `[[`, "lambda.min")
min_mse <- which.min(mse)
cva.min = data.frame(alpha = alpha[min_mse], lambdaMin = lambdaMin[min_mse], mse = mse[min_mse])
cva.min
```

    ##   alpha lambdaMin   mse
    ## 1 0.512   0.02482 10.61

``` r
elastic.mod = glmnet(x,y, alpha = cva.min$alpha, lambda = cva.min$lambdaMin)
elastic.pred=predict(elastic.mod ,s=cva.min$lambdaMin ,newx=xtest)
elastic.pred_coef=predict(elastic.mod ,s=cva.min$lambdaMin ,newx=xtest, type = "coef")

testMSE_ELASTIC<-mean((ytest-elastic.pred)^2)
testMSE_ELASTIC
```

    ## [1] 9.565

``` r
# Metrics RMSE; R-squared; MAE
postResample(pred = elastic.pred, obs = ytest)
```

    ##     RMSE Rsquared      MAE 
    ##   3.0927   0.8831   2.3321

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

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-24.png)<!-- -->

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
    ## -20.877  -2.038   0.013   1.823  15.364 
    ## 
    ## Coefficients:
    ##                                    Estimate  Std. Error t value
    ## (Intercept)                     -146.456805   34.140537   -4.29
    ## StatusDeveloping                  -2.135260    0.287632   -7.42
    ## ContinentAmericas                  3.764104    0.284927   13.21
    ## ContinentAsia                      1.187955    0.277995    4.27
    ## ContinentEurope                    1.992013    0.352913    5.64
    ## ContinentOceania                   0.221231    0.405818    0.55
    ## Income.composition.of.resources    5.833500    0.631967    9.23
    ## Schooling                          0.509646    0.041794   12.19
    ## log.HIV.AIDS                      -2.350421    0.079783  -29.46
    ## log.GDP                            0.186962    0.059559    3.14
    ## log.percentage.expenditure         0.222219    0.032729    6.79
    ## BMI                                0.012915    0.004738    2.73
    ## Year                               0.101032    0.017073    5.92
    ## Adult.Mortality                   -0.013590    0.000778  -17.47
    ##                                             Pr(>|t|)    
    ## (Intercept)                         0.00001856819056 ***
    ## StatusDeveloping                    0.00000000000016 ***
    ## ContinentAmericas               < 0.0000000000000002 ***
    ## ContinentAsia                       0.00001998876310 ***
    ## ContinentEurope                     0.00000001845670 ***
    ## ContinentOceania                              0.5857    
    ## Income.composition.of.resources < 0.0000000000000002 ***
    ## Schooling                       < 0.0000000000000002 ***
    ## log.HIV.AIDS                    < 0.0000000000000002 ***
    ## log.GDP                                       0.0017 ** 
    ## log.percentage.expenditure          0.00000000001402 ***
    ## BMI                                           0.0065 ** 
    ## Year                                0.00000000371605 ***
    ## Adult.Mortality                 < 0.0000000000000002 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.61 on 2475 degrees of freedom
    ## Multiple R-squared:  0.859,  Adjusted R-squared:  0.859 
    ## F-statistic: 1.16e+03 on 13 and 2475 DF,  p-value: <0.0000000000000002

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
    ## -20.877  -2.038   0.013   1.823  15.364 
    ## 
    ## Coefficients:
    ##                                    Estimate  Std. Error t value
    ## (Intercept)                     -146.456805   34.140537   -4.29
    ## StatusDeveloping                  -2.135260    0.287632   -7.42
    ## ContinentAmericas                  3.764104    0.284927   13.21
    ## ContinentAsia                      1.187955    0.277995    4.27
    ## ContinentEurope                    1.992013    0.352913    5.64
    ## ContinentOceania                   0.221231    0.405818    0.55
    ## Income.composition.of.resources    5.833500    0.631967    9.23
    ## Schooling                          0.509646    0.041794   12.19
    ## log.HIV.AIDS                      -2.350421    0.079783  -29.46
    ## log.GDP                            0.186962    0.059559    3.14
    ## log.percentage.expenditure         0.222219    0.032729    6.79
    ## BMI                                0.012915    0.004738    2.73
    ## Year                               0.101032    0.017073    5.92
    ## Adult.Mortality                   -0.013590    0.000778  -17.47
    ##                                             Pr(>|t|)    
    ## (Intercept)                         0.00001856819056 ***
    ## StatusDeveloping                    0.00000000000016 ***
    ## ContinentAmericas               < 0.0000000000000002 ***
    ## ContinentAsia                       0.00001998876310 ***
    ## ContinentEurope                     0.00000001845670 ***
    ## ContinentOceania                              0.5857    
    ## Income.composition.of.resources < 0.0000000000000002 ***
    ## Schooling                       < 0.0000000000000002 ***
    ## log.HIV.AIDS                    < 0.0000000000000002 ***
    ## log.GDP                                       0.0017 ** 
    ## log.percentage.expenditure          0.00000000001402 ***
    ## BMI                                           0.0065 ** 
    ## Year                                0.00000000371605 ***
    ## Adult.Mortality                 < 0.0000000000000002 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.61 on 2475 degrees of freedom
    ## Multiple R-squared:  0.859,  Adjusted R-squared:  0.859 
    ## F-statistic: 1.16e+03 on 13 and 2475 DF,  p-value: <0.0000000000000002

``` r
residuals = resid(MLRT$finalModel)
postResample(pred = train_pred, obs = train$Life.expectancy)
```

    ##     RMSE Rsquared      MAE 
    ##   3.6024   0.8594   2.6576

``` r
postResample(pred = test_pred, obs = test$Life.expectancy)
```

    ##     RMSE Rsquared      MAE 
    ##   3.3403   0.8633   2.5128

``` r
### Checking Multiple Liner Regression model assumptions
fit = lm(Life.expectancy ~ Status + Continent + Income.composition.of.resources + Schooling + log.HIV.AIDS + log.GDP + log.percentage.expenditure + BMI + Year + Adult.Mortality, train)
confint(fit)
```

    ##                                       2.5 %    97.5 %
    ## (Intercept)                     -213.403767 -79.50984
    ## StatusDeveloping                  -2.699284  -1.57124
    ## ContinentAmericas                  3.205385   4.32282
    ## ContinentAsia                      0.642828   1.73308
    ## ContinentEurope                    1.299978   2.68405
    ## ContinentOceania                  -0.574546   1.01701
    ## Income.composition.of.resources    4.594261   7.07274
    ## Schooling                          0.427691   0.59160
    ## log.HIV.AIDS                      -2.506869  -2.19397
    ## log.GDP                            0.070171   0.30375
    ## log.percentage.expenditure         0.158040   0.28640
    ## BMI                                0.003624   0.02221
    ## Year                               0.067553   0.13451
    ## Adult.Mortality                   -0.015116  -0.01206

``` r
hist(residuals, main = "Histogram of Residuals")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-25.png)<!-- -->

``` r
plot(residuals, main = "Residuals plot") 
abline(h=0, col="blue")
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-26.png)<!-- -->

``` r
plot(fit, which = 2)
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-27.png)<!-- -->

``` r
plot(fit, which = 4)
```

![](Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/Model%20building-28.png)<!-- -->

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
caret::nearZeroVar(LifeExpKNN %>% dplyr::select(where(is.numeric)), saveMetrics = TRUE) %>% 
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

    ## 2-nearest neighbor regression model

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

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-19-1.png" angle=90 style="display: block; margin: auto;" />

``` r
KNNvarImp = varImp(KNNRegressor)
plot(KNNvarImp, top = 5, main='Top 5 Variable predicting life expectancy (KNN)')
```

<img src="Tamas_Toth_MSDS_6372_Project1_files/figure-gfm/unnamed-chunk-19-2.png" angle=90 style="display: block; margin: auto;" />
