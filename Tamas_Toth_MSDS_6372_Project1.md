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
Belize
</td>
<td style="text-align:right;">
2004
</td>
<td style="text-align:left;">
Developing
</td>
<td style="text-align:right;">
68.7
</td>
<td style="text-align:right;">
197
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
6.20
</td>
<td style="text-align:right;">
325.681
</td>
<td style="text-align:right;">
97
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
43.2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
97
</td>
<td style="text-align:right;">
4.39
</td>
<td style="text-align:right;">
97
</td>
<td style="text-align:right;">
0.8
</td>
<td style="text-align:right;">
3831.54
</td>
<td style="text-align:right;">
27689
</td>
<td style="text-align:right;">
3.6
</td>
<td style="text-align:right;">
3.5
</td>
<td style="text-align:right;">
0.691
</td>
<td style="text-align:right;">
12.5
</td>
</tr>
<tr>
<td style="text-align:left;">
Malawi
</td>
<td style="text-align:right;">
2003
</td>
<td style="text-align:left;">
Developing
</td>
<td style="text-align:right;">
44.6
</td>
<td style="text-align:right;">
613
</td>
<td style="text-align:right;">
43
</td>
<td style="text-align:right;">
1.08
</td>
<td style="text-align:right;">
4.375
</td>
<td style="text-align:right;">
84
</td>
<td style="text-align:right;">
167
</td>
<td style="text-align:right;">
15.2
</td>
<td style="text-align:right;">
70
</td>
<td style="text-align:right;">
85
</td>
<td style="text-align:right;">
6.35
</td>
<td style="text-align:right;">
84
</td>
<td style="text-align:right;">
24.2
</td>
<td style="text-align:right;">
26.15
</td>
<td style="text-align:right;">
12336687
</td>
<td style="text-align:right;">
7.6
</td>
<td style="text-align:right;">
7.5
</td>
<td style="text-align:right;">
0.362
</td>
<td style="text-align:right;">
10.3
</td>
</tr>
<tr>
<td style="text-align:left;">
Niger
</td>
<td style="text-align:right;">
2006
</td>
<td style="text-align:left;">
Developing
</td>
<td style="text-align:right;">
54.5
</td>
<td style="text-align:right;">
277
</td>
<td style="text-align:right;">
54
</td>
<td style="text-align:right;">
0.11
</td>
<td style="text-align:right;">
43.422
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
59
</td>
<td style="text-align:right;">
15.7
</td>
<td style="text-align:right;">
110
</td>
<td style="text-align:right;">
52
</td>
<td style="text-align:right;">
7.39
</td>
<td style="text-align:right;">
51
</td>
<td style="text-align:right;">
1.5
</td>
<td style="text-align:right;">
258.46
</td>
<td style="text-align:right;">
1413264
</td>
<td style="text-align:right;">
11.6
</td>
<td style="text-align:right;">
11.5
</td>
<td style="text-align:right;">
0.286
</td>
<td style="text-align:right;">
3.7
</td>
</tr>
<tr>
<td style="text-align:left;">
Switzerland
</td>
<td style="text-align:right;">
2014
</td>
<td style="text-align:left;">
Developed
</td>
<td style="text-align:right;">
83.2
</td>
<td style="text-align:right;">
51
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
9.61
</td>
<td style="text-align:right;">
19479.912
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
57.0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
96
</td>
<td style="text-align:right;">
11.66
</td>
<td style="text-align:right;">
96
</td>
<td style="text-align:right;">
0.1
</td>
<td style="text-align:right;">
85814.59
</td>
<td style="text-align:right;">
8188649
</td>
<td style="text-align:right;">
0.4
</td>
<td style="text-align:right;">
0.3
</td>
<td style="text-align:right;">
0.936
</td>
<td style="text-align:right;">
15.9
</td>
</tr>
<tr>
<td style="text-align:left;">
Australia
</td>
<td style="text-align:right;">
2015
</td>
<td style="text-align:left;">
Developed
</td>
<td style="text-align:right;">
82.8
</td>
<td style="text-align:right;">
59
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
0.000
</td>
<td style="text-align:right;">
93
</td>
<td style="text-align:right;">
74
</td>
<td style="text-align:right;">
66.6
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
93
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
93
</td>
<td style="text-align:right;">
0.1
</td>
<td style="text-align:right;">
56554.39
</td>
<td style="text-align:right;">
23789338
</td>
<td style="text-align:right;">
0.6
</td>
<td style="text-align:right;">
0.6
</td>
<td style="text-align:right;">
0.937
</td>
<td style="text-align:right;">
20.4
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
