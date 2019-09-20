---
title: "9/5: Simple Libear Regression"
output: 
  html_document:
    keep_md: true
---




```r
load("/Users/isabellelangrock/Desktop/Stats500/Rst500.RData")
library(ltm)
```

```
## Loading required package: MASS
```

```
## Loading required package: msm
```

```
## Loading required package: polycor
```

```r
library(car) 
```

```
## Loading required package: carData
```
# Thursday, September 5 
## Simple Linear Regression - Chapter 2 in Sheather (Simon Sheather)
### *Living, Breathing, Plotting*


```r
fuel
```

```
##    ID state Fuel   Tax License   Inc   Road
## 1   1    ME  541  9.00    52.5 3.571  1.976
## 2   2    NH  524  9.00    57.2 4.092  1.250
## 3   3    VT  561  9.00    58.0 3.865  1.586
## 4   4    MA  414  7.50    52.9 4.870  2.351
## 5   5    RI  410  8.00    54.4 4.399  0.431
## 6   6    CN  457 10.00    57.1 5.342  1.333
## 7   7    NY  344  8.00    45.1 5.319 11.868
## 8   8    NJ  467  8.00    55.3 5.126  2.138
## 9   9    PA  464  8.00    52.9 4.447  8.577
## 10 10    OH  498  7.00    55.2 4.512  8.507
## 11 11    IN  580  8.00    53.0 4.391  5.939
## 12 12    IL  471  7.50    52.5 5.126 14.186
## 13 13    MI  525  7.00    57.4 4.817  6.930
## 14 14    WI  508  7.00    54.5 4.207  6.580
## 15 15    MN  566  7.00    60.8 4.332  8.159
## 16 16    IA  635  7.00    58.6 4.318 10.340
## 17 17    MO  603  7.00    57.2 4.206  8.508
## 18 18    ND  714  7.00    54.0 3.718  4.725
## 19 19    SD  865  7.00    72.4 4.716  5.915
## 20 20    NE  640  8.50    67.7 4.341  6.010
## 21 21    KS  649  7.00    66.3 4.593  7.834
## 22 22    DE  540  8.00    60.2 4.983  0.602
## 23 23    MD  464  9.00    51.1 4.897  2.449
## 24 24    VA  547  9.00    51.7 4.258  4.686
## 25 25    WV  460  8.50    55.1 4.574  2.619
## 26 26    NC  566  9.00    54.4 3.721  4.746
## 27 27    SC  577  8.00    54.8 3.448  5.399
## 28 28    GA  631  7.50    57.9 3.846  9.061
## 29 29    FA  574  8.00    56.3 4.188  5.975
## 30 30    KY  534  9.00    49.3 3.601  4.650
## 31 31    TN  571  7.00    51.8 3.640  6.905
## 32 32    AL  554  7.00    51.3 3.333  6.594
## 33 33    MS  577  8.00    57.8 3.063  6.524
## 34 34    AR  628  7.50    54.7 3.357  4.121
## 35 35    LA  487  8.00    48.7 3.528  3.495
## 36 36    OK  644  6.58    62.9 3.802  7.834
## 37 37    TX  640  5.00    56.6 4.045 17.782
## 38 38    MT  704  7.00    58.6 3.897  6.385
## 39 39    ID  648  8.50    66.3 3.635  3.274
## 40 40    WY  968  7.00    67.2 4.345  3.905
## 41 41    CO  587  7.00    62.6 4.449  4.639
## 42 42    NM  699  7.00    56.3 3.656  3.985
## 43 43    AZ  632  7.00    60.3 4.300  3.635
## 44 44    UT  591  7.00    50.8 3.745  2.611
## 45 45    NV  782  6.00    67.2 5.215  2.302
## 46 46    WN  510  9.00    57.1 4.476  3.942
## 47 47    OR  610  7.00    62.3 4.296  4.083
## 48 48    CA  524  7.00    59.3 5.002  9.794
```

```r
par(mfrow=c(1,2))
boxplot(fuel$Tax)
plot(fuel$Tax,fuel$Fuel)
```

![](9_5_Simple_Regression_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
arrowplot(fuel$Tax, fuel$Fuel)
```

![](9_5_Simple_Regression_files/figure-html/unnamed-chunk-2-2.png)<!-- -->

```r
plot(fuel$Tax, fuel$Fuel)
abline(fuel$Tax, fuel$Fuel)
abline(lm(fuel$Fuel~fuel$Tax), col="red")
lm_sample <- lm(fuel$Fuel~fuel$Tax)
res<- lm_sample$residual
res
```

```
##           1           2           3           4           5           6 
##   34.949053   17.949053   54.949053 -171.710393 -149.157245    4.055351 
##           7           8           9          10          11          12 
## -215.157245  -92.157245  -95.157245 -114.263542   20.842755 -114.710393 
##          13          14          15          16          17          18 
##  -87.263542 -104.263542  -46.263542   22.736458   -9.263542  101.736458 
##          19          20          21          22          23          24 
##  252.736458  107.395904   36.736458  -19.157245  -42.050947   40.949053 
##          25          26          27          28          29          30 
##  -72.604096   59.949053   17.842755   45.289607   14.842755   27.949053 
##          31          32          33          34          35          36 
##  -41.263542  -58.263542   17.842755   42.289607  -72.157245    9.431812 
##          37          38          39          40          41          42 
##  -78.476138   91.736458  115.395904  355.736458  -25.263542   86.736458 
##          43          44          45          46          47          48 
##   19.736458  -21.263542  116.630160    3.949053   -2.263542  -88.263542
```

```r
fit <- lm_sample$fitted
fit
```

```
##        1        2        3        4        5        6        7        8 
## 506.0509 506.0509 506.0509 585.7104 559.1572 452.9446 559.1572 559.1572 
##        9       10       11       12       13       14       15       16 
## 559.1572 612.2635 559.1572 585.7104 612.2635 612.2635 612.2635 612.2635 
##       17       18       19       20       21       22       23       24 
## 612.2635 612.2635 612.2635 532.6041 612.2635 559.1572 506.0509 506.0509 
##       25       26       27       28       29       30       31       32 
## 532.6041 506.0509 559.1572 585.7104 559.1572 506.0509 612.2635 612.2635 
##       33       34       35       36       37       38       39       40 
## 559.1572 585.7104 559.1572 634.5682 718.4761 612.2635 532.6041 612.2635 
##       41       42       43       44       45       46       47       48 
## 612.2635 612.2635 612.2635 612.2635 665.3698 506.0509 612.2635 612.2635
```

```r
str(lm_sample)
```

```
## List of 12
##  $ coefficients : Named num [1:2] 984 -53.1
##   ..- attr(*, "names")= chr [1:2] "(Intercept)" "fuel$Tax"
##  $ residuals    : Named num [1:48] 34.9 17.9 54.9 -171.7 -149.2 ...
##   ..- attr(*, "names")= chr [1:48] "1" "2" "3" "4" ...
##  $ effects      : Named num [1:48] -3996 346.2 48.5 -175.5 -153.9 ...
##   ..- attr(*, "names")= chr [1:48] "(Intercept)" "fuel$Tax" "" "" ...
##  $ rank         : int 2
##  $ fitted.values: Named num [1:48] 506 506 506 586 559 ...
##   ..- attr(*, "names")= chr [1:48] "1" "2" "3" "4" ...
##  $ assign       : int [1:2] 0 1
##  $ qr           :List of 5
##   ..$ qr   : num [1:48, 1:2] -6.928 0.144 0.144 0.144 0.144 ...
##   .. ..- attr(*, "dimnames")=List of 2
##   .. .. ..$ : chr [1:48] "1" "2" "3" "4" ...
##   .. .. ..$ : chr [1:2] "(Intercept)" "fuel$Tax"
##   .. ..- attr(*, "assign")= int [1:2] 0 1
##   ..$ qraux: num [1:2] 1.14 1.18
##   ..$ pivot: int [1:2] 1 2
##   ..$ tol  : num 1e-07
##   ..$ rank : int 2
##   ..- attr(*, "class")= chr "qr"
##  $ df.residual  : int 46
##  $ xlevels      : Named list()
##  $ call         : language lm(formula = fuel$Fuel ~ fuel$Tax)
##  $ terms        :Classes 'terms', 'formula'  language fuel$Fuel ~ fuel$Tax
##   .. ..- attr(*, "variables")= language list(fuel$Fuel, fuel$Tax)
##   .. ..- attr(*, "factors")= int [1:2, 1] 0 1
##   .. .. ..- attr(*, "dimnames")=List of 2
##   .. .. .. ..$ : chr [1:2] "fuel$Fuel" "fuel$Tax"
##   .. .. .. ..$ : chr "fuel$Tax"
##   .. ..- attr(*, "term.labels")= chr "fuel$Tax"
##   .. ..- attr(*, "order")= int 1
##   .. ..- attr(*, "intercept")= int 1
##   .. ..- attr(*, "response")= int 1
##   .. ..- attr(*, ".Environment")=<environment: R_GlobalEnv> 
##   .. ..- attr(*, "predvars")= language list(fuel$Fuel, fuel$Tax)
##   .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
##   .. .. ..- attr(*, "names")= chr [1:2] "fuel$Fuel" "fuel$Tax"
##  $ model        :'data.frame':	48 obs. of  2 variables:
##   ..$ fuel$Fuel: int [1:48] 541 524 561 414 410 457 344 467 464 498 ...
##   ..$ fuel$Tax : num [1:48] 9 9 9 7.5 8 10 8 8 8 7 ...
##   ..- attr(*, "terms")=Classes 'terms', 'formula'  language fuel$Fuel ~ fuel$Tax
##   .. .. ..- attr(*, "variables")= language list(fuel$Fuel, fuel$Tax)
##   .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
##   .. .. .. ..- attr(*, "dimnames")=List of 2
##   .. .. .. .. ..$ : chr [1:2] "fuel$Fuel" "fuel$Tax"
##   .. .. .. .. ..$ : chr "fuel$Tax"
##   .. .. ..- attr(*, "term.labels")= chr "fuel$Tax"
##   .. .. ..- attr(*, "order")= int 1
##   .. .. ..- attr(*, "intercept")= int 1
##   .. .. ..- attr(*, "response")= int 1
##   .. .. ..- attr(*, ".Environment")=<environment: R_GlobalEnv> 
##   .. .. ..- attr(*, "predvars")= language list(fuel$Fuel, fuel$Tax)
##   .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
##   .. .. .. ..- attr(*, "names")= chr [1:2] "fuel$Fuel" "fuel$Tax"
##  - attr(*, "class")= chr "lm"
```

```r
cbind(fuel, fit, res)
```

```
##    ID state Fuel   Tax License   Inc   Road      fit         res
## 1   1    ME  541  9.00    52.5 3.571  1.976 506.0509   34.949053
## 2   2    NH  524  9.00    57.2 4.092  1.250 506.0509   17.949053
## 3   3    VT  561  9.00    58.0 3.865  1.586 506.0509   54.949053
## 4   4    MA  414  7.50    52.9 4.870  2.351 585.7104 -171.710393
## 5   5    RI  410  8.00    54.4 4.399  0.431 559.1572 -149.157245
## 6   6    CN  457 10.00    57.1 5.342  1.333 452.9446    4.055351
## 7   7    NY  344  8.00    45.1 5.319 11.868 559.1572 -215.157245
## 8   8    NJ  467  8.00    55.3 5.126  2.138 559.1572  -92.157245
## 9   9    PA  464  8.00    52.9 4.447  8.577 559.1572  -95.157245
## 10 10    OH  498  7.00    55.2 4.512  8.507 612.2635 -114.263542
## 11 11    IN  580  8.00    53.0 4.391  5.939 559.1572   20.842755
## 12 12    IL  471  7.50    52.5 5.126 14.186 585.7104 -114.710393
## 13 13    MI  525  7.00    57.4 4.817  6.930 612.2635  -87.263542
## 14 14    WI  508  7.00    54.5 4.207  6.580 612.2635 -104.263542
## 15 15    MN  566  7.00    60.8 4.332  8.159 612.2635  -46.263542
## 16 16    IA  635  7.00    58.6 4.318 10.340 612.2635   22.736458
## 17 17    MO  603  7.00    57.2 4.206  8.508 612.2635   -9.263542
## 18 18    ND  714  7.00    54.0 3.718  4.725 612.2635  101.736458
## 19 19    SD  865  7.00    72.4 4.716  5.915 612.2635  252.736458
## 20 20    NE  640  8.50    67.7 4.341  6.010 532.6041  107.395904
## 21 21    KS  649  7.00    66.3 4.593  7.834 612.2635   36.736458
## 22 22    DE  540  8.00    60.2 4.983  0.602 559.1572  -19.157245
## 23 23    MD  464  9.00    51.1 4.897  2.449 506.0509  -42.050947
## 24 24    VA  547  9.00    51.7 4.258  4.686 506.0509   40.949053
## 25 25    WV  460  8.50    55.1 4.574  2.619 532.6041  -72.604096
## 26 26    NC  566  9.00    54.4 3.721  4.746 506.0509   59.949053
## 27 27    SC  577  8.00    54.8 3.448  5.399 559.1572   17.842755
## 28 28    GA  631  7.50    57.9 3.846  9.061 585.7104   45.289607
## 29 29    FA  574  8.00    56.3 4.188  5.975 559.1572   14.842755
## 30 30    KY  534  9.00    49.3 3.601  4.650 506.0509   27.949053
## 31 31    TN  571  7.00    51.8 3.640  6.905 612.2635  -41.263542
## 32 32    AL  554  7.00    51.3 3.333  6.594 612.2635  -58.263542
## 33 33    MS  577  8.00    57.8 3.063  6.524 559.1572   17.842755
## 34 34    AR  628  7.50    54.7 3.357  4.121 585.7104   42.289607
## 35 35    LA  487  8.00    48.7 3.528  3.495 559.1572  -72.157245
## 36 36    OK  644  6.58    62.9 3.802  7.834 634.5682    9.431812
## 37 37    TX  640  5.00    56.6 4.045 17.782 718.4761  -78.476138
## 38 38    MT  704  7.00    58.6 3.897  6.385 612.2635   91.736458
## 39 39    ID  648  8.50    66.3 3.635  3.274 532.6041  115.395904
## 40 40    WY  968  7.00    67.2 4.345  3.905 612.2635  355.736458
## 41 41    CO  587  7.00    62.6 4.449  4.639 612.2635  -25.263542
## 42 42    NM  699  7.00    56.3 3.656  3.985 612.2635   86.736458
## 43 43    AZ  632  7.00    60.3 4.300  3.635 612.2635   19.736458
## 44 44    UT  591  7.00    50.8 3.745  2.611 612.2635  -21.263542
## 45 45    NV  782  6.00    67.2 5.215  2.302 665.3698  116.630160
## 46 46    WN  510  9.00    57.1 4.476  3.942 506.0509    3.949053
## 47 47    OR  610  7.00    62.3 4.296  4.083 612.2635   -2.263542
## 48 48    CA  524  7.00    59.3 5.002  9.794 612.2635  -88.263542
```

```r
boxplot(res)
plot(fit, res)
plot(lm_sample)
```

![](9_5_Simple_Regression_files/figure-html/unnamed-chunk-2-3.png)<!-- -->

```r
fuelsim()
```

![](9_5_Simple_Regression_files/figure-html/unnamed-chunk-2-4.png)<!-- -->

```r
par(mfrow=c(2,2))
fuelsim()
fuelsim()
fuelsim()
fuelsim()
```

![](9_5_Simple_Regression_files/figure-html/unnamed-chunk-2-5.png)<!-- -->

```r
s<-fuelsimbeta(1000)
boxplot(s)
mean(s)
```

```
## [1] -49.38838
```

```r
sd(s)
```

```
## [1] 15.38789
```

![](9_5_Simple_Regression_files/figure-html/unnamed-chunk-2-6.png)<!-- -->

$\alpha + \beta X_{1}$ is the fitted value for  $\widehat{x}$
$y_{i} - \widehat{y_{i}} = y_{i}-(\widehat{\alpha}+\widehat{s}x_{i}) = R_{i}$
Want $R_{i}$ to be small 
Want $y_{i} - \widehat{y_{i}}$ to be small 
Total or mean residual: $\sum R_{i}$
Least Squares $\sum_{i=1}^n (y_{i} - \widehat{y_{i}})^2 = \sum_{i=1}^n R_i^2$

How do you do it? How do you find $\widehat{\alpha}$ and $\widehat{\beta}$

-  you let the computer do it 
- you do calculus 
- formulas ("not that helpful")
  - 1 X  (pg. 19 in Sheather)
  - Many Xs -> matrix formulas
- You do something computationally stable 

Least squares pays a lot of attention to every point, so outliers can have **big** effects.  
**fitted** values tell us what the linear model predicts and the **residuals** show the error between the expected and the actual values. We would like to see no obvious pattern in the residuals. The plot of fit x res should **NOT** be interesting, otherwise it indicates that you could find a much better model to fit the data. 


$y_{i} = \alpha + \beta x_{i} + e_{i}$

$Fuel_{i} =\alpha + \beta TAX_{i} + e_{i}$

$e_{i}$ are independent, identically distributed, Normal with mean 0 and variance $\sigma^2$
or $e_{i}$ ~ $N(0,\sigma^2)$

What about $\sigma^2$? How do I get an estimate of $\sigma^2$? 
Not taking deviations around the mean (1 parameter), taking deviations around the line which has 2 parameters. 
thus, $(y_{1}-\widehat{y_{i}})^2 + (y_{2}-\widehat{y_{i}})^2 + ... + (y_{n}-\widehat{y_{i}})^2]/(n+2)$ 

General version: $(y_{1}-\widehat{y_{i}})^2 + (y_{2}-\widehat{y_{i}})^2 + ... + (y_{n}-\widehat{y_{i}})^2]/(sample size - Number of parameters for model)$ 
