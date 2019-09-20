---
title: "9_10_Simple_Regression"
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

# Tuesday, September 10
## Simple Regression (one X)
### Properties of Least Squares estimates 
 - unbiased
 - minimum variance unbiased
 - not robust 
 




```r
head(fuel)
```

```
##   ID state Fuel  Tax License   Inc  Road
## 1  1    ME  541  9.0    52.5 3.571 1.976
## 2  2    NH  524  9.0    57.2 4.092 1.250
## 3  3    VT  561  9.0    58.0 3.865 1.586
## 4  4    MA  414  7.5    52.9 4.870 2.351
## 5  5    RI  410  8.0    54.4 4.399 0.431
## 6  6    CN  457 10.0    57.1 5.342 1.333
```

```r
attach(fuel)
arrowplot(Tax,Fuel)
```

![](9_10_Simple_Regression_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
m<- lm(Fuel~Tax)
m$residual
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
summary(m)
```

```
## 
## Call:
## lm(formula = Fuel ~ Tax)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -215.16  -72.27    6.74   41.28  355.74 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   984.01     119.62   8.226 1.38e-10 ***
## Tax           -53.11      15.48  -3.430  0.00128 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 100.9 on 46 degrees of freedom
## Multiple R-squared:  0.2037,	Adjusted R-squared:  0.1863 
## F-statistic: 11.76 on 1 and 46 DF,  p-value: 0.001285
```

```r
dogdata
```

```
##   Before After Change
## 1    350   480    130
## 2    200   130    -70
## 3    240   250     10
## 4    290   310     20
## 5     90   280    190
## 6    370  1450   1080
## 7    240   280     40
```

```r
attach(dogdata)
boxplot(Change)
qqnorm(Change)
qqline(Change)
shapiro.test(Change)
```

```
## 
## 	Shapiro-Wilk normality test
## 
## data:  Change
## W = 0.65385, p-value = 0.001138
```

![](9_10_Simple_Regression_files/figure-html/unnamed-chunk-2-2.png)<!-- -->


```r
par(mfrow=c(2,2))
fuelsim()
fuelsim()
fuelsim()
fuelsim()
```

![](9_10_Simple_Regression_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
fs <- fuelsimbeta(1000)
boxplot(fs) 
abline(h=-50)

mean(fs)
```

```
## [1] -49.39641
```

```r
sd(fs)
```

```
## [1] 15.39063
```

```r
set.seed(88)
gm<-fuelsimbeta(1000)
sd(gm)
```

```
## [1] 15.44205
```

```r
summary(m)
```

```
## 
## Call:
## lm(formula = Fuel ~ Tax)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -215.16  -72.27    6.74   41.28  355.74 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   984.01     119.62   8.226 1.38e-10 ***
## Tax           -53.11      15.48  -3.430  0.00128 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 100.9 on 46 degrees of freedom
## Multiple R-squared:  0.2037,	Adjusted R-squared:  0.1863 
## F-statistic: 11.76 on 1 and 46 DF,  p-value: 0.001285
```

![](9_10_Simple_Regression_files/figure-html/unnamed-chunk-3-2.png)<!-- -->


Fitted line- least square fit 
True Line : $Y_{i}=\alpha +\beta X_{i} + E_{i}$
 
 
 Under the **normal model** least squares estimates are unbiased. 
 -$E(\widehat{\beta})= \beta$
 -$E(\widehat{\alpha})= \alpha$
 -$E(\widehat{\sigma}^2)= \sigma^2$
 
 Want the variability of $\widehat{\beta}$ to be small. Thus compute the variation or SD of $\widehat{\beta}$ 
 
 $var(\widehat{\beta})=E[(\widehat{\beta}-E(\beta)^2)]= E((\widehat{\beta}-\beta)^2)$
 or in words, Variance of estimate = expected value of the squared difference of estimate - parameter
 
 ### **Gauss-Markov Theorem**
 Among all unbiased estimates of $\beta$, the least squares estimate has the smallest variance. 
 
 $var(\widehat{\beta})=\frac{\sigma^2}{\sum_{i=1}^n(x_{i}-\bar{x})^2}$
 
This forumla tells you it's hard to estimate $\beta$ when there's a lot of variation. But it's good to estimate $\beta$ over a wide range. However you want most of the data points to account for the variabilty in x.You don't want the variability to be coming from just a few outlining data points in x. 
 
If the normal model is true: 
- $\hat\beta$ is normal 
- with mean $\beta$ 
- and variance, $\frac{\sigma^2}{\sum_{i=1}^n(x_{i}-\bar x)^2}$

95% Confidence Interval for $\beta$
$\hat\beta \pm$ allow for error
$\hat\beta \pm t_{.025} * \hat{se}(\hat\beta)$


```r
qt(.975,46)
```

```
## [1] 2.012896
```

```r
confint(m)
```

```
##                 2.5 %     97.5 %
## (Intercept) 743.21779 1224.79747
## Tax         -84.27315  -21.93945
```

```r
-53.11+2.012*15.48
```

```
## [1] -21.96424
```

### Hypothesis Testing 
$H_{0}: \beta = \beta_{0}$
most commonly, $\beta_{0}=0$
$t=\frac{\hat\beta -\beta_{0}}{\hat{se}(\hat\beta)}$

### Making Predictions: New X', what should I predict? 
 - Where is $\alpha +\beta X'$
 - Where is a new observation" $\alpha+ \beta X' + \epsilon$

```r
plot(Tax,Fuel)
```

![](9_10_Simple_Regression_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
 
