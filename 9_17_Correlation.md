---
title: "9_17_Correlation"
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



# Tuesday, September 17 
## Correlation 


```r
attach(fuel)
cor(Fuel, Tax)
```

```
## [1] -0.4512803
```
** Pearson's Correlation** 
- Oldest form of correlation and most tied to linear regression, not necessarily the most intuitive. If type of correlation is unspecified, it's pearson's correlation. 

Correlation is between -1 and 1. 



```r
Fuels<- (Fuel-mean(Fuel))/sd(Fuel)
Taxs <- (Tax-mean(Tax))/sd(Tax)
mean(Fuels)
```

```
## [1] -3.488872e-16
```

```r
sd(Fuels)
```

```
## [1] 1
```

```r
plot(Tax, Taxs)
```

![](9_17_Correlation_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
summary (Tax)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   5.000   7.000   7.500   7.668   8.125  10.000
```

```r
summary (Taxs)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -2.8065 -0.7029 -0.1770  0.0000  0.4803  2.4524
```

```r
par(mfrow=c(1,2))
plot(Tax, Fuel)
plot(Taxs, Fuels)
lm(Fuel~Tax)
```

```
## 
## Call:
## lm(formula = Fuel ~ Tax)
## 
## Coefficients:
## (Intercept)          Tax  
##      984.01       -53.11
```

```r
lm(Fuels~Taxs)
```

```
## 
## Call:
## lm(formula = Fuels ~ Taxs)
## 
## Coefficients:
## (Intercept)         Taxs  
##  -2.319e-16   -4.513e-01
```

```r
abline(lm(Fuels~Taxs), lwd=2, col="red")
```

![](9_17_Correlation_files/figure-html/unnamed-chunk-3-2.png)<!-- -->

```r
head(corEG)
```

```
##        x        y1        y2       y3        y4
## 1 -5.000 -26.81805 -5.035085 25.00000 0.8187308
## 2 -4.995 -67.19159 -4.975585 24.95003 0.8189355
## 3 -4.990 -16.33850 -4.938653 24.90010 0.8191402
## 4 -4.985 -31.21171 -5.035250 24.85023 0.8193450
## 5 -4.980 -58.38006 -4.973527 24.80040 0.8195499
## 6 -4.975 -32.98024 -5.031364 24.75062 0.8197548
```

```r
attach(corEG)
cor.test(Fuel, Tax)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  Fuel and Tax
## t = -3.4298, df = 46, p-value = 0.001285
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  -0.651834 -0.191730
## sample estimates:
##        cor 
## -0.4512803
```
How big are the values in terms of the mean and standard deviation. 

## What correlation doesn't explain 

```r
cor(x,y3)
```

```
## [1] 7.080628e-20
```

```r
plot(x,y3)
abline(lm(y3~x), col="blue", lwd=2)
```

![](9_17_Correlation_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


Pearson's Correlation only explains when variables are related by a line. Thus it gives us 0 for the plot of x, y3 above. Even though we know they are perfectly related. 


```r
cor(x, y4)
```

```
## [1] 0.9979248
```

```r
plot(x, y4)
abline(lm(y4~x), col="green", lwd=2)
```

![](9_17_Correlation_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
plot(lm(y4~x))
```

![](9_17_Correlation_files/figure-html/unnamed-chunk-5-2.png)<!-- -->![](9_17_Correlation_files/figure-html/unnamed-chunk-5-3.png)<!-- -->![](9_17_Correlation_files/figure-html/unnamed-chunk-5-4.png)<!-- -->![](9_17_Correlation_files/figure-html/unnamed-chunk-5-5.png)<!-- -->

```r
cor(x,y1)
```

```
## [1] 0.8210135
```

```r
cor(x, y2)
```

```
## [1] 0.9994091
```

```r
y<-rnorm(2001)
m<-lm(y~x)
summary(m)
```

```
## 
## Call:
## lm(formula = y ~ x)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.0559 -0.6353  0.0212  0.6536  3.0288 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)
## (Intercept)  0.027806   0.022126   1.257    0.209
## x           -0.009082   0.007661  -1.185    0.236
## 
## Residual standard error: 0.9898 on 1999 degrees of freedom
## Multiple R-squared:  0.0007025,	Adjusted R-squared:  0.0002026 
## F-statistic: 1.405 on 1 and 1999 DF,  p-value: 0.236
```

```r
res <- lm(y~x)$residual
cor(res,y)
```

```
## [1] 0.9996487
```

```r
cor(res, m$fitted)
```

```
## [1] -5.153613e-17
```

What correlation does not measure: 
- whether variables are realted 
- whether a line is appropriate 
- "bang for buck"
- whether model is useful -- coefficients are always right for the mdoel that they are in, so their meaning and implications highly depend on the model. 



```r
fuel[1:4,]
```

```
##   ID state Fuel Tax License   Inc  Road
## 1  1    ME  541 9.0    52.5 3.571 1.976
## 2  2    NH  524 9.0    57.2 4.092 1.250
## 3  3    VT  561 9.0    58.0 3.865 1.586
## 4  4    MA  414 7.5    52.9 4.870 2.351
```

```r
plot(License, Fuel)
identify(License, Fuel, label=state)
```

![](9_17_Correlation_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```
## integer(0)
```

```r
pairs(cbind(Fuel, Tax, License))
```

![](9_17_Correlation_files/figure-html/unnamed-chunk-6-2.png)<!-- -->

```r
cor(cbind(Fuel, Tax, License))
```

```
##               Fuel        Tax    License
## Fuel     1.0000000 -0.4512803  0.6989654
## Tax     -0.4512803  1.0000000 -0.2880372
## License  0.6989654 -0.2880372  1.0000000
```

```r
library(lattice)
levelplot(License~Tax*Fuel)
```

![](9_17_Correlation_files/figure-html/unnamed-chunk-6-3.png)<!-- -->

```r
### need to install rgl in order to make this command run. 
##plot3d(Tax,License,Fuel)
```
Plotting in 3d not the most helpful. So what do we do?


```r
fuelstar<- Fuel/(License/100)
cor(Fuel, fuelstar)
```

```
## [1] 0.860274
```

```r
summary(lm(fuelstar~Tax))
```

```
## 
## Call:
## lm(formula = fuelstar ~ Tax)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -235.65 -110.68    2.28   79.68  391.61 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1477.35     155.87   9.478 2.18e-12 ***
## Tax           -61.21      20.17  -3.034  0.00396 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 131.5 on 46 degrees of freedom
## Multiple R-squared:  0.1668,	Adjusted R-squared:  0.1486 
## F-statistic: 9.206 on 1 and 46 DF,  p-value: 0.00396
```

```r
cor(fuelstar, Tax)
```

```
## [1] -0.4083552
```

```r
#cor(fuel,Tax)
#cor(Fuel,Tax)
```

$ Y= \beta_{0} +\beta_{1}Tax + \beta_{2}License +\epsilon$
$\epsilon ~ N(0, \sigma^2)$

pick $\hat\beta_{0}, \hat\beta_{1}, \hat\beta_{2}$ to minimize --> Now just solving three linear equations with three unknowns. 

General form of the linear model $ Y= \beta_{0} + \beta_{1}X_{1} + \beta_{2}X_{2} + ... + \beta_{k}X_{k} + \epsilon$ where $ \epsilon ~ N(0, \sigma^2)$



```r
lm(Fuel~Tax + License)
```

```
## 
## Call:
## lm(formula = Fuel ~ Tax + License)
## 
## Coefficients:
## (Intercept)          Tax      License  
##      108.97       -32.08        12.51
```

```r
fuel[1:3,]
```

```
##   ID state Fuel Tax License   Inc  Road
## 1  1    ME  541   9    52.5 3.571 1.976
## 2  2    NH  524   9    57.2 4.092 1.250
## 3  3    VT  561   9    58.0 3.865 1.586
```

```r
m0<-lm(Fuel~1)
m1<- lm(Fuel~Tax)
m2 <- lm(Fuel~Tax + License)
anova(m0, m1)
```

```
## Analysis of Variance Table
## 
## Model 1: Fuel ~ 1
## Model 2: Fuel ~ Tax
##   Res.Df    RSS Df Sum of Sq      F   Pr(>F)   
## 1     47 588366                                
## 2     46 468543  1    119823 11.764 0.001285 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
anova(m0, m2)
```

```
## Analysis of Variance Table
## 
## Model 1: Fuel ~ 1
## Model 2: Fuel ~ Tax + License
##   Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
## 1     47 588366                                  
## 2     45 260834  2    327532 28.253 1.125e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
m2$resid
```

```
##           1           2           3           4           5           6 
##   63.676838  -12.143007   14.845104 -116.442089 -123.176719  -45.816200 
##           7           8           9          10          11          12 
##  -72.788515  -77.440093  -50.404428  -77.263929   64.344086  -54.436144 
##          13          14          15          16          17          18 
##  -77.796622  -58.503526  -79.347148   17.185545    2.706350  153.753904 
##          19          20          21          22          23          24 
##   74.480469  -43.586704  -65.178881  -65.762910    4.197643   79.688726 
##          25          26          27          28          29          30 
##  -65.899461   64.898603   38.817337   37.983608   17.045046   96.724392 
##          31          32          33          34          35          36 
##   38.286597   27.544028    1.272755   75.031162   25.157987  -41.099990 
##          37          38          39          40          41          42 
##  -16.935376   86.185545  -18.065899  242.557744  -80.873897  109.969725 
##          43          44          45          46          47          48 
##   -7.089718   70.801458   24.482423  -24.891521  -54.119439 -102.574857
```

```r
summary(m2)
```

```
## 
## Call:
## lm(formula = Fuel ~ Tax + License)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -123.177  -60.172   -2.908   45.032  242.558 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  108.971    171.786   0.634   0.5291    
## Tax          -32.075     12.197  -2.630   0.0117 *  
## License       12.515      2.091   5.986 3.27e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 76.13 on 45 degrees of freedom
## Multiple R-squared:  0.5567,	Adjusted R-squared:  0.537 
## F-statistic: 28.25 on 2 and 45 DF,  p-value: 1.125e-08
```
**The coefficient of a variable depends upon what other variables are in the model** 

