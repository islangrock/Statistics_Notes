---
title: "Notes, September 24th, More_Regression"
output: 
  html_document:
    keep_md: true
---

# Tuesday, September 24th 
## Mutliple Regression 
## Read Sheather chapters 6 (Regression by Stages) & 5 (General Linear Hypothesis)

### problem set out Thursday, 9/26. Due Tuesday Oct 15. 


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

```r
library(ppcor)
```



```r
vocabulary
```

```
##     Age Vocab
## 1  0.67     0
## 2  0.83     1
## 3  1.00     3
## 4  1.25    19
## 5  1.50    22
## 6  1.75   118
## 7  2.00   272
## 8  2.50   446
## 9  3.00   896
## 10 3.50  1222
## 11 4.00  1540
## 12 4.50  1870
## 13 5.00  2072
## 14 5.50  2289
## 15 6.00  2562
```

```r
attach(vocabulary)
```

```
## The following object is masked from package:carData:
## 
##     Vocab
```

```r
summary(lm(Vocab~Age))
```

```
## 
## Call:
## lm(formula = Vocab ~ Age)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -249.66 -104.98   13.14   78.47  268.25 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  -621.16      74.04  -8.389 1.32e-06 ***
## Age           526.73      22.12  23.808 4.17e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 148 on 13 degrees of freedom
## Multiple R-squared:  0.9776,	Adjusted R-squared:  0.9759 
## F-statistic: 566.8 on 1 and 13 DF,  p-value: 4.17e-12
```

```r
confint(lm(Vocab~Age))
```

```
##                 2.5 %    97.5 %
## (Intercept) -781.1248 -461.2001
## Age          478.9350  574.5272
```

```r
plot(Age,Vocab, xlim=c(0, 7), ylim=c(-1000,3000))
abline(lm(Vocab~Age))
abline(h=0, col="brown")
```

![](9_24_Multiple_Regression_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
agegt15<-pmax(0,Age-1.5)
agegt15
```

```
##  [1] 0.00 0.00 0.00 0.00 0.00 0.25 0.50 1.00 1.50 2.00 2.50 3.00 3.50 4.00
## [15] 4.50
```

```r
summary(lm(Vocab~Age+agegt15))
```

```
## 
## Call:
## lm(formula = Vocab ~ Age + agegt15)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -138.087  -22.182    1.278   22.627  111.817 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   11.086     92.013   0.120    0.906    
## Age           -9.365     73.540  -0.127    0.901    
## agegt15      596.413     81.083   7.356 8.79e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 65.63 on 12 degrees of freedom
## Multiple R-squared:  0.9959,	Adjusted R-squared:  0.9953 
## F-statistic:  1468 on 2 and 12 DF,  p-value: 4.545e-15
```

```r
confint(lm(Vocab~Age+agegt15))
```

```
##                 2.5 %   97.5 %
## (Intercept) -189.3925 211.5654
## Age         -169.5948 150.8650
## agegt15      419.7488 773.0769
```

```r
rbind(Age, agegt15)
```

```
##         [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
## Age     0.67 0.83    1 1.25  1.5 1.75  2.0  2.5  3.0   3.5   4.0   4.5
## agegt15 0.00 0.00    0 0.00  0.0 0.25  0.5  1.0  1.5   2.0   2.5   3.0
##         [,13] [,14] [,15]
## Age       5.0   5.5   6.0
## agegt15   3.5   4.0   4.5
```

```r
plot(Age, Vocab)
points(Age, lm(Vocab~Age+agegt15)$fitted, pch=16, col="orange")
```

![](9_24_Multiple_Regression_files/figure-html/unnamed-chunk-2-2.png)<!-- -->

```r
plot(lm(Vocab~Age))
```

![](9_24_Multiple_Regression_files/figure-html/unnamed-chunk-2-3.png)<!-- -->![](9_24_Multiple_Regression_files/figure-html/unnamed-chunk-2-4.png)<!-- -->![](9_24_Multiple_Regression_files/figure-html/unnamed-chunk-2-5.png)<!-- -->![](9_24_Multiple_Regression_files/figure-html/unnamed-chunk-2-6.png)<!-- -->

```r
detach(vocabulary)
```
agegt15 <- how much older than they are than 1.5 years. 

## Regression by Stages 

- Regress Fuel on Licenses, get residuals (fl)
- Regress Tax on on Licenses get residuals (tl)
- Regress residuals on residuals (fl on tl)
 - remove everything that Licenses can uniquely explain about Fuel and Tax. Then see what the residuals can explain. What's unique to Licenses that can't be predicted by Tax. 




```r
attach(fuel)
m2<-lm(Fuel~Tax+License)
m2
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
fl<- lm(Fuel~License)$residual
tl<- lm(Tax~License)$residual
lm(fl~tl)
```

```
## 
## Call:
## lm(formula = fl ~ tl)
## 
## Coefficients:
## (Intercept)           tl  
##   5.125e-15   -3.208e+01
```

```r
ft<-lm(Fuel~Tax)$residual
lt<-lm(License~Tax)$residual
lm(ft~lt)
```

```
## 
## Call:
## lm(formula = ft ~ lt)
## 
## Coefficients:
## (Intercept)           lt  
##  -1.119e-14    1.251e+01
```

```r
plot(tl, fl, xlab="Tax Residuals", ylab="Fuel Residuals")
abline(lm(fl~tl), col="blue")
```

![](9_24_Multiple_Regression_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
avPlot(m2, Tax)
```

![](9_24_Multiple_Regression_files/figure-html/unnamed-chunk-3-2.png)<!-- -->

```r
avPlot(m2,License)
```

![](9_24_Multiple_Regression_files/figure-html/unnamed-chunk-3-3.png)<!-- -->

```r
avPlots(m2)
```

![](9_24_Multiple_Regression_files/figure-html/unnamed-chunk-3-4.png)<!-- -->

```r
addedvarplot(lm(Fuel~License), Tax)
```

![](9_24_Multiple_Regression_files/figure-html/unnamed-chunk-3-5.png)<!-- -->

### Added Variable plot
abline(lm(fl~tl), col="blue") OR avPlot(m2, Tax)
 -> this is the line of the multiple regression (The Tax coefficient of m2)
 
### partial correlation: correlation between two residuals 


```r
cor(tl, fl)
```

```
## [1] -0.3649755
```

```r
cor(Tax, Fuel)
```

```
## [1] -0.4512803
```

```r
pcor(cbind(Fuel, Tax, License))
```

```
## $estimate
##               Fuel         Tax    License
## Fuel     1.0000000 -0.36497552 0.66581436
## Tax     -0.3649755  1.00000000 0.04292147
## License  0.6658144  0.04292147 1.00000000
## 
## $p.value
##                 Fuel        Tax      License
## Fuel    0.000000e+00 0.01165401 3.273022e-07
## Tax     1.165401e-02 0.00000000 7.745234e-01
## License 3.273022e-07 0.77452339 0.000000e+00
## 
## $statistic
##              Fuel        Tax   License
## Fuel     0.000000 -2.6297371 5.9862137
## Tax     -2.629737  0.0000000 0.2881915
## License  5.986214  0.2881915 0.0000000
## 
## $n
## [1] 48
## 
## $gp
## [1] 1
## 
## $method
## [1] "pearson"
```

```r
attach(partialcorEG)
pcor(partialcorEG)
```

```
## $estimate
##            y         x         z
## y  1.0000000 0.9061083 -0.712534
## x  0.9061083 1.0000000  0.931764
## z -0.7125340 0.9317640  1.000000
## 
## $p.value
##              y            x            z
## y 0.000000e+00 5.115484e-38 1.328672e-16
## x 5.115484e-38 0.000000e+00 1.802530e-44
## z 1.328672e-16 1.802530e-44 0.000000e+00
## 
## $statistic
##           y        x         z
## y   0.00000 21.09495 -10.00181
## x  21.09495  0.00000  25.27599
## z -10.00181 25.27599   0.00000
## 
## $n
## [1] 100
## 
## $gp
## [1] 1
## 
## $method
## [1] "pearson"
```

```r
m<-lm(Fuel~Tax+License+Inc)

iTL<-lm(Inc~Tax+License)$residual
fTL<-lm(Fuel~Tax+License)$residual
lm(fTL~iTL)
```

```
## 
## Call:
## lm(formula = fTL ~ iTL)
## 
## Coefficients:
## (Intercept)          iTL  
##   1.858e-15   -6.802e+01
```

```r
plot(iTL, fTL)
```

![](9_24_Multiple_Regression_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
avPlot(lm(Fuel~Tax+License+Inc), Inc)
```

![](9_24_Multiple_Regression_files/figure-html/unnamed-chunk-4-2.png)<!-- -->

### General Linear Hypothesis  -- Chapter 5 in Sheather

$H_{0}:\beta =0$ look at t-test 
$H_{o}:\beta_{1}=\beta_{2}=0$ look at F-test 

C is a subset of of {1,2,3,...,k}, $H_{0}: \beta_{d}=0$ for j in C

Fit Full model all k variables (Tax, License, Inc)
Fit Reduced model 


```r
m0<-lm(Fuel~1)
m0
```

```
## 
## Call:
## lm(formula = Fuel ~ 1)
## 
## Coefficients:
## (Intercept)  
##       576.8
```

```r
m1<-lm(Fuel~Tax)
anova(m0,m1)
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
anova(m0,m)
```

```
## Analysis of Variance Table
## 
## Model 1: Fuel ~ 1
## Model 2: Fuel ~ Tax + License + Inc
##   Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
## 1     47 588366                                  
## 2     44 191302  3    397064 30.442 8.235e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

Reduced 
R




