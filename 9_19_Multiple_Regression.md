---
title: "9_19_Multiple_Regression"
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

# Thursday, September 19 
## Multiple Regression 
### Read Chapter 5 in Sheather 


```r
attach(fuel)
arrowplot(Tax, Fuel)
```

![](9_19_Multiple_Regression_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
pairs(cbind(Fuel, Tax, License))
```

![](9_19_Multiple_Regression_files/figure-html/unnamed-chunk-2-2.png)<!-- -->

```r
m<- lm(Fuel~Tax+License)
m
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
m0 <- lm(Fuel~1)
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
plot(m0)
```

```
## hat values (leverages) are all = 0.02083333
##  and there are no factor predictors; no plot no. 5
```

![](9_19_Multiple_Regression_files/figure-html/unnamed-chunk-2-3.png)<!-- -->

```r
mean(Fuel)
```

```
## [1] 576.7708
```

```r
anova(m0, m)
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
cor(Tax, Fuel)^2
```

```
## [1] 0.2036539
```

```r
m$fit
```

```
##        1        2        3        4        5        6        7        8 
## 477.3232 536.1430 546.1549 530.4421 533.1767 502.8162 416.7885 544.4401 
##        9       10       11       12       13       14       15       16 
## 514.4044 575.2639 515.6559 525.4361 602.7966 566.5035 645.3471 617.8145 
##       17       18       19       20       21       22       23       24 
## 600.2936 560.2461 790.5195 683.5867 714.1789 605.7629 459.8024 467.3113 
##       25       26       27       28       29       30       31       32 
## 525.8995 501.1014 538.1827 593.0164 556.9550 437.2756 532.7134 526.4560 
##       33       34       35       36       37       38       39       40 
## 575.7272 552.9688 461.8420 685.1000 656.9354 617.8145 666.0659 725.4423 
##       41       42       43       44       45       46       47       48 
## 667.8739 589.0303 639.0897 520.1985 757.5176 534.8915 664.1194 626.5749
```

```r
cor(Fuel, m$fit)^2
```

```
## [1] 0.5566811
```

Anova is used to compare two models. Right now (M0) we are comparing a model to a trivial model so it seems like there is an extra step (the setting up of M0). But this will be useful when we start to compare more complex models. 

The sum of squares says that the more variabels we add, the closer the model will get to the dataset. This is both good (better fitting model) and bad (as we want the model to be general enough to fit data outside of the model)

Correlation: $R^2$ what fraction of my variation did my model capture. OR how well do $\hat y$ reproduce y. 


** Estimate $\sigma^2$** 
$ s^2 $ = $\sigma^2$ = $\frac{SSE}{DF}$ = $\frac{\sum(Y-\hat Y)^2}{n- # of parameters}$


```r
m2<- lm(Fuel~Tax+License)
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

Var($\hat\beta_{0}$), perhaps var($\hat\beta_{TAX}$)
- Previously: $\frac{\sigma^2}{\sum(X-\bar X)^2}$
- as $s^2$ increases it's harder to attribute $\hat\beta_{TAX}$ 
- as $\sum(X- \bar X)^2$ increases it's easier to attribute  $\hat\beta_{TAX}$ 

In multiple regression there is a new element: how correlated are the X's. The more correlated the Xs are the harder it is to estimate the coefficients. The more uncorrelated the Xs are the easier it is to estimate the coefficients. 

tax 1 and tax 2 are very highy correlated.Opinions about 1 are highly dependent on opinions about the other. 

```r
set.seed(88)
tax1<- Tax + rnorm(48)/10
tax2<- Tax + rnorm(48)/10
plot(tax1, tax2)
```

![](9_19_Multiple_Regression_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
summary(lm(Fuel~Tax))
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
summary(lm(Fuel~tax1+tax2))
```

```
## 
## Call:
## lm(formula = Fuel ~ tax1 + tax2)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -213.21  -69.02    5.94   36.65  353.32 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  980.242    120.262   8.151 2.08e-10 ***
## tax1         -62.546    107.933  -0.579    0.565    
## tax2           9.902    106.363   0.093    0.926    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 102.1 on 45 degrees of freedom
## Multiple R-squared:  0.2025,	Adjusted R-squared:  0.1671 
## F-statistic: 5.714 on 2 and 45 DF,  p-value: 0.006148
```

```r
confidenceEllipse(m2)
```

![](9_19_Multiple_Regression_files/figure-html/unnamed-chunk-4-2.png)<!-- -->

```r
confidenceEllipse(lm(Fuel~tax1+tax2))
confint(lm(Fuel~tax1+tax2))
```

```
##                 2.5 %    97.5 %
## (Intercept)  738.0227 1222.4615
## tax1        -279.9334  154.8419
## tax2        -204.3245  224.1280
```

```r
points(0,0,pch=16,col="red")
```

![](9_19_Multiple_Regression_files/figure-html/unnamed-chunk-4-3.png)<!-- -->
The confidence ellipse covers the true pair of coefficients 95% of the time. 

Hypothesis Testing 
** T-test** 
- Estimated coefficients minus hypothesized value divided by the standard error 
- $\frac{\hat \beta_{d} -\beta}{\hat{se}(\hat \beta)}$

** F-Test** 
- $H_{0} : \beta_{TAX} = \beta_{LIC} = 0]$
- F test is testing whether both coefficients can be 0 in the same equation. 


$\beta_{TAX}$
$\beta_{TAX.LIC}$ <- what we are testing when we run the T-test? Is it possible for the Tac coefficient to be 0 when there is another coefficent in the equation. 


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

```r
anova(m2)
```

```
## Analysis of Variance Table
## 
## Response: Fuel
##           Df Sum Sq Mean Sq F value    Pr(>F)    
## Tax        1 119823  119823  20.672 4.091e-05 ***
## License    1 207709  207709  35.835 3.273e-07 ***
## Residuals 45 260834    5796                      
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


What does it mean for the F and T test to agree with each other? 
F-test it testing if they're all equal to 0
T-test tests each coefficient one at a time. 
Logically we expect these two sets to agree with each other. However they ** do not have to**.

F-test tells us someone's doing something, but T-Test tells us we might not know who's doing something. Know something did something (F-Test) but not what something (T-test). Seem to disagree but they are still in accordance because they are making assertions about different things. 

The second way that they can disagree is if they have a lot of variables. Still only one F test, but many t-tests. The tests are not always perfect (error), so it becomes probable there is a mistake the more t-tests we have to run. 


```r
rndt<-rt(200,45)
rndt 
```

```
##   [1]  1.797151000  1.020644043 -1.236861881 -0.820679720 -0.829626913
##   [6] -0.502467865 -1.029546321 -0.307092703 -0.628835760  0.217417270
##  [11]  0.598992598 -0.030859178  0.110410413 -1.315574507  1.015850996
##  [16] -0.670533691 -0.038648838  0.650661865  0.426578088  0.612404167
##  [21] -0.600880864 -0.346674267  0.199734464 -1.019315852 -0.557969731
##  [26] -0.951973818  0.078894133  0.613979543  1.592385789  0.133312060
##  [31] -0.211752558 -0.664828036  0.234552521 -0.442127135  2.126673738
##  [36] -0.509015116 -1.185550808 -0.781016874  1.189330082 -0.032932785
##  [41]  1.578425623  0.471213417  0.240193167 -0.697772342 -0.291929876
##  [46]  2.218301089 -0.657844826  0.072244050 -0.626336518  0.944307257
##  [51] -0.031650136  0.400000903  0.195133943 -1.475568469  0.093939697
##  [56]  1.267884803 -0.973234631 -1.495585914 -1.378905864  0.795473580
##  [61] -0.907442750 -1.098073627 -1.249898593  1.265617208 -1.126853325
##  [66]  0.121559080  0.098473210  0.196053603  1.154439203 -0.528371130
##  [71]  0.265634424  1.274760412 -0.458926498  1.399159752  0.660738674
##  [76]  1.264769841 -1.263460651 -0.565909316  1.473908827  0.848394699
##  [81]  0.252963277 -1.309304633  0.440266832 -0.314466279  0.188734763
##  [86] -0.655544993 -0.961491280  1.374526722  0.776042817  0.330794652
##  [91] -1.327450714  0.012569293 -1.423733556  0.515031226 -1.189821895
##  [96] -0.255184080  1.821590473 -0.116524531 -1.033833971  0.913377390
## [101]  0.835533910  1.078852005  1.967665828 -0.159543449  0.110721650
## [106] -0.567273547  0.418193669 -0.531585644  0.335066849  0.472641219
## [111]  0.277400000  0.569016130  0.953412589 -0.186580106  0.613058879
## [116]  1.397390779 -0.819484171  1.667854581 -0.430685620  0.507201606
## [121]  2.001369009 -0.923972071 -1.085184987 -0.630291161 -1.095618301
## [126]  1.325417243  1.811840325  0.917520370  1.266496678 -1.077476156
## [131] -0.945824175 -1.568707480  0.398081888  0.385318709  0.201418926
## [136]  0.795385555 -0.272394651  0.533175433 -1.178008230 -0.855125142
## [141]  0.512327885 -0.001538504  0.128899503  0.599228036  1.204047538
## [146]  1.227540211 -1.028567343  0.878401717  1.176606445  0.469172113
## [151]  1.117193466  1.046893884 -0.908136361 -0.165466070 -0.229710732
## [156] -1.112303065  0.687435380 -0.474507586 -1.986380302 -1.086836751
## [161] -0.271078506 -0.059536797 -1.377103167  0.523719104  0.389475783
## [166] -0.507762810  0.306629741 -0.796255478  0.378908495 -0.107768600
## [171] -0.362089523 -0.098277650 -0.059935822 -0.749661806 -0.274925081
## [176] -0.194194668  1.450740976 -0.967270356  0.399888083 -0.526827627
## [181]  0.033423051  0.139254551 -1.545166108 -0.948002402 -0.974684163
## [186] -0.794237522 -0.983519746  0.390264688 -1.034187435  1.360261530
## [191]  1.755322917 -0.714049784 -0.333571143  1.601659706 -1.279015750
## [196] -1.008633467  0.124106028  0.593659449 -0.588225381  1.311098957
```

```r
boxplot(rndt)
qt(.975,45)
```

```
## [1] 2.014103
```

```r
abline(h=qt(.975, 45))
```

![](9_19_Multiple_Regression_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
max(abs(rt(200,45)))
```

```
## [1] 2.707168
```

```r
boxplot(m2$residuals)
```

![](9_19_Multiple_Regression_files/figure-html/unnamed-chunk-6-2.png)<!-- -->

```r
qqnorm(m2$residuals)
qqline(m2$residuals)
```

![](9_19_Multiple_Regression_files/figure-html/unnamed-chunk-6-3.png)<!-- -->

```r
shapiro.test(m2$residuals)
```

```
## 
## 	Shapiro-Wilk normality test
## 
## data:  m2$residuals
## W = 0.95361, p-value = 0.05578
```

```r
plot(m2)
```

![](9_19_Multiple_Regression_files/figure-html/unnamed-chunk-6-4.png)<!-- -->![](9_19_Multiple_Regression_files/figure-html/unnamed-chunk-6-5.png)<!-- -->![](9_19_Multiple_Regression_files/figure-html/unnamed-chunk-6-6.png)<!-- -->![](9_19_Multiple_Regression_files/figure-html/unnamed-chunk-6-7.png)<!-- -->


Multiple Regression - what's the relationship between my variables? 


Building confidence intervals for regression coefficients. 
- $\hat \beta_{d} \pm t_{.025}$ --> $\hat{se} (\hat \beta_{d})$


```r
confint(m2)
```

```
##                   2.5 %     97.5 %
## (Intercept) -237.023698 454.965444
## Tax          -56.641661  -7.508982
## License        8.304148  16.725573
```


