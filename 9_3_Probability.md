---
title: "9_3_Probability"
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

# Tuesday, September 3 
## Probability 

### confidence interval 
95% - 95% of the time we use it it will cover $/mu$ and 5% of the time $/mu$ is missed 
[A,B] for $/mu$

$\bar{x} \pm s$ where s is the standard deviation 
$\bar{x} \pm (t*s)/\sqrt(n)$


t- value from t table with n-1 degrees of freedom (d.f.) 



```r
bloodpressure$change
```

```
##  [1]  -9  -4 -21  -3 -20 -31 -17 -26 -26 -10 -23 -33 -19 -19 -23
```

```r
boxplot(bloodpressure$change)
```

![](9_3_Probability_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
sd(bloodpressure$change)
```

```
## [1] 9.027471
```

```r
qt(.975, 10000)
```

```
## [1] 1.960201
```

```r
t.test(17+rnorm(100))$conf.int
```

```
## [1] 17.06401 17.44032
## attr(,"conf.level")
## [1] 0.95
```

```r
qnorm((1:15)/16)
```

```
##  [1] -1.5341205 -1.1503494 -0.8871466 -0.6744898 -0.4887764 -0.3186394
##  [7] -0.1573107  0.0000000  0.1573107  0.3186394  0.4887764  0.6744898
## [13]  0.8871466  1.1503494  1.5341205
```

```r
#plot(abline(v=qnorm((1:15)/16)))
plot(qnorm((1:15)/16), sort(bloodpressure$change))
```

![](9_3_Probability_files/figure-html/unnamed-chunk-2-2.png)<!-- -->

```r
par(mfrow=c(2,2))
qqnorm(rnorm(15))
qqnorm(rnorm(100))
qqnorm(rnorm(500))
qqnorm(rnorm(5000))
```

![](9_3_Probability_files/figure-html/unnamed-chunk-2-3.png)<!-- -->

```r
par(mfrow=c(2,2))
qqnorm(rcauchy(100))
qqnorm(rcauchy(200))
qqnorm(rcauchy(100))
qqnorm(rcauchy(300))
```

![](9_3_Probability_files/figure-html/unnamed-chunk-2-4.png)<!-- -->

### hypothesis testing 
#### an example 
Given 5 cards in a poker hand, whats the chance of a Royal Straight Flush


```r
possible_hands<-choose(52,5)
RSF <- 4/possible_hands
```
Chance that it will happen twice 

```r
RSF*RSF 
```

```
## [1] 2.368759e-12
```
Chance that it will happen thrice 

```r
RSF*RSF*RSF
```

```
## [1] 3.645702e-18
```

**The logic of hypothesis testing**: start with a hypothesis that we are not agnostic about. We have a preference. Compute a chance of it being true. 


Null Hypothesis: $H_{0}: \mu = \mu_{0}$ 
Alternative Hypothesis: $H_{A}: \mu \neq 0$

pt command: p-value, from t tables, chance that t is less than 1 standard deviation away from the mean. pt(s,d.f.)
multiply pt by two because we're doing a two-sided test. 


```r
pt(-8.12, 14)
```

```
## [1] 5.755231e-07
```

```r
pt(-8.12,14)*2
```

```
## [1] 1.151046e-06
```

```r
t.test(bloodpressure$change)
```

```
## 
## 	One Sample t-test
## 
## data:  bloodpressure$change
## t = -8.1228, df = 14, p-value = 1.146e-06
## alternative hypothesis: true mean is not equal to 0
## 95 percent confidence interval:
##  -23.93258 -13.93409
## sample estimates:
## mean of x 
## -18.93333
```


What if I tested every $\me_{0}$ 


