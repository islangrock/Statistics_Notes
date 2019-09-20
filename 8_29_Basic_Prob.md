---
title: "8/29 Basic Probability"
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

# Thursday, August 29 
## Basic Probability 


**Sample Space:** Set of everything $\Omega$ that might happen 

**Random Vairable:** Rule or function that assignes a number to each element of $\Omega

**Independence:** Independence of 2 random variables X and Y: Pr(X=x and Y=y)=Pr(X=x)Pr(Y=y)

**Variance** Variance or how far is x from the expectation of x: variance= $E[(X-E(X))^2]$

**Standard Deviation:** $\sqrt(variance)$

**Density Function:** f(x) of density function has two properties $f(x) \ge 0$ and total area under f(x)=1

**Central Limit Theorem:** mean of n nice independent random vairable is nearly normal as n approaches $\infty$



```r
red<-sample(c("head", "tail"), 10000, replace=TRUE)
blue <- sample(c("head","tail"), 10000, replace=TRUE)
red[1:10]
```

```
##  [1] "tail" "head" "tail" "tail" "tail" "head" "tail" "head" "head" "head"
```

```r
blue[1:10]
```

```
##  [1] "tail" "tail" "tail" "tail" "tail" "head" "head" "head" "head" "tail"
```

```r
cbind(red,blue)[1:10,]
```

```
##       red    blue  
##  [1,] "tail" "tail"
##  [2,] "head" "tail"
##  [3,] "tail" "tail"
##  [4,] "tail" "tail"
##  [5,] "tail" "tail"
##  [6,] "head" "head"
##  [7,] "tail" "head"
##  [8,] "head" "head"
##  [9,] "head" "head"
## [10,] "head" "tail"
```

```r
table(red,blue)
```

```
##       blue
## red    head tail
##   head 2586 2488
##   tail 2462 2464
```

```r
y<-1*(blue=="head")
table(y, blue)
```

```
##    blue
## y   head tail
##   0    0 4952
##   1 5048    0
```

```r
x<-1*(red=="tail")
table(x,y)
```

```
##    y
## x      0    1
##   0 2488 2586
##   1 2464 2462
```

```r
addmargins(table(x,y))
```

```
##      y
## x         0     1   Sum
##   0    2488  2586  5074
##   1    2464  2462  4926
##   Sum  4952  5048 10000
```

```r
toss10<- rbinom(10000,10,.5)
toss10[1:30]
```

```
##  [1] 7 4 7 5 7 0 2 4 6 4 7 5 5 1 2 5 6 5 7 4 4 6 7 6 5 6 5 3 4 4
```

```r
table(toss10)
```

```
## toss10
##    0    1    2    3    4    5    6    7    8    9   10 
##    9   98  420 1186 2067 2499 2006 1158  447   98   12
```

```r
barplot(table(toss10))
```

![](8_27_Basic_Prob_files/figure-html/unnamed-chunk-2-1.png)<!-- -->




```r
data(Abortion)
head(Abortion)
```

```
##   Item 1 Item 2 Item 3 Item 4
## 1      1      1      1      1
## 2      1      1      1      1
## 3      1      1      1      1
## 4      1      1      1      1
## 5      1      1      1      1
## 6      1      1      1      1
```

```r
help(Abortion)
apply(Abortion,2,mean)
```

```
##    Item 1    Item 2    Item 3    Item 4 
## 0.4379947 0.5936675 0.6358839 0.6174142
```

```r
table(Abortion[,1], Abortion[,3])
```

```
##    
##       0   1
##   0 131  82
##   1   7 159
```

Picking a random number 


```r
sample(0:9,1)
```

```
## [1] 1
```

**Density Functions:**
*Normal density* is not the normal density- it's not particularly normal. Sometimes called the Gaussian distribution. 

X~N(0,1) -> expectation 0, variance 1
$y= \sigma X +\mu$ ~$N(\mu, \sigma^2)$ -> expectation $\mu$, variance $\sigma^2$

Normal distribution has 2 roles: as a model for Data and in the Central limit theorem 

mean of n independent Y(i) ~ $N(\mu, \sigma^2)$ is $N(\mu, \sigma^2/n)$



```r
plotnormal(0,1)
abline(v=0, col="blue")
```

![](8_27_Basic_Prob_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
pnorm(1)
```

```
## [1] 0.8413447
```

```r
rnorm(100)
```

```
##   [1]  1.00572560 -0.22965862  0.83083803  1.18451700  0.35325254
##   [6] -0.31857261  0.42373864  1.26726016  0.44200507  1.55079292
##  [11]  0.72014903  0.82793332  0.52994834  0.82628089  0.71161251
##  [16] -0.21820364  1.22865137 -0.60372928  1.11729615 -1.19838135
##  [21] -0.66094264 -1.21598800 -1.09240117 -0.96285577  0.36546381
##  [26]  1.37180490  2.55749102 -0.56262407  0.55662327  0.61140014
##  [31] -0.03775265 -1.66735633  0.66037343 -1.07727622  0.35073023
##  [36] -0.77432336  1.11365938 -0.08885857 -0.51397165  0.57185987
##  [41]  0.80070222 -1.23555580  0.23955433  0.34844393  1.30093852
##  [46]  1.09954514  1.25782332 -0.19591907  1.71224332 -0.58002657
##  [51]  0.21569535  0.74443930 -0.83979092  1.11838061 -0.46216073
##  [56]  1.84930415  1.40868568  1.13026150 -0.92111109  1.06570607
##  [61]  0.86047474 -1.36052520 -0.95300489  0.66959859 -0.38263364
##  [66] -0.51639296 -0.06747694 -1.26189623 -0.13392104 -0.11573762
##  [71] -0.63538751 -0.25354452  1.37195468  0.04699301 -1.15378610
##  [76]  0.89858406 -0.32536010  1.07797828 -0.94094125 -1.99150439
##  [81]  0.71866388 -0.30308569  1.20419234  1.06833560  1.06513588
##  [86]  0.49792774 -0.97891190  1.37094336 -1.01231455 -1.40225646
##  [91]  0.94376967  0.23166814  1.72867393 -0.75655387  0.40829889
##  [96]  0.08529704  0.48532837 -0.89566872 -0.64062691 -0.02672666
```

```r
mean(rnorm(100))
```

```
## [1] -0.2192537
```

```r
mean(rnorm(1000))
```

```
## [1] -0.00499978
```


*Cauchy Distribution* Y is Cauchy then the mean of n indepentdent Caushy's is Cauchy (???)


```r
plotcauchy(0,1)
```

![](8_27_Basic_Prob_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
rcauchy(100)
```

```
##   [1]   4.70949458  -3.52672834  -0.35814684   0.84966267   3.43843424
##   [6]  -0.21621810  -9.20049838   0.76856551 -39.71826890  -0.73697063
##  [11]  -0.99341177  -1.99215134   0.27767361   3.96433594  -0.20208691
##  [16]  25.19457729   0.20049829  -0.21657591  -0.24095334  -0.37933432
##  [21]   1.20461242  -0.35216340   0.85757543  -1.00473226   0.81444093
##  [26]   0.39230192   0.12346967   0.25505524   7.89326787  66.40963372
##  [31]   0.66464560  -0.35861741   0.07979487  -0.57999544  -0.50478207
##  [36]   0.83413701   1.27717882   0.05196273  -2.24452767  -1.80379525
##  [41]   0.96527091  17.49169426   0.59398681  -1.55989315  -1.84889099
##  [46]  -3.32860743   0.22168911   0.06015605   1.61106219   1.79899355
##  [51]   3.42740555  -0.52791881   0.97336853   0.40528055  -3.27676357
##  [56]   2.29446610  -0.32386382   0.18477671  -0.06560804  -2.12540071
##  [61]  13.71672386  -0.95185693  -0.09660784   2.98553226   1.05838646
##  [66]   0.46798898  -0.63277897  -5.01573236  -2.20673571  -0.17169330
##  [71]   0.99369833  -1.57710481   0.79085301  -2.13311527   1.18836935
##  [76]  -0.53592665  -4.97028151  -0.18984315   1.82569915   4.73715184
##  [81]  -0.41157268  -1.32190590 -10.66097175   0.49790645   0.78947034
##  [86]  -1.40172475   4.04869478  -0.52558586  -0.01685530  -0.11696800
##  [91]  -0.95013944  -1.34526445  -0.17335861  -0.54357056  -0.52576714
##  [96]   0.74226227   0.39850698   1.49805819 -19.71092453  -2.19154028
```

```r
mean(rcauchy(100))
```

```
## [1] 15.3492
```

```r
mean(rcauchy(10000))
```

```
## [1] 3.163198
```
