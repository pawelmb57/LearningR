# Statistical Tests


## Playing with rbinom

* Create a dataframe that includes the following:
    + The number of successes 1:100
    + The lower bound output from binom.test
    + The upper bound output from binom.test
    + The p-value output from binom.test
* Plot the lower and upper bounds by the number of successes
    + The graph will have both upper and lower bounds
    + Add an abline at 0.5
    + Color code the points that indicate a significant results (CI include 0.5 or pvalue>=0.5)



```r
N <- 100
p <- 0.5

A <- as.data.frame(matrix(rep(0,N*4),ncol=4,nrow=N))
colnames(A) <- c("successes","lb","ub","pvalue")


for(i in 1:N){
  A[i,1] <- i
  
  results <- binom.test(i,N,p=p)
  
  A[i,2] <- results$conf.int[1]
  A[i,3] <- results$conf.int[2]
  A[i,4] <- results$p.value
}

plot(A$successes,A$lb,ylim=0:1,xlab="Number of Successes",ylab="Upper and Lower Confidence Interval"
     ,col=ifelse(A$pvalue>=0.05,'green','black'))
par(new=TRUE)
plot(A$successes,A$ub,ylim=0:1,axes = FALSE, xlab = "", ylab = "",col=ifelse(A$pvalue>=0.05,'green','black'))
abline(h = p)
```

![plot of chunk unnamed-chunk-1](C_F_Chapter_6_Exercises_files/figure-html/unnamed-chunk-11.png) 

```r
plot(A$successes,A$pvalue,xlab="Number of Successes",ylab="p-value",col=ifelse(A$pvalue>=0.05,'green','black'))
```

![plot of chunk unnamed-chunk-1](C_F_Chapter_6_Exercises_files/figure-html/unnamed-chunk-12.png) 



#### Apocalyptic Tires

Create the data

```r
# Number of trials
n <- 100


tires <- data.frame(matrix(rep(0,(n*3)),nrow=(n*6),ncol=3))
colnames(tires) <- c("brand","temp","mpg")

tires$brand <- rep(c("A","B","C"),1,each=n*2)
tires$temp  <- as.factor(rep(c(40,90),(n*6)/2))

tires$mpg[tires$brand=="A"&tires$temp==40] <- rnorm(10,25,sd=5)
tires$mpg[tires$brand=="A"&tires$temp==90] <- rnorm(10,22,sd=5)

tires$mpg[tires$brand=="B"&tires$temp==40] <- rnorm(10,33,sd=5)
tires$mpg[tires$brand=="B"&tires$temp==90] <- rnorm(10,15,sd=5)

tires$mpg[tires$brand=="C"&tires$temp==40] <- rnorm(10,27,sd=5)
tires$mpg[tires$brand=="C"&tires$temp==90] <- rnorm(10,28,sd=5)
```



The zombies are coming and we need new tires for the Hummer. The market for new tires includes three brands, Rick, Carol, and Glenn. Each brand of tires were tested 10 times and the average miles per gallon were recorded. In addition to picking new tires, we need to decide whether to go north or south. Accordingly, the tires were tested at 40 degrees F and 90 degrees F. Determine if the tires performed differently and whether the brand or temperature were more important factors (incase the roads are blocked off by a hoard of zombies).



* Return the results of 4 anova tests for all possible combinations of variables
    + Determine which variables are significant in each test.


```r
a1 <- anova(aov(mpg ~ brand , data=tires))
a2 <- anova(aov(mpg ~ temp , data=tires))
a3 <- anova(aov(mpg ~ brand + temp , data=tires))
a4 <- anova(aov(mpg ~ brand + temp + brand*temp , data=tires))
```

* A1
  + Brand not significant
* A2
  + Temp is significant
* A3
  + Temp is significant, Brand is not significant
* A4
  + Brand is not significant , temp is significant, brand*temp is significant


* Compare the A2,A3,A4 together and determine which model is best.


```r
anova(
  aov(mpg ~ temp , data=tires),
  aov(mpg ~ brand + temp , data=tires),
  aov(mpg ~ brand + temp + brand*temp , data=tires)
  )
```

```
## Analysis of Variance Table
## 
## Model 1: mpg ~ temp
## Model 2: mpg ~ brand + temp
## Model 3: mpg ~ brand + temp + brand * temp
##   Res.Df   RSS Df Sum of Sq     F Pr(>F)    
## 1    598 21321                              
## 2    596 20415  2       906  26.4  1e-11 ***
## 3    594 10186  2     10229 298.2 <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

The results indicate that A4 is the best model which include all interaction terms.

* Graph the group confidence intervations by brand and tires


```r
library(multcomp)
```

```
## Loading required package: mvtnorm
## Loading required package: survival
## Loading required package: TH.data
```

```r
brand.aov <- aov(mpg ~ -1 + brand , data=tires)
par(mar=c(6,10,2,2))
plot(glht(brand.aov))
```

![plot of chunk unnamed-chunk-5](C_F_Chapter_6_Exercises_files/figure-html/unnamed-chunk-51.png) 

```r
temp.aov <- aov(mpg ~ -1 + temp , data=tires)
par(mar=c(6,10,2,2))
plot(glht(temp.aov))
```

![plot of chunk unnamed-chunk-5](C_F_Chapter_6_Exercises_files/figure-html/unnamed-chunk-52.png) 

Determine which brand has the best gas mileage and whether to go north or south for the apocolypse.

```r
both.aov <- aov(mpg ~ -1 + brand*temp , data=tires)
par(mar=c(6,10,2,2))
plot(glht(both.aov))
```

![plot of chunk unnamed-chunk-6](C_F_Chapter_6_Exercises_files/figure-html/unnamed-chunk-6.png) 

Brand C and go South


















