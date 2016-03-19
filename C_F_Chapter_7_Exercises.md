
height weight bmi smoking active/notactive gender 

  * Create the data for the exercises


```r
library(data.table)
set.seed(2)

N <- 100 # 100 Patients

data = data.table(matrix(rep(NA,600),nrow=100,ncol=8))
setnames(data,colnames(data),c("gender"
                               ,"activityLevel"
                               ,"height"
                               ,"weight"
                               ,"bmi"
                               ,"smoker"
                               ,"age"
                               ,"risk"))

data$gender <- factor(ifelse(rbinom(N,1,0.5)==1,"female","male"))
data$activityLevel <- factor(sample(c(1,2,3),N,replace=TRUE))
data$smoker <- factor(ifelse(rbinom(N,1,0.1)==1,"yes","no"))

data$height[data$gender=="female"] <- rpois(length(data$gender[data$gender=="female"]),64)
data$height[data$gender=="male"] <- rpois(length(data$gender[data$gender=="male"]),69)

data$weight[data$gender=="female"] <- rpois(length(data$gender[data$gender=="female"]),150)
data$weight[data$gender=="male"] <- rpois(length(data$gender[data$gender=="male"]),180)

data$age <-  round(runif(N , min=18 , max=70))

data$bmi <- (data$weight / data$height^2)*703

data$risk <- ifelse(data$bmi >= 35 , rnorm(length(data$bmi[data$bmi>=35]),8,2) , 
                    ifelse(data$smoker=="yes",rnorm(length(data$smoker[data$smoker=="yes"]),8,2),
                           ifelse(data$bmi >= 25 & data$bmi < 30 , rnorm(length(data$bmi[data$bmi>=25 & data$bmi<30]),3,3) , NA)))
data$risk[is.na(data$risk)] <- rnorm(length(data$risk[is.na(data$risk)]),5,3)
data$risk <- ifelse(data$risk<1,1,ifelse(data$risk>10,10,data$risk))
```


#### Explore the dataset

Use gpairs 
* Do all of the variable seem to be normally distributed
    + Notice that height and weight (as expcted) may seem a little skewed
* Use corrplot
    + Notice that height and bmi seem to be very correlated (as expected) with a correlation coefficient of -0.85. When putting these into the linear model, we should expect that height will not be a significant variable and any variation should be sufficiently explained by bmi.  
    + However, because weight is also used in the bmi calculation, why does it not correlate more strongly with bmi?  This is due to the bmi formula.  Weight is divided by weight^2 and multiplied by 708.  Thus, the weight may be overshadowed by larger numbers.
    + Also notice that there is a correlation between bmi and risk with a correlation coefficient of -0.58.  This was expected as persons with a bmi over 35 are given a risk of one.  Why is this not a stronger coefficient then?  The factor smoker is also a strong predictor of risk.



```r
library(gpairs)
library(corrplot)
data <- data.frame(data)

gpairs(data)
```

```
## Loading required package: grid
## Loading required package: lattice
```

![plot of chunk unnamed-chunk-2](C_F_Chapter_7_Exercises_files/figure-html/unnamed-chunk-21.png) 

```r
newdata <- data
newdata[,c(2:5,7:8)] <- sapply(data[,c(2:5,7:8)],as.numeric)

corrplot.mixed(cor(newdata[,c(2,3,4,5,7,8)]),upper="ellipse")
```

![plot of chunk unnamed-chunk-2](C_F_Chapter_7_Exercises_files/figure-html/unnamed-chunk-22.png) 

```r
# Make all of the variables numeric and plot the new corrplot
newdata <- sapply(data,as.numeric)
corrplot.mixed(cor(newdata))
```

![plot of chunk unnamed-chunk-2](C_F_Chapter_7_Exercises_files/figure-html/unnamed-chunk-23.png) 



  * Plot risk by bmi adding the line of best fit using abline



```r
m1 <- lm(risk ~ bmi , data=data)

plot(risk ~ bmi , data=data
     ,xlab="bmi",ylab="risk")
abline(m1,col='blue')
```

![plot of chunk unnamed-chunk-3](C_F_Chapter_7_Exercises_files/figure-html/unnamed-chunk-3.png) 


  * Using the coefficient from the linear model, calculate the residuals and return a summary.  Confirm the calculated residuals using summary(model)



```r
residuals <- data$risk-(m1$coefficients[1]+data$bmi*m1$coefficients[2])

summary(residuals)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  -4.990  -2.600   0.185   0.000   2.320   6.000
```

```r
summary(m1)
```

```
## 
## Call:
## lm(formula = risk ~ bmi, data = data)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -4.990 -2.602  0.185  2.315  5.996 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)   
## (Intercept)   2.1382     1.0734    1.99   0.0491 * 
## bmi           0.1181     0.0359    3.29   0.0014 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.92 on 98 degrees of freedom
## Multiple R-squared:  0.0995,	Adjusted R-squared:  0.0903 
## F-statistic: 10.8 on 1 and 98 DF,  p-value: 0.00139
```



  * Summarize two linear models (bmi+smoker+heigher and bmi+smoker+bmiXsmoker

```r
m2 <- lm(risk ~ bmi + smoker + height, data=data)
summary(m2)
```

```
## 
## Call:
## lm(formula = risk ~ bmi + smoker + height, data = data)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -4.790 -2.552  0.245  2.180  6.075 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)  
## (Intercept)   7.1610     6.1466    1.17    0.247  
## bmi           0.0657     0.0681    0.97    0.337  
## smokeryes     2.2406     0.9167    2.44    0.016 *
## height       -0.0573     0.0664   -0.86    0.391  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.85 on 96 degrees of freedom
## Multiple R-squared:  0.163,	Adjusted R-squared:  0.137 
## F-statistic: 6.25 on 3 and 96 DF,  p-value: 0.000636
```

```r
m3 <- lm(risk ~ bmi + smoker + bmi*smoker , data=data  )
summary(m3)
```

```
## 
## Call:
## lm(formula = risk ~ bmi + smoker + bmi * smoker, data = data)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -4.757 -2.429  0.284  2.010  5.968 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)   
## (Intercept)     1.7076     1.0977    1.56   0.1231   
## bmi             0.1242     0.0368    3.38   0.0011 **
## smokeryes       4.8771     3.6382    1.34   0.1832   
## bmi:smokeryes  -0.0869     0.1205   -0.72   0.4726   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.85 on 96 degrees of freedom
## Multiple R-squared:  0.162,	Adjusted R-squared:  0.135 
## F-statistic: 6.17 on 3 and 96 DF,  p-value: 0.000707
```


* M2 shows that smoker:yes is significant but the other tow variables are not.  From our earlier analysis and just conceptually we expected that BMI would be an important variables.  Why is BMI not significant any more?  Due to how the data was created (IF smokeryes=1 then risk=10), the model could be overfitting on smokeryes.



For M2 above, remove height and create two new models for M2 and M3 using the step function. Also compare the models to using all of the variables.  Compare the models and interpret the results.  Which model is best and interpret the model results.


```r
m4 <- step(lm(risk ~ bmi + smoker , data=data))
```

```
## Start:  AIC=211.9
## risk ~ bmi + smoker
## 
##          Df Sum of Sq RSS AIC
## <none>                784 212
## - smoker  1      53.4 837 216
## - bmi     1      89.3 873 221
```

```r
summary(m4)
```

```
## 
## Call:
## lm(formula = risk ~ bmi + smoker, data = data)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -4.726 -2.513  0.233  2.159  5.887 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)   
## (Intercept)   1.9399     1.0467    1.85   0.0669 . 
## bmi           0.1161     0.0349    3.32   0.0013 **
## smokeryes     2.3371     0.9087    2.57   0.0116 * 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.84 on 97 degrees of freedom
## Multiple R-squared:  0.157,	Adjusted R-squared:  0.14 
## F-statistic: 9.03 on 2 and 97 DF,  p-value: 0.000253
```

```r
m5 <- step(lm(risk ~ bmi + smoker + bmi*smoker , data=data  ))
```

```
## Start:  AIC=213.3
## risk ~ bmi + smoker + bmi * smoker
## 
##              Df Sum of Sq RSS AIC
## - bmi:smoker  1      4.22 784 212
## <none>                    779 213
## 
## Step:  AIC=211.9
## risk ~ bmi + smoker
## 
##          Df Sum of Sq RSS AIC
## <none>                784 212
## - smoker  1      53.4 837 216
## - bmi     1      89.3 873 221
```

```r
summary(m5)
```

```
## 
## Call:
## lm(formula = risk ~ bmi + smoker, data = data)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -4.726 -2.513  0.233  2.159  5.887 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)   
## (Intercept)   1.9399     1.0467    1.85   0.0669 . 
## bmi           0.1161     0.0349    3.32   0.0013 **
## smokeryes     2.3371     0.9087    2.57   0.0116 * 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.84 on 97 degrees of freedom
## Multiple R-squared:  0.157,	Adjusted R-squared:  0.14 
## F-statistic: 9.03 on 2 and 97 DF,  p-value: 0.000253
```

```r
m6 <- lm(risk ~ ., data=data  )
summary(m6)
```

```
## 
## Call:
## lm(formula = risk ~ ., data = data)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -4.570 -1.936 -0.284  1.842  5.384 
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    -1.47e+01   8.91e+00   -1.65  0.10333    
## gendermale     -2.84e-01   9.42e-01   -0.30  0.76323    
## activityLevel2  9.93e-01   6.83e-01    1.45  0.14948    
## activityLevel3  2.36e+00   6.35e-01    3.72  0.00034 ***
## height          2.91e-01   1.28e-01    2.28  0.02509 *  
## weight         -7.29e-02   3.16e-02   -2.31  0.02329 *  
## bmi             4.28e-01   1.31e-01    3.28  0.00148 ** 
## smokeryes       1.94e+00   8.76e-01    2.22  0.02888 *  
## age            -3.37e-04   1.77e-02   -0.02  0.98485    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.65 on 91 degrees of freedom
## Multiple R-squared:  0.314,	Adjusted R-squared:  0.254 
## F-statistic: 5.21 on 8 and 91 DF,  p-value: 2.26e-05
```

```r
summary(m2)$r.squared
```

```
## [1] 0.1635
```

```r
summary(m3)$r.squared
```

```
## [1] 0.1615
```

```r
summary(m4)$r.squared
```

```
## [1] 0.157
```

```r
summary(m5)$r.squared
```

```
## [1] 0.157
```

```r
summary(m6)$r.squared
```

```
## [1] 0.3142
```

```r
plot(data$risk , fitted(m1) , col='red')
points(data$risk , fitted(m2) , col='blue')
```

![plot of chunk unnamed-chunk-6](C_F_Chapter_7_Exercises_files/figure-html/unnamed-chunk-6.png) 

```r
# library(coefplot)
# coefplot(m1,intercept=FALSE,outerCI=1.96,lwdOuter=1.5)
```


The model using all of the variables is best while being able to explain approximately 31% of the variation where the other models explain about 15%.  The model found that activity level 3 was significant and an increase in activity level increased risk by about 2.3 points.  BMI and smoker: were also found to be significant.  

On a concluding thought, the best model in this case explained 30% of the variation.  The r-squared value is 0.31.  Due to this being clinical data, it would be expected that the model would perform better.  Even though we found significance in variables that conceptually make sense, we might have to disregard 








