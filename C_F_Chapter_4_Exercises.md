
# Data relationships using Rossmann Store Sales

This exercise is based on the Kaggle competition "Rossmann Store Sales"
The data can be found here: https://www.kaggle.com/c/rossmann-store-sales/data

The first part merges select variables from store.csv and train.csv.  From previous knowledge of the dataset, rows with sales = 0 just add noise to the dataset and are removed.

* This part creates a dataset that can be used for these exercises.  Ideally if this code is reproduced, either the two csv files or the final file can be provided.

  * Load the datasets.  Explore the data to notice that train includes data on sales data and store houses information on individual stores.

```r
train <- read.csv("http://goo.gl/Zlr24r")
store <- read.csv("http://goo.gl/MtLCTf")
```

  * Remove store sales that are equal to zero.
    + Why? 
    
![Just Do It!](http://pawelb.com/content/data/justdoit.png) 

    + From prior knowledge of the dataset, 0 sales indicate that the store was closed on that day and not useful for this analysis.
  

  * Remove sales equal to 0

```r
train <- train[train$Sales > 0,]  
```

  * Merge the train and store data sets by the column "Store"

```r
train <- merge(train,store,by="Store")
```

  * Keep only the columns 1,2,3,4,5,7,10,11,12
  * Remove the rows where CompetitionDistance is missing

```r
raw <- train[,c(1,2,4,5,7,10,11,12)]
raw[is.na(raw$CompetitionDistance),8] <- mean(raw$CompetitionDistance)
```

  * Take a sample of 1000 rows just to make the analysis a big quicker

```r
data <- raw[sample(1:nrow(raw) , 1000 , replace=FALSE),]
rm(raw,store,train)
```


  * Use the data provided to create a corrplot of all of the variables.


```r
library(corrplot)
library(gplots)
```

```
## 
## Attaching package: 'gplots'
## 
## The following object is masked from 'package:stats':
## 
##     lowess
```

```r
data$StoreType <- as.numeric(data$StoreType)
data$Assortment <- as.numeric(data$Assortment)

corrplot.mixed(corr=cor((data) , use="complete.obs")
               , upper="ellipse"
               , tl.pos="lt"
               , col=colorpanel(50,"red","gray60","blue4"))
```

![plot of chunk unnamed-chunk-6](C_F_Chapter_4_Exercises_files/figure-html/unnamed-chunk-6.png) 

The correlation plot shows that there is some correlation
  * DayOfWeek ~ Sales + Promo
  * Sales ~ Customers + Promo
  * Customers ~ StoreType + CompetitionDistance
  * StoreType ~ Assortment

There is obvious positive correlation between customers and sales, which intuitively checks out.

  * Make two plots Sales by Customers and color the points by store type and promo
    + Use a log scale
    + Add appropriate axis labels, title, legend


```r
col.p1 <- c("black" , "green" , "blue" , "red")

plot( data$Customers , data$Sales
     , log="xy"
     , pch=c(1)
     , col=col.p1[data$StoreType] 
     , xlab="Customers"
     , ylab="Sales ($)"
     , main="Rossmann Sales")
legend(x="bottomright" , legend=paste("Store Type:" , levels(data$StoreType)) , col=col.p1 , pch=c(1) )
```

![plot of chunk unnamed-chunk-7](C_F_Chapter_4_Exercises_files/figure-html/unnamed-chunk-71.png) 

```r
col.p2 <- c("black","blue" )

plot( data$Customers , data$Sales
     , log="xy"
     , pch=c(1)
     , col=col.p2[data$Promo+1] 
     , xlab="Customers"
     , ylab="Sales ($)"
     , main="Rossmann Sales")
legend(x="bottomright" , legend=paste("Promo:" , levels(data$Promo)) , col=col.p2 , pch=c(1) )
```

![plot of chunk unnamed-chunk-7](C_F_Chapter_4_Exercises_files/figure-html/unnamed-chunk-72.png) 


  * Plot Sales by Competition and use promo as color
    + Use a log scale
    + Add appropriate axis labels, title, legend


```r
col.p3 <- c("black","blue")
plot(data$Sales,data$CompetitionDistance,log="xy",col=col.p3[data$Promo+1],pch=c(19))
```

![plot of chunk unnamed-chunk-8](C_F_Chapter_4_Exercises_files/figure-html/unnamed-chunk-8.png) 


  * Determine the correlation between Sales and Promo/StoreType/CompetitionDistance


```r
cor.test(data$Sales , data$StoreType)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  data$Sales and data$StoreType
## t = 0.0207, df = 998, p-value = 0.9835
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  -0.06134  0.06264
## sample estimates:
##       cor 
## 0.0006544
```

```r
cor.test(data$Sales , data$Promo)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  data$Sales and data$Promo
## t = 10.09, df = 998, p-value < 2.2e-16
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  0.2470 0.3595
## sample estimates:
##    cor 
## 0.3043
```

```r
cor.test(data$Sales , data$CompetitionDistance)   # Why does cor.test return value but cor return NA?
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  data$Sales and data$CompetitionDistance
## t = -2.231, df = 998, p-value = 0.02587
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  -0.131877 -0.008504
## sample estimates:
##      cor 
## -0.07046
```

These tests show that the correlation for Sales and Promo is 0.38 and is statistically significant.

  * Transform Sales using Box-Cox Transformations and test the same correlations as in (1)


```r
library(car)

l.sales <- coef(powerTransform(data$Sales))
l.comp <- coef(powerTransform(data$CompetitionDistance))

cor.test(bcPower(data$Sales,l.sales) , data$StoreType)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  bcPower(data$Sales, l.sales) and data$StoreType
## t = 0.9239, df = 998, p-value = 0.3558
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  -0.03282  0.09106
## sample estimates:
##     cor 
## 0.02923
```

```r
cor.test(bcPower(data$Sales,l.sales) , data$Promo)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  bcPower(data$Sales, l.sales) and data$Promo
## t = 11.56, df = 998, p-value < 2.2e-16
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  0.2877 0.3971
## sample estimates:
##    cor 
## 0.3435
```

```r
cor.test(bcPower(data$Sales,l.sales) , bcPower(data$CompetitionDistance , l.comp))
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  bcPower(data$Sales, l.sales) and bcPower(data$CompetitionDistance, l.comp)
## t = -2.4, df = 998, p-value = 0.01656
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  -0.13712 -0.01384
## sample estimates:
##      cor 
## -0.07577
```

Using the power transform, storetype is still not statistically correlated. 
Promo became more correlated with pearsons going from 0.38 to 0.42.
CompetitionDistance became correlated. Before the transform it had a coefficient of -0.05 and not statistically significant (p-value=0.16).  After the transform, the coefficient became -0.07 with a p-value of 0.02.








