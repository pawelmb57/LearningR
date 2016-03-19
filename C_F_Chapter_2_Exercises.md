# C&F Chapter 2 Exercies
Pawel Bogdanowicz  

# Basic R Exercises

### Manipulating Dataframes

##### Create the following dataframe

transactionID | Marketing Channel | Product | Returns | Sales
--------------|-------------------|---------|--------|------
1001 | Amazon | Macbook | Yes | 1200
1002 | Amazon | Macbook | Yes | 1200
1003 | Amazon | Macbook | Yes | 1200
1004 | Amazon | Macbook | No | 1200
1005 | Amazon | Macbook | No | 1200
1006 | Best Buy | Macbook | Yes  | 1000
1007 | Best Buy | Macbook | No | 1000
1008 | Best Buy | Macbook | No | 1000
1009 | Best Buy | Macbook | No | 1000
1010 | Best Buy | Macbook | No | 1000


```r
transactionID <- seq(from=1001,to=1010,by=1)
marketingChannel <- c(rep("Amazon",5),rep("Best Buy",5))
product <- rep("Macbook",10)
returns <- c("Yes","Yes","Yes","No","No","Yes","No","No","No","No")
sales <- c(rep(1200,5),rep(1000,5))

(salesData <- data.frame(transactionID,marketingChannel,product,returns,sales))
```

```
##    transactionID marketingChannel product returns sales
## 1           1001           Amazon Macbook     Yes  1200
## 2           1002           Amazon Macbook     Yes  1200
## 3           1003           Amazon Macbook     Yes  1200
## 4           1004           Amazon Macbook      No  1200
## 5           1005           Amazon Macbook      No  1200
## 6           1006         Best Buy Macbook     Yes  1000
## 7           1007         Best Buy Macbook      No  1000
## 8           1008         Best Buy Macbook      No  1000
## 9           1009         Best Buy Macbook      No  1000
## 10          1010         Best Buy Macbook      No  1000
```

#### Display the data types of the dataframe
    + transactionID= factor
    + Marketing Channel= Factor
    + Product= Factor
    + Returns= Logical
    + Sales= numeric
Hint: Yes and No in returns must be transformed to TRUE FALSE
    

```r
salesData$transactionID <- factor(salesData$transactionID)

salesData$returns <- ifelse(salesData$returns=="Yes",TRUE,FALSE)
salesData$returns <- as.logical(salesData$returns)

str(salesData)
```

```
## 'data.frame':	10 obs. of  5 variables:
##  $ transactionID   : Factor w/ 10 levels "1001","1002",..: 1 2 3 4 5 6 7 8 9 10
##  $ marketingChannel: Factor w/ 2 levels "Amazon","Best Buy": 1 1 1 1 1 2 2 2 2 2
##  $ product         : Factor w/ 1 level "Macbook": 1 1 1 1 1 1 1 1 1 1
##  $ returns         : logi  TRUE TRUE TRUE FALSE FALSE TRUE ...
##  $ sales           : num  1200 1200 1200 1200 1200 1000 1000 1000 1000 1000
```

### Exercise 2
##### Change four of the Sales figues to NA without creating a new dataframe


```r
salesData[c(1,3,5,7),5] <- NA
```

##### Using is.na, change the NA values to be the mean of the sales column


```r
salesData[is.na(salesData[,5]),5] <- mean(salesData$sales,na.rm=TRUE)
```

##### Reduce the sales value for all returned items by 30%


```r
salesData$sales <- ifelse(salesData$returns==TRUE,0.7*salesData$sales,salesData$sales)
```

##### Return a table that shows the total sales by marketingChannel


```r
(totalAmazon <- sum(salesData[1:5,5]))
```

```
## [1] 4600
```

```r
(totalBestBuy <- sum(salesData[6:10,5]))
```

```
## [1] 4766.667
```
