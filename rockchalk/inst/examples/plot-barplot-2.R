## Paul Johnson
## barplot data input. What a Hassle.
## 2011-06-22

## Here's a case where it is best to understand what R wants,
## before trying to work your example.  Or end goal is to
## make a grouped barplot.

## Start easy, give barplot one column


x <- c(.14, .23, .66)
barplot(x)


## table or aggregate can produce same kind of thing
## Get some small integers in a data frame
rawdata <- rpois(200, lambda=2)

x <- table(rawdata)

## Convert to proportions

x <- x / sum(x)

barplot(x)


## Now work on richer information

## Suppose the input is a matrix with 2 columns

x <- matrix( c(.14, .23, .66, .44, .53, .55), ncol=2)

## look at x

x

barplot(x)

## I hate stacked charts

## I think this would be called a grouped bar plot.
barplot(x, beside=T)

## That has no names because my input table had no names.

## Think of the columns as sex 
colnames(x) <- c("Male","Female")
x

## Row represents cities
rownames(x) <- c("NY","LA","SF")
x


barplot(x, beside=T)

## How to decorate that?
## Name individual bars? OK:
barplot(x, beside=T, names.arg=c("A","B","C","D","E","F"))

## Instead, lets go for two-layered output.
## Grab the output from barplot in order
## to see where bars are positioned.  
bp1 <- barplot(x, beside=T)

mtext(text = c("first","second","third"), side=1, line=0, at= bp1[,1])

mtext(text = c("fourth","fifth","sixth"), side=1, line=0, at=bp1[ ,2])

## Instead, lets write vertically inside the bars!
## Let's write at one-half of the column's height (that's why
## I have 0.5*x in the text commands

bp1 <- barplot(x, beside=T)
text( bp1[ ,1], 0.5*x[ ,1], c("first","second","third"))

## srt will rotate text strings by degree
bp1 <- barplot(x, beside=T)
text( bp1[ ,1], 0.5*x[ ,1], c("first","second","third"), srt=66)


bp1 <- barplot(x, beside=T)
text( bp1[ ,1], 0.5*x[ ,1], c("first","second","third"), srt=90)
text( bp1[ ,2], 0.5*x[ ,2], c("first","second","third"), srt=90)


### Note problem: Fill colors in legend not correct
bp1 <- barplot(x, beside=T)
legend("topleft", legend=c("first","second","third"), fill=c(1,2,3))

### Need to figure out what colors barplot uses
### I believe it is drawing colors from the function "gray.colors"
gray.colors(3)

bp1 <- barplot(x, beside=T)
legend("topleft", legend=c("first","second","third"), fill= gray.colors(3))



### So, what do you get out of this?

### barplot wants you to give it a matrix, one column per group of bars.

### So if your data is like this


### data
### id  sex region iq age
### 01  M    W     122 12
### 02  F    W     111 08
### 03  M    E     89  07
### 04  F    S     144 19
### 05  F    N     123 44

### You want a barplot that shows this the
## mean "iq" subdivided by sex, then region

##          | |               |
##        | | |               | 
##      | | | |               | | | |
##      E W N S               E W N S

##         Male                 Female

## So we need a matrix with 2 columns, Male and Female,
## Rows for regions and cells are means.

## First, manufacture the data

id <- 1:1000
sex <- sample(x= c("M","F"), size=1000, replace=T)
region <- sample(x= c("E","W","N","S"), size=1000, replace=T)
iq <- rnorm(1000, m=100, sd=15)
age <- rpois(1000, lambda=20)
dat <- data.frame(id, sex, region, iq, age)

## Use R's "aggregate" go produce that
aggdat <- aggregate(dat$iq, by=list(sex=sex,region=region), FUN= mean)
aggdat
colnames(aggdat)[3] = "meaniq"

## aggdat is in the "long" format, but we need the "wide" format
x <- unstack(aggdat, meaniq ~ sex )
x <- as.matrix(x)
barplot(x, beside=T)


## And I think that's all I need to show
