library(UsingR, lib.loc="/home/roeilv/R")

#Hostograms

x=father.son$fheight
round(sample(x,20),1)
hist(x, breaks=seq(floor(min(x)),ceiling(max(x))), main="Height histogram", xlab = "Height in inches")

#percent of any individual that is below a threshold

xs = seq(floor(min(x)), ceiling(max(x)),0.1)
plot(xs, ecdf(x)(xs), type="l", xlab="Height in inches", ylab="F(x)")

#compute prop of people taller than 70 inches:

mean(x>70)
#...using the normal approximation:
1-pnorm(70,mean(x),sd(x))

#...and less than 70:
pnorm(70,mean(x),sd(x))

#QQ plot
ps=seq(0.01,0.99,0.01)
#compute what are the percentiles:
qs = quantile(x,ps)
#compute the same percentiles for the normal dist.:
normalqs = qnorm(ps,mean(x),sd(x))
plot(normalqs,qs,xlab="Normal percentiles",ylab="Height")
abline(0,1)

#question 2.1
load("skew.RData")

par(mfrow = c(3,3))
for (i in 1:9) {
  qqnorm(dat[,i],main=i)
}

par(mfrow = c(2,2))
hist(dat[,4],main="4")
hist(dat[,9], main="9")
qqnorm(dat[,4],main="4")
qqnorm(dat[,9],main="9")

#salaries is a data which is NOT normally dist.:
hist(exec.pay) #mean and SD do not give a good idea of the dist. of the data
head(InsectSprays)
a=factor(InsectSprays[,2])
boxplot(split(InsectSprays[,1],a))
#OR
boxplot(InsectSprays[,1]~a )


#Correlation: to compute, we satndardize the uiits and multiply, to take the mean.
signif(cor(x,y),2)
#Data stratification
boxplot(split(y,round(x)))
mean(y[round(x)==72])
#---------------------
#standartization
x=(x-mean(x))/sd(x)
y=(y-mean(y))/sd(y)
means=tapply(y,round(x*4)/4,mean)
#extract fathers height
fatherheights=as.numeric(names(means))
plot(fatherheights,means,ylab="average of strata of son heights",ylim=range(fatherheights))
abline(0,cor(x,y)) #correlation is the slope of the line needed to derive x from y

#dplyr
library(dplyr, lib.loc="/home/roeilv/R/")
library(downloader, lib.loc="/home/roeilv/R/")
library(lazyeval, lib.loc="R/")
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/msleep_ggplot2.csv"
filename <- "msleep_ggplot2.csv"
if (!file.exists(filename)) download(url,filename)
msleep <- read.csv("msleep_ggplot2.csv")
head(msleep)
sleepData=select(msleep,name,sleep_total)
#select all except ("-") column
select(msleep, -name)
#select by ":"
head(select(msleep, name:order))
#selet those that start with "x":
head(select(msleep, starts_with("sl")))
#FILTER
filter(msleep, sleep_total>=16, bodywt>=1)

#pipe operator - pipe the OUTPUT of one operation into the INPUT of another
msleep %>%
    select(name,sleep_total) %>%
    head
#arrange
#pipe msleep under "arrange"
msleep%>%arrange(order)%>%head
#select three columns, arrange by order and sleep
msleep%>%select(name,order,sleep_total)%>%arrange(order,sleep_total)%>%head

#filter: sleep more than 16h
msleep%>%select(name,order,sleep_total)%>%arrange(order,sleep_total)%>%filter(sleep_total>=16)

#arrange by descending order
msleep%>%select(name,order,sleep_total)%>%arrange(order,desc(sleep_total))%>%filter(sleep_total>=16)

#mutate: add columns to a DF
msleep%>%mutate(rem_prop=sleep_rem/sleep_total, bodywt_grams=bodywt*1000)%>%head

#summary statistis
msleep%>%summarize(avg_sleep=mean(sleep_total))

msleep%>%summarize(avgSlp=mean(sleep_total),minSlp=min(sleep_total),maxSlp=max(sleep_total),total=n())

#group-by

msleep%>%group_by(order)%>%summarize(avgSlp=mean(sleep_total),minSlp=min(sleep_total),maxSlp=max(sleep_total),total=n())

#HW
#Using dplyr and the pipe command %>%, and perform the following steps:

#Add a column of the proportion of REM sleep to total sleep

#Group the animals by their taxonomic order

#Summarise by the median REM proportion

#Arrange by the median REM proportion

#Take the head() of this to see just the orders with smallest median REM proportion

#What is the median REM proportion of the order with the smallest median REM proportion?

msleep%>%mutate(rem_prop=sleep_rem/sleep_total)%>%group_by(order)%>%summarize(remMED=median(rem_prop))%>%arrange(remMED)%>%head


#robust summary stats
#Median, MAD, and Spearman Correlation 
#MAD - robust estimate of sd (median absolute deviation)

mad(x)
#Spearman correlation - computed by ranks
ds1=rnorm(100,sd=1,mean=0)
ds2=rnorm(100,sd=1,mean=0)
#rank
r1=rank(ds1)
r2=rank(ds2)
#...then correlate
cor(r1,r2)
#or:
cor(ds1,ds2,method="spearman")

#HW
data(ChickWeight)
plot(ChickWeight$Time, ChickWeight$weight, col=ChickWeight$Diet)

#reshape - from long to wide
chick = reshape(ChickWeight,idvar=c("Chick","Diet"),timevar="Time",direction="wide")

chick = na.omit(chick)

#Mann-Whitney-Wilcoxon Test

#HW
#stripchart
stripchart(chick$weight.4 ~ chick$Diet, method="jitter", vertical=TRUE)

# for small sample sizes, the p-value can't be very small, even when the difference is very large. Compare:
wilcox.test(c(1,2,3),c(4,5,6))

wilcox.test(c(1,2,3),c(400,500,600))

