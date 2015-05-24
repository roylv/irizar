dat = read.csv("femaleMiceWeights.csv")

mean(dat[13:24,2]) - mean(dat[1:12,2])

s = split(dat[,2], dat[,1])
stripchart(s, vertical=TRUE, col=1:2)

abline(h=sapply(s, mean), col=1:2)

sum(dat[13:24,2] < mean(dat[1:12,2]))

#count logical values as "0" and "1"s:

sum(sort(highfat)>30)

#...proportion of 1s:
mean(sort(highfat)>30)

ctr = read.csv("femaleControlsPopulation.csv")
n=10000
for(i in 1:n){
  control = sample(ctr[,1],12)
  treatment = sample(ctr[,1],12)
  null[i] = mean(treatment) - mean(control)
}

#calculate 10000 times mean of a sample from a population
sampleMean = replicate(10000, mean(sample(ctr[,1], 12)))

#calculate the D between random means:
null = replicate(10000, mean(sample(ctr[,1], 12)) - mean(sample(ctr[,1], 12)))

#Illustration of the null distribution

#Let's repeat the loop above but this time let's add a point to the figure every time we re-run the experiment

n <- 100
plot(0,0,xlim=c(-5,5),ylim=c(1,30),type="n")
totals <- vector("numeric",11)
for(i in 1:n){
  control <- sample(ctr[,1],12)
  treatment <- sample(ctr[,1],12)
  nulldiff <- mean(treatment) - mean(control)

#What is the two-tailed probability of seeing as big a difference as we observed, calculated from your null distribution?

mean(abs(null) > abs(diff))
  j <- pmax(pmin(round(nulldiff)+6,11),1)
  totals[j]<-totals[j]+1
  text(j-6,totals[j],pch=15,round(nulldiff,1))
  ##if(i < 15) scan() ##You can add this line to interactively see values appear
  }

#Distributions
values <- seq(min(null),max(null),len=300)
myecdf <- ecdf(null)
plot(values,myecdf(values),type="l")





#loop for proportion
prop = function(q) {
  mean(x <= q)
}

#create an array
qs = seq(from=min(x), to=max(x), length=20)

#all in 1 line
props = sapply(qs, function(q) mean(x <= q))

#with ecdf
plot(ecdf(x))


#standardize (z is log10 of pop in 1952 for gapminder
z = (z-mean(z))/sd(z)

#function which gives back the Normal distribution cumulative density function for a Normal with the same mean and standard deviation as the vector x
F = function(q) pnorm(q, mean=mean(x), sd=sd(x))



