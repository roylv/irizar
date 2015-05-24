#downloader
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- tempfile()
download(url,destfile=filename)
dat <- read.csv(filename)
head(dat)

#Sample
Ns = c(3,5,10,25)
B = 10000

res = sapply(Ns, function(n){
  sapply(1:B, function(j){
    mean(sample(hf,n))-mean(sample(chow,n))
  })
 })

#Plot
par(mfrow=c(2,2))  
for(i in seq(along=Ns)){
  title = paste("Avg=", signif(mean(res[,i]),3))
  title = paste(title, "SD=", signif(sd(res[,i]),3))
  qqnorm(res[,i], main=title)
  qqline(res[,i])
}


#Suppose we obtain two samples, each of size N, from non-smoking mothers (dat.ns) and smoking mothers (dat.s). Following lecture, we compute the t-value, which we call tval.

X.ns = mean(dat.ns)
sd.ns = sd(dat.ns)
X.s = mean(dat.s)
sd.s = sd(dat.s)
sd.diff = sqrt(sd.ns^2/N+sd.s^2/N)
tval = (X.ns - X.s)/sd.diff

#This t-value, or t-statistic, is also returned by 

t.test(dat.ns, dat.s)$statistic

pval = 1-(pnorm(abs(tval))-pnorm(-abs(tval)))


#Computing t.stat
se =  sqrt(var(ctr)/12+var(trt)/12)
diff = mean(ctr)-mean(trt)
tstat = diff/se

#1 - the prob that a norm rand is bigger than tstat 
1-pnorm(tstat)

#2 sided:

1-pnorm(tstat)+pnorm(-tstat)



mu_chow <- mean(chowPopulation)
N <- 30
hf <- sample(chowPopulation,N)
se <- sd(hf)/sqrt(N)


#HERE WE ASSUME CLT (NORMAL DIST) WHICH IS NOT ALWAYS CORRECT!!!
Q = qnorm(1-0.05/2)

#to correct for t dist (N-1 = degrees of freedom:
Q = qt(1-0.05/2, N-1)

#hence, 95% of the RVs fall in this interval:
-Q< (mean(pop) - mean(y))/se <Q

#because we don't know mean(pop):
mean(y)-Q*se< (mean(pop))/se < mean(y)+Q*se

#now our interval is:
interval=c(mean(y)-Q*se, mean(y)+Q*se)

plot(mu+c(-7,7), c(1,1), type="n", xlab="weights", ylab="intervals", ylim=c(1,1000))
abline(v=mean(pop))

lines(interval,c(1,1))

#outcome of 100 CIs
for(i in 2:100){
  
  #mu_chow <- mean(chowPopulation)
  #N <- 30
  y <- sample(pop,N)
  se = sd(y)/sqrt(N)
  interval=c(mean(y)-Q*se, mean(y)+Q*se)
  color=ifelse(interval[1]<=mean(pop) & 
		 interval[2]>=mean(pop),1,2)
  lines(interval,c(i,i), col=color)
} 
#POWER CALCULATIONS
setwd("irizar")
dat=read.csv("mice_pheno.csv")
hfpop=fat[dat$Sex=="F" & datDiet=="hf",3]
hfpop=fat[dat$Sex=="F" & dat$Diet=="hf",3]
hfpop=dat[dat$Sex=="F" & dat$Diet=="hf",3]
chowpop=dat[dat$Sex=="F" & dat$Diet=="chow",3]
N=5
hf=sample(hfpop,N)
chow=sample(chowpop,N)
t.test(hf,chow)

#Power for N=12
N=12
alpha=0.05
B=10000
Ns=seq(5,50,5)
power=sapply(Ns, function(N){
  
  rejections = sapply(1:B, function(i){
    hf=sample(hfpop,N)
    chow=sample(chowpop,N)
    t.test(hf,chow)$p.value<alpha
  })
return(mean(rejections))
})

plot(Ns, power)

#Assesment
B=1000
alphas=c(0.1,0.05,0.01)
power = sapply(alphas, function(alpha){
  rejections=sapply(1:B, function(i){
    s=sample(bwt.smoke,15)
    ns=sample(bwt.nonsmoke,15)
    t.test(s,ns)$p.value<alpha
  })
return(mean(rejections))
})

#Association tests
tab=matrix(c(180,40,20,10),2,2)
rownames(tab)=c("AA or Aa","aa")
colnames(tab)=c("Controls","Cases")
prop.table(tab)
prop.table(tab,1)
ctest=chisq.test(tab)



#MONTE CARLO
#Pseudo random data
set.seed(0)
N=50
smokers=sample(dat$bwt[dat$smoke==1],N)
nonsmokers=sample(dat$bwt[dat$smoke==0],N)
obs=mean(smokers)-mean(nonsmokers)

avgdiff = sapply(1:1000, function(i){
  all=sample(c(smokers,nonsmokers))
  smokersstar=all[1:N]
  nonsmokersstar=all[(N+1):(2*N)]



set.seed(1)
smokers=sample(dat$bwt[dat$smoke==1],10)
nonsmokers=sample(dat$bwt[dat$smoke==0],10)
cat("observd difference = ", mean(smokers)-mean(nonsmokers), "ounces")
for(i in 1:10){
  smokers=sample(dat$bwt[dat$smoke==1],10)
  nonsmokers=sample(dat$bwt[dat$smoke==0],10)
  cat("observd difference = ", mean(smokers)-mean(nonsmokers), "ounces\n")

}


ttestgenerator=function(b=1000, n=10){
  sapply(1:b, function(i){
    #sample 2 times from nonsmokers
     smokers=sample(dat$bwt[dat$smoke==0],10)
     nonsmokers=sample(dat$bwt[dat$smoke==0],10)
     return((mean(smokers)-mean(nonsmokers))/sqrt((var(smokers))/n+var(nonsmokers)/n))
  })
}
ttests = ttestgenerator(1000,10)

hist(ttests)

#check if the t-dist is a good approximation; a qqplot against th t-dist:
qs = (seq(0,999)+0.5)/1000
qqplot(qt(qs,df=2*3-2),ttests,xlim=c(-6,6),ylim=c(-6,6))
abline(0,1)

#generate pseudorandom normally distributed nubers
nonsmokerweights=rnorm(25000,123,17)

#HW
vars=replicate(1000, var(sample(bwt.nonsmoke,10)))

#PERMUTATIONS
set.seed(0)
N <- 50
smokers <- sample(dat$bwt[dat$smoke==1],N)
nonsmokers <- sample(dat$bwt[dat$smoke==0],N)
obs <- mean(smokers)-mean(nonsmokers)


avgdiff <- replicate(1000, {
    all <- sample(c(smokers,nonsmokers))
    smokersstar <- all[1:N]
    nonsmokersstar <- all[(N+1):(2*N)]
  return(median(smokersstar) - median(nonsmokersstar))
})
hist(avgdiff)
abline(v=obs)
