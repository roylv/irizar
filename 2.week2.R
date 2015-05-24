# SE of an estimate = SD of the sampling distribution of an estimate = SD of the mean of samples from the population distribution

# = SD of betahat estimations for repeated runs
# LSE (least squares estimates )
# y = b1 + b1t +b2t^2 + e (g=-2b2)
g=9.8
n=25
tt=seq(0,3.4,len=n)
f=56.67 + 0*tt - 0.5*g*tt^2
y = f+rnorm(n, sd=1)

X=cbind(1,tt,tt^2)
A = solve(crossprod(X))%*%t(X)  # = ((Xt*X)-1)*Xt (crossprod(X) = t(X)%*%X)
gE = -2*(A%*%y)[3] # an estimate of g


yr=replicate(100000, f+rnorm(n, sd=1))
gEn=-2*(A%*%yr)[3]
sd(gEn)


betahat = replicate(B,{

y = 56.67 - 0.5*g*tt^2 + rnorm(n,sd=1)

betahats = -2*A%*%y

return(betahats[3])

})

sqrt(mean( (betahat-mean(betahat) )^2))

#Question 2.1.3
library(UsingR)

x = father.son$fheight

y = father.son$sheight

n = length(y)

N = 50

set.seed(1)
index = sample(n,N)

sampledat = father.son[index,]

x = sampledat$fheight

y = sampledat$sheight

betahat = lm(y~x)$coef

B=10000
betahat1=replicate(B,{index = sample(n,N)

sampledat = father.son[index,]

x = sampledat$fheight

y = sampledat$sheight

betahats = lm(y~x)$coef

return(betahats[[2]])
})

#covariance:
# The covariance of two lists of numbers X=X1,...,Xn and Y=Y1,...,Yn is mean( (Y - mean(Y))*(X-mean(X) ) )

x = father.son$fheight

y = father.son$sheight

cov=mean((x-mean(x))*(y-mean(y)))

#Question 2.2.1

SE(betahat) = sqrt(var(betahat))

var(betahat) = sigma^2 (X^T X)^-1

fit = lm(y ~ x)

y_hat=fit$fitted.values
SSR=sum((y-y_hat)^2)) #sum of squared residuals (SSR)

sigma2=SSR/(N-p) #p=2, N=50; sample and terms (intercept and slope)
sigma2=SSR/48

#Question 2.2.2
X=cbind(1,x) #design matrix X
solve(t(X)%*%X)

dia=diag(solve(t(X)%*%X))

sigma2*dia #estimated variance of beta-hat
sqrt(sigma2*dia) #SE of beta-hat: intercept and slope


#builidng design matrices
x=c(1,1,2,2)
f=formula(~x)
model.matrix(~factor(x)) #0's and 1's: indiccator variable

x = factor(c(1,1,2,2,3,3))
model.matrix(~x)

model.matrix(~x,contrasts=list(x="contr.sum")) #contrast matrix

x=factor(c(1,1,1,1,2,2,2,2))
y=factor(c("a","a","b","b","a","a","b","b"))
model.matrix(~x+y)

model.matrix(~x+y+x:y)
model.matrix(~x*y)

#effects of level

x=relevel(x,"2") #swaps the levels
levels(x)
model.matrix(~x)

x=factor(x, levels=c("1","2"))
z=1:4
model.matrix(~z)
model.matrix(~0 + z)
model.matrix(~z + I(z^2))

dat=read.csv("femaleMiceWeights.csv")
stripchart(dat$Bodyweight ~  dat$Diet, vertical=T, method="jitter",main="Bodyweight over diet")
#a linear model with one variable
levels(dat$Diet)
X=model.matrix(~Diet,data=dat)
X
colnames(X)
dat$Diet=relevel(dat$Diet, ref="hf")
model.matrix(~Diet,data=dat)
dat$Diet=relevel(dat$Diet,ref="chow")

#running the linear model
fit=lm(Bodyweight~Diet, data=dat)
summary(fit)
(coefs=coef(fit))
#The math behind lm()
$$betahat=(X^t X)^(-1) X^t y $$

y=dat$Bodyweight
X=model.matrix(~Diet, data=dat)
solve(t(X)%*%X)%*%t(X)%*%y

#
s=split(dat$Bodyweight, dat$Diet)
mean(s[["chow"]])
mean(s[["hf"]])-mean(s[["chow"]])



stripchart(dat$Bodyweight ~  dat$Diet, vertical=T, method="jitter",main="Bodyweight over Diet",ylim=c(0,40), xlim=c(0,3))

a=-0.25
lgth=.1
library(RColorBrewer)
cols=brewer.pal(3,"Dark2")
abline(h=0)
arrows(1+a,0,1+a,coefs[1],lwd=3,col=cols[1],length=lgth)
abline(h=coefs[1],col=cols[1])
arrows(2+a,coefs[1],2+a,coefs[1]+coefs[2],lwd=3,col=cols[2], length=lgth)
abline(h=coefs[1]+coefs[2],col=cols[2])
legend("right",names(coefs),fill=cols,cex=.75,bg="white")

#comparing simple two group to a t-test
summary(fit)$coefficients
(ttest=t.test(s[["chow"]],s[["hf"]],var.equal=T))
summary(fit)$coefficients[2,3]
ttest$statistic
t.test(s[["hf"]],s[["chow"]],var.equal=T)$statistic

#HW
nx=5
ny=7
X=cbind(rep(1,nx+ny),rep(c(0,1),c(nx,ny)))
(t(X)%*%X)[1,1]

