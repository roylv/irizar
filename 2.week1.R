#simulate object fall
set.seed(1)
g <- 9.8 ## meters per second
n <- 25
tt <- seq(0,3.4,len=n) ##time in secs, t is a base function
d <- 56.67  - 0.5*g*tt^2 + rnorm(n,sd=1)

plot(tt,d,ylab="Distance in meters",xlab="Time in seconds")

#matrix operations
x=matrix(1:12,4,3)
a=2
a*x

x=matrix(c(1,3,2,1,-2,1,1,1,-1),3,3)
beta=c(3,2,1)
x%*%beta

diag(5)

#the Inverse
#SOME square matrices have inverse: X-1*X=I
x=matrix(c(1,3,2,1,-2,1,1,1,-1),3,3)
y=matrix(c(6,2,1),3,1)
solve(x)%*%y

#1.3.2
x=t(matrix(c(3,4,-5,1,2,2,2,-1,1,-1,5,-5,5,0,0,1),4,4))
y=matrix(c(10,5,7,4),4,1)
solve(x)%*%y


#linear models as matrix multiplications 1
y = rnorm(1e6)
x = cbind(rep(1,1e6),rep(0:1,each=5e5))
beta = c(1,1)
sum((y-x%*%beta)^2)
system.time({sum((y-x%*%beta)^2)})  ##==(Y-bX)T(Y-bX)

#average
y=df2$BVTV
mean(y)

N=length(y)
Y=matrix(y,N,1)
A=matrix(1,N,1)
barY=t(A)%*%Y/N
barY=crossprod(A,Y)/N
barY

#Variance
r=y-barY
crossprod(r)/N

#...which is equivalent to:
var(y)*(N-1)/N

#Minimizing RSS

library(UsingR, lib.loc="/home/roeilv/R")
x=father.son$fheight
y=father.son$sheight
X=cbind(1,x)
betahat = solve(t(X)%*%X)%*%t(X)%*%y

##or
bethat = solve(crossprod(X))%*%crossprod(X,y)

#matrix algebra
g=9.8
n=25
tt=seq(0,3.4,len=n)
f=56.67 + 0*tt - 0.5*g*tt^2
y = f+rnorm(n, sd=1)

plot(tt,y,xlab="Time in secs", ylab="Distance in meters")

#find now the three variables that make the above parabola:
rss=function(beta0,beta1,beta2) {
  r = y - (beta0+beta1*tt+beta2*tt^2)
  sum(r^2)
}

#find whatr minimizes the Betas?
beta2s=seq(-10,0,len=100)
RSS = sapply(beta2s,rss,beta0=55,beta1=0)
plot(beta2s,RSS,type="l")

#with "lm"
tt2=tt^2
fit=lm(y~tt+tt2)
summary(fit)

X=cbind(1,tt,tt^2)
beta=matrix(c(55,0,5),3,1)
#residuals:
r=y-X%*%beta
#RSS
RSS=t(r)%*%r

#inverse is obtained with "solve"; similar results to "fit"
betahat=solve(crossprod(X))%*%t(X)%*%y
#QR decomposition instead of "solve"
QR = qr(X)
Q=qr.Q(QR)
R = qr.R(QR)
backsolve(R,crossprod(Q,y))

#HW 
X <- matrix(c(1,1,1,1,0,0,1,1),nrow=4)
rownames(X) <- c("a","a","b","b")
beta <- c(5, 2)
