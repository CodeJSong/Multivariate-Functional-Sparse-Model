#install.packages("matlib")
library(fda)
#library(matlib)
# Generate k=50 browinain motion (different mean), with p=100 point observations
# but take sample of n=1000 of these curves
# inastead of time in (0,1) we take time between (0,100)
p=19 # number of functional predictor for each observation
n=200# sample size
nt=500 # number of recorded time points for each functional covariate.


#set.seed(123)
#practice
someData <- rep(NaN, p*n*nt);
X= array(NaN, c(p, n, nt));
for(j in 1:p){
  for (i in 1:n){
    X[j,i,]=cumsum(rnorm(nt,0,p-j+1))/sqrt(nt)}
}




#true beta 1 and beta 2

beta1 = function(t){
  return(sin(3*pi*t/2))}
beta2 = function(t){
  return(sin(5*pi*t/2))}
beta3=function(t){
  return(t^2)}
b1=matrix(0, ncol=1, nrow=nt)
b2=matrix(0, ncol=1, nrow=nt)
b3=matrix(0, ncol=1, nrow=nt)


#evaulate true beta 1 and beta 2 on (0,1) , basically on each of p=1000 observation point

for(i in 0:nt){
  j=i
  b1[i]=beta1(j/nt)
  b2[i]=beta2(j/nt)
  b3[i]=beta3(j/nt)
}



#inner product of X and beta 1 and 2 and constructing Y
# Y=beta1.X+beta2.x+ beta.3.X+epsilon
Y=matrix(0, ncol=n, nrow=1)

eps=matrix(0, ncol=n, nrow=1)
for(n in 1:n){
  eps[, n]=rnorm(1,0,0.05)
}

Xb1=matrix(0, ncol=n, nrow=1)
Xb2=matrix(0, ncol=n, nrow=1)
Xb3=matrix(0, ncol=n, nrow=1)

for(j in 1:n){
  Xb1[j]=(X[1,j,]%*%b1)/nt
  Xb2[j]=(X[2,j,]%*%b2)/nt
  Xb3[j]=(X[3,j,]%*%b3)/nt
}

Y=Xb1+Xb2+Xb3+eps

# make a matrix
X.obs = X[,,(1:100)*nt/100, drop=F]
tt=(1:100)/100




trainIndex=sample(1:n, size = round(0.5*n), replace=FALSE)
Ytrain=Y[, trainIndex, drop = FALSE ]
Ytest=Y[, -trainIndex, drop = FALSE ]
Xtrain=X.obs[,trainIndex,, drop=F]
Xtest=X.obs[,-trainIndex,, drop=F]



