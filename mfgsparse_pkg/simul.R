# install the package
# install.packages("https://github.com/CodeJSong/Multivariate-Functional-Sparse-Model/raw/main/mfgsparse_pkg/gglassomod.tar.gz",  repos = NULL, type="source")
#load package
library(gglassomod)
#load function
source("mfsg.R")
#load sim data 

# The model:
# total 19 functional predictors
# beta_1(t),..., beta_3(t) are nonzero, the others are zero



source("sim0.R")

# plot X^1, ..., X^9   (out of total p=19)
par(mfrow=c(3,3))
for(j in 1:9){
  plot(tt,Xtrain[j,1,],type='l', ylim=c(-30,30), main=paste0("j=",j))
  for(k in 2:10)lines(tt, Xtrain[j,k,])
}

par(mfrow=c(1,1))


# run
m=11 #basisino
part=rep(m,p)

# green: true beta  (only beta1,beta2,beta3 are the nonzero functions)
# black: estimated beta
ols=MFSGrp(Ytrain,Xtrain,basisno=m,tt, part=part,Xpred=Xtest,Ypred=Ytest, Penalty = "glasso" , bspline=T, sixplotnum="max" , lamdermax=1e-4, lamdermin = 1e-7)
ols$MSEpredict  # test MSE
sum(ols$coef==0)/m    # number of zero functional coefficients.
ols$lambda



ols=MFSGrp(Ytrain,Xtrain,basisno=m,tt, part=part,Xpred=Xtest,Ypred=Ytest, Penalty = "gelast" , bspline=T, forcezero=T, nlamder=15, nalpha=5, nfolder=4)
sum(ols$coef==0)/m
ols$MSEpredict
ols$lambda

#
system.time(MFSGrp(Ytrain,Xtrain,basisno=m,tt, part=part,Xpred=Xtest,Ypred=Ytest, Penalty = "gelast" , bspline=T, sixplotnum="max", lambdaderivative =0.0002154435 , nalpha=15  ))




ols=MFSGrp(Ytrain,Xtrain,basisno=m,tt, part=part,Xpred=Xtest,Ypred=Ytest, Penalty = "gelast" , bspline=T, sixplotnum="max")
sum(ols$coef==0)/m
ols$MSEpredict
ols$lambda



ols=MFSGrp(Ytrain,Xtrain,basisno=m,tt, part=part,Xpred=Xtest,Ypred=Ytest, Penalty = "gelast" , bspline=F, nlamder=15, nalpha=5, nfolder=4 )
sum(ols$coef==0)/m
ols$MSEpredict
ols$lambda


#oracel
ols=MFSGrp(Y=Ytrain,X=Xtrain[1:3,,],basisno=m,tt, part=part,Xpred=Xtest[1:3,,],Ypred=Ytest, Penalty ="OLS", bspline = T  , sixplotnum = "max" , lambdaderivative = NULL, lamdermax = 1e-2, lamdermin=1e-10, nlamder=20)
ols$MSEpredict




#simple regression
ols=MFSGrp(Ytrain,X=Xtrain,basisno=m,tt, part=part,Xpred=Xtest,Ypred=Ytest, Penalty ="OLS", bspline = T, sixplotnum = 2, nlamder = 20)
ols$MSEpredict
sum(ols$coef==0)/m








