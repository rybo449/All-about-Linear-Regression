###Question 1 Part a)
library(lasso2)
data(Prostate)
full=lm(lpsa~.,data=Prostate)
summary(full)
y<-Prostate$lpsa
x1<-Prostate$lcavol
x2<-Prostate$lweight
x3<-Prostate$age
x4<-Prostate$lbph
x5<-Prostate$svi
x6<-Prostate$lcp
x7<-Prostate$gleason
x8<-Prostate$pgg45


###Question 1 Part b)
a<-sample(1,length(Prostate$lcavol),replace = T)
constant<-matrix(a)
x1<-matrix(Prostate$lcavol)
x2<-matrix(Prostate$lweight)
x3<-matrix(Prostate$age)
x4<-matrix(Prostate$lbph)
x5<-matrix(Prostate$svi)
x6<-matrix(Prostate$lcp)
x7<-matrix(Prostate$gleason)
x8<-matrix(Prostate$pgg45)
x<-cbind(a,x1,x2,x3,x4,x5,x6,x7,x8)
y<-matrix(Prostate$lpsa)
betahat<-solve(t(x)%*%x)%*%t(x)%*%y
betahat #This gives us the coefficients of each of the features in the dataset

Vbeta<-(solve(t(x)%*%x))
stderror<-sqrt(diag(Vbeta))
stderror #This gives us the standard error of each of the coefficients in the summary and also the intercept given by a

residuals<-y-x%*%betahat
residuals
quantile(residuals)#This gives us the residuals of the data


y_pred<-x%*%betahat
y_pred<-mean(y_pred)
y_mean_squared<-y-y_pred
r_squared<-1-(sum(residuals^2)/sum(y_mean_squared^2))
r_squared #The value of R^2 which is derived is as follows


###Question 1 Part c)
full<-lm(lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45, data = Prostate)
plot(full, which = 1:2)



##Question 1 Part d)
library(MASS)
full<-lm(lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45, data = Prostate)
partd<-stepAIC(full, direction = "both", scope = list(upper = full, lower = .~1))
fit1<-eval(partd)
summary(fit1)


##Question 1 Part e)
parte<-stepAIC(full,direction = "both", scope = list(upper =~(lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45)^2,lower = .~1))
fit2<-eval(parte)
summary(fit2)



#####################Question 2###################################
data(transact)
library(alr3)
install.packages("alr3")
data(transact)
transact
T1<-transact$T1
T2<-transact$T2
time<-transact$Time
A<-(T1+T2)/2
D<-T1-T2
M1<-lm(time~T1+T2)
M2<-lm(time~A+D)
M3<-lm(time~T2+D)
M4<-lm(time~T1+T2+A+D)


summary(M1)
summary(M2)
summary(M3)
summary(M4)



########################Question3#################################

##Question 3 Part a)

x1<-seq(1,100)
x2<-sample(0:1,100,replace=T)
intercept = 0
x1_s = 0
x2_s = 0
for (i in 1:1000){
  error<-rt(100,2)
  y<-3 + 0.1*x1 + 0.5*x2 + error
  fit<-lm(y~x1+x2)
  #summary(fit)
  a<-confint(fit, level = 0.68)
  if ((a[1]<3) & (a[4]>3)){ 
    intercept = intercept + 1
  }
  if ((a[2]<0.1) & (a[5]>0.1)){ 
    x1_s = x1_s + 1
  }
  if ((a[3]<0.5) & (a[6]>0.5)){
    x2_s = x1_s + 1
  }
}

intercept_f = intercept/1000
x1_f = x1_s/1000
x2_f = x2_s/1000

intercept_f
x1_f
x2_f

##Question 3 Part b)
install.packages("hett")
library(effects)
library(lme4)
library(hett)
library(arm)
x1<-seq(1,100)
x2<-sample(0:1,100,replace=T)

intercept = 0
x1_s = 0
x2_s = 0
summary(fit)$coefficients[,2]
for (i in 1:1000){
  error<-rt(100,2)
  y<-3 + 0.1*x1 + 0.5*x2 + error
  fit<-tlm(y~x1+x2)
  #summary(fit)
  a<-effect(fit, level = 0.68)
  if ((a[1]<3) & (a[4]>3)){ 
    intercept = intercept + 1
  }
  if ((a[2]<0.1) & (a[5]>0.1)){ 
    x1_s = x1_s + 1
  }
  if ((a[3]<0.5) & (a[6]>0.5)){
    x2_s = x1_s + 1
  }
}

intercept_f = intercept/1000
x1_f = x1_s/1000
x2_f = x2_s/1000

intercept_f
x1_f
x2_f

