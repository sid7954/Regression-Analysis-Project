getwd();
setwd("C:/Users/Sunidhi Garg/Desktop/SI422 Project");
data=read.table("Data6Team5.txt",header=T)
data
attach(data);

######Box Plots#######
boxplot(y,horizontal = TRUE);
boxplot(x1,horizontal = TRUE);
boxplot(x2,horizontal = TRUE);
boxplot(x3,horizontal = TRUE);
boxplot(x4,horizontal = TRUE);
boxplot(x5,horizontal = TRUE);
boxplot(x6,horizontal = TRUE);
boxplot(x7,horizontal = TRUE);
boxplot(x8,horizontal = TRUE);
boxplot(x9,horizontal = TRUE);
boxplot(x10,horizontal = TRUE);
boxplot(x11,horizontal = TRUE);
pairs(~y+x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11);
pairs(~y+x1+x2+x3);
pairs(~y+x4+x5+x6+x7);
pairs(~y+x8+x9+x10+x11);
pairs(~y+x1+x7+x10);

######Normality Tests#######
shapiro.test(log(x1));
shapiro.test(log(y));
qqnorm(log(y));
qqline(log(y));
a=rnorm(500,mean=20,sd=5);
qqnorm(a);
qqline(a);
qqnorm((x1));
qqline((x1));
qqnorm(log(x7));
qqline(log(x7));
qqnorm(log(x10));
qqline(log(x10));

######QQ Plot of residuals#######
mod=lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11);
resid1=resid(mod);
e=data.frame(resid1);
colnames(e)<-c("Residual");
attach(e);
qqnorm(Residual);
qqline(Residual);

mod=lm(log(y)~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11);
resid1=resid(mod);
e=data.frame(resid1);
colnames(e)<-c("Residual");
attach(e);
qqnorm(Residual);
qqline(Residual);

y=log(y);

########Outlier Detection##########
X=matrix (c(rep(1,nrow(data)),x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11),nrow=nrow(data),ncol=12);
H = X%*%solve(t(X)%*%X)%*%t(X);
e=resid(mod)
MSE=tail(anova(mod)[,3],1);
h=diag(H);
semistud <- NULL;
stud <-NULL;
semistud[1:nrow(data)]=0;
stud[1:nrow(data)]=0;
for (i in (1:nrow(data))){
  semistud[i]=e[i]/(sqrt(MSE))
  stud[i]=e[i]/(sqrt(MSE*(1-h[i])))}
png('stud.png');
plot(stud);
dev.off();
stdstud=3*sd(stud);
outlier1=(abs(stud[])>stdstud)
sum(outlier1)
png('semistud.png');
plot(semistud);
dev.off();
stdsemistud=3*sd(semistud);
outlier2=(abs(semistud[])>stdsemistud)
sum(outlier2)

SSE=tail(anova(mod)[,2],1);
del <- NULL;
studdel <- NULL;
del[1:nrow(data)]=0;
studdel[1:nrow(data)]=0;
for (i in (1:nrow(data))){
  semistud[i]=e[i]*sqrt(  (nrow(data)-12-1)  /  ((1-h[i])*(SSE*(1-h[i])-e[i]*e[i]))  )
  del[i]=e[i]*sqrt(  (nrow(data)-12-1)  /  ((1-h[i])*(SSE*(1-h[i])-e[i]*e[i]))  )
  semistud[i]=e[i]/(sqrt(MSE))
  studdel[i]=e[i]*sqrt(  (nrow(data)-12-1)  /  (SSE*(1-h[i])-e[i]*e[i])  )}

png('studdel.png');
plot(studdel);
dev.off();
stdstuddel=3*sd(studdel);
outlier3=(abs(studdel[])>stdstuddel);
sum(outlier3)

png('del.png');
plot(del);
dev.off();
stddel=3*sd(del);
outlier4=(abs(del[])>stddel);
sum(outlier4)

z=(outlier1==TRUE)&(outlier2==TRUE)&(outlier3==TRUE)&(outlier4==TRUE);
sum(z)

png('leverage.png');
plot(h);
dev.off();

bound=2*12/nrow(data);
outlier5=(abs(h[])>bound);
sum(outlier5)

z=((outlier1==TRUE)&(outlier2==TRUE)&(outlier3==TRUE)&(outlier4==TRUE))|(outlier5==TRUE);
sum(z)

#######Checking for influential observations#########
dffits <-NULL;
dffits[1:nrow(data)]=0;
for (i in (1:nrow(data))){
  dffits[i] = studdel[i]*sqrt(h[i]/(1-h[i]))}
png('dffits.png');
plot(dffits);
dev.off();
bound2=2*sqrt(12/nrow(data));
influential1=(abs(dffits[])>bound2);
sum(influential1);

# c = diag(solve(t(X)%*%X));
# mse=0;
# dfbetas1 <-NULL;
# dfbetas1 [1:nrow(data),1:11]=0;
# for (i in (1:nrow(data))){
#     fit = lm(y[-i]~x1[-i]+x2[-i]+x3[-i]+x4[-i]+x5[-i]+x6[-i]+x7[-i]+x8[-i]+x9[-i]+x10[-i]+x11[-i])
#     mse <- tail(anova(fit)[,3],1)
#     for (j in (1:12)){
#     dfbetas1[i][j]=(mod$coeff[j] - fit$coeff[j])/sqrt(mse*c[j])}}
# 
# png('dfbetas1.png');
# plot(dfbetas1);
# dev.off();
# influential2=(abs(dfbetas[])>bound2);
# sum(influential2);

A=dfbeta(mod);
B=ifelse(A[,]>bound2,1,0)
C=data.frame(rowSums(B));
sum(C)

influential=(C==1)&(influential1==TRUE);
sum(influential)

detected=(influential==TRUE)&(z==TRUE);
sum(detected)

# count=0;
# newdata=matrix(0,(nrow(data)-sum(detected)),13);
# for (i in (1:nrow(data)) ){
#   if(detected[i]==FALSE)
#      {
#        count=count+1;
#        count
#        newdata[count,] <- c(data[i,]);
#      }
# }

temp<-NULL;
for(i in (1:nrow(data)) ){
  if(detected[i]==TRUE)
    temp <- c(temp,data[i,1])
}

remove(y);
newdata <- data[-c(temp),];
attach(newdata);
length(y)
length(x2)

########Multicollienarity###########
vif<-NULL;
mod1=lm(y~x2+x3+x4+x5+x6+x7+x8+x9+x10+x11);
R=summary(mod1)$r.squared;
vi=1/(1-R);
vif <- c(vif,vi);
mod1=lm(y~x1+x3+x4+x5+x6+x7+x8+x9+x10+x11);
R=summary(mod1)$r.squared;
vi=1/(1-R);
vif <- c(vif,vi);
mod1=lm(y~x2+x1+x4+x5+x6+x7+x8+x9+x10+x11);
R=summary(mod1)$r.squared;
vi=1/(1-R);
vif <- c(vif,vi);
mod1=lm(y~x2+x3+x1+x5+x6+x7+x8+x9+x10+x11);
R=summary(mod1)$r.squared;
vi=1/(1-R);
vif <- c(vif,vi);
mod1=lm(y~x2+x3+x4+x1+x6+x7+x8+x9+x10+x11);
R=summary(mod1)$r.squared;
vi=1/(1-R);
vif <- c(vif,vi);
mod1=lm(y~x2+x3+x4+x5+x1+x7+x8+x9+x10+x11);
R=summary(mod1)$r.squared;
vi=1/(1-R);
vif <- c(vif,vi);
mod1=lm(y~x2+x3+x4+x5+x6+x1+x8+x9+x10+x11);
R=summary(mod1)$r.squared;
vi=1/(1-R);
vif <- c(vif,vi);
mod1=lm(y~x2+x3+x4+x5+x6+x7+x1+x9+x10+x11);
R=summary(mod1)$r.squared;
vi=1/(1-R);
vif <- c(vif,vi);
mod1=lm(y~x2+x3+x4+x5+x6+x7+x8+x1+x10+x11);
R=summary(mod1)$r.squared;
vi=1/(1-R);
vif <- c(vif,vi);
mod1=lm(y~x2+x3+x4+x5+x6+x7+x8+x9+x1+x11);
R=summary(mod1)$r.squared;
vi=1/(1-R);
vif <- c(vif,vi);
mod1=lm(y~x2+x3+x4+x5+x6+x7+x8+x9+x10+x1);
R=summary(mod1)$r.squared;
vi=1/(1-R);
vif <- c(vif,vi);
vif
#All the Variance Inflation Factors are less than 10, so no multicolinearity in dataset

#######Removing Categorical Variable#########
x9_1=matrix(0,517,1);
x9_2=matrix(0,517,1);
x9_3=matrix(0,517,1);
x9_4=matrix(0,517,1);
x9_5=matrix(0,517,1);
x9_6=matrix(0,517,1);
x9_7=matrix(0,517,1);
x9_8=matrix(0,517,1);
x9_9=matrix(0,517,1);
x9_10=matrix(0,517,1);
x9_11=matrix(0,517,1);
for (i in (1:nrow(newdata))){
  if (x9[i]==1) x9_1[i]=1
  else if (x9[i]==2) x9_2[i]=1
  else if (x9[i]==3) x9_3[i]=1
  else if (x9[i]==4) x9_4[i]=1
  else if (x9[i]==5) x9_5[i]=1
  else if (x9[i]==6) x9_6[i]=1
  else if (x9[i]==7) x9_7[i]=1
  else if (x9[i]==8) x9_8[i]=1
  else if (x9[i]==9) x9_9[i]=1
  else if (x9[i]==10) x9_10[i]=1
  else if (x9[i]==11) x9_11[i]=1
}

x2_1=matrix(0,517,1);
x2_2=matrix(0,517,1);
x2_3=matrix(0,517,1);
x2_4=matrix(0,517,1);
x2_5=matrix(0,517,1);
x2_6=matrix(0,517,1);
for (i in (1:nrow(newdata))){
  if (x2[i]==1) x2_1[i]=1
  else if (x2[i]==2) x2_2[i]=1
  else if (x2[i]==3) x2_3[i]=1
  else if (x2[i]==4) x2_4[i]=1
  else if (x2[i]==5) x2_5[i]=1
  else if (x2[i]==6) x2_6[i]=1
}

x3_1=matrix(0,517,1);
x3_2=matrix(0,517,1);
x3_3=matrix(0,517,1);
x3_4=matrix(0,517,1);
x3_5=matrix(0,517,1);
x3_6=matrix(0,517,1);
for (i in (1:nrow(newdata))){
  if (x3[i]==1) x3_1[i]=1
  else if (x3[i]==2) x3_2[i]=1
  else if (x3[i]==3) x3_3[i]=1
  else if (x3[i]==4) x3_4[i]=1
  else if (x3[i]==5) x3_5[i]=1
  else if (x3[i]==6) x3_6[i]=1
}

x5_1=matrix(0,517,1);
x5_2=matrix(0,517,1);
x5_3=matrix(0,517,1);
x5_4=matrix(0,517,1);
x5_5=matrix(0,517,1);
x5_6=matrix(0,517,1);
for (i in (1:nrow(newdata))){
  if (x5[i]==1) x5_1[i]=1
  else if (x5[i]==2) x5_2[i]=1
  else if (x5[i]==3) x5_3[i]=1
  else if (x5[i]==4) x5_4[i]=1
  else if (x5[i]==5) x5_5[i]=1
  else if (x5[i]==6) x5_6[i]=1
}

x8_1=matrix(0,517,1);
x8_2=matrix(0,517,1);
for (i in (1:nrow(newdata))){
  if (x8[i]==1) x8_1[i]=1
  else if (x8[i]==2) x8_2[i]=1
}

keeps <- c("y", "x1","x4","x6","x7","x10","x11");
newdata=newdata[keeps];

newdata <- data.frame(newdata,x2_1,x2_2,x2_3,x2_4,x2_5,x2_6,x3_1,x3_2,x3_3,x3_5,x3_6,x5_1,x5_2,x5_4,x8_1,x9_1,x9_2,x9_3,x9_4,x9_6);

newdata1=newdata[1:390,];
newdata2=newdata[391:522,];

#########AIC Criterion##########
lm1 <- lm(y ~ .,data=newdata1);
step(lm1, direction="both",steps=2000, k=2);

#########SBC Criterion##########
lm1 <- lm(y ~ .,data=newdata1);
step(lm1, direction="both",steps=2000, k=log(nrow(data)));

#########Cp Criterion###########
step(lm1, direction="both", scale=(summary(lm1)$sigma)^2);
library(leaps);
P<-model.matrix(lm1)[,-1];
#a=leaps(P,y,nbest=3);
#b=matrix(c(a$size-1,a$Cp, a$which),ncol=10);

########Adjusted R^2 and Press Criterion#########
library(leaps)
P<-model.matrix(lm1)[,-1]
#c=leaps(P, y, method="adjr2",nbest=3);
#d=leaps(P, y, method="r2",nbest=3);


mod=lm(y~x1+x7+x10+x3_1+x3_2+x3_6+x5_2+x8_1+x9_1,data=newdata1);
anova(mod);
keeps <- c("y", "x1","x7","x10","x3_1","x3_2","x3_6","x5_2","x8_1","x9_1");
newdata2=newdata2[keeps];

F=predict(mod,newdata2);
G=F-newdata2$y;
G=G^2;
Val=sum(G,na.rm=TRUE);
MSE=Val/132;
end