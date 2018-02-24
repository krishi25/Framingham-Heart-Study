fdata<-read.table("data.txt", header=TRUE, sep="\t")

#Select 1st period
far<-fdata[which(fdata$PERIOD==1),]

#Supporting functions
getna<-function(x){
  return (sum(is.na(x)))
}

getnaidx<-function(x){
  return (which(is.na(x)))
}

#create X
X<-cbind(far$SEX, 
         far$AGE,
         far$CURSMOKE,
         far$CIGPDAY,
         far$BPMEDS,
         far$PREVSTRK,
         far$PREVHYP,
         far$DIABETES,
         far$TOTCHOL, 
         far$SYSBP, 
         far$DIABP, 
         far$BMI,
         far$GLUCOSE, 
         far$HEARTRTE,
         far$TIMEMI)
colnames(X)<-c("Sex","Age","Cur.Smoke","Cigar.Day","BP.Med","Prev.Strk","Prev.Hyp","Diab","Tol.Chol","SysBP","DiaBP","BMI","Gluc","HeaRate","TimeMI")

#Try to remove all the NA row
na.list<-apply(X, MARGIN=2, FUN = getnaidx)
unlist.na.list<-unlist(na.list)
rem.row<-unique(unlist.na.list)
X<-X[-rem.row,]
apply(X, MARGIN=2, FUN = getna)

#See Correlation
cor(X)

#See: if the person got HOSPMI event within 10 years, there are some rows (about 50) with time=0, it's still counted under "less than 10 years",
evnt<-(X[,15]<3650)#to take 0 out, change it here
data<-as.data.frame(cbind(X,Event=evnt))

#Change the Gender value to 0=MALE and 1=FEMALE
data$Sex<-data$Sex-1

#try to fit a model
head(data)
attach(data)
fit<-glm(Event~Sex+Age+Cigar.Day+BP.Med+Prev.Strk+Prev.Hyp+Diab+Tol.Chol+SysBP+BMI+Gluc+HeaRate,family = binomial(link="logit"))
summary(fit)

library(caTools)
train_rows = sample.split(data$Event, SplitRatio=0.7)
train = data[ train_rows,]
test  = data[-train_rows,]

fit<-glm(Event~Sex+Cur.Smoke+Cigar.Day+
           BP.Med+Prev.Strk+Prev.Hyp+Diab+
           Tol.Chol+SysBP+DiaBP+
           BMI+Gluc+HeaRate+age.group,data=data,family = binomial(link="logit"))

#Create random subset of data --100 observations

set.seed(0)

obs <- c(1:3927)   ####!!!! total num of observations in data set  --this may change if we delete the one w/o a score for MI

sample.est <- sort(sample(obs, 100))  #index of items to delete --will become validation data points

sample.val <- (1:3927)[-sample.est]  #index of items to keep    #####may need to update this too

data.est <- data[sample.est,] #validation data

data.val <- data[sample.val,] #training data

fit_v <-glm(Event~Sex+Age+Cigar.Day+BP.Med+Prev.Strk+Prev.Hyp+Diab+Tol.Chol+BMI+Gluc+HeaRate+Blood.Pressure.Category+Cur.Smoke, data = data.val, family = binomial(link="logit"))   ###insert final predictors here



#predict probabilites

#p <- fitted.values(fit_v)   

predictions <- predict(fit_v, data.est, type='response')



#ROC Curve to assess model 

#Citation: Tobias Sing, Oliver Sander, Niko Beerenwinkel, Thomas Lengauer. ROCR: visualizing classifier performance in R. Bioinformatics 21(20):3940-3941 (2005).  

# model good if has high true positive rate or low false positive rate(or both) for most threshold values

library(ROCR)

ROCRpred <- prediction(predictions, data.est$Event)   #####predicted probabilites, response

ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')

plot(ROCRperf, colorize =T, text.adj=c(-.2,1.7))

#plot(ROCRperf, avg = 'threshold', spread.estimate='stddev', colorize =T) ##print avg across multiple runs



#Area under the curve -- a single number measure of how well the model performs-- the closer to 1 the better

AUROC<- performance(ROCRpred, 'auc')

AUROC <- AUROC@y.values

AUROC 

pos_cigar.day=which(Cigar.Day>30)#
length(pos_cigar.day)
pos_tol.chol_1=which(data$Tol.Chol<177)
pos_tC=which(Tol.Chol>293)
pos_tolchol=c(pos_tol.chol_1,pos_tC)#
pos_BMI1=which(data$BMI<20.5)
pos_BMI2=which(data$BMI>30.5)
pos_BMI=c(pos_BMI1,pos_BMI2)
pos_heart1=which(data$HeaRate<61)
pos_heart2=which(data$HeaRate>89)
pos_heartrate=c(pos_heart1,pos_heart2)
data1=data[-pos_BMI,]
data1=data1[-pos_tolchol,]
data1=data1[-pos_cigar.day,]
data1=data1[-pos_heartrate,]
fit2=fit<-glm(Event~Sex+Cigar.Day+Cur.Smoke+
                BP.Med+Prev.Strk+Prev.Hyp+Diab+ Tol.Chol+ BMI+Gluc+HeaRate+age.group+bp.level,data=data1,family = binomial(link="logit"))
step(fit2)


fit1 <- glm(Event~BMI+Gluc+Cigar.Day+Tol.Chol+SysBP+Sex+Age,data=data,family = binomial(link="logit"))
fit2 <- glm(Event~HeaRate+BMI+Gluc+Cigar.Day+Tol.Chol+SysBP+Sex+Age,data=data,family=binomial(link = 'logit'))
fit3 <- glm(Event~BMI+Cigar.Day+Tol.Chol+SysBP+Sex+Age,data=data,family=binomial(link = 'logit'))
fit4<- glm(Event~HeaRate+BMI+Cigar.Day+Tol.Chol+SysBP+Sex+Age,data=data,family=binomial(link = 'logit'))
fit5 <- glm(Event~Gluc+Cigar.Day+Tol.Chol+SysBP+Sex+Age,data=data,family=binomial(link = 'logit'))
fit6 <- glm(Event~HeaRate+Gluc+Cigar.Day+Tol.Chol+SysBP+Sex+Age,data=data,family=binomial(link = 'logit'))
fit7 <- glm(Event~Cigar.Day+Tol.Chol+SysBP+Sex+Age,data=data,family=binomial(link = 'logit'))
fit8 <- glm(Event~HeaRate+Cigar.Day+Tol.Chol+SysBP+Sex+Age,data=data,family=binomial(link = 'logit'))

R tell us that Heart Rate is not significant (based on sign coef test), we we reduce to this 4 models

fit1 <- glm(Event~Cigar.Day+Tol.Chol+SysBP+Sex+Age+BMI+Gluc,data=data,family = binomial(link="logit"))
fit3 <- glm(Event~Cigar.Day+Tol.Chol+SysBP+Sex+Age+BMI,data=data,family=binomial(link = 'logit'))
fit5 <- glm(Event~Cigar.Day+Tol.Chol+SysBP+Sex+Age+Gluc,data=data,family=binomial(link = 'logit'))
fit7 <- glm(Event~Cigar.Day+Tol.Chol+SysBP+Sex+Age,data=data,family=binomial(link = 'logit'))

#We realized they all predictors are contained in the model, except BMI and Gluc (fit 1 has both, fit2 has BMI, fit3 has Gluc and fit7 without anything)
#So we perform the "Extra-sum-of-square" test
#Find Anova
a1<-anova(fit1, test="LRT")
a3<-anova(fit3, test="LRT")
a5<-anova(fit5, test="LRT")
a7<-anova(fit7, test="LRT")

#get sum of Deviance, I think the same concept as MSRes in OLS
l3<-sum(a3$Deviance[2:7])
l5<-sum(a5$Deviance[2:7])
l1<-sum(a1$Deviance[2:8])
l7<-sum(a7$Deviance[2:6])

#Then, perform the "extra-sum-of-square" test
pchisq(l1-l7, df=2, lower.tail = FALSE) #0.002
pchisq(l1-l5, df=1, lower.tail = FALSE) #0.005
pchisq(l1-l3, df=1, lower.tail = FALSE)#0.03
summary(fit2)