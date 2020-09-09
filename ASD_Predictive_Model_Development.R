##################################################
### Basic Set Up                                ##
##################################################

# Clear plots
if(!is.null(dev.list())) dev.off()
# Clear console
cat("\014") 
# Clean workspace
rm(list=ls())
#Set work directory
setwd("C:/R-Projeccts/ASD")


##################################################
### Install Libraries                           ##
##################################################

install.packages("pastecs")  
library("pastecs")  

if(!require(farff)){install.packages("farff")}
library("farff")

if(!require(ggplot2)){install.packages("ggplot2")}
library("ggplot2")

if(!require(gclus)){install.packages("gclus")}
library("gclus")


##################################################
### Read data and do preliminary data checks    ##
##################################################
# Read "comma separated value" files (".csv")

AutismDC <- read.csv("AutismClass.csv", header = TRUE, sep = ",")
#trimming white spaces for proper data
trimws(AutismDC)

#Writes output to a file
#sink("Divya_Chandwani.txt")
#sink()

head(AutismDC,5)  #Print a Few Observations to Verify

#Rename columns and added initials '_DC'
colnames(AutismDC)
names(AutismDC) <- c("X.1_DC","X_DC","A01_DC","A02_DC","A03_DC","A04_DC","A05_DC","A06_DC","A07_DC","A08_DC","A09_DC","A10_DC","Age_DC","gender_DC","Ethnicity_DC","Jaundic_DC","App_DC","Res_DC","Rel_DC","Autism_DC")
str(AutismDC)

#Checking unique values in order to transform them to numeric
unique(AutismDC$gender_DC, incomparables = FALSE)
unique(AutismDC$Ethnicity_DC, incomparables = FALSE)
unique(AutismDC$Jaundic_DC, incomparables = FALSE)
unique(AutismDC$Rel_DC, incomparables = FALSE)
unique(AutismDC$Autism_DC, incomparables = FALSE)
unique(AutismDC$App_DC, incomparables = FALSE)

#transroming column to numeric, Male is 2 and female is 1
AutismDC$gender_DC_numeric[AutismDC$gender_DC=='m'] <- 2
AutismDC$gender_DC_numeric[AutismDC$gender_DC=='f'] <- 1
# 1 value means the column value gender_DC_numeric is missing
AutismDC$gender_DC_numeric[is.na(AutismDC$gender_DC_numeric)==TRUE]<-0



#transroming column Ethnicity_DC to numeric
AutismDC$Ethnicity_DC_numeric[AutismDC$Ethnicity_DC=='Middle Eastern'] <- 1
AutismDC$Ethnicity_DC_numeric[AutismDC$Ethnicity_DC=='South Asian'] <- 2
AutismDC$Ethnicity_DC_numeric[AutismDC$Ethnicity_DC=='Asian'] <- 3
AutismDC$Ethnicity_DC_numeric[AutismDC$Ethnicity_DC=='Hispanic'] <- 4
AutismDC$Ethnicity_DC_numeric[AutismDC$Ethnicity_DC=='Black'] <- 5
AutismDC$Ethnicity_DC_numeric[AutismDC$Ethnicity_DC=='Turkish'] <- 6
AutismDC$Ethnicity_DC_numeric[AutismDC$Ethnicity_DC=='Latino'] <- 7
AutismDC$Ethnicity_DC_numeric[AutismDC$Ethnicity_DC=='Pasifika'] <- 8
AutismDC$Ethnicity_DC_numeric[AutismDC$Ethnicity_DC=='Others'] <- 9
AutismDC$Ethnicity_DC_numeric[AutismDC$Ethnicity_DC=='others'] <- 9
#treating NA's 
AutismDC$Ethnicity_DC_numeric[is.na(AutismDC$Ethnicity_DC_numeric)==TRUE]<-0

#treating Na's in age
y <- table(AutismDC$Age_DC)
mode_age<-names(y)[which(y==max(y))]
AutismDC$Age_DC[is.na(AutismDC$Age_DC)] <- as.numeric(mode_age)
factor(AutismDC$Age_DC)


#transroming column Jaundic_DC to numeric
AutismDC$Jaundic_DC_numeric[AutismDC$Jaundic_DC=='no'] <- 2
AutismDC$Jaundic_DC_numeric[AutismDC$Jaundic_DC=='yes'] <- 1
AutismDC$Jaundic_DC_numeric[is.na(AutismDC$Jaundic_DC)==TRUE]<-0

#transroming column Rel_DC to numeric
AutismDC$Rel_DC_numeric[AutismDC$Rel_DC=='Self'] <- 1
AutismDC$Rel_DC_numeric[AutismDC$Rel_DC=='Relative'] <- 2
AutismDC$Rel_DC_numeric[AutismDC$Rel_DC=='Parent'] <- 3
AutismDC$Rel_DC_numeric[AutismDC$Rel_DC=='Others'] <- 4
AutismDC$Rel_DC_numeric[AutismDC$Rel_DC=='Health care professional'] <- 5
#treating NA's
AutismDC$Rel_DC_numeric[is.na(AutismDC$Rel_DC)==TRUE]<-0


#transroming column Autism_DC to numeric
AutismDC$Autism_DC_numeric[AutismDC$Autism_DC=='NO'] <- 0
AutismDC$Autism_DC_numeric[AutismDC$Autism_DC=='YES'] <- 1

#transroming column App_DC to numeric
AutismDC$App_DC_numeric[AutismDC$App_DC=='no'] <- 0
AutismDC$App_DC_numeric[AutismDC$App_DC=='yes'] <- 1

colnames(AutismDC)
#keeping only numeric columns, new data frame Transformed_AutismDC is created.
Transformed_AutismDC<-AutismDC[c(1,2,3,4,5,6,7,8,9,10,11,12,13,18,21,22,23,24,25,26)]

#Verify structure of the dataframe, having only numeric data.
str(Transformed_AutismDC)
#Printig a Few Observations to Verify
head(Transformed_AutismDC,5)  

###################################################
##Creating Univariate Descriptive Analysis       ##
###################################################
colnames(Transformed_AutismDC)
names(Transformed_AutismDC)<-c("X.1_DC","X_DC","A01_DC",
                               "A02_DC","A03_DC","A04_DC","A05_DC","A06_DC",
                               "A07_DC","A08_DC","A09_DC","A10_DC","Age_DC","Res_DC",
                               "gender_DC","Ethnicity_DC","Jaundic_DC",
                               "Rel_DC","Autism_DC","App_DC")
str(Transformed_AutismDC)

summary(Transformed_AutismDC)
stat.desc(Transformed_AutismDC)

par(mfrow=c(3,2))    #Fit more graphs in!

# Creating univarriate graphs for all variables -> Histogram is created for all variables.
# loop over column *names* instead of actual columns
sapply(names(Transformed_AutismDC), function(cname){
  # (make sure we only plot the numeric columns)
  if (is.numeric(Transformed_AutismDC[[cname]]))
    # use the `main` param to put column name as plot title
    print(hist(Transformed_AutismDC[[cname]], xlab = cname, main=cname))
})
par(mfrow=c(1,1))


###################################################
## Finding Outliers                              ##
###################################################

par(mfrow=c(3,2))

# Creating box plots for outlier analysis, BoxPlot is created for all variables.
# loop over column *names* instead of actual columns
sapply(names(Transformed_AutismDC), function(cname){
  # (make sure we only plot the numeric columns)
  if (is.numeric(Transformed_AutismDC[[cname]]))
    # use the `main` param to put column name as plot title
    print(boxplot(Transformed_AutismDC[[cname]], main=cname, xlab=cname))
})

par(mfrow=c(1,1))


#################################################################
# Actions to be performed on outliers                           #
#################################################################

# checking the quantiles of variables for which the box plot shows outliers.
# There is a need of removing the outleirs in age. since 99% values lie below 56
quantile(Transformed_AutismDC$Age_DC,.99, na.rm=TRUE)


#There is no need to removing outliers for the Ethnicity since 93% values lie below 9,and there are no values greater than 9
# If we remove 9 as an outlier, it will cause loss of data related to Ethinicity type 'Others'
quantile(Transformed_AutismDC$Ethnicity_DC,.99,na.rm=TRUE)
quantile(Transformed_AutismDC$Ethnicity_DC,.95,na.rm=TRUE)

#There is no need for outlier analysis for Dichotomous variables, even though 1 is shown as an outlier in the box plot,
#it will cause a loss of data related to Patients having jaundice.
quantile(Transformed_AutismDC$Jaundic_DC,.99, na.rm=TRUE)


# As per the box plot,we have an outlier.
# however 98% of the users have not used a screening app previously
# now we haveto decide if we want to eminiate this outlier.
#I believe that removing this outlier will prevent the model from predicting 
#the case where atleaset 2% of patients have used a screeing app previously.
quantile(Transformed_AutismDC$App_DC,.98, na.rm=TRUE)
quantile(Transformed_AutismDC$App_DC,.99, na.rm=TRUE)



#****Removing outliers from age , Adjust age to 51 for all values greater than 50******
Transformed_AutismDC$Age_DC <- with(Transformed_AutismDC, ifelse(Age_DC>50, 51,Age_DC))

# Verify that the outliers in age are removed 
par(mfrow=c(1,2))
cname='Age_DC'
print(boxplot(Transformed_AutismDC[[cname]], main=cname))
print(hist(Transformed_AutismDC[[cname]], main=cname, xlab=cname))


par(mfrow=c(1,1))

###################################################
## Comparing Correlation of Predictors           ##
## All                                           ##
###################################################

str(Transformed_AutismDC)
factor(Transformed_AutismDC$Age_DC)
Transformed_AutismDC$Age_DC<-as.numeric(gsub('[$,]', '', Transformed_AutismDC$Age_DC))
res <- cor(Transformed_AutismDC, method="spearman")
round(res, 2)
#Observations-
#X.1 and X are coliner hence we can eliminte one of the two from our model
#Res is had a strong corellation with autism with a value of 0.78.
#Hence i am selecting it asone of the most significant predictors of autism
#A09 is also one of the most significant predictors of autism

#GRAPHICAL REPRESENTATION OD CORELATION
colnames(Transformed_AutismDC)
SubDS_Autism_DC <- Transformed_AutismDC[c(5:8,11,14,19)]   
SubDS_Autism_DC.corr <- abs(cor(SubDS_Autism_DC))                  #Create Correlations
SubDS_Autism_DC.col <- dmat.color(SubDS_Autism_DC.corr)              #Assign Colors - NOTE - 657 colours in HEX
SubDS_Autism_DC.ordered <- order.single(SubDS_Autism_DC.corr)            #Reorders variables by correlation
cpairs(SubDS_Autism_DC, 
       SubDS_Autism_DC.ordered, 
       panel.colors=SubDS_Autism_DC.col, 
       gap=.5, 
       main="Key Variables Ordered and Coloured by Correlation")

###################################################
## Comparing Outcome with Predictors             ##
###################################################

###################################################
## Comparing Outcome with Predictors             ##
## Categorical Data                              ##
###################################################

cross <- table( Transformed_AutismDC$gender_DC,Transformed_AutismDC$Autism_DC)

barplot(prop.table(cross,2), xlab='Result',ylab='Autism',main="Autism by gender",
        col=c("darkblue","darkred")
        ,legend=rownames(cross), args.legend = list(x = "topleft"))
#

table(Transformed_AutismDC$gender_DC,Transformed_AutismDC$Autism_DC)   #Contingency Table
prop.table(table(Transformed_AutismDC$gender_DC, Transformed_AutismDC$Autism_DC), margin=1)*100  #Contingency Table Pct.
summary(table(Transformed_AutismDC$gender_DC, Transformed_AutismDC$Autism_DC))   #Chi-Sq
chisq.test(Transformed_AutismDC$gender_DC, Transformed_AutismDC$Autism_DC)       #Chi-Sq - specific

#Considering the significance level of 0.5, the p value is less than the significance level,  
#hence the two variables are independant of each other


###################################################
## Comparing Outcome with Predictors             ##
## Continuous                                    ##
###################################################


###Box Plots by variable

par(mfrow=c(2,2))

boxplot(A09_DC~Autism_DC, data=Transformed_AutismDC, xlab="Autism indicator", ylab="A09", Main="A09 vs Autism") 
boxplot(Res_DC~Autism_DC, data=Transformed_AutismDC, xlab="Autism indicator ",ylab="Res", Main="Result vs Autism") 
par(mfrow=c(1,1))


#########################################
## Creating Baseline Model             ##
#########################################
#**************************************** MODEL 1 *****************************************************
Autism_DC_glm1 = glm(Autism_DC ~ A04_DC + A05_DC + A06_DC + A09_DC + Res_DC,
              family="binomial", data=Transformed_AutismDC, na.action=na.omit)
Autism_DC_glm1
summary(Autism_DC_glm1)
# Above model is too good to be true, sine res is highly corelated with Autism, removing res from the model

#**************************************** MODEL 2 *****************************************************
Autism_DC_glm2 = glm(Autism_DC ~ A01_DC+A02_DC + A03_DC+A04_DC + A05_DC + A06_DC + A09_DC + A10_DC+App_DC+gender_DC,
                     family="binomial", data=Transformed_AutismDC, na.action=na.omit)
Autism_DC_glm2
summary(Autism_DC_glm2)

#**************************************** MODEL 3 *****************************************************

Autism_DC_glm3 = glm(Autism_DC ~A01_DC+A02_DC + A03_DC+ A04_DC + A05_DC + A06_DC + A09_DC+App_DC,Rel_DC,
                     family="binomial", data=Transformed_AutismDC, na.action=na.omit)
Autism_DC_glm3
summary(Autism_DC_glm3)

#**************************************** MODEL 4 *****************************************************
Autism_DC_glm4 = glm(Autism_DC ~ A04_DC + A05_DC + A06_DC + A09_DC+ gender_DC + App_DC,
                     family="binomial", data=Transformed_AutismDC, na.action=na.omit)
Autism_DC_glm4
summary(Autism_DC_glm4)

#**************************************** MODEL 5 *****************************************************
Autism_DC_glm5 = glm(Autism_DC ~ A05_DC + A06_DC + A09_DC + A10_DC+App_DC+gender_DC,
                     family="binomial", data=Transformed_AutismDC, na.action=na.omit)
Autism_DC_glm5
summary(Autism_DC_glm5)



#########################################
## Evaluating Logistic Model           ##
#########################################

# **************************** Evaluating model 2 *********************************************
### Confusion Matrix  ####

pred <- predict(Autism_DC_glm2, newdata=Transformed_AutismDC)
pred_y <- as.numeric(pred > 0)
true_y <- as.numeric(Transformed_AutismDC$Autism_DC==1)
true_pos <- (true_y==1) & (pred_y==1)
true_neg <- (true_y==0) & (pred_y==0)
false_pos <- (true_y==0) & (pred_y==1)
false_neg <- (true_y==1) & (pred_y==0)

conf_mat <- matrix(c(sum(true_pos), sum(false_pos),
                     sum(false_neg), sum(true_neg)),2,2)
colnames(conf_mat) <- c('Yhat = 1', 'Yhat = 0')
rownames(conf_mat) <- c('Y = 1', 'Y = 0')
conf_mat

### Precision, Recall, Specificity ###

Autism_DC_glm2$Precis <- conf_mat[1,1]/sum(conf_mat[,1])

Autism_DC_glm2$Recall <- conf_mat[1,1]/sum(conf_mat[1,])

Autism_DC_glm2$Specif <- conf_mat[2,2]/sum(conf_mat[2,])
Autism_DC_glm2$Precis
Autism_DC_glm2$Recall
Autism_DC_glm2$Specif


#Calculating Accuracy
sum_Pos<-sum(conf_mat[1,])
sum_Neg<-sum(conf_mat[2,])
Total<-sum_Pos+sum_Neg
TP_TN<-conf_mat[1,1]+conf_mat[2,2]
Autism_DC_glm2$Accuracy<- TP_TN/Total
Autism_DC_glm2$Accuracy


### ROC Curve ###
idx <- order(-pred)
recall <- cumsum(true_y[idx]==1)/sum(true_y==1)
specificity <- (sum(true_y==0) - cumsum(true_y[idx]==0))/sum(true_y==0)
roc_df <- data.frame(recall = recall, specificity = specificity)
ggplot(roc_df, aes(x=specificity, y=recall)) +
  geom_line(color='red') + 
  scale_x_reverse(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  geom_line(data=data.frame(x=(0:100)/100), aes(x=x, y=1-x), 
            linetype='dashed', color='blue')

### AUC ###

AUC <- sum(roc_df$recall[-1] * diff(1-roc_df$specificity))
AUC



# **************************** Evaluating model 3 *********************************************
### Confusion Matrix  ####

pred3 <- predict(Autism_DC_glm3, newdata=Transformed_AutismDC)
pred_y3 <- as.numeric(pred3 > 0)
true_y3 <- as.numeric(Transformed_AutismDC$Autism_DC==1)
true_pos3 <- (true_y==1) & (pred_y==1)
true_neg3 <- (true_y==0) & (pred_y==0)
false_pos3 <- (true_y==0) & (pred_y==1)
false_neg3 <- (true_y==1) & (pred_y==0)

conf_mat3 <- matrix(c(sum(true_pos), sum(false_pos),
                     sum(false_neg), sum(true_neg)),2,2)
colnames(conf_mat3) <- c('Yhat = 1', 'Yhat = 0')
rownames(conf_mat3) <- c('Y = 1', 'Y = 0')
conf_mat3

### Precision, Recall, Specificity ###

Autism_DC_glm3$Precis <- conf_mat3[1,1]/sum(conf_mat3[,1])

Autism_DC_glm3$Recall <- conf_mat3[1,1]/sum(conf_mat3[1,])

Autism_DC_glm3$Specif <- conf_mat3[2,2]/sum(conf_mat3[2,])
Autism_DC_glm3$Precis
Autism_DC_glm3$Recall
Autism_DC_glm3$Specif


#Calculating Accuracy
sum_Pos3<-sum(conf_mat3[1,])
sum_Neg3<-sum(conf_mat3[2,])
Total3<-sum_Pos+sum_Neg
TP_TN3<-conf_mat3[1,1]+conf_mat3[2,2]
Autism_DC_glm3$Accuracy<- TP_TN3/Total3
Autism_DC_glm3$Accuracy


### ROC Curve ###
idx <- order(-pred3)
recall <- cumsum(true_y[idx]==1)/sum(true_y==1)
specificity <- (sum(true_y==0) - cumsum(true_y[idx]==0))/sum(true_y==0)
roc_df <- data.frame(recall = recall, specificity = specificity)
ggplot(roc_df, aes(x=specificity, y=recall)) +
  geom_line(color='red') + 
  scale_x_reverse(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  geom_line(data=data.frame(x=(0:100)/100), aes(x=x, y=1-x), 
            linetype='dashed', color='blue')

### AUC ###

AUC <- sum(roc_df$recall[-1] * diff(1-roc_df$specificity))
AUC






