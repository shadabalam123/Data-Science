customer_churn=read.csv("C:\\Users\\shada\\Downloads\\Data-Set-2\\Data-Set\\Customer_Churn.csv",stringsAsFactors = TRUE)

#_____________
View(customer_churn)
library(caTools)
library(dplyr)
library(ROCR)

#__________Splitting the data into train and test data

split_churn=sample.split(customer_churn$Churn,SplitRatio = 0.80)
train=subset(customer_churn,split_churn==T)
test=subset(customer_churn,split_churn==F)

#__________________Building logistic regression model

log_mod=glm(Churn~MonthlyCharges+TechSupport+tenure,data=train,family = "binomial")
result_log=predict(log_mod,newdata=test,type="response")
tab=table(test$Churn,result_log>0.29)
tab
acc=sum(diag(tab))/sum(tab)
acc

309/(309+65)
377/(377+658)

268/(268+106)
259/(259+776)

# here accuracy is 76 percent


#__________ Building graph of Accuracy vs cutoff plot
log_mod_pediction=prediction(result_log,test$Churn)
performance_acc=performance(log_mod_pediction,"acc")
plot(performance_acc)

#________Building ROC curve
performance_roc=performance(log_mod_pediction,"tpr","fpr")
plot(performance_roc,colorize=T)
abline(a=0,b=1)

# Building AUC to check the accuracy 
performance_auc=performance(log_mod_pediction,"auc")
performance_auc

# Since the area covered is almost 81 percent, reflecting a greater
# affinity/closeness  towards 1 

# hence the model has good accuracy








