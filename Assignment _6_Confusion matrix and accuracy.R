Pharmacovigilance=read.csv("C:\\Users\\shada\\Downloads\\datasets_r_programming\\Pharmacovigilance_audit_Data.csv",stringsAsFactors = TRUE)
city_temperature=read.csv("C:\\Users\\shada\\Downloads\\datasets_r_programming\\city_temperature.csv",stringsAsFactors = TRUE)
customer_churn=read.csv("C:\\Users\\shada\\Downloads\\Data-Set-2\\Data-Set\\Customer_Churn.csv",stringsAsFactors = TRUE)


#__________________Installing the required package

library(caTools)

# Splitting the data frame:-

#___________________Qno.1

splitting_region=sample.split(city_temperature$Region,SplitRatio = 0.70)
train_ct=subset(city_temperature,splitting_region=T)
test_ct=subset(city_temperature,splitting_region=F)

#_______Building the model

glm_ct=glm(Region~AvgTemperature,data=train_ct,family = "binomial")
test_pred_ct=predict(glm_ct,newdata=test_ct,type="response")
range(test_pred_ct)



#____________________Qno.2

splitting_churn=sample.split(customer_churn$Churn,SplitRatio = 0.80)
train_cc=subset(customer_churn,splitting_churn=T)
test_cc=subset(customer_churn,splitting_churn=F)

#____Building the model
glm_churn=glm(Churn~MonthlyCharges,data=train_cc,family = "binomial")
test_pred_cc=predict(glm_churn,newdata=test_cc,type="response")
range(test_pred_cc)


#___________________Qno.3

splitting_drugid=sample.split(Pharmacovigilance$DrugID,SplitRatio = 0.75)
train_drugid=subset(Pharmacovigilance,splitting_drugid=T)
test_drugid=subset(Pharmacovigilance,splitting_drugid=F)

#________Building the model

glm_drugid=glm(DrugID~Age,data=train_drugid,family="binomial")
test_pred_drugid=predict(glm_drugid,newdata = test_drugid,type="response")
range(test_pred_drugid)
#

