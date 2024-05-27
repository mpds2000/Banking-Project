# Setting the directory
getwd()
setwd("D:")
getwd()

# Importing files
bank_train = read.csv("bank-full_train.csv",stringsAsFactors = FALSE,header = T)
bank_test = read.csv("bank-full_test.csv",stringsAsFactors = FALSE,header = T)

# No. of rows and columns
dim(bank_train)
dim(bank_test)

# Adding new column
bank_test$y = NA
bank_train$data = 'train'
bank_test$data = 'test'

# Combining data sets
all_bank_data = rbind(bank_train,bank_test)
str(all_bank_data)

library(dplyr)
library(car)
library(randomForest)
library(tree)
library(pROC)

sum(is.na(bank_train))
colSums(is.na(bank_train))

# Number of unique values of each column
apply(all_bank_data,2,function(x) length(unique(x)))

glimpse(all_bank_data)

# Data Preparation
# Cleaning the variable  'job'
t = table(all_bank_data$job)
sort(t)

final = round(prop.table(table(all_bank_data$job,all_bank_data$y),1)*100,1)
final

s = addmargins(final,2)
sort(s[,1])

all_bank_data=all_bank_data %>% 
  mutate(job_1=as.numeric(job %in% c("self-employed","unknown","technician")), 
         job_2=as.numeric(job %in% c("services","housemaid","entrepreneur")),
         job_3=as.numeric(job %in% c("management","admin")),
         job_4=as.numeric(job=="student"),
         job_5=as.numeric(job=="retired"),
         job_6=as.numeric(job=="unemployed")) %>% 
  select(-job)

glimpse(all_bank_data)

# Cleaning the variable 'marital'
t = table(all_bank_data$marital)
sort(t)

all_bank_data = all_bank_data %>% 
  mutate(
  single = as.numeric(marital %in% c("single")),
  married = as.numeric(marital %in% c("married")),
) %>% 
  select(-marital)

glimpse(all_bank_data)

# Cleaning the variable 'education'
t = table(all_bank_data$education)
sort(t,desc = TRUE)

all_bank_data = all_bank_data %>% 
  mutate(
    primary = as.numeric(education %in% c("primary")),
    secondary = as.numeric(education %in% c("secondary")),
    tertiary = as.numeric(education %in% c("tertiary"))
  ) %>% 
  select(-education)

glimpse(all_bank_data)

# Cleaning the variable 'default'
table(all_bank_data$default)

all_bank_data$default = as.numeric(all_bank_data$default=="yes")

# Cleaning the variable 'housing'
table(all_bank_data$housing)

all_bank_data$housing = as.numeric(all_bank_data$housing=="yes")

glimpse(all_bank_data)

# Cleaning the variable 'loan'
table(all_bank_data$loan)

all_bank_data$loan = as.numeric(all_bank_data$loan=="yes")
glimpse(all_bank_data)

# Cleaning the variable 'contact'
table(all_bank_data$contact)

all_bank_data = all_bank_data %>% 
  mutate(
    telephone = as.numeric(contact %in% c("telephone")),
    cellular = as.numeric(contact %in% c("cellular"))
  ) %>% 
  select(-contact)
glimpse(all_bank_data)

# Cleaning the variable 'month'
table(all_bank_data$month)

finalmnth = round(prop.table(table(all_bank_data$month,all_bank_data$y),1)*100,1)
sss = addmargins(finalmnth,2)
sort(sss[,1])

all_bank_data = all_bank_data %>% 
  mutate(
         month_1=as.numeric(month %in% c("aug","jun","nov","jan","jul")), 
         month_2=as.numeric(month %in% c("dec","sep")),
         month_3=as.numeric(month=="mar"),
         month_4=as.numeric(month=="oct"),
         month_5=as.numeric(month=="apr"),
         month_6=as.numeric(month=="feb")
  ) %>% 
  select(-month)
glimpse(all_bank_data)

# Cleaning the variable 'poutcome'
t = table(all_bank_data$poutcome)
sort(t)

all_bank_data = all_bank_data %>% 
  mutate(poc_success=as.numeric(poutcome %in% c("success")),
         poc_failure=as.numeric(poutcome %in% c("failure")),
         poc_other=as.numeric(poutcome %in% c("other"))
  )%>% 
  select(-poutcome)

glimpse(all_bank_data)

#all_bank_data = all_bank_data %>% 
 # mutate(
 #   success = as.numeric(poutcome %in% "success"),
 #   other = as.numeric(poutcome %in% "other"),
 #  failure = as.numeric(poutcome %in% "failure")
 #) %>% 
 #select(-poutcome)

glimpse(all_bank_data)

# Cleaning the variable 'y'
table(all_bank_data$y)
table(bank_train$y)
table(bank_test$y)

all_bank_data$y = as.numeric(all_bank_data$y == "yes")
table(all_bank_data$y)

glimpse(all_bank_data)

# Splitting the data into train and test
bank_train = all_bank_data %>% 
  filter(data == 'train') %>% 
  select(-data)

bank_test = all_bank_data %>% 
  filter(data=='test') %>% 
  select(-data,-y)

glimpse(bank_train)
glimpse(bank_test)

# Divide the data in 80-20 ratio
set.seed(5)
s = sample(1:nrow(bank_train),0.80*nrow(bank_train))
train_80 = bank_train[s,]
train_20 = bank_test[-s,]

dim(train_80)
dim(train_20)

#Model Building
library(car)

for_vif = lm(y ~.,data = bank_train)
summary(for_vif)

t = vif(for_vif)
sort(t,decreasing = T)[1:3]

for_vif = lm(y ~.-secondary,data = bank_train)
t = vif(for_vif)
sort(t,decreasing = T)[1:3]

summary(for_vif)

colnames(bank_train)

fit_train = bank_train %>% 
  select(-secondary)

colnames(fit_train)

# Building model on fit_train data set
fit = glm(y ~. , family = 'binomial',data = fit_train)
summary(fit)

fit = step(fit)

formula(fit)

fit_final = glm(y ~ balance + housing + loan + day + duration + campaign + ID + 
                  job_2 + job_3 + job_4 + job_5 + married + primary + telephone + 
                  cellular + month_1 + month_2 + month_3 + month_4 + month_5 + 
                  month_6 + poc_success + poc_other + poc_failure, family = 'binomial',data = fit_train)
summary(fit_final)

formula(fit_train)

bank_train$score=predict(fit_final,newdata = bank_train,type="response")

cutoff_data=data.frame(cutoff=0,TP=0,FP=0,FN=0,TN=0)
cutoffs=seq(0,1,length=100)

for (i in cutoffs){
  predicted=as.numeric(bank_train$score>i)
  
  TP=sum(predicted==1 & bank_train$y==1)
  FP=sum(predicted==1 & bank_train$y==0)
  FN=sum(predicted==0 & bank_train$y==1)
  TN=sum(predicted==0 & bank_train$y==0)
  cutoff_data=rbind(cutoff_data,c(i,TP,FP,FN,TN))
}
# Lets remove the dummy data containing top row in data frame cutoff_data
cutoff_data=cutoff_data[-1,]

# Calculating performance
cutoff_data = cutoff_data %>%
  mutate(P = FN+TP,
         N = TN+FP,
         Sn=TP/P, 
         Sp=TN/N, 
         KS=abs((TP/P)-(FP/N)),
         Accuracy=(TP+TN)/(P+N),
         Lift=(TP/P)/((TP+FP)/(P+N)),
         Precision=TP/(TP+FP),
         Recall=TP/P) %>% 
  select(-P,-N)

KS_cutoff = cutoff_data$cutoff[which.max(cutoff_data$KS)]
KS_cutoff

# Final Output on test data set
bank_test$score = predict(fit_final,newdata = bank_test,type = "response")

# Predicting whether customer have subscribed or no
bank_test$left=as.numeric(bank_test$score>KS_cutoff) #if score is greater than cutoff
table(bank_test$left)

# Final Predictions
bank_test$leftfinal = factor(bank_test$left,levels = c(0,1),labels=c("no","yes"))
table(bank_test$leftfinal)
write.csv(bank_test$leftfinal,"Shreya_Gupta_P5_part2.csv",row.names = F)













































































































