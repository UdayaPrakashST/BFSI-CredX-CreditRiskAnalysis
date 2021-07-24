########################################################################
# FileName:Analysis.R
# Code Created by: Uday,Sneha, Aakanksha
# Code last modified by : Uday
# Code last created date: 2019-03-17
####################Objective:#############################
# CredX, a leading credit card provider wants to minimize
# credit loss by acquiring the 'right' customers
#
# The acquisition team of CredX is responsible for identifying the
# right prospects to target and provide suitable product(s)
# In the past few years, CredX has experienced an increase in Credit Loss
# CredX wants to determine the factors affecting credit risk,and create strategies to mitigate the risk and assess the financial benefit of the risk model.
##########################################################################


#######################################################################

#Set Working Directory

#######################################################################

#Load Relevant Packages
if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}

if (!require(tidyr)) {
  install.packages("tidyr")
  library(tidyr)
}

if (!require(InformationValue)) {
  install.packages("InformationValue")
  library(InformationValue)
}



if (!require(DMwR)) {
  install.packages("DMwR")
  library(DMwR)
}


if (!require(caret)) {
  install.packages("caret")
  library(caret)
}

if (!require(caTools)) {
  install.packages("caTools")
  library(caTools)
}

if (!require(dummies)) {
  install.packages("dummies")
  library(dummies)
}

if (!require(MASS)) {
  install.packages("MASS")
  library(MASS)
}


if (!require(car)) {
  install.packages("car")
  library(car)
}


if (!require(randomForest)) {
  install.packages("randomForest")
  library(randomForest)
}


if (!require(e1071)) {
  install.packages("e1071")
  library(e1071)
}



#########################################################################

###########Read demographics and credit bureau data######################

cred_beau_data <-
  read.csv(
    "Credit Bureau data.csv",
    stringsAsFactors = F,
    header = T,
    check.names = T
  )
demographics_data <-
  read.csv(
    "Demographic data.csv",
    stringsAsFactors = F,
    header = T,
    check.names = T
  )


##############################################Data cleaning & Outlier treatment##################################

summary(demographics_data)
str(demographics_data)

summary(cred_beau_data)
str(cred_beau_data)


#Check if primary column is unique.
#The table should be at applicationID[Customer] level.



paste0("Total Number of records in demographics data: ",
       nrow(demographics_data))
paste0("Number of unique applicationIDs in demographics data: ",
       length(unique(demographics_data$Application.ID)))


paste0("Total Number of records in credit bureau data: ",
       nrow(cred_beau_data))
paste0("Number of unique applicationIDs in credit bureau data: ",
       length(unique(cred_beau_data$Application.ID)))


#Get the  duplicate appplication IDs"


demographics_data[which(duplicated(demographics_data$Application.ID)), 1]

cred_beau_data[which(duplicated(cred_beau_data$Application.ID)), 1]

#Duplicate IDs are the same across both the datasets.




#Get the subset of duplicate ApplicationIDs in demographics data

demographics_duplicate_data <-
  subset(
    demographics_data,
    Application.ID == 765011468 |
      Application.ID == 653287861 | Application.ID == 671989187
  )

table(demographics_duplicate_data$Application.ID,
      demographics_duplicate_data$Age)

#Each ApplicationID in duplicate data frame varies by Age. Assuming the IDs
# with higher age as the right one

demographics_deduplicated_applicationID_age <-
  demographics_duplicate_data %>% group_by(Application.ID) %>% summarise(Age =
                                                                           max(Age))



demographics_unique_data <-
  subset(
    demographics_data,
    Application.ID != 765011468 &
      Application.ID != 653287861 & Application.ID != 671989187
  )

demographics_duplicated_treated <-
  inner_join(
    demographics_data,
    demographics_deduplicated_applicationID_age,
    by = c("Application.ID", "Age")
  )

demographics_uniq_data <-
  rbind(demographics_unique_data, demographics_duplicated_treated)


n_distinct(demographics_data$Application.ID)
nrow(demographics_uniq_data)


#Duplicates are removed.

#


demog_data <- demographics_uniq_data



#Remove the intermediate DFs created.
demographics_duplicate_data <- NULL
demographics_deduplicated_applicationID_age <- NULL
demographics_unique_data <- NULL
demographics_duplicated_treated <- NULL
demographics_uniq_data <- NULL


#Check the duplicates in credit bureau data
cred_duplicate_data <-
  subset(
    cred_beau_data,
    Application.ID == 765011468 |
      Application.ID == 653287861 | Application.ID == 671989187
  )


#No observable pattern in duplicates in cred bureau data

#Check percentage of duplicate values
paste0(
  "percentage of duplicate values is :",
  3 / n_distinct(cred_beau_data$Application.ID) * 100
)

#Duplicates are less than 1% in the dataset.
#Therefore removing them from cred_bureau_data

cred_deduplicate_data <-
  subset(
    cred_beau_data,
    Application.ID != 765011468 &
      Application.ID != 653287861 & Application.ID != 671989187
  )

cred_data <- cred_deduplicate_data

#Join credit bureau and demographics data.


master_df <- inner_join(x = demog_data, y = cred_data, "Application.ID")

table(master_df$Performance.Tag.x, master_df$Performance.Tag.y)
#Remove performancetag.y column as it depicts the same value
master_df$Performance.Tag.y <- NULL





#Check for missing values

sum(is.na(master_df))
#Analyzing each missing value column separately
na_count <- master_df %>%
  select(everything()) %>%  # replace to your needs
  summarise_all(funs(sum(is.na(.))))

#There are 3031 Missing values in master_df
#Following are the columns having NA values
# No of dependents 3
# performancetag  1425(Indicates rejection)
# Average creditcard utilization in last 6 Months 1058
# No of trades opened in last 6 months 1
# Presence of open home loan 272
# Outstanding balance 272


#If count of NA values are less than 10% of the total observations,we will
#discard them

#No of dependents has  3 NA values which is less than 10% of observations.
#So discarding them

na_dataset <- master_df[which(is.na(master_df$No.of.dependents) == T), ]

master_df <- master_df[which(is.na(master_df$No.of.dependents) == F), ]

#No of trades in last 6 months has 1 NA value, discarding it.

na_dataset <-
  rbind(na_dataset, master_df[which(is.na(master_df$No.of.trades.opened.in.last.6.months) ==
                                      T), ])


master_df <-
  master_df[which(is.na(master_df$No.of.trades.opened.in.last.6.months) ==
                    F), ]

#272 observations having NA in presence of homeloan column have NAs in
#Outstanding balance as well as Average credit utilization. Discarding
#them since it is less than 10%


na_dataset <-
  rbind(na_dataset, master_df[which(is.na(master_df$Presence.of.open.home.loan) ==
                                      T), ])

master_df <-
  master_df[which(is.na(master_df$Presence.of.open.home.loan) == F), ]


summary(master_df$Performance.Tag.x)[7]




#1425 Values have NA values in Performance.Tag in demographics data.
#These indicate that the applicationID has been rejected.Lets keep them in a
#separate dataset.

rejected_applicants <-
  master_df[which(is.na(master_df$Performance.Tag.x) == T), ]



#Removing rejected applicants from master_df

master_df <- master_df[which(is.na(master_df$Performance.Tag.x) == F), ]



na_count1 <- master_df %>%
  select(everything()) %>%  # replace to your needs
  summarise_all(funs(sum(is.na(.))))



quantile(
  master_df$Avgas.CC.Utilization.in.last.12.months,
  na.rm = T,
  probs = seq(0, 1, 0.05)
)


#750 NAs in Average Credit Utilization in last 12 Months.
#Lets have them in the dataset.




#WoE analysis

#For this analysis 1 should be good and 0 should be bad.
# According to our dataset 1 is defaulted(bad) and 0 is non-defaulted.

#Create a new variable changing the flag

master_df$woe_performance_tag <-
  ifelse(master_df$Performance.Tag.x == 1, 0, 1)


table(master_df$Performance.Tag.x, master_df$woe_performance_tag)


#Categorize continuous values to decile values.

quantile(master_df$Age, probs = seq(0, 1, 0.1))
summary(master_df$Age)

#Minimum age for CreditCard applicant is 18. Lets change those values

master_df$Age <- if_else(master_df$Age < 18, 18, master_df$Age)


quantile(master_df$Age, probs = seq(0, 1, 0.1))
summary(master_df$Age)

master_df$age_bucket <-
  cut(
    master_df$Age,
    include.lowest = TRUE,
    breaks = c(18, 31, 36, 39, 42, 45, 48, 51, 54, 58, 65)
  )


quantile(master_df$Income, probs = seq(0, 1, 0.05))
summary(master_df$Income)

#Income has negative value. Flooring with 5% value

master_df$Income <- ifelse(master_df$Income < 0, 4.5, master_df$Income)

quantile(master_df$Income, probs = seq(0, 1, 0.1))
summary(master_df$Income)

master_df$Income_bucket <-
  cut(
    master_df$Income,
    include.lowest = TRUE,
    breaks = c(0, 6, 11, 17, 22, 27, 32, 37, 42, 49, 60)
  )



quantile(master_df$No.of.months.in.current.residence,
         probs = seq(0, 1, 0.2))
summary(master_df$Income)

master_df$No.of.months.in.current.residence_bucket <-
  cut(
    master_df$No.of.months.in.current.residence,
    include.lowest = TRUE,
    breaks = c(6, 30, 73, 126)
  )


quantile(master_df$No.of.months.in.current.company, probs = seq(0, 1, 0.1))
summary(master_df$No.of.months.in.current.company)

master_df$No.of.months.in.current.company_bucket <-
  cut(
    master_df$No.of.months.in.current.company,
    include.lowest = TRUE,
    breaks = c(3, 6, 13, 20, 27, 34, 41, 48, 54, 62, 133)
  )

quantile(
  master_df$Avgas.CC.Utilization.in.last.12.months,
  probs = seq(0, 1, 0.1),
  na.rm = T
)
summary(master_df$Avgas.CC.Utilization.in.last.12.months)


master_df$Avgas.CC.Utilization.in.last.12.months_bucket <-
  cut(
    master_df$Avgas.CC.Utilization.in.last.12.months,
    include.lowest = TRUE,
    breaks = c(0, 5, 7, 9, 12, 15, 22, 38, 52, 72, 113)
  )

summary(master_df$Avgas.CC.Utilization.in.last.12.months_bucket)

quantile(master_df$Outstanding.Balance,
         probs = seq(0, 1, 0.05),
         na.rm = T)
summary(master_df$Outstanding.Balance)

#Capping at 95 percentile

master_df$Outstanding.Balance <-
  ifelse(
    master_df$Outstanding.Balance > 3650669.90,
    3650669.90,
    master_df$Outstanding.Balance
  )
quantile(master_df$Outstanding.Balance,
         probs = seq(0, 1, 0.1),
         na.rm = T)
summary(master_df$Outstanding.Balance)



a <-
  WOE(factor(master_df$Outstanding.Balance_bucket),
      master_df$woe_performance_tag)

summary(master_df$Outstanding.Balance_bucket)

master_df$Outstanding.Balance_bucket <-
  cut(
    master_df$Outstanding.Balance,
    include.lowest = TRUE,
    breaks = c(
      0,
      6847,
      25533,
      386878,
      585482,
      774273,
      972478,
      1357471,
      2961023,
      3282423,
      3650669.9
    )
  )

summary(master_df$Outstanding.Balance_bucket)


quantile(master_df$Total.No.of.Trades,
         probs = seq(0, 1, 0.1),
         na.rm = T)
summary(master_df$Total.No.of.Trades)


master_df$Total.No.of.Trades_bucket <-
  cut(
    master_df$Total.No.of.Trades,
    include.lowest = TRUE,
    breaks = c(0, 2, 3, 4, 5, 6, 7, 9, 11, 20, 44)
  )


colnames(master_df)

woe_columns <- c(
  "Gender",
  "Marital.Status..at.the.time.of.application.",
  "Education",
  "Profession",
  "Type.of.residence",
  "No.of.times.90.DPD.or.worse.in.last.6.months",
  "No.of.times.60.DPD.or.worse.in.last.6.months",
  "No.of.times.30.DPD.or.worse.in.last.6.months",
  "No.of.times.90.DPD.or.worse.in.last.12.months",
  "No.of.times.60.DPD.or.worse.in.last.12.months",
  "No.of.times.30.DPD.or.worse.in.last.12.months",
  "No.of.trades.opened.in.last.6.months",
  "No.of.trades.opened.in.last.12.months",
  "No.of.PL.trades.opened.in.last.6.months",
  "No.of.PL.trades.opened.in.last.12.months",
  "No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.",
  "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.",
  "Presence.of.open.home.loan",
  "Presence.of.open.auto.loan",
  "age_bucket",
  "Income_bucket",
  "No.of.months.in.current.residence_bucket",
  "No.of.months.in.current.company_bucket",
  "Avgas.CC.Utilization.in.last.12.months_bucket",
  "Outstanding.Balance_bucket",
  "Total.No.of.Trades_bucket",
  "No.of.dependents"
)

a <- data.frame("ColumnName", "IV")

for (i in 1:length(woe_columns))
{
  print(woe_columns[i])
  print(WOETable(as.factor(master_df$i), master_df$woe_performance_tag))
  #print(IV(factor(master_df[,woe_columns[i]]),master_df$woe_performance_tag))
}



#Treat NAs in avg utilisation with 999 (An uncommon value for indication)

master_df$Avgas.CC.Utilization.in.last.12.months[which(is.na(master_df$Avgas.CC.Utilization.in.last.12.months))] <-
  999

summary(master_df$Avgas.CC.Utilization.in.last.12.months)
quantile(master_df$Avgas.CC.Utilization.in.last.12.months,
         probs = seq(0, 1, 0.1))




master_df$Avgas.CC.Utilization.in.last.12.months_bucket <-
  as.factor(
    cut(
      master_df$Avgas.CC.Utilization.in.last.12.months,
      breaks = c(0, 5, 7, 9, 12, 15, 23, 39, 54, 75, 999),
      include.lowest = T
    )
  )







#Calculate Woe columns values for each of the bucket variables.


woe_columns_buckets <- woe_columns[which(grepl("bucket", woe_columns))]

woe_columns_buckets[8] <-
  "Avgas.CC.Utilization.in.last.12.months_bucket"


for (i in 1:length(woe_columns_buckets))
{
  print(woe_columns_buckets[i])
  new_column_name <- paste0(woe_columns_buckets[i], "_woevalues")
  a <-
    WOETable(factor(master_df[, woe_columns_buckets[i]]), master_df$woe_performance_tag)
  print(a)
  a[, woe_columns_buckets[i]] <- a[, 1]
  sel.id <-
    match(as.character(master_df[, woe_columns_buckets[i]]), a[, woe_columns_buckets[i]])
  master_df[, new_column_name] <- a$WOE[sel.id]
  print(summary(master_df[, new_column_name]))
  #print(IV(factor(master_df[,woe_columns[i]]),master_df$woe_performance_tag))
}





#######################################################################################################
###############################################EDA Plots#########################################################


#Plot variables against Defaulters rate.


plot_response <- function(cat_var, var_name) {
  a <- aggregate(Performance.Tag.x ~ cat_var, master_df, mean)
  title1 <-
    paste0("No of applicants and defaulters ratio vs ", var_name)
  count <- data.frame(table(cat_var))
  count <- count[, -1]
  agg_Performance <- cbind(a, count)
  colnames(agg_Performance) <-
    c(var_name, "Performance_rate", "No.of_applicant")
  agg_Performance[, 2] <- format(round(agg_Performance[, 2], 2))
  var_name
  ggplot(agg_Performance,
         aes(agg_Performance[, 1], count, label = Performance_rate)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5) + xlab(var_name) +
    ggtitle(title1)
  
  
}

plot_response(master_df$Gender, "Gender")

plot_response(master_df$Marital.Status..at.the.time.of.application.,
              "MaritalStatus")

plot_response(master_df$Education, "Education")

plot_response(master_df$Profession, "Profession")

#Default rate do not vary much across
#Gender , Marital Status, No of dependents and Education
plot_response(master_df$Type.of.residence, "Type.of.residence")
plot_response(master_df$age_bucket, "Age")
plot_response(master_df$Income_bucket, "Income")
plot_response(
  master_df$No.of.months.in.current.company_bucket,
  "No.of.months.in.current.company"
)
plot_response(
  master_df$No.of.months.in.current.residence_bucket,
  "No.of.months.in.current.residence"
)
#Default rate do not vary much across
#Age income and number of months in current company and residence


plot_response(
  master_df$No.of.times.60.DPD.or.worse.in.last.6.months,
  "No.of.times.60.DPD.or.worse.in.last.6.months"
)
plot_response(
  master_df$No.of.times.30.DPD.or.worse.in.last.6.months,
  "No.of.times.30.DPD.or.worse.in.last.6.months"
)
#Higher the number of times 30,60,90 DPD in last 6 months, higher the risk of defaults

plot_response(
  master_df$Avgas.CC.Utilization.in.last.12.months_bucket,
  "Avg Credit Utilisation in last 6 months"
)

plot_response(master_df$Presence.of.open.home.loan,
              "Presence of open home loan")

plot_response(master_df$Outstanding.Balance_bucket, "Outstanding balance")
#Higher the outstanding balance higher the risk of default
plot_response(master_df$Total.No.of.Trades_bucket, "Total no of trades")

plot_response(
  master_df$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,
  "No of inquiries in last12 months"
)



##########################################Predictive Modelling##############################


colnames(master_df)


################Build a logistic model with Only demographic variables.################


set.seed(200)

split_indices <-
  sample.split(master_df$Performance.Tag.x, SplitRatio = 0.70)

split_indices

train_df <- master_df[split_indices,]

test_df <- master_df[!split_indices,]


colnames(master_df)[which(grepl("woevalues", colnames(master_df)))]


train_df_demographics <-
  train_df[, c(
    "age_bucket_woevalues",
    "Gender",
    "Marital.Status..at.the.time.of.application.",
    "No.of.dependents",
    "No.of.months.in.current.residence_bucket_woevalues",
    "Income_bucket_woevalues",
    "No.of.months.in.current.company_bucket_woevalues",
    "Education",
    "Profession",
    "Type.of.residence",
    "Performance.Tag.x"
  )]

test_df_demographics <- test_df[, c(
  "age_bucket_woevalues",
  "Gender",
  "Marital.Status..at.the.time.of.application.",
  "No.of.dependents",
  "No.of.months.in.current.residence_bucket_woevalues",
  "Income_bucket_woevalues",
  "No.of.months.in.current.company_bucket_woevalues",
  "Education",
  "Profession",
  "Type.of.residence",
  "Performance.Tag.x"
)]



#Dummify data frame to convert categories to flag
train_df_demographics1 <- dummy.data.frame(train_df_demographics)


test_df_demographics1 <- dummy.data.frame(test_df_demographics)









logistic_1 <-
  glm(Performance.Tag.x ~ ., family = "binomial", data = train_df_demographics1)

summary(logistic_1)


logistic_2 <- stepAIC(logistic_1, direction = "both")


#STEPAIC has removed most of the variables lets check the glm results
#for remaining variables

logistic_3 <-
  glm(
    Performance.Tag.x ~ age_bucket_woevalues + No.of.months.in.current.residence_bucket_woevalues +
      Income_bucket_woevalues + No.of.months.in.current.company_bucket_woevalues +
      EducationOthers + ProfessionSE,
    data = train_df_demographics1,
    family = "binomial"
  )


vif(logistic_3)

#All the variables have VIF less than 5

summary(logistic_3)

#All the variables are significant.


# Predicting probabilities of responding for the test data


predictions_logit <-
  predict(logistic_3, newdata = test_df_demographics1[,-28], type = "response")
summary(predictions_logit)



## Model Evaluation: Logistic Regression

# Let's use the probability cutoff of 0.04082(MeanValue)

predicted_response <-
  factor(ifelse(predictions_logit >= 0.04082, 1, 0))

# Creating confusion matrix for identifying the model evaluation.



conf <-
  confusionMatrix(
    predicted_response,
    as.factor(test_df_demographics1$Performance.Tag.x),
    positive = "1"
  )

conf

# Confusion Matrix and Statistics
#
# Reference
# Prediction     0     1
# 0 10099   337
# 1  9896   545
#
# Accuracy : 0.5098
# 95% CI : (0.503, 0.5166)
# No Information Rate : 0.9578
# P-Value [Acc > NIR] : 1
#
# Kappa : 0.0199
# Mcnemar's Test P-Value : <2e-16
#
# Sensitivity : 0.61791
# Specificity : 0.50508
# Pos Pred Value : 0.05220
# Neg Pred Value : 0.96771
# Prevalence : 0.04225
# Detection Rate : 0.02611
# Detection Prevalence : 0.50012
# Balanced Accuracy : 0.56150
#
# 'Positive' Class : 1



#Lets determine the optimalcutoff


perform_fn <- function(cutoff)
{
  predicted_response <-
    factor(ifelse(predictions_logit >= cutoff, 1, 0))
  conf <-
    confusionMatrix(
      predicted_response,
      as.factor(test_df_demographics1$Performance.Tag.x),
      positive = "1"
    )
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc)))
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}



s = seq(.01, .99, length = 100)

OUT = matrix(0, 100, 3)


for (i in 1:100)
{
  OUT[i, ] = perform_fn(s[i])
}

plot(
  s,
  OUT[, 1],
  xlab = "Cutoff",
  ylab = "Value",
  cex.lab = 1.5,
  cex.axis = 1.5,
  ylim = c(0, 1),
  type = "l",
  lwd = 2,
  axes = FALSE,
  col = 2
)
axis(1, seq(0, 1, length = 5), seq(0, 1, length = 5), cex.lab = 1.5)
axis(2, seq(0, 1, length = 5), seq(0, 1, length = 5), cex.lab = 1.5)
lines(s, OUT[, 2], col = "darkgreen", lwd = 2)
lines(s, OUT[, 3], col = 4, lwd = 2)
box()
#legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

locator()

#Optimal cut off value 0.04055513




# Let's choose a cutoff value of 12% for final model

predicted_response <-
  factor(ifelse(predictions_logit >= 0.04055513, 1, 0))

conf_final <-
  confusionMatrix(
    predicted_response,
    as.factor(test_df_demographics1$Performance.Tag.x),
    positive = "1"
  )

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

paste0(acc)

paste0(sens)


paste0(spec)

#Accuracy: 50%
#Sensitivity: 62%
#Specificity: 49%




#data imbalance problem handled using smote sampling. #Lets build logistic
#model using SMOTEd sample

train_df_demographics_dummy$Performance.Tag.x <-
  factor(ifelse(train_df_demographics_dummy$Performance.Tag.x == 1, 1, 0))


smote_sample <-
  SMOTE(
    Performance.Tag.x ~ .,
    train_df_demographics_dummy,
    perc.over = 400,
    perc.under = 200
  )

table(smote_sample$Performance.Tag.x)


lg_smote <-
  glm(Performance.Tag.x ~ ., family = "binomial", data = smote_sample)

summary(lg_smote)


lg_smote2 <- stepAIC(lg_smote, direction = "both")


summary(lg_smote2)
vif(lg_smote2)
#All VIF values are less than 4
#Removing Gender with least significance   0.934223

lg_smote3 <-
  glm(
    formula = Performance.Tag.x ~ age_bucket_woevalues +
      GenderF + Marital.Status..at.the.time.of.application. + Marital.Status..at.the.time.of.application.Married +
      No.of.dependents + No.of.months.in.current.residence_bucket_woevalues +
      Income_bucket_woevalues + No.of.months.in.current.company_bucket_woevalues +
      Education + EducationBachelor + EducationMasters + EducationOthers +
      EducationPhd + ProfessionSAL + ProfessionSE + `Type.of.residenceCompany provided` +
      `Type.of.residenceLiving with Parents` + Type.of.residenceOthers,
    family = "binomial",
    data = smote_sample
  )

summary(lg_smote3)
vif(lg_smote3)
#All VIF values are less than 4
#Removing Marital.Status..at.the.time.of.application. with least significance   0.893640



lg_smote4 <-
  glm(
    formula = Performance.Tag.x ~ age_bucket_woevalues +
      GenderF + Marital.Status..at.the.time.of.application.Married +
      No.of.dependents + No.of.months.in.current.residence_bucket_woevalues +
      Income_bucket_woevalues + No.of.months.in.current.company_bucket_woevalues +
      Education + EducationBachelor + EducationMasters + EducationOthers +
      EducationPhd + ProfessionSAL + ProfessionSE + `Type.of.residenceCompany provided` +
      `Type.of.residenceLiving with Parents` + Type.of.residenceOthers,
    family = "binomial",
    data = smote_sample
  )


summary(lg_smote4)
vif(lg_smote4)
#All VIF values are less than 4
#Removing Education with least significance   0.167074


lg_smote5 <-
  glm(
    formula = Performance.Tag.x ~ age_bucket_woevalues +
      GenderF + Marital.Status..at.the.time.of.application.Married +
      No.of.dependents + No.of.months.in.current.residence_bucket_woevalues +
      Income_bucket_woevalues + No.of.months.in.current.company_bucket_woevalues +
      EducationBachelor + EducationMasters + EducationOthers +
      EducationPhd + ProfessionSAL + ProfessionSE + `Type.of.residenceCompany provided` +
      `Type.of.residenceLiving with Parents` + Type.of.residenceOthers,
    family = "binomial",
    data = smote_sample
  )


summary(lg_smote5)
vif(lg_smote5)
#All VIF values are less than 4
#All the variables have high significance


predictions_logit <-
  predict(lg_smote5, newdata = test_df_demographics_dummy[,-27], type = "response")
summary(predictions_logit)

test_data_response <-
  factor(ifelse(test_df_demographics_dummy$Performance.Tag.x == 1, "yes", "no"))


table(test_data_response)


perform_fn <- function(cutoff)
{
  predicted_response <-
    factor(ifelse(predictions_logit >= cutoff, "yes", "no"))
  conf <-
    confusionMatrix(predicted_response, test_data_response, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc)))
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#---------------------------------------------------------

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(.01, .99, length = 100)

OUT = matrix(0, 100, 3)


for (i in 1:100)
{
  OUT[i, ] = perform_fn(s[i])
}

#---------------------------------------------------------

# plotting cutoffs
plot(
  s,
  OUT[, 1],
  xlab = "Cutoff",
  ylab = "Value",
  cex.lab = 1.5,
  cex.axis = 1.5,
  ylim = c(0, 1),
  type = "l",
  lwd = 2,
  axes = FALSE,
  col = 2
)
axis(1, seq(0, 1, length = 5), seq(0, 1, length = 5), cex.lab = 1.5)
axis(2, seq(0, 1, length = 5), seq(0, 1, length = 5), cex.lab = 1.5)
lines(s, OUT[, 2], col = "darkgreen", lwd = 2)
lines(s, OUT[, 3], col = 4, lwd = 2)
box()
legend(
  0,
  .50,
  col = c(2, "darkgreen", 4, "darkred"),
  lwd = c(2, 2, 2, 2),
  c("Sensitivity", "Specificity", "Accuracy")
)
locator()

#---------------------------------------------------------

cutoff <- s[which(abs(OUT[, 1] - OUT[, 2]) < 0.01)]


# Let's choose a cutoff value of 12% for final model

predicted_response <-
  factor(ifelse(predictions_logit >= 0.3151321, "yes", "no"))

conf_final <-
  confusionMatrix(predicted_response, test_data_response, positive = "yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

#
# > acc
# Accuracy
# 0.5003592
# >
#   > sens
# Sensitivity
# 0.4909297
# >
#   > spec
# Specificity
# 0.5007752

#
# #Using Only demographic attributes give very low values of accuracy sensitivity
# and Specificity. Also the IV values of demographic attributes  are very low





#Lets use both the demographics and credit bureau data
colnames(train_df)

train_df_demo_cred <- train_df[, c(
  "Application.ID",
  "Gender",
  "Marital.Status..at.the.time.of.application.",
  "No.of.dependents",
  "Education",
  "Profession",
  "Type.of.residence",
  "Performance.Tag.x",
  "No.of.times.90.DPD.or.worse.in.last.6.months",
  "No.of.times.60.DPD.or.worse.in.last.6.months",
  "No.of.times.30.DPD.or.worse.in.last.6.months",
  "No.of.times.90.DPD.or.worse.in.last.12.months",
  "No.of.times.60.DPD.or.worse.in.last.12.months",
  "No.of.times.30.DPD.or.worse.in.last.12.months",
  "No.of.trades.opened.in.last.6.months",
  "No.of.trades.opened.in.last.12.months",
  "No.of.PL.trades.opened.in.last.6.months",
  "No.of.PL.trades.opened.in.last.12.months",
  "No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.",
  "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.",
  "Presence.of.open.home.loan",
  "Presence.of.open.auto.loan",
  "Total.No.of.Trades_bucket_woevalues",
  "age_bucket_woevalues",
  "Income_bucket_woevalues",
  "No.of.months.in.current.residence_bucket_woevalues",
  "No.of.months.in.current.company_bucket_woevalues",
  "Avgas.CC.Utilization.in.last.12.months_bucket_woevalues",
  "Outstanding.Balance_bucket_woevalues"
)]



test_df_demo_cred <- test_df[, c(
  "Application.ID",
  "Gender",
  "Marital.Status..at.the.time.of.application.",
  "No.of.dependents",
  "Education",
  "Profession",
  "Type.of.residence",
  "Performance.Tag.x",
  "No.of.times.90.DPD.or.worse.in.last.6.months",
  "No.of.times.60.DPD.or.worse.in.last.6.months",
  "No.of.times.30.DPD.or.worse.in.last.6.months",
  "No.of.times.90.DPD.or.worse.in.last.12.months",
  "No.of.times.60.DPD.or.worse.in.last.12.months",
  "No.of.times.30.DPD.or.worse.in.last.12.months",
  "No.of.trades.opened.in.last.6.months",
  "No.of.trades.opened.in.last.12.months",
  "No.of.PL.trades.opened.in.last.6.months",
  "No.of.PL.trades.opened.in.last.12.months",
  "No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.",
  "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.",
  "Presence.of.open.home.loan",
  "Presence.of.open.auto.loan",
  "Total.No.of.Trades_bucket_woevalues",
  "age_bucket_woevalues",
  "Income_bucket_woevalues",
  "No.of.months.in.current.residence_bucket_woevalues",
  "No.of.months.in.current.company_bucket_woevalues",
  "Avgas.CC.Utilization.in.last.12.months_bucket_woevalues",
  "Outstanding.Balance_bucket_woevalues"
)]


train_df_demo_cred_dummified <- dummy.data.frame(train_df_demo_cred)


test_df_demo_cred_dummified <- dummy.data.frame(test_df_demo_cred)

colnames(train_df_demo_cred_dummified)

logistics_1 <- glm(
  Performance.Tag.x ~ Gender +
    GenderF +
    GenderM +
    Marital.Status..at.the.time.of.application. +
    Marital.Status..at.the.time.of.application.Married +
    Marital.Status..at.the.time.of.application.Single +
    No.of.dependents +
    Education +
    EducationBachelor +
    EducationMasters +
    EducationOthers +
    EducationPhd +
    EducationProfessional +
    Profession +
    ProfessionSAL +
    ProfessionSE +
    ProfessionSE_PROF +
    Type.of.residence +
    `Type.of.residenceCompany provided` +
    `Type.of.residenceLiving with Parents` +
    Type.of.residenceOthers +
    Type.of.residenceOwned +
    Type.of.residenceRented +
    No.of.times.90.DPD.or.worse.in.last.6.months +
    No.of.times.60.DPD.or.worse.in.last.6.months +
    No.of.times.30.DPD.or.worse.in.last.6.months +
    No.of.times.90.DPD.or.worse.in.last.12.months +
    No.of.times.60.DPD.or.worse.in.last.12.months +
    No.of.times.30.DPD.or.worse.in.last.12.months +
    No.of.trades.opened.in.last.6.months +
    No.of.trades.opened.in.last.12.months +
    No.of.PL.trades.opened.in.last.6.months +
    No.of.PL.trades.opened.in.last.12.months +
    No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. +
    No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. +
    Presence.of.open.home.loan +
    Presence.of.open.auto.loan +
    Total.No.of.Trades_bucket_woevalues +
    age_bucket_woevalues +
    Income_bucket_woevalues +
    No.of.months.in.current.residence_bucket_woevalues +
    No.of.months.in.current.company_bucket_woevalues +
    Avgas.CC.Utilization.in.last.12.months_bucket_woevalues +
    Outstanding.Balance_bucket_woevalues
  ,
  family = "binomial",
  data = train_df_demo_cred_dummified
)

summary(logistics_1)



logistics_2 <- stepAIC(logistics_1, direction = "both")


vif(logistics_2)

#variables No.of.times.60.DPD.or.worse.in.last.6.months & No.of.times.30.DPD.or.worse.in.last.6.months
#have very high VIF value
summary(logistics_2)


#Removing No.of.times.60.DPD.or.worse.in.last.6.months since it has high VIF and lesser significance


logistics_3 <-
  glm(
    formula = Performance.Tag.x ~ EducationOthers + ProfessionSE +
      No.of.times.30.DPD.or.worse.in.last.6.months +
      No.of.times.90.DPD.or.worse.in.last.12.months + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. +
      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. +
      Total.No.of.Trades_bucket_woevalues + age_bucket_woevalues +
      Income_bucket_woevalues + No.of.months.in.current.residence_bucket_woevalues +
      Avgas.CC.Utilization.in.last.12.months_bucket_woevalues +
      Outstanding.Balance_bucket_woevalues,
    family = "binomial",
    data = train_df_demo_cred_dummified
  )


vif(logistics_3)


summary(logistics_3)

#Removing the variable No.of.times.90.DPD.or.worse.in.last.12.months  with least significance


logistics_4 <-
  glm(
    formula = Performance.Tag.x ~ EducationOthers + ProfessionSE +
      No.of.times.30.DPD.or.worse.in.last.6.months +
      No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. +
      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. +
      Total.No.of.Trades_bucket_woevalues + age_bucket_woevalues +
      Income_bucket_woevalues + No.of.months.in.current.residence_bucket_woevalues +
      Avgas.CC.Utilization.in.last.12.months_bucket_woevalues +
      Outstanding.Balance_bucket_woevalues,
    family = "binomial",
    data = train_df_demo_cred_dummified
  )
vif(logistics_4)


summary(logistics_4)

#Removing variable ProfessionSE with least significance



logistics_5 <- glm(
  formula = Performance.Tag.x ~ EducationOthers +
    No.of.times.30.DPD.or.worse.in.last.6.months +
    No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. +
    No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. +
    Total.No.of.Trades_bucket_woevalues + age_bucket_woevalues +
    Income_bucket_woevalues + No.of.months.in.current.residence_bucket_woevalues +
    Avgas.CC.Utilization.in.last.12.months_bucket_woevalues +
    Outstanding.Balance_bucket_woevalues,
  family = "binomial",
  data = train_df_demo_cred_dummified
)
vif(logistics_5)


summary(logistics_5)



#All the variables are significant and have VIF less than 5


logistics_final <- logistics_5




# Predicting probabilities of responding for the test data


predictions_logit <-
  predict(logistics_final, newdata = test_df_demo_cred_dummified[,-28], type = "response")
summary(predictions_logit)



## Model Evaluation: Logistic Regression

# Let's use the probability cutoff of 0.04242 (MeanValue)

predicted_response <-
  factor(ifelse(predictions_logit >= 0.04242, 1, 0))

# Creating confusion matrix for identifying the model evaluation.



conf <-
  confusionMatrix(
    predicted_response,
    as.factor(test_df_demo_cred_dummified$Performance.Tag.x),
    positive = "1"
  )

conf

#Accuracy with cutoff as mean value 0.5871


#Lets determine the optimal cut off



#Lets determine the optimalcutoff


perform_fn <- function(cutoff)
{
  predicted_response <-
    factor(ifelse(predictions_logit >= cutoff, 1, 0))
  conf <-
    confusionMatrix(
      predicted_response,
      as.factor(test_df_demo_cred_dummified$Performance.Tag.x),
      positive = "1"
    )
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc)))
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}



s = seq(.01, .99, length = 100)

OUT = matrix(0, 100, 3)


for (i in 1:100)
{
  OUT[i, ] = perform_fn(s[i])
}

plot(
  s,
  OUT[, 1],
  xlab = "Cutoff",
  ylab = "Value",
  cex.lab = 1.5,
  cex.axis = 1.5,
  ylim = c(0, 1),
  type = "l",
  lwd = 2,
  axes = FALSE,
  col = 2
)
axis(1, seq(0, 1, length = 5), seq(0, 1, length = 5), cex.lab = 1.5)
axis(2, seq(0, 1, length = 5), seq(0, 1, length = 5), cex.lab = 1.5)
lines(s, OUT[, 2], col = "darkgreen", lwd = 2)
lines(s, OUT[, 3], col = 4, lwd = 2)
box()
#legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

locator()

#Optimal cut off value 0.04684316



predicted_response <-
  factor(ifelse(predictions_logit >= 0.04684316, 1, 0))

conf_final <-
  confusionMatrix(
    predicted_response,
    as.factor(test_df_demo_cred_dummified$Performance.Tag.x),
    positive = "1"
  )

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

# Accuracy
# 0.6294008
# >
#   > sens
# Sensitivity
# 0.6077098
# >
#   > spec
# Specificity
# 0.6303576

#Logistic Regression results


######################Random forest Model########################


names(train_df_demo_cred) <- gsub(" ", ".", names(train_df_demo_cred))
names(test_df_demo_cred) <- gsub(" ", ".", names(test_df_demo_cred))


train_df_demo_cred$Performance.Tag.x <-
  factor(ifelse(train_df_demo_cred_dummified$Performance.Tag.x == 1, 1, 0))

test_df_demo_cred$Performance.Tag.x <-
  factor(ifelse(test_df_demo_cred_dummified$Performance.Tag.x == 1, 1, 0))

sum(is.na(train_df_demo_cred))


train_df_demo_cred$Gender <-
  ifelse(train_df_demo_cred$Gender == "",
         "M",
         train_df_demo_cred$Gender)

#Removing applicationID and running random forest

rf_train_dataset <-
  train_df_demo_cred[, -1] %>% mutate_if(is.character, as.factor)
rf_test_dataset <-
  test_df_demo_cred[, -1] %>% mutate_if(is.character, as.factor)


rf = randomForest(
  Performance.Tag.x ~ .,
  data = rf_train_dataset,
  proximity = FALSE,
  ntree = 1000,
  mtry = 10,
  do.trace = TRUE,
  na.action = na.omit
)




summary(rf)

plot(rf)


a <- data.frame(varImp(rf))

str(rf_test_dataset)
str(rf_train_dataset)


for (i in colnames(rf_train_dataset))
{
  if (is.factor(rf_train_dataset[, i]) == T)
  {
    print(levels(rf_train_dataset[, i]))
    print(levels(rf_test_dataset[, i]))
    levels(rf_train_dataset[, i]) <- levels(rf_test_dataset[, i])
  }
}


which(colnames(rf_train_dataset) == "Performance.Tag.x")

predicted.response <-
  predict(rf, rf_test_dataset[,-which(colnames(rf_train_dataset) == "Performance.Tag.x")])



summary(predicted.response)

confusionMatrix(data = predicted.response,
                reference = rf_test_dataset$Performance.Tag.x)
#
#
# Confusion Matrix and Statistics
#
# Reference
# Prediction     0     1
# 0 19994   882
# 1     1     0
#
# Accuracy : 0.9577
# 95% CI : (0.9549, 0.9604)
# No Information Rate : 0.9578
# P-Value [Acc > NIR] : 0.5227
#
# Kappa : -1e-04
# Mcnemar's Test P-Value : <2e-16
#
# Sensitivity : 0.9999
# Specificity : 0.1
# Pos Pred Value : 0.9578
# Neg Pred Value : 0.0000
# Prevalence : 0.9578
# Detection Rate : 0.9577
# Detection Prevalence : 1.0000
# Balanced Accuracy : 0.5000
#
# 'Positive' Class : 0


#Accuracy n sensitivity r very high , specificity is 0

smote_sample_rf <-
  SMOTE(
    Performance.Tag.x ~ .,
    rf_train_dataset,
    perc.over = 400,
    perc.under = 200
  )



rf_smote = randomForest(
  Performance.Tag.x ~ .,
  data = smote_sample_rf[, -1],
  proximity = FALSE,
  ntree = 1000,
  mtry = 10,
  do.trace = TRUE,
  na.action = na.omit
)




summary(rf_smote)

plot(rf_smote)





predicted.response_smote <-
  predict(rf_smote,
          rf_test_dataset[, -which(colnames(rf_test_dataset) == "Performance.Tag.x")])


summary(predicted.response_smote)

confusionMatrix(data = predicted.response_smote,
                reference = rf_test_dataset$Performance.Tag.x)




# Confusion Matrix and Statistics
#
# Reference
# Prediction     0     1
# 0 19892   878
# 1   103     4
#
# Accuracy : 0.953
# 95% CI : (0.9501, 0.9558)
# No Information Rate : 0.9578
# P-Value [Acc > NIR] : 0.9996
#
# Kappa : -0.0011
# Mcnemar's Test P-Value : <2e-16
#
# Sensitivity : 0.994849
# Specificity : 0.004535
# Pos Pred Value : 0.957727
# Neg Pred Value : 0.037383
# Prevalence : 0.957753
# Detection Rate : 0.952819
# Detection Prevalence : 0.994875
# Balanced Accuracy : 0.499692

#'Positive' Class : 0

#Accuracy n sensitivity r very high , specificity is 0.




predicted.response_smote <-
  predict(rf_smote, rf_test_dataset, type = "prob")


summary(predicted.response_smote[, 2])

test_data_response <-
  factor(ifelse(rf_test_dataset$Performance.Tag.x == 1, "yes", "no"))



perform_fn_rf_demo <- function(cutoff)
{
  predicted_response <-
    as.factor(ifelse(predicted.response_smote[, 2] >= cutoff, "yes", "no"))
  conf <-
    confusionMatrix(predicted_response, test_data_response, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf_demo <- t(as.matrix(c(sens, spec, acc)))
  colnames(OUT_rf_demo) <-
    c("sensitivity", "specificity", "accuracy")
  return(OUT_rf_demo)
}



s_demo = seq(.01, .99, length = 100)

OUT_rf_demo = matrix(0, 100, 3)


for (i in 1:100)
{
  OUT_rf_demo[i, ] = perform_fn_rf_demo(s_demo[i])
}




plot(
  s_demo,
  OUT_rf_demo[, 1],
  xlab = "Cutoff",
  ylab = "Value",
  cex.lab = 1.5,
  cex.axis = 1.5,
  ylim = c(0, 1),
  type = "l",
  lwd = 2,
  axes = FALSE,
  col = 2
)
axis(1, seq(0, 1, length = 5), seq(0, 1, length = 5), cex.lab = 1.5)
axis(2, seq(0, 1, length = 5), seq(0, 1, length = 5), cex.lab = 1.5)
lines(s_demo, OUT_rf_demo[, 2], col = "darkgreen", lwd = 2)
lines(s_demo, OUT_rf_demo[, 3], col = 4, lwd = 2)
box()
locator()



# #Cut off value 0.1872757
#
#
predicted_response <-
  factor(ifelse(predicted.response_smote[, 2] >= 0.1872757, "yes", "no"))


confusionMatrix(data = predicted_response,
                reference = test_data_response,
                positive = "yes")
#
#
# Confusion Matrix and Statistics
#
# Reference
# Prediction    no   yes
# no  12025   358
# yes  7970   524
#
# Accuracy : 0.6011
# 95% CI : (0.5944, 0.6077)
# No Information Rate : 0.9578
# P-Value [Acc > NIR] : 1
#
# Kappa : 0.0381
# Mcnemar's Test P-Value : <2e-16
#
# Sensitivity : 0.59410
# Specificity : 0.60140
# Pos Pred Value : 0.06169
# Neg Pred Value : 0.97109
# Prevalence : 0.04225
# Detection Rate : 0.02510
# Detection Prevalence : 0.40686
# Balanced Accuracy : 0.59775
#
# 'Positive' Class : yes






#################################
#Lets use logistic regression to build the entire scorecard as well.
#So we will go ahead with Logistic regression model as we are getting
#good accuracy sensitivity and specificity for that model.
################################

#Lets predict the defaullt for rejected population
#and check how data looks for them.
head(rejected_applicants)






#Categorize continuous values to decile values.

quantile(rejected_applicants$Age, probs = seq(0, 1, 0.1))
summary(rejected_applicants$Age)

#Minimum age for CreditCard applicant is 18. Lets change those values

rejected_applicants$Age <-
  if_else(rejected_applicants$Age < 18, 18, rejected_applicants$Age)


quantile(rejected_applicants$Age, probs = seq(0, 1, 0.1))
summary(rejected_applicants$Age)

rejected_applicants$age_bucket <-
  cut(
    rejected_applicants$Age,
    include.lowest = TRUE,
    breaks = c(18, 31, 36, 39, 42, 45, 48, 51, 54, 58, 65)
  )


quantile(rejected_applicants$Income, probs = seq(0, 1, 0.05))
summary(rejected_applicants$Income)

#Income has negative value. Flooring with 5% value

rejected_applicants$Income <-
  ifelse(rejected_applicants$Income < 0,
         4.5,
         rejected_applicants$Income)

quantile(rejected_applicants$Income, probs = seq(0, 1, 0.1))
summary(rejected_applicants$Income)

rejected_applicants$Income_bucket <-
  cut(
    rejected_applicants$Income,
    include.lowest = TRUE,
    breaks = c(0, 6, 11, 17, 22, 27, 32, 37, 42, 49, 60)
  )



quantile(rejected_applicants$No.of.months.in.current.residence,
         probs = seq(0, 1, 0.2))
summary(rejected_applicants$Income)

rejected_applicants$No.of.months.in.current.residence_bucket <-
  cut(
    rejected_applicants$No.of.months.in.current.residence,
    include.lowest = TRUE,
    breaks = c(6, 30, 73, 126)
  )


quantile(rejected_applicants$No.of.months.in.current.company,
         probs = seq(0, 1, 0.1))
summary(rejected_applicants$No.of.months.in.current.company)

rejected_applicants$No.of.months.in.current.company_bucket <-
  cut(
    rejected_applicants$No.of.months.in.current.company,
    include.lowest = TRUE,
    breaks = c(3, 6, 13, 20, 27, 34, 41, 48, 54, 62, 133)
  )

quantile(
  rejected_applicants$Avgas.CC.Utilization.in.last.12.months,
  probs = seq(0, 1, 0.1),
  na.rm = T
)
summary(rejected_applicants$Avgas.CC.Utilization.in.last.12.months)


rejected_applicants$Avgas.CC.Utilization.in.last.12.months_bucket <-
  cut(
    rejected_applicants$Avgas.CC.Utilization.in.last.12.months,
    include.lowest = TRUE,
    breaks = c(0, 5, 7, 9, 12, 15, 22, 38, 52, 72, 113)
  )

summary(rejected_applicants$Avgas.CC.Utilization.in.last.12.months_bucket)

quantile(
  rejected_applicants$Outstanding.Balance,
  probs = seq(0, 1, 0.05),
  na.rm = T
)
summary(rejected_applicants$Outstanding.Balance)

#Capping at 95 percentile

rejected_applicants$Outstanding.Balance <-
  ifelse(
    rejected_applicants$Outstanding.Balance > 3650669.90,
    3650669.90,
    rejected_applicants$Outstanding.Balance
  )
quantile(
  rejected_applicants$Outstanding.Balance,
  probs = seq(0, 1, 0.1),
  na.rm = T
)
summary(rejected_applicants$Outstanding.Balance)




summary(rejected_applicants$Outstanding.Balance_bucket)

rejected_applicants$Outstanding.Balance_bucket <-
  cut(
    rejected_applicants$Outstanding.Balance,
    include.lowest = TRUE,
    breaks = c(
      0,
      6847,
      25533,
      386878,
      585482,
      774273,
      972478,
      1357471,
      2961023,
      3282423,
      3650669.9
    )
  )

summary(rejected_applicants$Outstanding.Balance_bucket)


quantile(
  rejected_applicants$Total.No.of.Trades,
  probs = seq(0, 1, 0.1),
  na.rm = T
)
summary(rejected_applicants$Total.No.of.Trades)


rejected_applicants$Total.No.of.Trades_bucket <-
  cut(
    rejected_applicants$Total.No.of.Trades,
    include.lowest = TRUE,
    breaks = c(0, 2, 3, 4, 5, 6, 7, 9, 11, 20, 44)
  )


#Treat NAs in avg utilisation with 999 (An uncommon value for indication)

rejected_applicants$Avgas.CC.Utilization.in.last.12.months[which(is.na(rejected_applicants$Avgas.CC.Utilization.in.last.12.months))] <-
  999

summary(rejected_applicants$Avgas.CC.Utilization.in.last.12.months)
quantile(rejected_applicants$Avgas.CC.Utilization.in.last.12.months,
         probs = seq(0, 1, 0.1))
rejected_applicants$Avgas.CC.Utilization.in.last.12.months_bucket <-
  as.factor(
    cut(
      rejected_applicants$Avgas.CC.Utilization.in.last.12.months,
      breaks = c(0, 5, 7, 9, 12, 15, 23, 39, 54, 75, 999),
      include.lowest = T
    )
  )




colnames(rejected_applicants)

woe_columns <- c(
  "Gender",
  "Marital.Status..at.the.time.of.application.",
  "Education",
  "Profession",
  "Type.of.residence",
  "No.of.times.90.DPD.or.worse.in.last.6.months",
  "No.of.times.60.DPD.or.worse.in.last.6.months",
  "No.of.times.30.DPD.or.worse.in.last.6.months",
  "No.of.times.90.DPD.or.worse.in.last.12.months",
  "No.of.times.60.DPD.or.worse.in.last.12.months",
  "No.of.times.30.DPD.or.worse.in.last.12.months",
  "No.of.trades.opened.in.last.6.months",
  "No.of.trades.opened.in.last.12.months",
  "No.of.PL.trades.opened.in.last.6.months",
  "No.of.PL.trades.opened.in.last.12.months",
  "No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.",
  "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.",
  "Presence.of.open.home.loan",
  "Presence.of.open.auto.loan",
  "age_bucket",
  "Income_bucket",
  "No.of.months.in.current.residence_bucket",
  "No.of.months.in.current.company_bucket",
  "Avgas.CC.Utilization.in.last.12.months_bucket",
  "Outstanding.Balance_bucket",
  "Total.No.of.Trades_bucket",
  "No.of.dependents"
)










#Calculate Woe columns values for each of the bucket variables.


woe_columns_buckets <- woe_columns[which(grepl("bucket", woe_columns))]

woe_columns_buckets[8] <-
  "Avgas.CC.Utilization.in.last.12.months_bucket"


for (i in 1:length(woe_columns_buckets))
{
  print(woe_columns_buckets[i])
  new_column_name <- paste0(woe_columns_buckets[i], "_woevalues")
  a <-
    WOETable(factor(master_df[, woe_columns_buckets[i]]), master_df$woe_performance_tag)
  print(a)
  a[, woe_columns_buckets[i]] <- a[, 1]
  sel.id <-
    match(as.character(rejected_applicants[, woe_columns_buckets[i]]), a[, woe_columns_buckets[i]])
  rejected_applicants[, new_column_name] <- a$WOE[sel.id]
  print(summary(rejected_applicants[, new_column_name]))
  #print(IV(factor(rejected_applicants[,woe_columns[i]]),rejected_applicants$woe_performance_tag))
}





rejected_applicants_demo_cred <-
  rejected_applicants[, c(
    "Application.ID",
    "Gender",
    "Marital.Status..at.the.time.of.application.",
    "No.of.dependents",
    "Education",
    "Profession",
    "Type.of.residence",
    "Performance.Tag.x",
    "No.of.times.90.DPD.or.worse.in.last.6.months",
    "No.of.times.60.DPD.or.worse.in.last.6.months",
    "No.of.times.30.DPD.or.worse.in.last.6.months",
    "No.of.times.90.DPD.or.worse.in.last.12.months",
    "No.of.times.60.DPD.or.worse.in.last.12.months",
    "No.of.times.30.DPD.or.worse.in.last.12.months",
    "No.of.trades.opened.in.last.6.months",
    "No.of.trades.opened.in.last.12.months",
    "No.of.PL.trades.opened.in.last.6.months",
    "No.of.PL.trades.opened.in.last.12.months",
    "No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.",
    "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.",
    "Presence.of.open.home.loan",
    "Presence.of.open.auto.loan",
    "Total.No.of.Trades_bucket_woevalues",
    "age_bucket_woevalues",
    "Income_bucket_woevalues",
    "No.of.months.in.current.residence_bucket_woevalues",
    "No.of.months.in.current.company_bucket_woevalues",
    "Avgas.CC.Utilization.in.last.12.months_bucket_woevalues",
    "Outstanding.Balance_bucket_woevalues"
  )]







rejected_applicants_dummified <-
  dummy.data.frame(rejected_applicants_demo_cred)


intersect(colnames(train_df_demo_cred_dummified),
          colnames(rejected_applicants_dummified))

union(colnames(train_df_demo_cred_dummified),
      colnames(rejected_applicants_dummified))


setdiff(colnames(train_df_demo_cred_dummified),
        colnames(rejected_applicants_dummified))

#make cols 0 to match.

rejected_applicants_dummified$Gender <- 0
rejected_applicants_dummified$Marital.Status..at.the.time.of.application. <-
  0
rejected_applicants_dummified$Type.of.residence <- 0

predictions_logit_rejected <-
  predict(logistics_final, newdata = rejected_applicants_dummified[,-22], type = "response")
summary(predictions_logit_rejected)


predictions_logit <-
  predict(logistics_final, newdata = test_df_demo_cred_dummified[,-28], type = "response")
summary(predictions_logit)


#comparing the mean values of predictions between rejected n approved,
#rejected has very high prediction prob of default


setdiff(colnames(train_df_demo_cred_dummified),
        colnames(rejected_applicants_dummified))

setdiff(colnames(train_df_demo_cred_dummified),
        colnames(test_df_demo_cred_dummified))

test_df_demo_cred_dummified$Gender <- 0



rejected_applicants_dummified_reordered <-
  rejected_applicants_dummified[, names(train_df_demo_cred_dummified)]
colnames(rejected_applicants_dummified_reordered)
nrow(rejected_applicants_dummified_reordered)
test_df_demo_cred_dummified_reordered <-
  test_df_demo_cred_dummified[, names(train_df_demo_cred_dummified)]
colnames(test_df_demo_cred_dummified_reordered)
nrow(test_df_demo_cred_dummified_reordered)



masterdf_train_test_rejected_combined <-
  rbind(
    train_df_demo_cred_dummified,
    test_df_demo_cred_dummified_reordered,
    rejected_applicants_dummified_reordered
  )

nrow(masterdf_train_test_rejected_combined)

nrow(rejected_applicants_dummified_reordered) + nrow(train_df_demo_cred_dummified) +
  nrow(test_df_demo_cred_dummified_reordered)

masterdf_train_test_rejected_combined$prob_default <-
  predict(logistics_final, newdata = masterdf_train_test_rejected_combined[, -25] , type = "response")
summary(masterdf_train_test_rejected_combined$prob_default)


masterdf_train_test_rejected_combined$prob_non_default <-
  1 - masterdf_train_test_rejected_combined$prob_default


masterdf_train_test_rejected_combined$odds <-
  log(
    masterdf_train_test_rejected_combined$prob_non_default / masterdf_train_test_rejected_combined$prob_default
  )


summary(masterdf_train_test_rejected_combined$odds)

#
# Application scorecard
# Build an application scorecard with the good to bad odds of 10 to 1 at a score of 400 doubling every 20 points.
# For the rejected population, calculate the application scores and assess the results. Compare the scores of the rejected population with the approved candidates and comment on the observations.
# On the basis of the scorecard, identify the cut - off score below which you would not grant credit cards to applicants.
#

#In our case, PDO = 20, Base Score=400 & odds = 10
Factor = 20 / log(2)
paste0(Factor)

Offset = 400 - (28.8539 * log(10))
paste0(Offset)

masterdf_train_test_rejected_combined$new_score <-
  333.5614 + (28.8539 * masterdf_train_test_rejected_combined$odds)


masterdf_train_test_rejected_combined$predicted_default <-
  ifelse(masterdf_train_test_rejected_combined$prob_default >= 0.04684316,
         1,
         0)


masterdf_train_test_rejected_combined$reject_ind <-
  ifelse(
    is.na(masterdf_train_test_rejected_combined$Performance.Tag.x) == T,
    "Rejected",
    "Approved"
  )


#Average score of rejected is very low as compared to approved.
boxplot(
  masterdf_train_test_rejected_combined$new_score ~ masterdf_train_test_rejected_combined$reject_ind
)


summary(masterdf_train_test_rejected_combined$new_score)

rejected_approved_score_comparison <-
  masterdf_train_test_rejected_combined %>% group_by(reject_ind) %>% summarise(
    min = min(new_score),
    max(new_score),
    mean = mean(new_score),
    percentile_75 = quantile(new_score, 0.75),
    percentile_80 = quantile(new_score, 0.8)
  )

table(
  masterdf_train_test_rejected_combined$predicted_default,
  masterdf_train_test_rejected_combined$reject_ind
)



#Defining the cut off to maximize rejected.
#lets define the cut off to be 405.

masterdf_train_test_rejected_combined$predicted_rejects <-
  ifelse(masterdf_train_test_rejected_combined$new_score >= 405,
         "Approved",
         "Rejected")




confusionMatrix(
  as.factor(masterdf_train_test_rejected_combined$predicted_rejects),
  as.factor(masterdf_train_test_rejected_combined$reject_ind),
  positive = "Rejected"
)

#Model predicts 1099 of the 1425 rejects correctly.

paste0(
  "Model predicts ",
  round((1099 / 1425) * 100, 2),
  "% of the rejected population correctly based on cutoff score"
)



df_conf_matrix <-
  masterdf_train_test_rejected_combined %>% filter(is.na(Performance.Tag.x) ==
                                                     F)


df_conf_matrix$Performance.Tag.x <-
  as.factor(ifelse(df_conf_matrix$Performance.Tag.x == 1, 1, 0))

df_conf_matrix$predicted_default <-
  as.factor(ifelse(df_conf_matrix$predicted_default == 1, 1, 0))


confusionMatrix(df_conf_matrix$predicted_default,
                df_conf_matrix$Performance.Tag.x,
                positive = "1")

table(df_conf_matrix$Performance.Tag.x)


#Using the confusion matrix to evaluate credit loss and revenue loss.
#Credit Loss is the loss occurred from bad customers.

#CreditLoss with No model:4.409669
(2939 / 66649) * 100
#4.409669

#Credit loss with model.:1.651938
((2939 - 1838) / 66649) * 100
#1.651938

#CreditLoss saved 2.757731
paste0(4.409669 - 1.651938, "%")


#If total revenue from all the good customers  is say 100%,
#Revenue loss is incurred by wrongly identified "bad" to the good customers.


revenue_loss = (24530 / (42119 + 24530)) * 100

paste0("Revenue Loss : ", round(revenue_loss, 2), "%")


#############################################################################################################
#Insights & Recommendations

#Predictive modelling should be used for auto approval of applicants
#Credit loss can be reduced from 4.4% to 1.65%. (2.75% saved)
#A revenue loss of 36% might occur if the model is used for auto approval
#Significant variables from logistic regression model indicate the behavior
#of defaulting customers is indicated by recent interactions with bank





#Plot Gain and Lift Charts

lift <- function(labels , predicted_prob, groups = 10) {
  if (is.factor(labels))
    labels  <- as.integer(as.character(labels))
  if (is.factor(predicted_prob))
    predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[, "bucket"] = ntile(-helper[, "predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels), funs(total = n(),
                                    totalresp = sum(., na.rm = TRUE))) %>%
    mutate(
      Cumresp = cumsum(totalresp),
      Gain = Cumresp / sum(totalresp) * 100,
      Cumlift = Gain / (bucket * (100 / groups))
    )
  return(gaintable)
}

masterdf_train_test_rejected_combined <-
  masterdf_train_test_rejected_combined[order(masterdf_train_test_rejected_combined$prob_default,
                                              decreasing = T),]


default_predictions <-
  as.factor(ifelse(
    masterdf_train_test_rejected_combined$predicted_default == 1,
    1,
    0
  ))


LG = lift(default_predictions,
          masterdf_train_test_rejected_combined$prob_default,
          groups = 10)

plot(
  LG$bucket,
  LG$Gain,
  col = "red",
  type = "l",
  main = "Gain Chart",
  xlab = "% of total targeted",
  ylab = "% of defaulters"
)



#Model identifies 76% deafaulters in top 3 deciles
#Model identifies all defaulters within 40% of data.
# Lift Chart

plot(
  LG$bucket,
  LG$Cumlift,
  col = "red",
  type = "l",
  main = "Lift Chart",
  xlab = "% of total targeted",
  ylab = "Lift"
)
