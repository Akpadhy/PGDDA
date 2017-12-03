#################### Loading libraries ##################################
library(lubridate)
library(stringr)
library(dplyr)
library(ggplot2)
library(forcats)
library(caTools)
library(MASS)
library(car)
library(e1071)
library(caret)
library(ROCR)
library(cowplot)

#################### Cleaning the environment ############################
rm(list = ls())

#################### Importing files into R ##############################
setwd(
  "C:/Users/egargas/Google Drive/PGDDA_DDA1710439/Course 3 Predictive Analysis 1/Module 5 Group Study HR Case Study"
)
surveyData <-
  read.csv(
    "general_data.csv",
    na.strings = c("", " ", "NA"),
    stringsAsFactors = FALSE
  )

employeeData <-
  read.csv("employee_survey_data.csv", stringsAsFactors = FALSE)
managerData <-
  read.csv("manager_survey_data.csv", stringsAsFactors = FALSE)

inData <-
  read.csv("in_time.csv",
           stringsAsFactors = FALSE,
           header = TRUE)

outData <-
  read.csv("out_time.csv",
           stringsAsFactors = FALSE,
           header = TRUE)

#################### Merging the data ####################################
## checking structures of imported data frames
str(surveyData)
str(employeeData)
str(inData)
str(outData)
str(managerData)

## Checking if employee id is same in all the imported data frames
length(unique(surveyData$EmployeeID))  #4410, employee id is unique for each row
length(unique(employeeData$EmployeeID))#4410, employee id is unique for each row
length(unique(managerData$EmployeeID)) #4410, employee id is unique for each row
length(unique(inData$X))               #4410, employee id is unique for each row
length(unique(outData$X))              #4410, employee id is unique for each row

setdiff(surveyData$EmployeeID, inData$X)  # Identical employee id
setdiff(inData$X, outData$X)              # Identical employee id
setdiff(surveyData$EmployeeID, managerData$EmployeeID)  # Identical employee id
setdiff(surveyData$EmployeeID, employeeData$EmployeeID) # Identical employee id

## Calculating total working time for the employees
totalCol <- ncol(inData)
workingTime <- inData
for (i in 2:totalCol)
{
  workingTime[, i] <- ifelse(is.na(outData[, i]),
                             NA,
                             parse_date_time(outData[, i], "Y-m-d H:M:S")) -
    ifelse(is.na(inData[, i]),
           NA,
           parse_date_time(inData[, i], "Y-m-d H:M:S"))
}

ncol(workingTime)
str(workingTime)

## Calculating mean time for employees for the whole period
meanWorkingTime <- as.data.frame(workingTime[, 1])
meanWorkingTime <-
  cbind(meanWorkingTime, meanWorkingTime =
          rowMeans(workingTime[, -1], na.rm = TRUE))
names(meanWorkingTime)[1] <- "EmployeeID"
sum(is.na(meanWorkingTime))

#Merging the files
surveyData1 <-
  merge(surveyData, meanWorkingTime, by = "EmployeeID")
surveyData1 <- merge(surveyData1, managerData, by = "EmployeeID")
surveyData1 <- merge(surveyData1, employeeData, by = "EmployeeID")

#################### Cleaning data #######################################
#################### Checking for NA and duplicate rows #################
sum(is.na(surveyData1))     # 111 values are NA

sapply(surveyData1, function(x)
  length(which(is.na(x))))
# NumCompaniesWorked  19 are NA
# TotalWorkingYears 9 are NA
# EnvironmentSatisfaction 25 are NA
# JobSatisfaction 10 are NA
# WorkLifeBalance 38 are NA
# View(subset(surveyData1, is.na(JobSatisfaction))) # All 11 NAs in TotalCharges correspond to tenure 0
# View(subset(surveyData1, surveyData1$`EducationFieldHuman Resources`==0)) # All 11 NAs in TotalCharges correspond to tenure 0

surveyData1$NumCompaniesWorked[which(is.na(surveyData1$NumCompaniesWorked))] <-
  median(surveyData1$NumCompaniesWorked, na.rm = TRUE)

surveyData1$TotalWorkingYears[which(is.na(surveyData1$TotalWorkingYears))] <-
  median(surveyData1$TotalWorkingYears, na.rm = TRUE)

surveyData1$EnvironmentSatisfaction[which(is.na(surveyData1$EnvironmentSatisfaction))] <-
  median(surveyData1$EnvironmentSatisfaction, na.rm = TRUE)

surveyData1$JobSatisfaction[which(is.na(surveyData1$JobSatisfaction))] <-
  median(surveyData1$JobSatisfaction, na.rm = TRUE)

surveyData1$WorkLifeBalance[which(is.na(surveyData1$WorkLifeBalance))] <-
  median(surveyData1$WorkLifeBalance, na.rm = TRUE)

sum(is.na(surveyData1))     # Now, 0 values are NA

sum(sapply(surveyData1, function(x)
  length(which(x == " "))))      # 0 values are blank values

sum(sapply(surveyData1, function(x)
  length(which(x == ""))))     # 0 values are present with space character only

sum(duplicated(surveyData1)) #0, So no duplicate entries

#################### Checking for outliers ##############################
str(surveyData1)
ggplot(data = surveyData1, aes(x = Attrition, y = Age)) +
  geom_boxplot() + labs(x = "Attrition", y = "Age")   # No outliers

ggplot(data = surveyData1, aes(x = Attrition, y = DistanceFromHome)) +
  geom_boxplot() + labs(x = "Attrition", y = "Distance From Home") # No outliers

ggplot(data = surveyData1, aes(x = Attrition, y = Education)) +
  geom_boxplot() + labs(x = "Attrition", y = "Education") # No outliers

ggplot(data = surveyData1, aes(x = Attrition, y = EmployeeCount)) +
  geom_boxplot() + labs(x = "Attrition", y = "Employee Count") # Static value for all rows
table(surveyData1$EmployeeCount)       # Only single value is present for all rows, can be removed

ggplot(data = surveyData1, aes(x = Attrition, y = MonthlyIncome)) +
  geom_boxplot() + labs(x = "Attrition", y = "Monthly Income") # outliers, trimming at 94%
quantile(surveyData1$MonthlyIncome, seq(0, 1, .01))
surveyData1$MonthlyIncome[surveyData1$MonthlyIncome > 171740] <-
  171740

ggplot(data = surveyData1, aes(x = Attrition, y = NumCompaniesWorked)) +
  geom_boxplot() + labs(x = "Attrition", y = "Number Companies Worked")   # No outliers

ggplot(data = surveyData1, aes(x = Attrition, y = PercentSalaryHike)) +
  geom_boxplot() + labs(x = "Attrition", y = "Percent Salary Hike")   # No outliers

ggplot(data = surveyData1, aes(x = Attrition, y = StandardHours)) +
  geom_boxplot() + labs(x = "Attrition", y = "Standard Hours")
table(surveyData1$StandardHours)  # Only single value is present for all rows, can be removed

ggplot(data = surveyData1, aes(x = Attrition, y = StockOptionLevel)) +
  geom_boxplot() + labs(x = "Attrition", y = "StockOptionLevel")   # No outliers

ggplot(data = surveyData1, aes(x = Attrition, y = TotalWorkingYears)) +
  geom_boxplot() + labs(x = "Attrition", y = "Total Working Years")
quantile(surveyData1$TotalWorkingYears, seq(0, 1, .01), na.rm = TRUE) ## Outliers, trimmed at 98%
surveyData1$TotalWorkingYears[surveyData1$TotalWorkingYears > 32] <-
  32

ggplot(data = surveyData1, aes(x = Attrition, y = TrainingTimesLastYear)) +
  geom_boxplot() + labs(x = "Attrition", y = "Training Times Last Year")   # No outliers
table(surveyData1$TrainingTimesLastYear)  ## No outliers

ggplot(data = surveyData1, aes(x = Attrition, y = YearsAtCompany)) +
  geom_boxplot() + labs(x = "Attrition", y = "YearsAtCompany")
quantile(surveyData1$YearsAtCompany, seq(0, 1, .01), na.rm = TRUE)  ## Outliers, trimmed at 98%
surveyData1$YearsAtCompany[surveyData1$YearsAtCompany > 24] <- 24

ggplot(data = surveyData1, aes(x = Attrition, y = YearsSinceLastPromotion)) +
  geom_boxplot() + labs(x = "Attrition", y = "Years Since Last Promotion")
quantile(surveyData1$YearsSinceLastPromotion, seq(0, 1, .01), na.rm = TRUE)  ## Outliers, trimmed at 95%
surveyData1$YearsSinceLastPromotion[surveyData1$YearsSinceLastPromotion > 9] <-
  9

ggplot(data = surveyData1, aes(x = Attrition, y = YearsWithCurrManager)) +
  geom_boxplot() + labs(x = "Attrition", y = "Years With Current Manager")
quantile(surveyData1$YearsWithCurrManager, seq(0, 1, .01), na.rm = TRUE)  ## Outliers, trimmed at 98%
surveyData1$YearsWithCurrManager[surveyData1$YearsWithCurrManager > 14] <-
  14

ggplot(data = surveyData1, aes(x = Attrition, y = meanWorkingTime)) +
  geom_boxplot() + labs(x = "Attrition", y = "Mean Working Time of Employee")
quantile(surveyData1$meanWorkingTime, seq(0, 1, .01), na.rm = TRUE)   # No outliers

ggplot(data = surveyData1, aes(x = Attrition, y = JobInvolvement)) +
  geom_boxplot() + labs(x = "Attrition", y = "Job Involvement")    # No outliers

ggplot(data = surveyData1, aes(x = Attrition, y = PerformanceRating)) +
  geom_boxplot() + labs(x = "Attrition", y = "Performance Rating")
factor(surveyData1$PerformanceRating) ## 2 levels, so converting them to 0 and 1
surveyData1$PerformanceRating <-
  ifelse(surveyData1$PerformanceRating == 3, 0, 1)

ggplot(data = surveyData1, aes(x = Attrition, y = EnvironmentSatisfaction)) +
  geom_boxplot() + labs(x = "Attrition", y = "EnvironmentSatisfaction") # No outliers
ggplot(data = surveyData1, aes(x = Attrition, y = JobSatisfaction)) +
  geom_boxplot() + labs(x = "Attrition", y = "Job Satisfaction") # No outliers
ggplot(data = surveyData1, aes(x = Attrition, y = WorkLifeBalance)) +
  geom_boxplot() + labs(x = "Attrition", y = "WorkLifeBalance") # No outliers

#################### Checking relationship between columns ##############
str(surveyData1)
bar_theme1 <-
  theme(axis.text.x = element_text(
    angle = 90,
    hjust = 1,
    vjust = 0.5
  ),
  legend.position = "none")
plot_grid(
  ggplot(surveyData1, aes(
    x = BusinessTravel, fill = as.factor(Attrition)
  )) + geom_bar() + bar_theme1,
  ggplot(surveyData1, aes(
    x = Department, fill = as.factor(Attrition)
  )) + geom_bar() + bar_theme1,
  ggplot(surveyData1, aes(
    x = EducationField, fill = as.factor(Attrition)
  )) + geom_bar() + bar_theme1,
  ggplot(surveyData1, aes(
    x = JobRole, fill = as.factor(Attrition)
  )) + geom_bar() + bar_theme1,
  ggplot(surveyData1, aes(
    x = MaritalStatus, fill = as.factor(Attrition)
  )) + geom_bar() + labs(fill = "Attrition"),
  align = "h"
)

ggplot(data = surveyData1, aes(x = DistanceFromHome, y = meanWorkingTime)) +
  geom_point() + labs(x = "Distance from Home" ,
                      y = "Mean Working Time")
# More the distance from home for the person, lesser is the working time

ggplot(data = surveyData1, aes(x = YearsAtCompany,
                               y = YearsWithCurrManager)) +
  geom_point(aes(col = Attrition)) + labs(x = "Years at company" ,
                                          y = "Years with Current Manager")
# People staying at same company for a longer period tends to work with same manager

#################### Checking character columns ##########################
surveyData1$Attrition <- ifelse(surveyData1$Attrition == "No", 0, 1)

surveyData1$BusinessTravel <-
  as.factor(surveyData1$BusinessTravel) ## 3 levels, dummy to be created
levels(surveyData1$BusinessTravel)

surveyData1$Department <-
  as.factor(surveyData1$Department)  ## 3 levels, dummy to be created
levels(surveyData1$Department)

surveyData1$EducationField <-
  as.factor(surveyData1$EducationField)  ## 6 levels, dummy to be created
levels(surveyData1$EducationField)

surveyData1$Gender <- ifelse(surveyData1$Gender == "Female", 0, 1)

surveyData1$JobRole <- as.factor(surveyData1$JobRole)
levels(surveyData1$JobRole) # 9 levels, dummy to be created

surveyData1$MaritalStatus <- as.factor(surveyData1$MaritalStatus)
levels(surveyData1$MaritalStatus) # 3 levels, dummy to be created

surveyData1$Over18 <- as.factor(surveyData1$Over18)
table(surveyData1$Over18) # all rows are mapped to Y, so this field can be removed from analysis

#################### Removing employee id and other columns having single value ####
indices <-
  which(names(surveyData1) %in% c("EmployeeID", "EmployeeCount", "StandardHours" , "Over18"))
surveyData1 <- surveyData1[,-indices]

#################### Converting character columns to dummy variables #####
dummyBusinessTravel <-
  model.matrix(~ BusinessTravel - 1, data = surveyData1)
surveyData1 <-
  cbind(surveyData1, dummyBusinessTravel[,-ncol(dummyBusinessTravel)])

## Converting Department column
dummyDepartment <-
  model.matrix(~ Department - 1, data = surveyData1)
surveyData1 <-
  cbind(surveyData1, dummyDepartment[,-ncol(dummyDepartment)])

dummyEducationField <-
  model.matrix(~ EducationField - 1, data = surveyData1)
surveyData1 <-
  cbind(surveyData1, dummyEducationField[,-ncol(dummyEducationField)])

dummyJobRole <- model.matrix(~ JobRole - 1, data = surveyData1)
surveyData1 <-
  cbind(surveyData1, dummyJobRole[,-ncol(dummyJobRole)])

dummyMaritalStatus <-
  model.matrix(~ MaritalStatus - 1, data = surveyData1)
surveyData1 <-
  cbind(surveyData1, dummyMaritalStatus[,-ncol(dummyMaritalStatus)])

indices1 <-
  which(
    names(surveyData1) %in% c(
      "BusinessTravel",
      "Department",
      "EducationField" ,
      "JobRole",
      "MaritalStatus"
    )
  )
surveyData1 <- surveyData1[,-indices1]
str(surveyData1)

#################### Scaling the dataset ###############################
indices2 <-
  which(
    names(surveyData1) %in% c(
      "Age",
      "DistanceFromHome",
      "Education" ,
      "MonthlyIncome",
      "PercentSalaryHike",
      "TotalWorkingYears",
      "TrainingTimesLastYear",
      "YearsAtCompany",
      "YearsSinceLastPromotion",
      "YearsWithCurrManager",
      "meanWorkingTime",
      "JobInvolvement",
      "PerformanceRating",
      "EnvironmentSatisfaction",
      "JobSatisfaction",
      "WorkLifeBalance"
    )
  )

surveyData2 <-
  as.data.frame(sapply(surveyData1[, indices2], scale, simplify = TRUE))

surveyData3 <-
  as.data.frame(cbind(surveyData2,
                      surveyData1[, -indices2]))
str(surveyData3)

#################### Dividing the data into test and train ###############
set.seed(100)
rowIndices <- sample.split(surveyData3, SplitRatio = .7)
trainData <- surveyData3[rowIndices,]
testData <- surveyData3[!rowIndices,]

#################### Creating model #####################################
model1 <-
  glm(Attrition ~ ., data = trainData, family = "binomial")
summary(model1)  ## AIC: 2161.3

model2 <- stepAIC(model1, direction = "both")
summary(model2) ## AIC: 2140.8
vif(model2)   ## All Vif value are normal
## removing JobLevel due to high p value

model3 <-
  glm(
    formula = Attrition ~ Age + Education + NumCompaniesWorked +
      TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion +
      YearsWithCurrManager + meanWorkingTime + JobInvolvement +
      EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance +
      `BusinessTravelNon-Travel` + BusinessTravelTravel_Frequently +
      `DepartmentHuman Resources` + `EducationFieldHuman Resources` +
      `JobRoleHealthcare Representative` + `JobRoleLaboratory Technician` +
      `JobRoleResearch Director` + `JobRoleResearch Scientist` +
      `JobRoleSales Executive` + MaritalStatusDivorced + MaritalStatusMarried,
    family = "binomial",
    data = trainData
  )
summary(model3) ## AIC: 2141.3
## removing `JobRoleHealthcare Representative` due to high p value

model4 <-
  glm(
    formula = Attrition ~ Age + Education + NumCompaniesWorked +
      TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion +
      YearsWithCurrManager + meanWorkingTime + JobInvolvement +
      EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance +
      `BusinessTravelNon-Travel` + BusinessTravelTravel_Frequently +
      `DepartmentHuman Resources` + `EducationFieldHuman Resources` +
      `JobRoleLaboratory Technician` +
      `JobRoleResearch Director` + `JobRoleResearch Scientist` +
      `JobRoleSales Executive` + MaritalStatusDivorced + MaritalStatusMarried,
    family = "binomial",
    data = trainData
  )
summary(model4) ## AIC: 2141.9
## removing `JobRoleResearch Scientist` due to high p value

model5 <-
  glm(
    formula = Attrition ~ Age + Education + NumCompaniesWorked +
      TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion +
      YearsWithCurrManager + meanWorkingTime + JobInvolvement +
      EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance +
      `BusinessTravelNon-Travel` + BusinessTravelTravel_Frequently +
      `DepartmentHuman Resources` + `EducationFieldHuman Resources` +
      `JobRoleLaboratory Technician` +
      `JobRoleResearch Director` +
      `JobRoleSales Executive` + MaritalStatusDivorced + MaritalStatusMarried,
    family = "binomial",
    data = trainData
  )
summary(model5) ## AIC: 2141.4
## removing `JobRoleLaboratory Technician` due to high p value

model6 <-
  glm(
    formula = Attrition ~ Age + Education + NumCompaniesWorked +
      TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion +
      YearsWithCurrManager + meanWorkingTime + JobInvolvement +
      EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance +
      `BusinessTravelNon-Travel` + BusinessTravelTravel_Frequently +
      `DepartmentHuman Resources` + `EducationFieldHuman Resources` +
      `JobRoleResearch Director` +
      `JobRoleSales Executive` + MaritalStatusDivorced + MaritalStatusMarried,
    family = "binomial",
    data = trainData
  )
summary(model6) ## AIC:  2141.7
## removing EnvironmentSatisfaction due to high p value

model7 <-
  glm(
    formula = Attrition ~ Age + Education + NumCompaniesWorked +
      TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion +
      YearsWithCurrManager + meanWorkingTime + JobInvolvement +
      JobSatisfaction + WorkLifeBalance +
      `BusinessTravelNon-Travel` + BusinessTravelTravel_Frequently +
      `DepartmentHuman Resources` + `EducationFieldHuman Resources` +
      `JobRoleResearch Director` +
      `JobRoleSales Executive` + MaritalStatusDivorced + MaritalStatusMarried,
    family = "binomial",
    data = trainData
  )
summary(model7) ## AIC: 2186.8
## removing JobInvolvement due to less significance

model8 <-
  glm(
    formula = Attrition ~ Age + Education + NumCompaniesWorked +
      TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion +
      YearsWithCurrManager + meanWorkingTime +
      JobSatisfaction + WorkLifeBalance +
      `BusinessTravelNon-Travel` + BusinessTravelTravel_Frequently +
      `DepartmentHuman Resources` + `EducationFieldHuman Resources` +
      `JobRoleResearch Director` +
      `JobRoleSales Executive` + MaritalStatusDivorced + MaritalStatusMarried,
    family = "binomial",
    data = trainData
  )
summary(model8) ## AIC: 2187.8
## removing `DepartmentHuman Resources` due to less significance

model9 <-
  glm(
    formula = Attrition ~ Age + Education + NumCompaniesWorked +
      TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion +
      YearsWithCurrManager + meanWorkingTime +
      JobSatisfaction + WorkLifeBalance +
      `BusinessTravelNon-Travel` + BusinessTravelTravel_Frequently +
      `EducationFieldHuman Resources` +
      `JobRoleResearch Director` +
      `JobRoleSales Executive` + MaritalStatusDivorced + MaritalStatusMarried,
    family = "binomial",
    data = trainData
  )
summary(model9) ## AIC: 2188.5
## removing Education due to less significance

model10 <-
  glm(
    formula = Attrition ~ Age + NumCompaniesWorked +
      TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion +
      YearsWithCurrManager + meanWorkingTime +
      JobSatisfaction + WorkLifeBalance +
      `BusinessTravelNon-Travel` + BusinessTravelTravel_Frequently +
      `EducationFieldHuman Resources` +
      `JobRoleResearch Director` +
      `JobRoleSales Executive` + MaritalStatusDivorced + MaritalStatusMarried,
    family = "binomial",
    data = trainData
  )
summary(model10) ## AIC: 2189.9
## removing `JobRoleResearch Director` due to less significance

model11 <-
  glm(
    formula = Attrition ~ Age + NumCompaniesWorked +
      TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion +
      YearsWithCurrManager + meanWorkingTime +
      JobSatisfaction + WorkLifeBalance +
      `BusinessTravelNon-Travel` + BusinessTravelTravel_Frequently +
      `EducationFieldHuman Resources` +
      `JobRoleSales Executive` + MaritalStatusDivorced + MaritalStatusMarried,
    family = "binomial",
    data = trainData
  )
summary(model11) ## AIC: 2192
## removing `JobRoleSales Executive` due to less significance

model12 <-
  glm(
    formula = Attrition ~ Age + NumCompaniesWorked +
      TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion +
      YearsWithCurrManager + meanWorkingTime +
      JobSatisfaction + WorkLifeBalance +
      `BusinessTravelNon-Travel` + BusinessTravelTravel_Frequently +
      `EducationFieldHuman Resources` +
      MaritalStatusDivorced + MaritalStatusMarried,
    family = "binomial",
    data = trainData
  )
summary(model12) ## AIC: 2194
## removing BusinessTravelNon_Travel due to less significance

model13 <-
  glm(
    formula = Attrition ~ Age + NumCompaniesWorked +
      TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion +
      YearsWithCurrManager + meanWorkingTime +
      JobSatisfaction + WorkLifeBalance +
      `BusinessTravelTravel_Frequently` +
      `EducationFieldHuman Resources` +
      MaritalStatusDivorced + MaritalStatusMarried,
    family = "binomial",
    data = trainData
  )
summary(model13) ## AIC: 2203.8
## removing BusinessTravelTravel_Frequently due to less significance

finalModel <- model13

test_prediction <-
  predict(finalModel, type = "response", newdata = testData[,-which(names(testData) %in% "Attrition")])
summary(test_prediction)
summary(surveyData1$Attrition)

test_predAttrition <-
  factor(ifelse(test_prediction >= 0.50, "Yes", "No"))
test_actualAttrition <-
  factor(ifelse(testData$Attrition == 1, "Yes", "No"))

table(test_predAttrition, test_actualAttrition)

test_predAttrition <-
  factor(ifelse(test_prediction >= 0.40, "Yes", "No"))
test_conf <-
  confusionMatrix(test_predAttrition, test_actualAttrition, positive = "Yes")
test_conf

#################### Choosing cutoff value ##############################
perform_fn <- function(cutoff,
                       predictedAttrition,
                       actualAttrition)
{
  predicted_attrition <-
    factor(ifelse(predictedAttrition >= cutoff, "Yes", "No"))
  conf <-
    confusionMatrix(predicted_attrition, actualAttrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc)))
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Summary of test probability
summary(test_predAttrition)

s = seq(.01, .80, length = 100)
OUT = matrix(0, 100, 3)

for (i in 1:100)
{
  OUT[i, ] = perform_fn(s[i], test_prediction, test_actualAttrition)
}


## Plotting graph between accuracy, sensitivity and specificity
plot(
  s,
  OUT[, 1],
  xlab = "Cutoff",
  ylab = "Values",
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
  .65,
  1,
  col = c(2, "darkgreen", 2, "darkred"),
  lwd = c(1, 1, 1, 1),
  c("Sensitivity", "Specificity", "Accuracy")
)

## Finding best cutoff value
cutoff <- s[which(abs(OUT[, 1] - OUT[, 2]) < 0.02)]

cutoff  # 0.1775758
test_cutoff_attrition = perform_fn(cutoff, test_prediction, test_actualAttrition)
test_cutoff_attrition  # sens #0.706422 # spec #0.7180417 # acc #0.7161241
################### KS -statistic - for Test Data ########################

test_cutoff_attrition <- ifelse(test_prediction >= cutoff, 1, 0)
test_actual_attrition <- ifelse(test_actualAttrition == "Yes", 1, 0)

#KS for testing  data
pred_object_test <-
  prediction(test_cutoff_attrition, test_actual_attrition)

performance_measures_test <-
  performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] -
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test) #0.4244637
################### Lift & Gain Chart ###################################
lift <- function(labels , test_prediction, groups = 10) {
  if (is.factor(labels))
    labels  <- as.integer(as.character(labels))
  if (is.factor(test_prediction))
    test_prediction <- as.integer(as.character(test_prediction))
  helper = data.frame(cbind(labels , test_prediction))
  helper[, "bucket"] = ntile(-helper[, "test_prediction"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels),
                 funs(total = n(),
                      totalresp = sum(., na.rm = TRUE))) %>%
    mutate(
      Cumresp = cumsum(totalresp),
      Gain = Cumresp / sum(totalresp) * 100,
      Cumlift = Gain / (bucket * (100 / groups))
    )
  return(gaintable)
}

attrition_decile = lift(test_actual_attrition, test_prediction, groups = 10)
attrition_decile

baseModel <-
  as.data.frame(cbind(
    Decile = c(1:10),
    Gain = seq(10, 100, 10),
    Lift = 1
  ))

## Plotting Gain chart
ggplot(data = attrition_decile, aes(x = bucket, y = Gain), col = black) + geom_line() +
  scale_x_continuous(breaks = c(1:10)) +
  labs(x = "Decile", y = "Gain %") +
  geom_point() + theme_bw() +
  geom_line(data = baseModel, aes(x = Decile, y = Gain), col = "red") +
  geom_point(data = baseModel, aes(x = Decile, y = Gain), col = "red")

## Plotting Lift chart
ggplot(data = attrition_decile, aes(x = bucket, y = Cumlift)) + geom_line() +
  scale_x_continuous(breaks = c(1:10)) + 
  labs(x = "Decile", y = "Lift %") +
  geom_point() + theme_bw() + geom_line(data = baseModel, aes(x = Decile, y =
                                                                  Lift), col = "red") +
  geom_point(data = baseModel, aes(x = Decile, y = Lift), col = "red")
