library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(plyr)
library(scales)
library(MASS)
library(car)
library(lubridate)
library(caTools)
library(e1071)
library(cowplot)
library(ROCR)




general_data <- read.csv("general_data.csv")
manager_survey <- read.csv("manager_Survey_data.csv")
employee_survey <- read.csv("employee_Survey_data.csv")
in_time<-read.csv("in_time.csv",header=TRUE,stringsAsFactors = F)
out_time<-read.csv("out_time.csv",header=TRUE,stringsAsFactors = F)


#----------------EDA--------------------------------------------

#Data Preparation

#NA value
sapply(general_data, function(x) sum(is.na(x))) 
# shows 19 NAs in NumCompaniesWorked and 9 in TotalWorkingYears

sapply(manager_survey, function(x) sum(is.na(x)))
#no na values

sapply(employee_survey, function(x) sum(is.na(x)))
#it has few na values
# EmployeeID EnvironmentSatisfaction         JobSatisfaction  WorkLifeBalance 
#   0                      25                      20                 38 


#1. check for duplicated values 

#for general data :0
sum(duplicated(general_data))

#for manager df : 0
sum(duplicated(manager_survey))

#for employee df : there are 0
sum(duplicated(employee_survey))

#2.2 check for blank values : no missing values in any column
sapply(general_data, function(x) length(which(x == "")))


#2.2 check for blank values : no missing values in any column
sapply(manager_survey, function(x) length(which(x == "")))


#2.2 check for blank values : no missing values in any column
sapply(employee_survey, function(x) length(which(x == "")))

#number of unique values in each column for general_data
#EmployeeCount ,Over18 & StandardHours have only 1 unique value so we will remove them
sapply(general_data, function(x) length(unique(x)))

general_data <- subset(general_data,select= -c(EmployeeCount , Over18 , StandardHours))


#number of unique values in each column for manager_survey
#each column has 2 or above unique values
sapply(manager_survey, function(x) length(unique(x)))

#number of unique values in each column for employee_survey
#each column has 2 or above unique values
sapply(employee_survey, function(x) length(unique(x)))

#merging all files
merge_all <- function(x, y){
  master_data <- merge(x, y, by= "EmployeeID", all.x= TRUE, all.y= TRUE) 
  return(master_data)
  }
master_data <- Reduce(merge_all, list(general_data, employee_survey, manager_survey))

#--------------------time_in : time_out manipulation-------------------------------------

# 1.5 Checking if the EmployeeID column in all data frames is the same

setdiff(employee_survey$EmployeeID, general_data$EmployeeID)
setdiff(employee_survey$EmployeeID, manager_survey$EmployeeID)


in_time <- in_time[,colSums(is.na(in_time))<nrow(in_time)]

out_time <- out_time[,apply(out_time, 2, function(a) length(unique(a)) !=1)]

#checking if we only have leaves as na and no other discrepancy : we get TRUE
sum(is.na(in_time))==sum(is.na(out_time))

#also we have the same indexes as NA in both , making our clain a certainty
sum(which(is.na(in_time))!= which(is.na(out_time)))

#calculating leaves
leaves <- apply(in_time, 1, function(a) sum(is.na(a)))

# Computing the average working hours for every employee

temp_in_time <- as.data.frame(sapply(in_time[,-1],function(x) as.POSIXlt(x, format = "%Y-%m-%d %H:%M:%S")))
temp_out_time <- as.data.frame(sapply(out_time[,-1],function(x) as.POSIXlt(x, format = "%Y-%m-%d %H:%M:%S")))

diff <- as.data.frame(mapply( FUN = `-` , temp_out_time[,] , temp_in_time[,] ))

avgwrkhrs <- rowMeans(diff , na.rm = TRUE)
#avgwrkhrs <- na.omit(avgwrkhrs)

#since standard time is 8 hrs we see the frequency of early exits
early_exits <- apply(diff , 1 , function(x) sum(x<8.00 , na.rm = TRUE))

#we let time in diff be in numeric for modelling purposes

#combining these vectors to master_data
master_data <- Reduce(cbind , list(master_data,leaves , avgwrkhrs , early_exits))

#changing column names as per their description
colnames(master_data)[27] <- "leaves"
colnames(master_data)[28] <- "avg_working_hrs"
colnames(master_data)[29] <- "early_exit_count"

#----------------------------------------------------------------------

#checking for attritions across na values

master_data$Attrition[is.na(master_data$JobSatisfaction)] # just 1 attrition

#for others no-yes attrition are spread out
master_data$Attrition[is.na(master_data$WorkLifeBalance)]

master_data$Attrition[is.na(master_data$EnvironmentSatisfaction)]
master_data$Attrition[is.na(master_data$NumCompaniesWorked)]
master_data$Attrition[is.na(master_data$TotalWorkingYears)]

#checking total na : 111
sum(is.na(master_data))

#count of Attritions in na rows : 16
sum(is.na(master_data[master_data$Attrition == "Yes" ,]))

#count of Non-Attritions in na rows : 95
sum(is.na(master_data[master_data$Attrition == "No" ,]))

#count of total Attrition and Non attrition in data set
length(master_data$EmployeeID[master_data$Attrition == "No"]) #3699
length(master_data$EmployeeID[master_data$Attrition == "Yes"]) #711

#so if we remove na rows we will reduce count of yes and no attrition by
#2.2 and 2.5 percentage , which is not a huge value
#hence we remove the rows which contain NA values
16/711 #2.2%
95/3699 #2.5%

#since na values are quite low < 5% we remove the rows which has na values

#master_data <- master_data[complete.cases(master_data), ]

#sum(is.na(master_data))

master_data <- na.omit(master_data)

#checking na across all columns : since these two columns dont have na across them
#we can say there won't be a single record which has all na's in these columns
master_data$EmployeeID[is.na(master_data$JobSatisfaction && master_data$WorkLifeBalance)]

#-------------------------FActor 

#converting columns to factors
cols <- c(7,10,14,16,18,22,23,24,25,26)

master_data[cols] <- lapply(master_data[cols], factor)

#converting NumCompaniesWorked into a factor
levels(master_data$NumCompaniesWorked) <- c('0', '1' , '2-4' , '2-4' , '2-4' , '5+' , '5+' ,'5+' , '5+' , '5+' )

#converting TrainingTimesLastYear into a factor
levels(master_data$TrainingTimesLastYear) <- c('<2', '<2' , '2' , '3' , '3+' , '3+' , '3+')

str(master_data)

# Function for distribution of categorical variables (plotting bar charts)

#Univariate and Segmented univariate analysis
univariate_categorical <- function(dataset,var,var_name){
  
  dataset %>% ggplot(aes(x = as.factor(var))) +
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + 
    scale_y_continuous(labels = percent) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = var_name, y = percent, x = var_name)+theme(
      axis.text.y=element_blank(), axis.ticks=element_blank(),
      axis.title.y=element_blank()
    ) 
}

univariate_categorical(master_data,master_data$JobSatisfaction,"job satisfaction")
univariate_categorical(master_data,master_data$EnvironmentSatisfaction,"EnvironmentSatisfaction")
univariate_categorical(master_data,master_data$WorkLifeBalance,"WorkLifeBalance")
univariate_categorical(master_data,master_data$JobInvolvement,"JobInvolvement")
univariate_categorical(master_data,master_data$PerformanceRating,"PerformanceRating")
univariate_categorical(master_data,master_data$StockOptionLevel,"StockOptionLevel")
univariate_categorical(master_data,master_data$MaritalStatus,"MaritalStatus")
univariate_categorical(master_data,master_data$NumCompaniesWorked,"NumCompaniesWorked")
univariate_categorical(master_data,master_data$JobRole,"JobRole")
univariate_categorical(master_data,master_data$JobRole,"JobRole")
univariate_categorical(master_data,master_data$Gender,"Gender")
univariate_categorical(master_data,master_data$EducationField,"EducationField")
univariate_categorical(master_data,master_data$Department,"Department")
univariate_categorical(master_data,master_data$BusinessTravel,"BusinessTravel")

univariate_categorical(master_data,master_data$Attrition,"Attrition")

# Barcharts for categorical features with stacked telecom information
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")



plot_grid(ggplot(master_data, aes(x=BusinessTravel,fill=Attrition))+ geom_bar() + bar_theme1, 
          ggplot(master_data, aes(x=Department,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(master_data, aes(x=EducationField,fill=Attrition))+ geom_bar()+bar_theme1,
          align ="h")

plot_grid(ggplot(master_data, aes(x=JobRole,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(master_data, aes(x=MaritalStatus,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(master_data, aes(x=Gender,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")   

plot_grid(ggplot(master_data, aes(x=TrainingTimesLastYear,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(master_data, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(master_data, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")   

plot_grid(ggplot(master_data, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(master_data, aes(x=JobInvolvement,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(master_data, aes(x=PerformanceRating,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")   

plot_grid(ggplot(master_data, aes(x=StockOptionLevel,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(master_data, aes(x=NumCompaniesWorked,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(master_data, aes(x=JobLevel,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(master_data, aes(x=Education,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")   

# Correlation between numeric variables
library(GGally)
ggpairs(master_data[, c("Age", "DistanceFromHome", "MonthlyIncome" , "PercentSalaryHike" , "TotalWorkingYears" , "YearsAtCompany" ,"YearsSinceLastPromotion" , "YearsWithCurrManager" , "early_exit_count" , "avg_working_hrs" , "leaves" )])

#As expected, YearsAtCompany, YearsSinceLastPromotion ,YearsWithCurrManager & TotalWorkingYears
#are highly correlated , age is also highly correlated with TotalWorkingYears and
#moderately correlated with the other 3 , also early_exit is highly correlated with avg_working_hours




#----------------------------------Outlier Treatment---------------------

#whenever there is a extreme jump in the value we cap the outlier values

#no outlier
quantile(master_data$Age , seq(0,1,0.01))

#no outlier
quantile(master_data$DistanceFromHome , seq(0,1,0.01))

#there is a jump at 89% but we cannot lose so much data points
#there is also a jump at 91%
#we will scale this data set
quantile(master_data$MonthlyIncome , seq(0,1,0.01))

boxplot(master_data$MonthlyIncome)

#no outlier: categorical in nature
quantile(master_data$NumCompaniesWorked , seq(0,1,0.01) , na.rm = TRUE)


#no outlier
quantile(master_data$PercentSalaryHike , seq(0,1,0.01))

#there is a jump so we cap it
quantile(master_data$TotalWorkingYears , seq(0,1,0.01) , na.rm = TRUE)
master_data$TotalWorkingYears[which(master_data$TotalWorkingYears > 35)]<-35

#there are outliers
quantile(master_data$YearsAtCompany , seq(0,1,0.01) , na.rm = TRUE)
master_data$YearsAtCompany[which(master_data$YearsAtCompany > 24)]<-24

#no outliers
quantile(master_data$YearsSinceLastPromotion , seq(0,1,0.01) , na.rm = TRUE)

#there are outliers
quantile(master_data$YearsWithCurrManager , seq(0,1,0.01) , na.rm = TRUE)
boxplot(master_data$YearsWithCurrManager)
master_data$YearsWithCurrManager[which(master_data$YearsWithCurrManager > 14)]<-14

#no outliers in leaves
boxplot(master_data$leaves)

#early_exit_count
#multiple jumps in the begining , and middle ,needs to be scaled
#data is skewed so cannot cap it 
boxplot(master_data$early_exit_count)
quantile(master_data$early_exit_count , seq(0,1,0.01) , na.rm = TRUE)

#couple of outliers in avg working hours
boxplot(master_data$avg_working_hrs)
quantile(master_data$avg_working_hrs , seq(0,1,0.01) , na.rm = TRUE)
#but we can see from quantile there isn't any jump




#---------------Binning mnthly income
#row.names(master_data) <- 1:nrow(master_data) 
#iv.num(master_data,"MonthlyIncome","Attrition")
#iv.plot.woe(iv.num(master_data,"MonthlyIncome","Attrition"))

# Feature standardisation

# Normalising continuous features 
master_data$Age <- scale(master_data$Age)
master_data$DistanceFromHome <- scale(master_data$DistanceFromHome)
master_data$MonthlyIncome <- scale(master_data$MonthlyIncome)
master_data$PercentSalaryHike <- scale(master_data$PercentSalaryHike)
master_data$TotalWorkingYears <- scale(master_data$TotalWorkingYears)
master_data$YearsAtCompany <- scale(master_data$YearsAtCompany)
master_data$YearsSinceLastPromotion <- scale(master_data$YearsSinceLastPromotion)
master_data$YearsWithCurrManager <- scale(master_data$YearsWithCurrManager)
master_data$leaves <- scale(master_data$leaves)
master_data$avg_working_hrs <- scale(master_data$avg_working_hrs)
master_data$early_exit_count <- scale(master_data$early_exit_count)


# creating a dataframe of categorical features
master_data_attrition <- master_data[,c(3,4,5,7,8,9,10,11,12,14,16,18,22,23,24,25,26)]

# converting categorical attributes to factor
#master_data_attrition<- data.frame(sapply(master_data_attrition, function(x) factor(x)))
#str(master_data_attrition)

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(master_data_attrition, 
                            function(x) data.frame(model.matrix(~x-1,data =master_data_attrition))[,-1]))

# Final dataset : we also remove EployeeID as it containes all ID's
master_data<- cbind(master_data[,-c(1,3,4,5,7,8,9,10,11,12,14,16,18,22,23,24,25,26)],dummies) 
str(master_data) 




#------------------Modelling-------------------------------


# splitting the data between train and test
set.seed(100)

indices = sample.split(master_data$Attrition, SplitRatio = 0.7)

train = master_data[indices,]

test = master_data[!(indices),]

########################################################################
# Logistic Regression: 

#Initial model
model_1 <- glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) #AIC 2076.4

#Stepwise selection
model_2<- stepAIC(model_1, direction="both")

summary(model_2)
sort(vif(model_2))

#we remove YearsAtCompany because it has a high vif and it is not much significant
model_3 <- glm(formula = Attrition ~ Age + TotalWorkingYears + 
        YearsSinceLastPromotion + YearsWithCurrManager + avg_working_hrs + 
        early_exit_count + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
        Education.x5 + EducationField.xLife.Sciences + EducationField.xMarketing + 
        EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
        Gender + JobLevel.x2 + JobLevel.x5 + JobRole.xHuman.Resources + 
        JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
        JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
        NumCompaniesWorked.x1 + NumCompaniesWorked.x2.4 + NumCompaniesWorked.x5. + 
        StockOptionLevel.x1 + TrainingTimesLastYear.x3. + EnvironmentSatisfaction.x2 + 
        EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
        JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
        WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
        JobInvolvement.x3 + PerformanceRating, family = "binomial", 
      data = train)

summary(model_3)
sort(vif(model_3))

#we remove avg_working_hrs because it is not much significant and has a high vif
model_4 <- glm(formula = Attrition ~ Age + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 early_exit_count + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Education.x5 + EducationField.xLife.Sciences + EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 Gender + JobLevel.x2 + JobLevel.x5 + JobRole.xHuman.Resources + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 NumCompaniesWorked.x1 + NumCompaniesWorked.x2.4 + NumCompaniesWorked.x5. + 
                 StockOptionLevel.x1 + TrainingTimesLastYear.x3. + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 JobInvolvement.x3 + PerformanceRating, family = "binomial", 
               data = train)
summary(model_4)
sort(vif(model_4))

#we remove PerformanceRating because it is insignificant and variables with high vif has 
#high significance
model_5 <- glm(formula = Attrition ~ Age + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 early_exit_count + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Education.x5 + EducationField.xLife.Sciences + EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 Gender + JobLevel.x2 + JobLevel.x5 + JobRole.xHuman.Resources + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 NumCompaniesWorked.x1 + NumCompaniesWorked.x2.4 + NumCompaniesWorked.x5. + 
                 StockOptionLevel.x1 + TrainingTimesLastYear.x3. + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 JobInvolvement.x3 , family = "binomial", 
               data = train)
summary(model_5)
sort(vif(model_5))

#we remove JobLevel.x5 because it has low significance
model_6 <- glm(formula = Attrition ~ Age + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 early_exit_count + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Education.x5 + EducationField.xLife.Sciences + EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 Gender + JobLevel.x2 + JobRole.xHuman.Resources + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 NumCompaniesWorked.x1 + NumCompaniesWorked.x2.4 + NumCompaniesWorked.x5. + 
                 StockOptionLevel.x1 + TrainingTimesLastYear.x3. + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 JobInvolvement.x3 , family = "binomial", 
               data = train)
summary(model_6)
sort(vif(model_6))

# we remove Education.x5  because it is insignificant
model_7 <- glm(formula = Attrition ~ Age + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 early_exit_count + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 EducationField.xLife.Sciences + EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 Gender + JobLevel.x2 + JobRole.xHuman.Resources + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 NumCompaniesWorked.x1 + NumCompaniesWorked.x2.4 + NumCompaniesWorked.x5. + 
                 StockOptionLevel.x1 + TrainingTimesLastYear.x3. + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 JobInvolvement.x3 , family = "binomial", 
               data = train)
summary(model_7)
sort(vif(model_7))


# we remove Gender because it is insignificant
model_8 <- glm(formula = Attrition ~ Age + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 early_exit_count + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 EducationField.xLife.Sciences + EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobLevel.x2 + JobRole.xHuman.Resources + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 NumCompaniesWorked.x1 + NumCompaniesWorked.x2.4 + NumCompaniesWorked.x5. + 
                 StockOptionLevel.x1 + TrainingTimesLastYear.x3. + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 JobInvolvement.x3 , family = "binomial", 
               data = train)
summary(model_8)
sort(vif(model_8))

# we remove EducationField.xMedical because it has a high vif from the start
model_9 <- glm(formula = Attrition ~ Age + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 early_exit_count + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 EducationField.xLife.Sciences + EducationField.xMarketing + 
                 EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobLevel.x2 + JobRole.xHuman.Resources + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 NumCompaniesWorked.x1 + NumCompaniesWorked.x2.4 + NumCompaniesWorked.x5. + 
                 StockOptionLevel.x1 + TrainingTimesLastYear.x3. + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 JobInvolvement.x3 , family = "binomial", 
               data = train)
summary(model_9)
sort(vif(model_9))

# we remove EducationField.xLife.Sciences because it has turned insignificant
model_10 <- glm(formula = Attrition ~ Age + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  early_exit_count + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  EducationField.xMarketing + 
                  EducationField.xOther + EducationField.xTechnical.Degree + 
                  JobLevel.x2 + JobRole.xHuman.Resources + 
                  JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                  NumCompaniesWorked.x1 + NumCompaniesWorked.x2.4 + NumCompaniesWorked.x5. + 
                  StockOptionLevel.x1 + TrainingTimesLastYear.x3. + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                  JobInvolvement.x3 , family = "binomial", 
                data = train)
summary(model_10)
sort(vif(model_10))

# we remove EducationField.xMarketing because it is insignificant
model_11 <- glm(formula = Attrition ~ Age + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  early_exit_count + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  EducationField.xOther + EducationField.xTechnical.Degree + 
                  JobLevel.x2 + JobRole.xHuman.Resources + 
                  JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                  NumCompaniesWorked.x1 + NumCompaniesWorked.x2.4 + NumCompaniesWorked.x5. + 
                  StockOptionLevel.x1 + TrainingTimesLastYear.x3. + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                  JobInvolvement.x3 , family = "binomial", 
                data = train)
summary(model_11)
sort(vif(model_11))

# we remove EducationField.xTechnical.Degree because it is the least significant
model_12 <- glm(formula = Attrition ~ Age + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  early_exit_count + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  EducationField.xOther + JobLevel.x2 + JobRole.xHuman.Resources + 
                  JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                  NumCompaniesWorked.x1 + NumCompaniesWorked.x2.4 + NumCompaniesWorked.x5. + 
                  StockOptionLevel.x1 + TrainingTimesLastYear.x3. + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                  JobInvolvement.x3 , family = "binomial", 
                data = train)
summary(model_12)
sort(vif(model_12))

# we remove EducationField.xOther because it is the least significant
model_13 <- glm(formula = Attrition ~ Age + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  early_exit_count + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  JobLevel.x2 + JobRole.xHuman.Resources + 
                  JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                  NumCompaniesWorked.x1 + NumCompaniesWorked.x2.4 + NumCompaniesWorked.x5. + 
                  StockOptionLevel.x1 + TrainingTimesLastYear.x3. + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                  JobInvolvement.x3 , family = "binomial", 
                data = train)
summary(model_13)
sort(vif(model_13))

# we remove StockOptionLevel.x1 because it is not much significant
model_14 <- glm(formula = Attrition ~ Age + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  early_exit_count + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  JobLevel.x2 + JobRole.xHuman.Resources + 
                  JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                  NumCompaniesWorked.x1 + NumCompaniesWorked.x2.4 + NumCompaniesWorked.x5. + 
                  TrainingTimesLastYear.x3. + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                  JobInvolvement.x3 , family = "binomial", 
                data = train)
summary(model_14)
sort(vif(model_14))

#we remove JobRole.xManager because it is the least significant
model_15 <- glm(formula = Attrition ~ Age + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  early_exit_count + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  JobLevel.x2 + JobRole.xHuman.Resources + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                  NumCompaniesWorked.x1 + NumCompaniesWorked.x2.4 + NumCompaniesWorked.x5. + 
                  TrainingTimesLastYear.x3. + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                  JobInvolvement.x3 , family = "binomial", 
                data = train)
summary(model_15)
sort(vif(model_15))

#we remove MaritalStatus.xMarried because it is the least significant
model_16 <- glm(formula = Attrition ~ Age + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  early_exit_count + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  JobLevel.x2 + JobRole.xHuman.Resources + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  NumCompaniesWorked.x1 + NumCompaniesWorked.x2.4 + NumCompaniesWorked.x5. + 
                  TrainingTimesLastYear.x3. + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                  JobInvolvement.x3 , family = "binomial", 
                data = train)
summary(model_16)
sort(vif(model_16))

#we remove JobRole.xHuman.Resources because it is the least significant
model_17 <- glm(formula = Attrition ~ Age + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  early_exit_count + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  JobLevel.x2 + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  NumCompaniesWorked.x1 + NumCompaniesWorked.x2.4 + NumCompaniesWorked.x5. + 
                  TrainingTimesLastYear.x3. + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                  JobInvolvement.x3 , family = "binomial", 
                data = train)
summary(model_17)
sort(vif(model_17))

#we remove BusinessTravel.xTravel_Rarely because it has slightly high vif
#and that too from the start
model_18 <- glm(formula = Attrition ~ Age + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  early_exit_count + BusinessTravel.xTravel_Frequently +  
                  JobLevel.x2 + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  NumCompaniesWorked.x1 + NumCompaniesWorked.x2.4 + NumCompaniesWorked.x5. + 
                  TrainingTimesLastYear.x3. + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                  JobInvolvement.x3 , family = "binomial", 
                data = train)
summary(model_18)
sort(vif(model_18))


#we removeNumCompaniesWorked.x2.4 because it has slightly high vif
#and low significance
model_19 <- glm(formula = Attrition ~ Age + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  early_exit_count + BusinessTravel.xTravel_Frequently +  
                  JobLevel.x2 + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  NumCompaniesWorked.x1 + NumCompaniesWorked.x5. + 
                  TrainingTimesLastYear.x3. + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                  JobInvolvement.x3 , family = "binomial", 
                data = train)
summary(model_19)
sort(vif(model_19))

#we remove NumCompaniesWorked.x1 because it has low significance
model_20 <- glm(formula = Attrition ~ Age + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  early_exit_count + BusinessTravel.xTravel_Frequently +  
                  JobLevel.x2 + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  NumCompaniesWorked.x5. + 
                  TrainingTimesLastYear.x3. + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                  JobInvolvement.x3 , family = "binomial", 
                data = train)
summary(model_20)
sort(vif(model_20))

#we remove JobInvolvement.x3 because it has low significance
model_21 <- glm(formula = Attrition ~ Age + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  early_exit_count + BusinessTravel.xTravel_Frequently +  
                  JobLevel.x2 + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  NumCompaniesWorked.x5. + 
                  TrainingTimesLastYear.x3. + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4  
                  , family = "binomial", 
                data = train)
summary(model_21)
sort(vif(model_21))

#we remove JobLevel.x2 because it has low significance
model_22 <- glm(formula = Attrition ~ Age + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  early_exit_count + BusinessTravel.xTravel_Frequently +  
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  NumCompaniesWorked.x5. + 
                  TrainingTimesLastYear.x3. + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4  
                , family = "binomial", 
                data = train)
summary(model_22)
sort(vif(model_22))

#we remove WorkLifeBalance.x2 because it has slightly high vif
#right from the start
model_23 <- glm(formula = Attrition ~ Age + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  early_exit_count + BusinessTravel.xTravel_Frequently +  
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  NumCompaniesWorked.x5. + 
                  TrainingTimesLastYear.x3. + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x3 + WorkLifeBalance.x4  
                , family = "binomial", 
                data = train)
summary(model_23)
sort(vif(model_23))

#we remove WorkLifeBalance.x4 because it has turned insignificant
model_24 <- glm(formula = Attrition ~ Age + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  early_exit_count + BusinessTravel.xTravel_Frequently +  
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  NumCompaniesWorked.x5. + 
                  TrainingTimesLastYear.x3. + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x3 , family = "binomial", 
                data = train)
summary(model_24)
sort(vif(model_24))

#we remove WorkLifeBalance.x4 because it has turned insignificant
model_25 <- glm(formula = Attrition ~ Age + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  early_exit_count + BusinessTravel.xTravel_Frequently +  
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  NumCompaniesWorked.x5. + 
                  TrainingTimesLastYear.x3. + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x3 , family = "binomial", 
                data = train)
summary(model_25)
sort(vif(model_25))


#we remove JobRole.xSales.Executive because it has not much significant
model_26 <- glm(formula = Attrition ~ Age + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  early_exit_count + BusinessTravel.xTravel_Frequently +  
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  MaritalStatus.xSingle + NumCompaniesWorked.x5. + 
                  TrainingTimesLastYear.x3. + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x3 , family = "binomial", 
                data = train)
summary(model_26)
sort(vif(model_26))

#we remove JobRole.xResearch.Director because it has not much significant
model_27 <- glm(formula = Attrition ~ Age + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  early_exit_count + BusinessTravel.xTravel_Frequently +  
                  JobRole.xManufacturing.Director +  
                  MaritalStatus.xSingle + NumCompaniesWorked.x5. + 
                  TrainingTimesLastYear.x3. + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x3 , family = "binomial", 
                data = train)
summary(model_27)
sort(vif(model_27))



#this is our Final Model 
#we can see YearsSinceLastPromotion has a positive relation with our dependent variable
#also BusinessTravel.xTravel_Frequently , MaritalStatus.xSingle has a very high relation with the dependent variable
#and so does NumCompaniesWorked.x5 whereas
#JobRole.xManufacturing.Director and EnvironmentSatisfaction.x3 and early_exit_count have a negative
#impact on the dependent variable


#-------------------------------------Driver Variables----------------------

#from business point of view we can say
#MaritalStatus.xSingle , EnvironmentSatisfaction , JobSatisfaction , YearsSinceLastPromotion,
#BusinessTravel.xTravel_Frequently , early_exit_count and NumCompaniesWorked are the driver variables

## view the coefficients as odds and log odds
#they provide a better interpretation of the coefficients

odds <- exp(coefficients(model_27))
log_odds <- odds / (1 + odds)

#so now we have interpreted our coefficients in a linear way and can use it fou model deployment
#we can use these new coeeficients to better understand and interpret our independent variables


#-----------------------------------------Model Evaluation----------------------

#Evaluating the model based on Sensitivity , Specificity and Accuracy

test_pred = predict(model_27, type = "response", 
                    newdata = test[,-22])

summary(test_pred)

test$prob <- test_pred
View(test)

test_pred_Attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_Attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))

table(test_actual_Attrition,test_pred_Attrition)

test_pred_Attrition <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))

test_conf <- confusionMatrix(test_pred_Attrition, test_actual_Attrition, positive = "Yes")
test_conf



# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_Attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_Attrition, test_actual_Attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.003575 to 0.812100 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]
#0.17758 : so we use it to get our sensitivity , specificity and accuracy

test_cutoff_Attrition <- factor(ifelse(test_pred >=0.17758, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_Attrition, test_actual_Attrition, positive = "Yes")

conf_final

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc #0.7782946

sens # 0.7751196

spec #0.7789084 

##################################################################################################
### KS -statistic - Test Data ######

test_cutoff_Attrition <- ifelse(test_cutoff_Attrition=="Yes",1,0)
test_actual_Attrition <- ifelse(test_actual_Attrition=="Yes",1,0)


library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_Attrition, test_actual_Attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

#Roc curve
plot(performance_measures_test,col="red")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test) #0.5540 is our ks-statistic , hence is quite good as ks should be > 40

#Plotting roc curve along with auc
auc<-performance(pred_object_test, measure = "auc")
auc <- auc@y.values[[1]]
auc #0.77
plot(performance_measures_test , main = "AUC=0.77" , colorize = TRUE , text.adj = c(-0.2,1.7))



####################################################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Attrition_decile = lift(test_actual_Attrition, test_pred, groups = 10)

#plotting lift chart
plot(Attrition_decile$bucket, Attrition_decile$Cumlift, type="l", ylab="Cumulative lift", xlab="Bucket")

pred_gain = prediction(test$prob, test$Attrition)
gain = performance(pred_gain, "tpr", "rpp")

#Plotting gain chart
plot(gain, col="orange", lwd=2)

plot(x=c(0, 1), y=c(0, 1), type="l", col="red", lwd=2,
     ylab="True Positive Rate", 
     xlab="Rate of Positive Predictions")
lines(x=c(0, 0.1, 1), y=c(0, 1, 1), col="darkgreen", lwd=2)

gain.x = unlist(slot(gain, 'x.values'))
gain.y = unlist(slot(gain, 'y.values'))

lines(x=gain.x, y=gain.y, col="orange", lwd=2)

#through our gain and lift chart we can see it's a good model 

#------------------------------------------------------------------------------------
