### Data Wrangling Assignment 2
## Group: Alexander Uitendaal , Milad Ehsan(10732918) , Daan Uittenhout(11057777) and Isabelle Slot(10676848)

rm(list=ls())         # remove working memory
install.packages(tidyverse)
library(tidyverse)    
library(class)

#1- Use an appropriate function in R to read the “ClassifyRisk.txt” dataset. (point 5)

#working directory (wd) is already set
data <- read.csv("ClassifyRisk.txt",stringsAsFactors = TRUE) 
View(data)

#2- Report a summary of the dataset by using appropriate R functions. Are there any missing
#values? What is the number of variables? Which type of variables we have? (point 5)

#missing values
missing <- is.na(data) # returns TRUE of ClassifyRisk data values if missing
View(missing)      # there are no missing values
which(is.na(data))

#number of variables
ncol(data) #gives 6 variables

#type of variables
typeof(data$mortgage)           #integer  
typeof(data$nr_loans)           #integer
typeof(data$age)                #integer
typeof(data$marital_status)     #integer
typeof(data$income)             #double
typeof(data$risk)               #integer

#3- For k-nearest neighbor algorithm, 
# a) which variables need to be normalized? Use an appropriate normalization function and
#write a function for it. (point 15)

#When measuring Euclidean distance, one or more attributes can have very large 
#values, relative to the other attributes.The value of some attributes 
#will overwhelmthe value of other attributes. Therefore we normalize all variables
#before doing the k-nearest neighbor algorithm. 

#numerical variables of the data need to be normalized using 
#Min-Max Normalization:
rescale01 <- function(x){
  rng <- range(x,na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

age_MM = rescale01(age)
nr_loans_MM = rescale01(nr_loans)
income_MM = rescale01(income)

#add age_MM to data
data <- data %>% mutate(age_MM)

#add nr_loans_MM to data
data <- data %>% mutate(nr_loans_MM)

#add income_MM to data
data <- data %>% mutate(income_MM)

#categorial variables of the data need dummy variables
install.packages("dummies")
library(dummies)

mortgage_D = dummy(mortgage)
marital_status_d = dummy(marital_status)
risk_D = dummy(risk)

#First categorial variable
mortgageDummy <- c(rep(999,length(mortgage)))
for (i in 1:length(mortgage)) {
  if(mortgage[i] == "y") mortgageDummy[i] = 1 else mortgageDummy[i] = 0  #n is the reference category
}
#add mortgagedummy to data
data <- data %>% mutate(mortgageDummy)

#second categorial variable
marital_statusDummy <- c(rep(999,length(marital_status)))
for (i in 1:length(marital_status)) {
  if(marital_status[i] == "married") married[i] = 1 else married[i] = 0   #other is the reference category
  if(marital_status[i] == "single") single[i] = 1 else single[i] = 0     #other is the reference category
}
#add marital_statusDummy to data
data <- data %>% mutate(marital_statusDummy)

#third categorial variable
riskDummy <- c(rep(999, length(risk)))
for (i in 1:length(risk)) {
  if(risk[i] == "good risk") riskDummy[i] = 1 else riskDummy[i] = 0  #bad_loss is the referece category
}
#add riskdummy to data
data <- data %>% mutate(riskDummy)

#4 twofold cross-validation technique to partition the dataset randomly into a training set and a test set
# To devide the data set randomly, we assign a random value to the observations in the dataset. 
#Since the partition is 70%/30%, we collect all the observations with a random value <= 0.70 to the training data set,
#and all the observations wit a random value >0.70 to the test dataset.

amount_training <- round(nrow(data)*0.70) #checking how many observations the training set should contain.
amount_training

set.seed(100)
data$part<- runif(nrow(data), min=0, max =1)
Training <- data[data$part <= 0.70,]
Testing <- data[data$part > 0.70,]

#5 Validate the partition. It is important that the composition of the training and test data sets are as similar as possible
#1 testing for risk:
sumtest <- sum(Testing$risk == "good risk")
sumtrain <- sum(Training$risk == "good risk")
ntest <- length(Testing$risk)
ntrain <- length(Training$risk)
meantest <- a1 / n1
meantrain <- a2 / n2
meantot <- (a1+a2) / (n1+n2)
zeta <- (meantest-meantrain) / sqrt(meantot*(1-meantot)*((1/ntest)+(1/ntrain)))
p_value <- 2*pnorm(abs(zeta), lower.tail = FALSE)
p_value

#p-value of z statistic s way above  5 or 10 % significance level, so there is no evidence that proves that the proportion  of good risk in the sets is significantly different.  

#2: testing for no loans:
testingmean <- mean(Testing$nr_loans)
trainingmean <- mean(Training$nr_loans)
stest <- sd(Testing$nr_loans)
strain <- sd(Training$nr_loans)
ntest <- length(Testing$nr_loans)
ntrain <- length(Training$nr_loans)
tstat <- (testingmean - trainingmean) / sqrt((stest^2/ntest)+(strain^2/ntrain))
p_value <- 2*pt(abs(tstat), df = dfs, lower.tail = FALSE)
p_value

#p-value >0.05, so for the 5 % significance level, there is no evidence of significant difference. 


#3: testing for age:
meantest <- mean(Testing$age)
meantrain <- mean(Training$age)
stest <- sd(Testing$age)
strain <- sd(Training$age)
ntest <- length(Testing$age)
ntrain <- length(Training$age)
dfs <- min(ntest-1, ntrain-1)
tstat <- (meantest - meantrain) / sqrt((stest^2/ntest)+(strain^2/ntrain))
pvalue <- 2*pt(abs(tstat), df = dfs, lower.tail = FALSE)
pvalue

# in this case the p-value is also above the 5 or 10% significance level, so no evidence that the age in Testing and Training is significantly different. 

#4. testing for income:

meantest <- mean(Testing$income)
meantrain <- mean(Training$income)
stest <- sd(Testing$income)
strain <- sd(Training$income)
ntest <- length(Testing$income)
ntrain <- length(Training$income)
dfs <- min(ntest-1, ntrain-1)
tstat <- (meantest - meantrain) / sqrt((stest^2/ntest)+(strain^2/ntrain))
tstat
pvalue <- 2*pt(abs(tstat), df = dfs, lower.tail = FALSE)
pvalue

#with p-value being way bigger than the 5 or 10 % singinificance level, there is no evidence that income in testing and training sets is significantly different.

#5. testing for marital status:
summary(data$marital_status)
testmarried <- sum(Testing$marital_status == "married")
testsingle <- sum(Testing$marital_status == "single")
testother <- sum(Testing$marital_status == "other")
trainmarried <- sum(Training$marital_status == "married")
trainsingle <- sum(Training$marital_status == "single")
trainother <- sum(Training$marital_status == "other")
freq_table <- as.table(rbind(c(testmarried, testsingle, testother),
                             c(trainmarried, trainsingle, trainother)))
dimnames(freq_table) <- list(
  Data = c("Testing Set", "Training Set"),
  Status = c("Married", "Single", "Other"))
freq_table
Xsquared_data <- chisq.test(freq_table)
Xsquared_data$statistic
Xsquared_data$p.value

#for 5 % siginificance level, there is no evidence that marital status in testing and training sets are significantly different.

# create the traning an testing set for KNN
TrainingKnn <- data.frame(Training[c(7,8,9,11,12,13)])
TestingKnn <- data.frame(Testing[c(7,8,9,11,12,13)])
cl <- unlist(Training[10])
head(TrainingKnn)
head(TestingKnn)
head(cl)

# predict Knn using k =2
predictK2 <- knn(TrainingKnn, TestingKnn, cl, k = 2, prob = FALSE)
predictK2
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
predictk2 <- as.numeric.factor(predictK2)
predictk2
# predicting Knn using k = 3

predictK3 <- knn(TrainingKnn, TestingKnn, cl, k = 3, prob = FALSE)
predictK3
predictK3 <- as.numeric.factor(predictK3)
predictK3


# to check the accuracy of the predictions
library(Metrics)

RMSE_k2 = rmse(Testing$risk_D, predictk2)
RMSE_k3 = rmse(Testing$risk_D, predictk3)
sprintf('For k = 2 the RMSE is: %.2f', RMSE_k2)
sprintf('For k = 3 the RMSE is: %.2f', RMSE_k3)
