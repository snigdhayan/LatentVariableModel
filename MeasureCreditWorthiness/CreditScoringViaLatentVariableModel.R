# load needed packages - caTools and lavaan (only needed once per session)
library(caTools)
library(lavaan)

# specify model based on two latent variables to measure 'creditability' or 'credit-worthiness'
model <- ' # regression
              creditability ~ latent_var1 + latent_var2
           # latent variables
              latent_var1 =~ duration_in_month + age_in_years 
              latent_var2 =~ credit_amount + credit_history
           # variances and covariances
              duration_in_month ~~ credit_history
              age_in_years ~~ credit_history' 
              
              
# read and cleanse data 
setwd('/Users/ibatu/Documents/MyProgramsWindows/Repositories/LatentVariableModel/MeasureCreditWorthiness/')
data <- read.csv(file = './CreditScoringData.csv', header = TRUE)
data <- data[complete.cases(data),] # there are approx. 1000 complete cases
# data <- data[names(data)!='creditability']

# change the 'credit_history' column to numeric type
data$credit_history <- as.factor(data$credit_history)
uniques = unique(data$credit_history)
levels <- c(1:length(uniques))
data$credit_history <- factor(data$credit_history, labels = levels)
data$credit_history <- as.numeric(data$credit_history)

# split train-test data
set.seed(101) 
split_ratio <- 0.7
sample <- sample.split(data, SplitRatio = split_ratio)
data_train <- subset(data, sample == TRUE)
data_test  <- subset(data, sample == FALSE)

# fit model
start_time <- proc.time()
fit <- cfa(model, 
           data=data_train, 
           std.lv=TRUE, 
           missing="fiml", 
           control=list(iter.max=2000))
training_time <- proc.time() - start_time

# display summary output
# summary(fit, fit.measures=TRUE)

# Predict labels of test dataset
pred <- as.data.frame(lavPredict(fit, type = "ov", newdata = data_test))

# Check prediction accuracy
myFn <- function(x) { # myFn is used to map predicted fractional values to 0 or 1
  if (x > 0.5) 1
  else 0
}

pred_binary <- apply(pred["creditability"], 1, FUN = myFn)
isCorrect <- pred_binary == data_test["creditability"]

# Print statistics
table(isCorrect)
print(paste0("Accuracy = ", round(sum(isCorrect,na.rm = TRUE)/length(isCorrect)*100,2)))
print("Training time (in seconds):")
print(training_time)