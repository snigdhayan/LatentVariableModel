# load the lavaan package (only needed once per session)
library(lavaan)

# specify model - predict model via two latent variables; the latent variables are modeled according to clusters of feature agglomeration
model <- ' label ~ latent_var1 + latent_var2
              latent_var1 =~ worst_area + worst_perimeter + worst_radius + area_error + perimeter_error 
                            + mean_radius + mean_perimeter + radius_error + mean_area  
              latent_var2 =~ mean_fractal_dimension + worst_smoothness + mean_smoothness + worst_fractal_dimension 
                            + mean_symmetry + worst_symmetry '
              
              
# read data
setwd('/Users/ibatu/Documents/MyProgramsWindows/R/LatentVariableModel/')
data_train <- read.csv(file = 'normalized_breast_cancer_dataset_train.csv', header = TRUE)
# data <- data[names(data)!='label']

# fit model
fit <- cfa(model, data=data_train, std.lv=TRUE, missing="fiml", control=list(iter.max=1000))

# display summary output
# summary(fit, fit.measures=TRUE)

# Predict on test dataset

data_test <- read.csv('normalized_breast_cancer_dataset_test.csv')
pred <- as.data.frame(lavPredict(fit, type = "ov", newdata = data_test))

# Check prediction accuracy
myFn <- function(x) {
  if (x > 0.5) 1
  else 0
}

pred_binary <- apply(pred["label"], 1, FUN = myFn)
isCorrect <- pred_binary == data_test["label"]
table(isCorrect)
print(paste0("Accuracy = ", round(sum(isCorrect,na.rm = TRUE)/length(isCorrect)*100,2)))