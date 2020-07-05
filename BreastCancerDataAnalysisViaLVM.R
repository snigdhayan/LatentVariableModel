# load the lavaan package (only needed once per session)
library(lavaan)

# specify model based on two latent variables - the latent variables are measured by the features in the clusters of feature agglomeration
model <- ' # regression
              label ~ latent_var1 + latent_var2
           # latent variables
              latent_var1 =~ worst_area + worst_perimeter + worst_radius + area_error + perimeter_error 
                            + mean_radius + mean_perimeter + radius_error + mean_area  
              latent_var2 =~ mean_fractal_dimension + worst_smoothness + mean_smoothness + worst_fractal_dimension 
                            + mean_symmetry + worst_symmetry 
           # variances and covariances
              worst_perimeter ~~ mean_perimeter 
              worst_radius ~~ mean_radius
              worst_area ~~ mean_area
              worst_smoothness ~~ mean_smoothness 
              mean_symmetry ~~ worst_symmetry 
              mean_fractal_dimension ~~ worst_fractal_dimension '
              
              
# read data and prepare train-test data
setwd('/Users/ibatu/Documents/MyProgramsWindows/R/LatentVariableModel/')
data <- read.csv(file = './normalized_breast_cancer_dataset.csv', header = TRUE)
# data <- data[names(data)!='label']

library(caTools)
# set.seed(101) 
split_ratio = 0.7
sample = sample.split(data, SplitRatio = split_ratio)
data_train = subset(data, sample == TRUE)
data_test  = subset(data, sample == FALSE)

# fit model
start_time <- proc.time()
fit <- cfa(model, data=data_train, std.lv=TRUE, missing="fiml", control=list(iter.max=1000))
training_time <- proc.time() - start_time

# display summary output
# summary(fit, fit.measures=TRUE)

# Predict labels of test dataset
pred <- as.data.frame(lavPredict(fit, type = "ov", newdata = data_test))

# Check prediction accuracy
myFn <- function(x) {
  if (x > 0.5) 1
  else 0
}

pred_binary <- apply(pred["label"], 1, FUN = myFn)
isCorrect <- pred_binary == data_test["label"]

# Print statistics
table(isCorrect)
print(paste0("Accuracy = ", round(sum(isCorrect,na.rm = TRUE)/length(isCorrect)*100,2)))
print("Training time (in seconds):")
print(training_time)