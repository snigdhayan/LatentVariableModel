# LatentVariableModel

The aim here is to use the Lavaan package (https://cran.r-project.org/web/packages/lavaan/index.html) to analyze the breast cancer dataset. The rough idea is as follows:

1. Regression - use feature agglomeration to cluster similar features (this is done separately - https://github.com/snigdhayan/FeatureEngineering/blob/master/BreastCancerFeatureAgglomeration.py). Predict label as a regression model based on latent variables corresponding to the top two clusters.
2. Latent Variables - each latent variable is measured by the features belonging to its corresponding cluster.
3. Variances and Covariances - correlated features within each cluster are added to (co)variances.

To simplify the whole process I have already prepared standardized dataset (mean = 0, standard deviation = 1 for each feature) and added it to the current working directory. With this approach I was able to achieve a validation accuracy consistently above 95%. In the 'CentralLimitTheorem.R' script I have included my down-to-earth explanation and hands-on verification of the Central Limit Theorem.