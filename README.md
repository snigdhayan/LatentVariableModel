# LatentVariableModel
WORK IN PROGRESS

The aim here is to use the Lavaan package (https://cran.r-project.org/web/packages/lavaan/index.html) to predict breast cancer labels. The rough idea is as follows:
1. Use feature agglomeration to cluster similar features (this is done separately - https://github.com/snigdhayan/FeatureEngineering/blob/master/BreastCancerFeatureAgglomeration.py).
2. Define a latent variable model (using Lavaan) that predicts labels based on two latent variables - these latent variables correspond to the top two feature clusters and are measured by the features belonging to the clusters.

To simplify the whole process I have already prepared standardized dataset (mean = 0, standard deviation = 1) for training and testing. There is also a dataset with randomized labels - https://github.com/snigdhayan/LatentVariableModel/blob/master/normalized_breast_cancer_dataset_random_labels.csv. This can be used to see how worse the model performs if it is trained on random labels.