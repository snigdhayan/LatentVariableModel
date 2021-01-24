# Verification of Central Limit Theorem

#Create population data of chosen size; values chosen uniformly distributed between min and max
pop_size <-100000
min_value <- 0
max_value <-100
set.seed(123)
population <- runif(pop_size,min = min_value, max = max_value)

#Actual population mean and standard deviation of the population
mu <- mean(population)
sig <- sd(population)

#Design sampling mechanism - no. of trials and sample size for each trial
trials = 100
sample_size = 50

#Generate sample statistics
mySample <- numeric(trials)
for (i in c(1:trials)) {
  spl <- sample(population,size = sample_size, replace = TRUE)
  mySample[i] <- mean(spl)
}

#Compute estimates from trials
estimate_mu <- mean(mySample) # mean of sample means is a good estimate for population mean
spl_sig <- sd(mySample)
estimate_sig <- spl_sig*sqrt(sample_size) # standard deviation of sample means is biased and needs to be corrected

#Comparison of actual and estimated parameters
result <- data.frame("Pop_mean" = mu,
                     "Estimate_mean" = estimate_mu,
                     "Pop_sd" = sig,
                     "Estimate_sd" = estimate_sig)

print(result)
plot(density(mySample)) #should look like a normal distribution