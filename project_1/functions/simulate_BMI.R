### simulate_BMI.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Apr 24 2024 (11:56) 
## Version: 
## Last-Updated: Oct 25 2024 (12:07) 
##           By: Thomas Alexander Gerds
##     Update #: 4
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
## Simulate trajectories using a Gaussian process
simulate_BMI <- function(n){
    ## 100 time points equally spaced between 40 and 60 years of age
    age_grid = seq(40,60, length.out = 100)
    # loop across individuals
    do.call(rbind, lapply(1:n, function(ii){
        # BMI is observed at a random number of ages
        number_age_observations = sample(1:40,size = 1)
        out =  data.table(id = ii, age = age_grid, type = "latent")
        # sample observation ages
        out[sample(1:.N,size = number_age_observations),type := "observed"]
        setorder(out, age)
        out[, bmi := {
            ## Simulate BMI values according to a Gaussion process
            ## Step 1: specify covariance matrix for the BMI at the age_grid
            Sigma = sapply(age,function(a){
                exp(-(age-a)^2/100)
            })
            ## Step 2: Simulate from a multivariate Gaussion distribution with this kernel
            ## the runif(1,-2,5) part sets a random intercept between 23 and 30
            ## the runif(1,0,1) part increases the variability with a random value between 0 and 1
            random_intercept <- runif(1,-2,5)
            random_variation <- runif(1,0,1)
            BMI = 25+random_intercept+2*c(mvtnorm::rmvnorm(1,sigma=Sigma+random_variation))
            return(BMI)
        }]
        out[]
    }))
}

######################################################################
### simulate_BMI.R ends here
