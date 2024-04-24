### simulate_BMI.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Apr 24 2024 (11:56) 
## Version: 
## Last-Updated: Apr 24 2024 (12:17) 
##           By: Thomas Alexander Gerds
##     Update #: 1
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
        number_age_observations = sample(1:7,size = 1)
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
            BMI = 25+2*c(mvtnorm::rmvnorm(1,sigma=Sigma))
            return(BMI)
        }]
        out[]
    }))
}

######################################################################
### simulate_BMI.R ends here
