### _targets.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Apr 24 2024 (11:55) 
## Version: 
## Last-Updated: Apr 24 2024 (13:56) 
##           By: Thomas Alexander Gerds
##     Update #: 8
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
try(setwd("~/research/Epidemi/BMI_trajectories/project_1/"))
library(targets)
tar_source("functions")
tar_option_set(packages = c("mvtnorm","data.table","ggplot2"))
if (FALSE){
    library(mvtnorm)
    library(data.table)
    library(ggplot2)
}
list(
    tar_target(sample_data,{
        # simulate BMI trajectories for 17 subjects
        simulate_BMI(n = 17)
    }),
    tar_target(observed_sample_data,{
        x = sample_data[type == "observed"][,type := NULL]
        x[]
    }),
    tar_target(plot_sample_data,{
        plot_BMI_trajectories(sample_data = sample_data)
    })
)


######################################################################
### _targets.R ends here
