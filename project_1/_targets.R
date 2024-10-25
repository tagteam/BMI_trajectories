### _targets.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Apr 24 2024 (11:55) 
## Version: 
## Last-Updated: Oct 25 2024 (12:20) 
##           By: Thomas Alexander Gerds
##     Update #: 22
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
try(setwd("~/research/Epidemi/BMI_trajectories/project_1/"),silent=TRUE)
try(setwd("L:/LovbeskyttetMapper/Researchproject/Project5_Trajectories/BMI_trajectories/project_1/"),silent=TRUE)

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
        simulate_BMI(n = 10000)
    }),
    tar_target(observed_sample_data,{
        x = sample_data[type == "observed"][,type := NULL]
        x[,number_of_measurements := .N,by = "id"]
        x[]
    }),
    tar_target(plot_sample_data,{
        plot_BMI_trajectories(sample_data = sample_data)
    }),
    tar_target(summary_statistics,{
        observed <- calculate_summary_statistics(data = observed_sample_data[number_of_measurements>1])
        sample <- calculate_summary_statistics(data = sample_data)
        # remove the value 100 because it is constant across id's
        sample[,Number_of_BMI_measurements := NULL]
        setnames(sample,"value", "gold_standard")
        setnames(observed,"value", "observed")
        out <- observed[sample,on = c("id","statistic")]
        out[,Number_of_BMI_measurements := factor(Number_of_BMI_measurements)]
        out        
    })
)


######################################################################
### _targets.R ends here
