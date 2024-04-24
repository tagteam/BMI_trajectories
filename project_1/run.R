### run.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Apr 24 2024 (13:42) 
## Version: 
## Last-Updated: Apr 24 2024 (13:57) 
##           By: Thomas Alexander Gerds
##     Update #: 3
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
# run all targets
tar_make()
# load the targets
tar_load_everything()
# look at the trajectories where the observation points are marked 
plot_sample_data
# look at the observed data (long format)
observed_sample_data



######################################################################
### run.R ends here
