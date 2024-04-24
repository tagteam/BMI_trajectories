### run.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Apr 24 2024 (13:42) 
## Version: 
## Last-Updated: Apr 24 2024 (13:46) 
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
try(setwd("~/research/Epidemi/BMI_trajectories/project_1/"))
library(targets)
tar_make()
tar_load_everything()
plot_sample_data

######################################################################
### run.R ends here
