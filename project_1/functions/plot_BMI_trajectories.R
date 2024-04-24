### plot_BMI_trajectories.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Apr 24 2024 (13:40) 
## Version: 
## Last-Updated: Apr 24 2024 (13:44) 
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
plot_BMI_trajectories <- function(sample_data){
    ggplot(sample_data, aes(x = age, y = bmi, group = id)) +
        theme_bw() +
        geom_line(col = "gray") +
        geom_point(data = sample_data[type == "observed"]) + 
        facet_wrap(~paste("id =", id))
}


######################################################################
### plot_BMI_trajectories.R ends here
