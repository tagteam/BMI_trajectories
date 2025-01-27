### calculate_summary_statistics.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Oct  1 2024 (11:05) 
## Version: 
## Last-Updated: Oct 25 2024 (12:17) 
##           By: Thomas Alexander Gerds
##     Update #: 19
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
calculate_summary_statistics <- function(data){
    calc_AUC <- function(age, bmi){
        delta = diff(age)
        mid_bmi = (bmi[-1] + bmi[-length(bmi)])/2
        return(sum(delta*mid_bmi))
    }
    calc_AUC_threshold <- function(age, bmi, threshold){
        at <- get_crossing_times(time = age,measurements = bmi,threshold = threshold)
        age_included <- c(age[bmi >= threshold],at)
        # substract the threshold from the observed values that are above the threshold
        bmi_included <- c(bmi[bmi >= threshold],rep(threshold,length(at)))-threshold
        bmi_included <- bmi_included[order(age_included)]
        age_included <- age_included[order(age_included)]
        calc_AUC(age = age_included,bmi = bmi_included)
    }
    get_crossing_times <- function(time, measurements, threshold) {
        # Compute differences in consecutive time and measurement points
        t1 <- head(time, -1)     # All elements except the last
        t2 <- tail(time, -1)     # All elements except the first
        m1 <- head(measurements, -1)
        m2 <- tail(measurements, -1)
        # Identify intervals where the measurements cross the threshold
        crossing_indices <- which((m1 < threshold & m2 > threshold) | (m1 > threshold & m2 < threshold))
        # Calculate crossing times for the intervals where crossing occurs
        crossing_times <- t1[crossing_indices] + 
            (t2[crossing_indices] - t1[crossing_indices]) * 
            (threshold - m1[crossing_indices]) / (m2[crossing_indices] - m1[crossing_indices])
        return(crossing_times)
    }
    
    data[,{
        age_interval_length <- age[length(age)]-age[1]
        AUC = calc_AUC(age,bmi)
        AUC_18 = calc_AUC_threshold(age,bmi,threshold = 18)
        AUC_25 = calc_AUC_threshold(age,bmi,threshold = 25)
        AUC_30 = calc_AUC_threshold(age,bmi,threshold = 30)
        count = .N
        Number_of_group_changes = 
            length(get_crossing_times(time = age,measurements = bmi,threshold = 18))+ 
            length(get_crossing_times(time = age,measurements = bmi,threshold = 25))+ 
            length(get_crossing_times(time = age,measurements = bmi,threshold = 30))
        result <- rbind(data.table(statistic = "FirstBMI", value = bmi[1]),
                        data.table(statistic = "LastBMI", value = bmi[length(bmi)]),
                        data.table(statistic = "AUC", value = AUC),
                        data.table(statistic = "AUC above 25", value = AUC_25),
                        data.table(statistic = "AUC above 30", value = AUC_30),
                        data.table(statistic = "TotalVariation", value = sum(abs(diff(bmi)))/bmi[1]),
                        # data.table(statistic = "Resilience normal",value = AUC_18),
                        # data.table(statistic = "Resilience overweight or obesity", value=AUC-AUC_18),
                        # data.table(statistic = "Resilience obesity", value=AUC-AUC_25),
                        data.table(statistic = "Coefficient of variation", value=sd(bmi)/mean(bmi)),
                        data.table(statistic = "Number of BMI category changes",value = Number_of_group_changes),
                        data.table(statistic = "Percentage of group changes",value = Number_of_group_changes/count),
                        data.table(statistic = "Number of relative changes 5%", value = sum(diff(bmi)/bmi[-1]>0.05)),
                        data.table(statistic = "Percentage of relative changes 5%", value = sum(diff(bmi)/bmi[-1]>0.05)/count),
                        data.table(statistic = "Number of relative changes 10%",value = sum(diff(bmi)/bmi[-1]>0.1)),
                        data.table(statistic = "Percentage of relative changes 10%",value= sum(diff(bmi)/bmi[-1]>0.1)/count),
                        data.table(statistic = "Total velocity",value = (bmi[length(bmi)]-bmi[1])/(age_interval_length)),
                        data.table(statistic = "First velocity",value = (bmi[2]-bmi[1])/(age[2]-age[1])))
        result[,Number_of_BMI_measurements := rep(count,.N)]
        result[]

    } ,by = "id"]
}


######################################################################
### calculate_summary_statistics.R ends here
