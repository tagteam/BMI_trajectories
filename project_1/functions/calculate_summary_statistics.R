### calculate_summary_statistics.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Oct  1 2024 (11:05) 
## Version: 
## Last-Updated: Oct  1 2024 (12:01) 
##           By: Thomas Alexander Gerds
##     Update #: 10
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
        .(count = count,
          AUC = AUC,
          AUC_25 = AUC_25,
          AUC_30 = AUC_30,
          TotalVariation = sum(abs(diff(bmi))),
          Resilience_normal = AUC_18-AUC_25,
          Resilience_overweight_or_obesity = AUC_25,
          Resilience_obesity = AUC_30,
          Coefficient_of_variation = sd(bmi)/mean(bmi),
          Number_of_group_changes = Number_of_group_changes,
          Percentage_of_group_changes = Number_of_group_changes/count,
          Number_of_relative_changes05 = sum(diff(bmi)/bmi[-1]>0.05),
          Percentage_of_relative_changes05 = sum(diff(bmi)/bmi[-1]>0.05)/count,
          Number_of_relative_changes10 = sum(diff(bmi)/bmi[-1]>0.1),
          Percentage_of_relative_changes10 = sum(diff(bmi)/bmi[-1]>0.1)/count,
          Total_velocity = (bmi[length(bmi)]-bmi[1])/(age_interval_length),
          First_velocity = (bmi[2]-bmi[1])/(age[2]-age[1])
          )
        #
    } ,by = "id"]
}


######################################################################
### calculate_summary_statistics.R ends here
