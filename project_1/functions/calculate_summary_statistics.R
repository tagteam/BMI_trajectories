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
  # Function to calculate total AUC
  calc_AUC <- function(age, bmi){
    delta = diff(age)
    mid_bmi = (bmi[-1] + bmi[-length(bmi)])/2
    return(sum(delta * mid_bmi))
  }
  
  # Function to calculate AUC above a threshold
  calc_AUC_threshold <- function(age, bmi, threshold){
    at <- get_crossing_times(time = age, measurements = bmi, threshold = threshold)
    age_included <- c(age[bmi >= threshold], at)
    bmi_included <- c(bmi[bmi >= threshold], rep(threshold, length(at))) - threshold
    bmi_included <- bmi_included[order(age_included)]
    age_included <- age_included[order(age_included)]
    calc_AUC(age = age_included, bmi = bmi_included)
  }
  
  # Function to get crossing times for BMI thresholds
  get_crossing_times <- function(time, measurements, threshold) {
    t1 <- head(time, -1)
    t2 <- tail(time, -1)
    m1 <- head(measurements, -1)
    m2 <- tail(measurements, -1)
    
    crossing_indices <- which((m1 < threshold & m2 >= threshold) | (m1 >= threshold & m2 < threshold))
    
    crossing_times <- t1[crossing_indices] + 
      (t2[crossing_indices] - t1[crossing_indices]) * 
      (threshold - m1[crossing_indices]) / (m2[crossing_indices] - m1[crossing_indices])
    
    return(crossing_times)
  }
  
  # Function to compute total time spent in a given BMI category
  calc_time_above_threshold <- function(age, bmi, threshold) {
    above_threshold <- bmi >= threshold
    time_intervals <- diff(age)
    return(sum(time_intervals[above_threshold[-length(above_threshold)]]))  # Sum only where BMI is above threshold
  }
  
  data[, {
    age_interval_length <- age[length(age)] - age[1]  # Total observation period
    AUC = calc_AUC(age, bmi)
    AUC_18 = calc_AUC_threshold(age, bmi, threshold = 18)
    AUC_25 = calc_AUC_threshold(age, bmi, threshold = 25)
    AUC_30 = calc_AUC_threshold(age, bmi, threshold = 30)
    count = .N
    
    # Calculate the time spent in each BMI category
    time_in_normal <- calc_time_above_threshold(age, bmi, threshold = 18) - calc_time_above_threshold(age, bmi, threshold = 25)
    time_in_overweight_or_obesity <- calc_time_above_threshold(age, bmi, threshold = 25)
    time_in_obesity <- calc_time_above_threshold(age, bmi, threshold = 30)
    
    Number_of_group_changes = 
      length(get_crossing_times(time = age, measurements = bmi, threshold = 18)) + 
      length(get_crossing_times(time = age, measurements = bmi, threshold = 25)) + 
      length(get_crossing_times(time = age, measurements = bmi, threshold = 30))
    
    result <- rbind(
      data.table(statistic = "First measured BMI", value = bmi[1]),
      data.table(statistic = "Last measured BMI", value = bmi[length(bmi)]),
      data.table(statistic = "AUC", value = AUC),
      data.table(statistic = "AUC (BMI>=25)", value = AUC_25),
      data.table(statistic = "AUC (BMI>=30)", value = AUC_30),
      data.table(statistic = "Total variation", value = sum(abs(diff(bmi))) / bmi[1]),
      data.table(statistic = "Percentage of time (BMI<25)", value = time_in_normal / age_interval_length),  # % Time BMI < 25
      data.table(statistic = "Percentage of time (BMI>=25)", value = time_in_overweight_or_obesity / age_interval_length),  # % Time BMI ≥ 25
      data.table(statistic = "Percentage of time (BMI>=30)", value = time_in_obesity / age_interval_length),  # % Time BMI ≥ 30
      data.table(statistic = "Coefficient of variation", value = sd(bmi) / mean(bmi)),
      data.table(statistic = "Percentage of BMI group changes", value = Number_of_group_changes / count),
      data.table(statistic = "Percentage of relative changes>=5%", value = sum(diff(bmi) / bmi[-1] >= 0.05) / count),
      data.table(statistic = "Percentage of relative changes>=10%", value = sum(diff(bmi) / bmi[-1] >= 0.1) / count),
      data.table(statistic = "Total velocity", value = (bmi[length(bmi)] - bmi[1]) / age_interval_length),
      data.table(statistic = "First velocity", value = (bmi[2] - bmi[1]) / (age[2] - age[1]))
    )
    
    result[, Number_of_BMI_measurements := count]
    result[]
  }, by = "id"]
}



######################################################################
### calculate_summary_statistics.R ends here
