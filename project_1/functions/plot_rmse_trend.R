plot_rmse_trend <- function(summary_statistics){
    # root mean squared error
    rmse <- summary_statistics[,sqrt(mean((gold_standard-observed)^2, na.rm = TRUE)), by = c("statistic", "Number_of_BMI_measurements")]%>%
        rename(rmse = V1)  %>%
        subset(!is.na(Number_of_BMI_measurements)) 
    g = ggplot(rmse, aes(y = rmse, x = Number_of_BMI_measurements, group = statistic)) +
        geom_point(size = 2) +  
        geom_line(color = "black", linewidth = 1) +  # Connect points with lines
        # geom_smooth(method = "loess", color = "red", linetype = "dashed", se = FALSE) +  # Add smooth trend
        facet_wrap(~statistic, scales = "free", ncol = 5) +  
        theme_bw() +
        labs(x = "Number of BMI Measurements", y = "RMSE", title = "RMSE Trends by Statistic")+
        scale_x_discrete(breaks = seq(0, 50, by = 5))+
        theme(
            plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
            legend.position = "top",
            axis.title = element_text(size = 16, face = "bold"),
            strip.text = element_text(size = 12, face = "bold"),  # Improve facet labels readability
            axis.text.x = element_text(angle = 45, hjust = 1)
        ) 
    ggsave(filename = 'figures/rmse_trend.pdf',plot = g, width = 20, height = 12)
    return(g)
}
