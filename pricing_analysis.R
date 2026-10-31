# Load visualization library
library(ggplot2)

# Load dataset
data <- read.csv("/Users/edwardli/Downloads/Rideshare Telematics Project/scored_drivers.csv")

print(data)

data <- na.omit(data)

print(data)

# Calculate 30th and 80th percentiles
pct_30 <- as.numeric(quantile(data$Final_Risk_Score, 0.30))
pct_80 <- as.numeric(quantile(data$Final_Risk_Score, 0.80))

print(paste("30th Percentile Cutoff:", pct_30))
print(paste("80th Percentile Cutoff:", pct_80))

# Categorize drivers in pricing tiers based on percentiles
data$Pricing_Tier <- ifelse(data$Final_Risk_Score <= pct_30, "Safe (15% Discount)", 
                            ifelse(data$Final_Risk_Score <= pct_80, "Standard (Base Rate)", 
                                   "High Risk (30% Surcharge)"))

# Density Chart
ggplot(data, aes(x = Final_Risk_Score, fill = as.factor(Is_Full_Time))) +
  geom_density(alpha = 0.5) + 
  scale_fill_manual(values = c("0" = "#d62728", "1" = "#1f77b4"),
                   labels = c("0" = "Part_Time Fleet", "1" = "Full_Time Fleet")) +
  labs(title = "Apex Telematics Index: Risk Profile Segmentation",
       subtitle = "Demonstrating structural risk variances between full-time and part-time drivers",
       x = "Final Calculated Risk Score",
       y = "Density (Frequency Concentration)",
       fill = "Driver Segment") +
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold", size = 14))

ggsave("telematics_risk_distribution.png", width = 8, height = 5, dpi = 300)

print(table(data$Pricing_Tier))