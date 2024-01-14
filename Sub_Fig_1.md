# Scatter Plot for Germination Rates

```R
# Load required libraries
library(readxl)
library(ggplot2)

# Read data from Excel file (replace 'path/to/your/file.xlsx' with the actual file path)
excel_data <- read_excel("path/to/your/file.xlsx")

# Convert to factor
excel_data$Condition <- factor(excel_data$Condition, levels = c("NB", "SB", "GH", "CN", "CR", "FN", "FR"))

# Calculate mean values for each condition
means <- aggregate(Percentage ~ Condition, data = excel_data, mean)

# Create the scatter plot with mean lines
ggplot(excel_data, aes(x = Condition, y = Percentage, color = Condition)) +
  geom_point(position = position_jitter(width = 0.2, height = 0.2), size = 3) +
  geom_hline(data = means, aes(yintercept = Percentage, color = Condition), linetype = "dotted") +
  labs(x = "Condition", y = "Germination Rate (%)", title = "Scatter Plot of Germination Rates") +
  theme_minimal() +
  ylim(0, 105)
```
