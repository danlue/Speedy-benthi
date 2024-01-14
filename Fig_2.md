# Violin plot for generation times

```R
# Load the necessary libraries
library(readxl)
library(ggplot2)

# Read data from Excel file (replace 'path/to/your/file.xlsx' with the actual file path)
data <- read_excel("path/to/your/file.xlsx")

# Define the desired order of conditions
condition_order <- c("CN", "CR", "FN", "FR")

# Calculate the means for each condition
mean_dpp_data <- aggregate(dpp ~ condition, data, mean)
print(mean_dpp_data)

# Calculate standard deviation for dpp within each condition
sd_dpp_data <- aggregate(dpp ~ condition, data, sd)
print(sd_dpp_data)

# Create the customized violin plot with data points, mean lines, and the desired order
p_dpp_violin_with_mean <- ggplot(data, aes(x = factor(condition, levels = condition_order), y = dpp, fill = condition)) +
  geom_violin(trim = FALSE, draw_quantiles = c(0.25, 0.5, 0.75), scale = "width") +
  geom_jitter(aes(color = condition), width = 0.15, alpha = 0.7, size = 3, linewidth = 1.5) + # Set linewidth within geom_jitter
  scale_fill_manual(values = c("FN" = "lightblue", "CN" = "lightcoral", "FR" = "mediumpurple1", "CR" = "brown2" )) +
  scale_color_manual(values = c("FN" = "blue", "CN" = "red", "FR" = "purple", "CR" = "brown4")) +
  labs(x = "Condition", y = "Days Post Planting (dpp)", title = "Comparison of dpp between FN, CN, and GH conditions") +
  
  # Add mean lines for each condition
  geom_hline(data = mean_dpp_data, aes(yintercept = dpp, color = condition), linetype = "dotted") + # Keep this line as it is
  
  # Adjust legend
  guides(color = guide_legend(title = "Condition"))

# Print the violin plot with mean lines
print(p_dpp_violin_with_mean)

# Save the plot
ggsave("path/to/your/file.pdf", width = 5, height = 3.5)
```

# Statistical analyses for generation times

```R
# Load the necessary libraries
library(dunn.test)

# Perform Shapiro-Wilk test for "dpp" in each condition
shapiro_dpp_FN <- shapiro.test(data$dpp[data$condition == "FN"])
shapiro_dpp_CN <- shapiro.test(data$dpp[data$condition == "CN"])
shapiro_dpp_FR <- shapiro.test(data$dpp[data$condition == "FR"])
shapiro_dpp_CR <- shapiro.test(data$dpp[data$condition == "CR"])

# Print the results
print("Shapiro-Wilk Test for dpp:")
print(shapiro_dpp_FN)
print(shapiro_dpp_CN)
print(shapiro_dpp_FR)
print(shapiro_dpp_CR)

# Perform Kruskal-Wallis test for "dpp" across all conditions
kruskal_test_result <- kruskal.test(dpp ~ condition, data)

# Print the results
print(kruskal_test_result)

# Perform Dunn test for post-hoc pairwise comparisons
posthoc_dunn <- dunn.test(data$dpp, data$condition, method = "bonferroni")

# Print the results
print(posthoc_dunn)
```

# Violin plot for plant height

```R
# Load the necessary libraries
library(readxl)
library(ggplot2)

# Read data from Excel file (replace 'path/to/your/file.xlsx' with the actual file path)
data <- read_excel("/path/to/your/file.xlsx")

# Define the desired order of conditions
condition_order <- c("CN", "CR", "FN", "FR")

# Calculate the means for each condition
mean_height_data <- aggregate(height ~ condition, data, mean)
print(mean_height_data)

# Calculate standard deviation for height within each condition
sd_height_data <- aggregate(height ~ condition, data, sd)
print(sd_height_data)

# Create the customized violin plot with data points, mean lines, and the desired order for "height"
p_height_violin_with_mean <- ggplot(data, aes(x = factor(condition, levels = condition_order), y = height, fill = condition)) +
  geom_violin(trim = FALSE, draw_quantiles = c(0.25, 0.5, 0.75), scale = "width") +
  geom_jitter(aes(color = condition), width = 0.15, alpha = 0.7, size = 3, linewidth = 1.5) + # Use linewidth instead of size
  scale_fill_manual(values = c("FN" = "lightblue", "CN" = "lightcoral", "FR" = "mediumpurple1", "CR" = "brown2" )) +
  scale_color_manual(values = c("FN" = "blue", "CN" = "red", "FR" = "purple", "CR" = "brown4")) +
  labs(x = "Condition", y = "Plant Height", title = "Comparison of Plant Height between FN, CN, and GH conditions") +
  
  # Add mean lines for "height" for each condition
  geom_hline(data = mean_height_data, aes(yintercept = height, color = condition), linetype = "dotted") +
  
  # Adjust legend
  guides(color = guide_legend(title = "Condition"))

# Print the violin plot with mean lines for "height"
print(p_height_violin_with_mean)

# Save the plot
ggsave("/path/to/your/file.pdf", width = 5, height = 3.5)
```

# Statistical analyses for plant height

```R
# Load the necessary libraries
library(car)

# Perform Shapiro-Wilk test for "height" in each condition
shapiro_height_FN <- shapiro.test(data$height[data$condition == "FN"])
shapiro_height_CN <- shapiro.test(data$height[data$condition == "CN"])
shapiro_height_FR <- shapiro.test(data$height[data$condition == "FR"])
shapiro_height_CR <- shapiro.test(data$height[data$condition == "CR"])

print("Shapiro-Wilk Test for height:")
print(shapiro_height_FN)
print(shapiro_height_CN)
print(shapiro_height_FR)
print(shapiro_height_CR)

# Perform Levene test for "height"
levene_test_result <- leveneTest(height ~ condition, data = data)

# Print the Levene's test result
print(levene_test_result)

# Perform anova for "height"
anova_result <- aov(height ~ condition, data = data)

# Print the ANOVA result
print(anova_result)

# Display the summary of ANOVA
summary(anova_result)

# post hoc test
tukey_result <- TukeyHSD(anova_result)

# Display the Tukey's HSD test result
print(tukey_result)
```
