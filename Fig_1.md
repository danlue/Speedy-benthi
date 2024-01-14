# Violin plot

```R
# Load the necessary libraries
library(readxl)
library(ggplot2)

# Read data from Excel file (replace 'path/to/your/file.xlsx' with the actual file path)
data <- read_excel("path/to/your/file.xlsx")

# Define the desired order of conditions
condition_order <- c("NB", "SB", "GH")

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
  scale_fill_manual(values = c("NB" = "lightblue", "SB" = "lightcoral", "GH" = "mediumpurple1")) +
  scale_color_manual(values = c("NB" = "blue", "SB" = "red", "GH" = "purple")) +
  labs(x = "Condition", y = "Days Post Planting (dpp)", title = "Comparison of dpp between NB, SB, and GH conditions") +
  
  # Add mean lines for each condition
  geom_hline(data = mean_dpp_data, aes(yintercept = dpp, color = condition), linetype = "dotted") + # Keep this line as it is
  
  # Adjust legend
  guides(color = guide_legend(title = "Condition"))

# Print the violin plot with mean lines
print(p_dpp_violin_with_mean)


# Save the plot
ggsave("path/to/your/file.pdf", width = 5, height = 3.5)
```
# Statistical analyses

```R
# Load the necessary libraries
library(dunn.test)

# Perform Shapiro-Wilk test for "dpp" in each condition
shapiro_dpp_NB <- shapiro.test(data$dpp[data$condition == "NB"])
shapiro_dpp_SB <- shapiro.test(data$dpp[data$condition == "SB"])
shapiro_dpp_GH <- shapiro.test(data$dpp[data$condition == "GH"])

# Print the results
print("Shapiro-Wilk Test for dpp:")
print(shapiro_dpp_NB)
print(shapiro_dpp_SB)
print(shapiro_dpp_GH)

# Perform Kruskal-Wallis test for "dpp" across all conditions
kruskal_test_result <- kruskal.test(dpp ~ condition, data)

# Print the results
print(kruskal_test_result)

# Perform Dunn test for post-hoc pairwise comparisons
posthoc_dunn <- dunn.test(data$dpp, data$condition, method = "bonferroni")

# Print the results
print(posthoc_dunn)
```
