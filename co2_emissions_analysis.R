# R code analyzing potential causes of CO2_emissions
# The objective is to define a regression model based on the dataset

################################################
#Simplified code - find the complete method below
################################################

# Load necessary libraries
library(car)
library(ggplot2)
library(scales)
library(dplyr)
library(reshape2)
library(GGally)
library(e1071)

# Load the dataset
head(ds_assign)
summary(ds_assign)

# Function to create histograms with optional log transformation
create_histogram <- function(data, var, log_transform = FALSE, bins = 30) {
  if (log_transform) {
    data[[paste0("log_", var)]] <- log(data[[var]])
    var <- paste0("log_", var)
  }
  ggplot(data, aes_string(x = var)) +
    geom_histogram(fill = "skyblue", color = "black", bins = bins) +
    labs(title = paste("Histogram of", var), x = var, y = "Frequency") +
    stat_bin(geom = "text", aes(label = ifelse(..count.. != 0, ..count.., ""), vjust = -0.5))
}

# Create histograms for CO2_emissions and Population
create_histogram(ds_assign, "CO2_emissions")
create_histogram(ds_assign, "CO2_emissions", log_transform = TRUE)
create_histogram(ds_assign, "Population")
create_histogram(ds_assign, "Population", log_transform = TRUE)

# Create summary and histograms for other variables
summary(ds_assign$Happiness_index)
create_histogram(ds_assign, "Happiness_index")
summary(ds_assign$Life_expectancy)
create_histogram(ds_assign, "Life_expectancy")
summary(ds_assign$Fertility_rate)
create_histogram(ds_assign, "Fertility_rate")
create_histogram(ds_assign, "Fertility_rate", log_transform = TRUE)
summary(ds_assign$Corruption_perception_index)
create_histogram(ds_assign, "Corruption_perception_index")
create_histogram(ds_assign, "Corruption_perception_index", log_transform = TRUE)

# Scatter plot function with optional log transformation
create_scatter_plot <- function(data, x_var, y_var, log_transform = FALSE) {
  if (log_transform) {
    data[[paste0("log_", x_var)]] <- log(data[[x_var]])
    data[[paste0("log_", y_var)]] <- log(data[[y_var]])
    x_var <- paste0("log_", x_var)
    y_var <- paste0("log_", y_var)
  }
  ggplot(data, aes_string(x = x_var, y = y_var)) +
    geom_point() +
    labs(title = paste("Scatter Plot of", x_var, "vs.", y_var), x = x_var, y = y_var) +
    theme_minimal()
}

# Create scatter plots
create_scatter_plot(ds_assign, "Population", "CO2_emissions")
create_scatter_plot(ds_assign, "Population", "CO2_emissions", log_transform = TRUE)
create_scatter_plot(ds_assign, "Happiness_index", "CO2_emissions")
create_scatter_plot(ds_assign, "Happiness_index", "CO2_emissions", log_transform = TRUE)
create_scatter_plot(ds_assign, "Fertility_rate", "CO2_emissions")
create_scatter_plot(ds_assign, "Fertility_rate", "CO2_emissions", log_transform = TRUE)

# Fit multiple regression models
fit_model <- function(data, formula) {
  model <- lm(formula, data = data)
  summary(model)
}

# Transform the dataset
ds_assign <- ds_assign %>%
  mutate(log_CO2_emissions = log(CO2_emissions),
         log_Population = log(Population),
         log_Fertility_rate = log(Fertility_rate),
         Income_level = factor(Income.level))

# Fit the model with log-transformed variables
model_formula <- log_CO2_emissions ~ log_Population + Happiness_index + log_Fertility_rate + Income_level
model_summary <- fit_model(ds_assign, model_formula)

# Print model summary and VIF
print(model_summary)
print(vif(model_summary))

# Create a pairs plot
ggpairs(ds_assign, columns = c("log_CO2_emissions", "log_Population", "Happiness_index", "log_Fertility_rate"))

# Cook's distance plot
cooks_d <- cooks.distance(model_summary)
ggplot(data.frame(Index = 1:length(cooks_d), CooksD = cooks_d), aes(x = Index, y = CooksD)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_hline(yintercept = 4/(nrow(ds_assign) - length(model_summary$coefficients) - 1), linetype = "dashed", color = "red") +
  labs(title = "Cook's Distance Plot", x = "Observation Index", y = "Cook's Distance") +
  theme_minimal()


#downloading images 


# Save correlation plot
ggpairs_plot <- ggpairs(ds_assign, columns = c("log_CO2_emissions", "log_Population", "Happiness_index", "log_Fertility_rate"))
ggsave("correlation_plot.png", ggpairs_plot, width = 8, height = 6)

# Save Q-Q plot of residuals
png("qq_plot.png", width = 800, height = 600)
qqnorm(residuals(model_summary))
qqline(residuals(model_summary))
dev.off()

# Save Residuals vs. Log Population plot
png("residuals_vs_log_population.png", width = 800, height = 600)
ggplot(model_frame, aes(x = log_Population, y = studentized_residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Studentized Residuals vs. Log Population", x = "Log Population", y = "Studentized Residuals") +
  theme_minimal()
dev.off()

# Save Component Plus Residual Plots
png("cp_residual_population.png", width = 800, height = 600)
crPlots(model_summary, variable = "log_Population")
dev.off()

png("cp_residual_happiness.png", width = 800, height = 600)
crPlots(model_summary, variable = "Happiness_index")
dev.off()

png("cp_residual_fertility.png", width = 800, height = 600)
crPlots(model_summary, variable = "log_Fertility_rate")
dev.off()


##############################
#Complete, step-by-step method
##############################

head(ds_assign)
summary(ds_assign)

#UNIVARIATE GRAPH OF CO2_emissions 

# Create the histogram with ggplot
ggplot(ds_assign, aes(x = CO2_emissions)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +  # Adjust the number of bins as needed
  labs(title = "Histogram of CO2 Emissions in metric tons", x = "CO2 Emissions in metric tons", y = "Frequency") +  # Add title and axis labels
  stat_bin(geom = "text", aes(label = ifelse(..count.. != 0, ..count.., ""), vjust = -0.5))  # Add counts over each bar if count is not zero

# Log transformation of CO2_emissions
ds_assign$log_CO2_emissions <- log(ds_assign$CO2_emissions)

# Create the histogram with ggplot
ggplot(ds_assign, aes(x = log_CO2_emissions)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +  # Adjust the number of bins as needed
  labs(title = "Histogram of Log Transformed CO2 Emissions", x = "Log CO2 Emissions", y = "Frequency") +  # Add title and axis labels
  stat_bin(geom = "text", aes(label = ifelse(..count.. != 0, ..count.., ""), vjust = -0.5))  # Add counts over each bar if count is not zero

#UNIVARIATE GRAPH OF POPULATION 

# Create the histogram with ggplot
ggplot(ds_assign, aes(x = Population)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +  # Adjust the number of bins as needed
  labs(title = "Histogram of Population", x = "Population", y = "Frequency") +  # Add title and axis labels
  scale_x_continuous(breaks = pretty(ds_assign$Population, n = 10), labels = scales::comma) +  # Set breaks and format labels for x-axis
  stat_bin(geom = "text", aes(label = ifelse(..count.. != 0, ..count.., ""), vjust = -0.5))  # Add counts over each bar if count is not zero

# Log transformation of Population
ds_assign$log_Population <- log(ds_assign$Population)

# Create the histogram with ggplot
ggplot(ds_assign, aes(x = log_Population)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +  # Adjust the number of bins as needed
  labs(title = "Histogram of Log Transformed Population", x = "Log Population", y = "Frequency") +  # Add title and axis labels
  scale_x_continuous(breaks = pretty(log(ds_assign$Population), n = 10), labels = scales::comma) +  # Set breaks and format labels for x-axis
  stat_bin(geom = "text", aes(label = ifelse(..count.. != 0, ..count.., ""), vjust = -0.5))  # Add counts over each bar if count is not zero

#UNIVARIATE GRAPH OF Happiness_index 
summary(ds_assign$Happiness_index)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 2.400   4.890   5.580   5.554   6.310   7.820      92 

# Create the histogram with ggplot
ggplot(ds_assign, aes(x = Happiness_index)) +
  geom_histogram(fill = "skyblue", color = "black") +  # Adjust fill and color as needed
  labs(title = "Histogram of Happiness Index", x = "Happiness Index", y = "Frequency") +  # Add title and axis labels
  stat_bin(geom = "text", aes(label = ..count.., y = ..count..), vjust = -0.5) +  # Add counts over each bin
  scale_x_continuous(labels = comma)  # Format the numbers with commas

# Create the density plot with ggplot
ggplot(ds_assign, aes(y = Happiness_index)) +
  geom_boxplot(fill = "skyblue", color = "black") +  # Adjust fill and color as needed
  labs(title = "BoxPlot of Happiness Index", x = "Happiness Index", y = "Density") +  # Add title and axis labels
  scale_x_continuous(labels = comma)  # Format the numbers with commas

#UNIVARIATE GRAPH OF Life_expectancy 
summary(ds_assign$Life_expectancy)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 52.50   66.28   72.70   71.86   77.17   85.50      19

# Create the histogram with ggplot
ggplot(ds_assign, aes(x = Life_expectancy)) +
  geom_histogram(fill = "skyblue", color = "black") +  # Adjust fill and color as needed
  labs(title = "Histogram of Life Expectancy", x = "Life Expectancy", y = "Frequency") +  # Add title and axis labels
  stat_bin(geom = "text", aes(label = ..count.., y = ..count..), vjust = -0.5) +  # Add counts over each bin
  scale_x_continuous(labels = comma)  # Format the numbers with commas

# Create the density plot with ggplot
ggplot(ds_assign, aes(x = Life_expectancy)) +
  geom_density(fill = "skyblue", color = "black") +  # Adjust fill and color as needed
  labs(title = "Density Plot of Life Expectancy", x = "Life Expectancy", y = "Density") +  # Add title and axis labels
  scale_x_continuous(labels = comma)  # Format the numbers with commas

#UNIVARIATE GRAPH OF Fertility_rate 
summary(ds_assign$Fertility_rate)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 1.230   1.740   2.110   2.601   3.415   6.930      46 

# Create the histogram with ggplot
ggplot(ds_assign, aes(x = Fertility_rate)) +
  geom_histogram(fill = "skyblue", color = "black") +  # Adjust fill and color as needed
  labs(title = "Histogram of Fertility Rate", x = "Fertility Rate", y = "Frequency") +  # Add title and axis labels
  stat_bin(geom = "text", aes(label = ..count.., y = ..count..), vjust = -0.5) +  # Add counts over each bin
  scale_x_continuous(labels = comma)  # Format the numbers with commas

# Create the density plot with ggplot
ggplot(ds_assign, aes(x = log(Fertility_rate))) +
  geom_density(fill = "skyblue", color = "black") +  # Adjust fill and color as needed
  labs(title = "Density Plot of Fertility Rate", x = "Log Fertility Rate", y = "Density") +  # Add title and axis labels
  scale_x_continuous(labels = comma)  # Format the numbers with commas

# Create the histogram with ggplot
ggplot(ds_assign, aes(x = log(Fertility_rate))) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +  # Adjust number of bins as needed
  labs(title = "Histogram of Log Fertility Rate", 
       x = "Log Fertility Rate", y = "Count")  # Add title and axis labels

# Log transformation of Fertility_rate
ds_assign$log_Fertility_rate <- log(ds_assign$Fertility_rate)

# Create the histogram with ggplot
ggplot(ds_assign, aes(x = log_Fertility_rate)) +
  geom_histogram(fill = "skyblue", color = "black") +  # Adjust fill and color as needed
  labs(title = "Histogram of Log Transformed Fertility Rate", x = "Log Fertility Rate", y = "Frequency") +  # Add title and axis labels
  stat_bin(geom = "text", aes(label = ..count.., y = ..count..), vjust = -0.5) +  # Add counts over each bin
  scale_x_continuous(labels = scales::comma)  # Format the numbers with commas

# Log transformation of Fertility_rate
ds_assign$log_Fertility_rate <- log(ds_assign$Fertility_rate)

# Create the density plot with ggplot
ggplot(ds_assign, aes(x = log_Fertility_rate)) +
  geom_density(fill = "skyblue", color = "black") +  # Adjust fill and color as needed
  labs(title = "Density Plot of Log Transformed Fertility Rate", x = "Log Fertility Rate", y = "Density") +  # Add title and axis labels
  scale_x_continuous(labels = scales::comma)  # Format the numbers with commas

#UNIVARIATE GRAPH OF Corruption_perception_index - we need to transform

summary(ds_assign$Corruption_perception_index)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 10.00   29.00   38.00   43.12   57.00   88.00      57

# Create the histogram with ggplot
ggplot(ds_assign, aes(x = Corruption_perception_index)) +
  geom_histogram(fill = "skyblue", color = "black") +  # Adjust fill and color as needed
  labs(title = "Histogram of Corruption Perception Index", x = "Corruption Perception Index", y = "Frequency") +  # Add title and axis labels
  stat_bin(geom = "text", aes(label = ..count.., y = ..count..), vjust = -0.5) +  # Add counts over each bin
  scale_x_continuous(labels = comma)  # Format the numbers with commas

# Create the density plot with ggplot
ggplot(ds_assign, aes(x = Corruption_perception_index)) +
  geom_density(fill = "skyblue", color = "black") +  # Adjust fill and color as needed
  labs(title = "Density Plot of Corruption Perception Index", x = "Corruption Perception Index", y = "Density") +  # Add title and axis labels
  scale_x_continuous(labels = comma)  # Format the numbers with commas

# Log transformation of Corruption_perception_index
ds_assign$log_Corruption_perception_index <- log(ds_assign$Corruption_perception_index)

# Create the histogram with ggplot
ggplot(ds_assign, aes(x = log_Corruption_perception_index)) +
  geom_histogram(fill = "skyblue", color = "black") +  # Adjust fill and color as needed
  labs(title = "Histogram of Log Transformed Corruption Perception Index", x = "Log Corruption Perception Index", y = "Frequency") +  # Add title and axis labels
  stat_bin(geom = "text", aes(label = ..count.., y = ..count..), vjust = -0.5) +  # Add counts over each bin
  scale_x_continuous(labels = scales::comma)  # Format the numbers with commas

# Log transformation of Corruption_perception_index
ds_assign$log_Corruption_perception_index <- log(ds_assign$Corruption_perception_index)

# Create the density plot with ggplot
ggplot(ds_assign, aes(x = log_Corruption_perception_index)) +
  geom_density(fill = "skyblue", color = "black") +  # Adjust fill and color as needed
  labs(title = "Density Plot of Log Transformed Corruption Perception Index", x = "Log Corruption Perception Index", y = "Density") +  # Add title and axis labels
  scale_x_continuous(labels = scales::comma)  # Format the numbers with commas

################
#BIVARIATE GRAPH
################
#BIVARIATE GRAPH
################
#BIVARIATE GRAPH
################

# y = CO2_emissions 
# x = Population

#Orginal 
ggplot(ds_assign, aes(x = Population, y = CO2_emissions)) +
  geom_point() +
  labs(title = "Scatter Plot of Population vs. CO2 Emissions", x = "Population", y = "CO2 Emissions")

# Load necessary libraries
library(ggplot2)
library(scales)  # This package provides the label_comma() function

# Adjusted plot to show full numeric values
ggplot(ds_assign, aes(x = Population, y = CO2_emissions)) +
  geom_point() +
  scale_x_continuous(labels = label_comma()) +  # Adjust x axis labels
  scale_y_continuous(labels = label_comma()) +  # Adjust y axis labels
  labs(title = "Scatter Plot of Population vs. CO2 Emissions",
       x = "Population", 
       y = "CO2 Emissions") +
  theme_minimal()

# Tranformation Log (CO2_emissions) and Log(Population)
# Assuming ds_assign is your data frame with 'Population' and 'CO2_emissions' columns
# Corrected code
ggplot(ds_assign, aes(x = log(Population), y = log(CO2_emissions))) + # Fixed placement of closing parenthesis
  geom_point() +
  labs(title = "Scatter Plot of Log Population vs. Log CO2 Emissions", 
       x = "Log(Population)", # Adjusted to reflect actual x-axis variable
       y = "log (CO2 Emissions)") # Corrected to match transformed y-axis variable

# x = CO2_emissions 
# y = Happiness_index

#Original 
ggplot(ds_assign, aes(x = Happiness_index, y = CO2_emissions)) +
  geom_point() +
  labs(title = "Scatter Plot of Happiness Index vs. CO2 Emissions", x = "Happiness Index", y = "CO2 Emissions")

# transformation Log CO2_emissions and Happiness Index
ggplot(ds_assign, aes(x = Happiness_index, y = log(CO2_emissions))) + # Corrected the placement of parenthesis
  geom_point() +
  labs(title = "Scatter Plot of Happiness Index vs. CO2 Emissions", 
       x = "Happiness Index", 
       y = "Log CO2 Emissions")

# Assuming ds_assign is your data frame and ggplot2 library is already loaded
ggplot(ds_assign, aes(x = Happiness_index, y = log(CO2_emissions))) +
  geom_point() +  # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Regression line
  labs(title = "Scatter Plot of Happiness Index vs. CO2 Emissions", 
       x = "Happiness Index", 
       y = "Log CO2 Emissions")

# Assuming ds_assign is your data frame
# Fit the linear model
model <- lm(log(CO2_emissions) ~ Happiness_index, data = ds_assign)

# Output the summary of the model
model_summary <- summary(model)

# Print the summary
print(model_summary)

# R^2 of log(CO2_emissions ~ happiness index) = 0.1052 
# Hence, around 9:10 of the variation of Log CO2_emissions is NOT explained by 
# happiness Index

# Create a scatter plot of log-transformed CO2 emissions against fertility rate with a local moving average curve
ggplot(ds_assign, aes(x = Happiness_index, y = log(CO2_emissions))) +
  geom_point() + # Add points
  geom_smooth(span = 0.3) + # Local moving average; adjust span for smoothness
  labs(title = "Local Moving Average of log CO2 Emissions vs Fertility Rate",
       x = "Happiness Index",
       y = "Log of CO2 Emissions") +
  theme_minimal()

# Check the structure after transformation
str(ds_assign)

# Now you can plot the transformed variables against each other using ggplot2
# Example of plotting Log transformed CO2 emissions against Happiness index
ggplot(ds_assign, aes(x = Happiness_index, y = log_CO2_emissions)) +
  geom_point() +
  labs(title = "Scatter Plot of Happiness Index vs. Log CO2 Emissions",
       x = "Happiness Index",
       y = "Log CO2 Emissions")

# You can create similar plots for each of the transformations

ggplot(ds_assign, aes(x = Fertility_rate, y = CO2_emissions)) +
  geom_point() +
  labs(title = "Scatter Plot of Fertility Rate vs. CO2 Emissions", x = "Fertility Rate", y = "CO2 Emissions")

ggplot(ds_assign, aes(x = Life_expectancy, y = CO2_emissions)) +
  geom_point() +
  labs(title = "Scatter Plot of Life Expectancy vs. CO2 Emissions", x = "Life Expectancy", y = "CO2 Emissions")

ggplot(ds_assign, aes(x = Corruption_perception_index, y = CO2_emissions)) +
  geom_point() +
  labs(title = "Scatter Plot of Corruption Perception Index vs. CO2 Emissions", x = "Corruption Perception Index", y = "CO2 Emissions")

#log transformation of y = CO2_emissions 

# Log y = CO2_emissions, x = Population
ggplot(ds_assign, aes(x = log(Population), y = log(CO2_emissions))) +
  geom_point() +
  labs(title = "Scatter Plot of Log-Transformed Population vs. Log-Transformed CO2 Emissions", 
       x = "Log(Population)", y = "Log(CO2 Emissions)")

# Log y = CO2_emissions, x = Happiness_index
log_transform <- ggplot(ds_assign, aes(x = Happiness_index, y = log(CO2_emissions))) +
  geom_point() +
  labs(title = "Scatter Plot of Happiness Index vs. Log-Transformed CO2 Emissions", 
       x = "Happiness Index", y = "Log(CO2 Emissions)")
log_transform

# Log y = CO2_emissions, x = Fertility rate
log_transform <- ggplot(ds_assign, aes(x = log(Fertility_rate), y = log(CO2_emissions))) +
  geom_point() +
  labs(title = "Scatter Plot of Log-Transformed Fertility Rate vs. Log-Transformed CO2 Emissions", 
       x = "Log(Fertility Rate)", y = "Log(CO2 Emissions)")
log_transform

# Log y = CO2_emissions, x = Life Expectancy
log_transform <- ggplot(ds_assign, aes(x = Life_expectancy, y = log(CO2_emissions))) +
  geom_point() +
  labs(title = "Scatter Plot of Life Expectancy vs. Log-Transformed CO2 Emissions", 
       x = "Life Expectancy", y = "Log(CO2 Emissions)")
log_transform

# y = population

ggplot(ds_assign, aes(x = CO2_emissions, y = Population)) +
  geom_point() +
  labs(title = "Scatter Plot of CO2 Emissions vs. Population", x = "CO2 Emissions", y = "Population")

# x = happiness index - NOT GOOD 
ggplot(ds_assign, aes(x = Happiness_index, y = Population)) +
  geom_point() +
  labs(title = "Scatter Plot of Happiness Index vs. Population", x = "Happiness Index", y = "Population")

# Transformation
ggplot(ds_assign, aes(x = Happiness_index, y = log(Population))) +  # Fixed the placement of parenthesis
  geom_point() +
  labs(title = "Scatter Plot of Happiness Index vs. Log Population",  # Title corrected to 'Log Population'
       x = "Happiness Index", 
       y = "Log of Population")  # Y label corrected to 'Log of Population'

# x = fertility rate 
ggplot(ds_assign, aes(x = Fertility_rate, y = Population)) +
  geom_point() +
  labs(title = "Scatter Plot of Fertility Rate vs. Population", x = "Fertility Rate", y = "Population")

# Assuming ggplot2 library is already loaded
ggplot(ds_assign, aes(x = Fertility_rate, y = log(Population))) + 
  geom_point() +
  labs(title = "Scatter Plot of Fertility Rate vs. Log of Population", 
       x = "Fertility Rate", 
       y = "Log of Population")

ggplot(ds_assign, aes(x = Life_expectancy, y = Population)) +
  geom_point() +
  labs(title = "Scatter Plot of Life Expectancy vs. Population", x = "Life Expectancy", y = "Population")

ggplot(ds_assign, aes(x = Life_expectancy, y = log(Population))) +
  geom_point() +
  labs(title = "Scatter Plot of Life Expectancy vs. Log of Population",
       x = "Life Expectancy", 
       y = "Log of Population")  # Corrected y-axis label

ggplot(ds_assign, aes(x = Corruption_perception_index, y = Population)) +
  geom_point() +
  labs(title = "Scatter Plot of Corruption Perception Index vs. Population", x = "Corruption Perception Index", y = "Population")

# Log transformation of y = Population

# Log y = Population, x = log(CO2_emissions)
log_transform_population <- ggplot(ds_assign, aes(x = log(CO2_emissions), y = log(Population))) +
  geom_point() +
  labs(title = "Scatter Plot of Log-Transformed CO2 Emissions vs. Log-Transformed Population", 
       x = "Log(CO2 Emissions)", y = "Log(Population)")

# Log y = Population, x = Happiness_index
log_transform_happiness <- ggplot(ds_assign, aes(x = Happiness_index, y = log(Population))) +
  geom_point() +
  labs(title = "Scatter Plot of Happiness Index vs. Log-Transformed Population", 
       x = "Happiness Index", y = "Log(Population)")

# Log y = Population, x = Fertility rate
log_transform_fertility <- ggplot(ds_assign, aes(x = log(Fertility_rate), y = log(Population))) +
  geom_point() +
  labs(title = "Scatter Plot of Log-Transformed Fertility Rate vs. Log-Transformed Population", 
       x = "Log(Fertility Rate)", y = "Log(Population)")

# Log y = Population, x = Life Expectancy
log_transform_life_expectancy <- ggplot(ds_assign, aes(x = Life_expectancy, y = log(Population))) +
  geom_point() +
  labs(title = "Scatter Plot of Life Expectancy vs. Log-Transformed Population", 
       x = "Life Expectancy", y = "Log(Population)")

# Life exp^2 and log(population)
exp_transform <- ggplot(ds_assign, aes(x = Life_expectancy^2, y = log(Population))) +
  geom_point() +
  labs(title = "Scatter Plot of Life Expectancy^2 vs. Log Transformed Population", 
       x = "Life Expectancy^2", y = "log(Population)")

# Log y = Population, x = Corruption_perception_index
log_transform_corruption <- ggplot(ds_assign, aes(x = Corruption_perception_index, y = log(Population))) +
  geom_point() +
  labs(title = "Scatter Plot of Corruption Perception Index vs. Log-Transformed Population", 
       x = "Corruption Perception Index", y = "Log(Population)")

# View plots
log_transform_population
log_transform_happiness
log_transform_fertility
log_transform_life_expectancy
exp_transform # y = log(Pop), x = (life exp)^2 
log_transform_corruption

# y = Happiness_index 

ggplot(ds_assign, aes(x = CO2_emissions, y = Happiness_index)) +
  geom_point() +
  labs(title = "Scatter Plot of CO2 Emissions vs. Happiness Index", x = "CO2 Emissions", y = "Happiness Index")

ggplot(ds_assign, aes(x = Population, y = Happiness_index)) +
  geom_point() +
  labs(title = "Scatter Plot of Population vs. Happiness Index", x = "Population", y = "Happiness Index")

ggplot(ds_assign, aes(x = Fertility_rate, y = Happiness_index)) +
  geom_point() +
  labs(title = "Scatter Plot of Fertility Rate vs. Happiness Index", x = "Fertility Rate", y = "Happiness Index")

ggplot(ds_assign, aes(x = Life_expectancy, y = Happiness_index)) +
  geom_point() +
  labs(title = "Scatter Plot of Life Expectancy vs. Happiness Index", x = "Life Expectancy", y = "Happiness Index")

ggplot(ds_assign, aes(x = Corruption_perception_index, y = Happiness_index)) +
  geom_point() +
  labs(title = "Scatter Plot of Corruption Perception Index vs. Happiness Index", x = "Corruption Perception Index", y = "Happiness Index")

# log transformation of y = Happiness_index 

# Log y = Happiness_index, x = log(Population)
log_transform_happiness <- ggplot(ds_assign, aes(x = log(Population), y = Happiness_index)) +
  geom_point() +
  labs(title = "Scatter Plot of Log-Transformed Population vs. Happiness Index", 
       x = "Log(Population)", y = "Happiness Index")

# Log y = Happiness_index, x = Fertility rate
log_transform_fertility <- ggplot(ds_assign, aes(x = log(Fertility_rate), y = log(Happiness_index))) +
  geom_point() +
  labs(title = "Scatter Plot of Log-Transformed Fertility Rate vs. Log-Transformed Happiness Index", 
       x = "log(Fertility Rate)", y = "log(Happiness Index)")

# View plots
log_transform_happiness
log_transform_fertility

# y = Fertility_rate

ggplot(ds_assign, aes(x = CO2_emissions, y = Fertility_rate)) +
  geom_point() +
  labs(title = "Scatter Plot of CO2 Emissions vs. Fertility Rate", x = "CO2 Emissions", y = "Fertility Rate")

ggplot(ds_assign, aes(x = Population, y = Fertility_rate)) +
  geom_point() +
  labs(title = "Scatter Plot of Population vs. Fertility Rate", x = "Population", y = "Fertility Rate")

ggplot(ds_assign, aes(x = Happiness_index, y = Fertility_rate)) +
  geom_point() +
  labs(title = "Scatter Plot of Happiness Index vs. Fertility Rate", x = "Happiness Index", y = "Fertility Rate")

ggplot(ds_assign, aes(x = Life_expectancy, y = Fertility_rate)) +
  geom_point() +
  labs(title = "Scatter Plot of Life Expectancy vs. Fertility Rate", x = "Life Expectancy", y = "Fertility Rate")

ggplot(ds_assign, aes(x = Corruption_perception_index, y = Fertility_rate)) +
  geom_point() +
  labs(title = "Scatter Plot of Corruption Perception Index vs. Fertility Rate", x = "Corruption Perception Index", y = "Fertility Rate")

# log y = Fertility_rate

# Log y = Fertility_rate, x = log(Population)
log_transform_fertility_population <- ggplot(ds_assign, aes(x = log(Population), y = Fertility_rate)) +
  geom_point() +
  labs(title = "Scatter Plot of Log-Transformed Population vs. Fertility Rate", 
       x = "Log(Population)", y = "Fertility Rate")

# Log y = Fertility_rate, x = Happiness_index - NO NEED 
log_transform_fertility_happiness <- ggplot(ds_assign, aes(x = Happiness_index, y = Fertility_rate)) +
  geom_point() +
  labs(title = "Scatter Plot of Happiness Index vs. Fertility Rate", 
       x = "Happiness Index", y = "Fertility Rate")

# Log y = Fertility_rate, x = Life Expectancy - NO NEED
log_transform_fertility_life_expectancy <- ggplot(ds_assign, aes(x = Life_expectancy, y = Fertility_rate)) +
  geom_point() +
  labs(title = "Scatter Plot of Life Expectancy vs. Fertility Rate", 
       x = "Life Expectancy", y = "Fertility Rate")

# Log y = Fertility_rate, x = Corruption_perception_index
log_transform_fertility_corruption <- ggplot(ds_assign, aes(x = log(Corruption_perception_index), y = log10(Fertility_rate))) +
  geom_point() +
  labs(title = "Scatter Plot of Log-Transformed Corruption Perception Index vs. Log-Transformed Fertility Rate", 
       x = "Log(Corruption Perception Index)", y = "Log(Fertility Rate)")

# View plots# View plotslog10()
log_transform_fertility_population
log_transform_fertility_happiness #no need
log_transform_fertility_life_expectancy #no need
log_transform_fertility_corruption

# y = Life_expectancy

ggplot(ds_assign, aes(x = CO2_emissions, y = Life_expectancy)) +
  geom_point() +
  labs(title = "Scatter Plot of CO2 Emissions vs. Life Expectancy", x = "CO2 Emissions", y = "Life Expectancy")

ggplot(ds_assign, aes(x = Population, y = Life_expectancy)) +
  geom_point() +
  labs(title = "Scatter Plot of Population vs. Life Expectancy", x = "Population", y = "Life Expectancy")

ggplot(ds_assign, aes(x = Happiness_index, y = Life_expectancy)) +
  geom_point() +
  labs(title = "Scatter Plot of Happiness Index vs. Life Expectancy", x = "Happiness Index", y = "Life Expectancy")

ggplot(ds_assign, aes(x = Fertility_rate, y = Life_expectancy)) +
  geom_point() +
  labs(title = "Scatter Plot of Fertility Rate vs. Life Expectancy", x = "Fertility Rate", y = "Life Expectancy")

ggplot(ds_assign, aes(x = Corruption_perception_index, y = Life_expectancy)) +
  geom_point() +
  labs(title = "Scatter Plot of Corruption Perception Index vs. Life Expectancy", x = "Corruption Perception Index", y = "Life Expectancy")

# log y = Life_expectancy 

# x = log(CO2) , y = log(Life_exp)
ggplot(ds_assign, aes(x = log(CO2_emissions), y = log(Life_expectancy))) +
  geom_point() +
  labs(title = "Scatter Plot of CO2 Emissions vs. Log-Transformed Life Expectancy", x = "CO2 Emissions", y = "Log(Life Expectancy)")

# Log y = Life_expectancy, x = log(Population)
log_transform_life_population <- ggplot(ds_assign, aes(x = log(Population), y = log(Life_expectancy))) +
  geom_point() +
  labs(title = "Scatter Plot of Log-Transformed Population vs. Life Expectancy", 
       x = "Log(Population)", y = "Life Expectancy")
log_transform_life_population

# y = Corruption_perception_index

ggplot(ds_assign, aes(x = CO2_emissions, y = Corruption_perception_index)) +
  geom_point() +
  labs(title = "Scatter Plot of CO2 Emissions vs. Corruption Perception Index", x = "CO2 Emissions", y = "Corruption Perception Index")

ggplot(ds_assign, aes(x = Population, y = Corruption_perception_index)) +
  geom_point() +
  labs(title = "Scatter Plot of Population vs. Corruption Perception Index", x = "Population", y = "Corruption Perception Index")

ggplot(ds_assign, aes(x = Happiness_index, y = Corruption_perception_index)) +
  geom_point() +
  labs(title = "Scatter Plot of Happiness Index vs. Corruption Perception Index", x = "Happiness Index", y = "Corruption Perception Index")

ggplot(ds_assign, aes(x = Fertility_rate, y = Corruption_perception_index)) +
  geom_point() +
  labs(title = "Scatter Plot of Fertility Rate vs. Corruption Perception Index", x = "Fertility Rate", y = "Corruption Perception Index")

ggplot(ds_assign, aes(x = Life_expectancy, y = Corruption_perception_index)) +
  geom_point() +
  labs(title = "Scatter Plot of Life Expectancy vs. Corruption Perception Index", x = "Life Expectancy", y = "Corruption Perception Index")

# Log y = Corruption_perception_index 

# Log y = Corruption_perception_index, x = log(Population)
log_transform_corruption_population <- ggplot(ds_assign, aes(x = log(Population), y = Corruption_perception_index)) +
  geom_point() +
  labs(title = "Scatter Plot of Log-Transformed Population vs. Corruption Perception Index", 
       x = "Log(Population)", y = "Corruption Perception Index")

# Log y = Corruption_perception_index, x = Happiness_index - NO NEED
log_transform_corruption_happiness <- ggplot(ds_assign, aes(x = Happiness_index, y = Corruption_perception_index)) +
  geom_point() +
  labs(title = "Scatter Plot of Happiness Index vs. Corruption Perception Index", 
       x = "Happiness Index", y = "Corruption Perception Index")

# Log y = Corruption_perception_index, x = Fertility_rate
log_transform_corruption_fertility <- ggplot(ds_assign, aes(x = Fertility_rate, y = Corruption_perception_index)) +
  geom_point() +
  labs(title = "Scatter Plot of Fertility Rate vs. Corruption Perception Index", 
       x = "Fertility Rate", y = "Corruption Perception Index")

# Log y = Corruption_perception_index, x = Life_expectancy - NO NEED
log_transform_corruption_life <- ggplot(ds_assign, aes(x = Life_expectancy, y = Corruption_perception_index)) +
  geom_point() +
  labs(title = "Scatter Plot of Life Expectancy vs. Corruption Perception Index", 
       x = "Life Expectancy", y = "Corruption Perception Index")

# View plots
log_transform_corruption_population
log_transform_corruption_happiness
log_transform_corruption_fertility
log_transform_corruption_life

#############
#Multivariate
#############

install.packages("devtools", dependencies = TRUE)
install.packages(PerformanceAnalytics, dependencies = TRUE)
install_github("caijun/ggcorrplot2")

pdf("eemgraphs3.pdf")
par(mfrow = c(4,6))
eemgraphs3[1:24]
dev.off()

library(dplyr)

# Create a new dataset without specific columns - without LOGs
new_dataset <- select(ds_assign, -one_of(c("log_CO2_emissions", "log_Corruption_perception_index", "log_Population", "log_Fertility_rate")))
head(new_dataset)

#generate bm multivariate graph
library(GGally)
library(ggplot2)
ggpairs(new_dataset[,-1])+ theme_bw() #omit Country column + theme black and white 
ggpairs(data_for_plot[,-1])+ theme_bw() #omit Country column + theme black and white 

g_transformed_columns <- c("CO2_emissions", "Population", "Happiness_index", "Life_expectancy", "Fertility_rate", "Corruption_perception_index")

# Generate scatterplot matrix excluding log-transformed columns
ggpairs(ds_assign[,-c("log_CO2_emissions")]) + theme_bw()

ggpairs(ds_assign)

# Load necessary library
library(dplyr)

# Assuming your dataset 'ds_assign' has no negative or zero values in the columns used for log transformation
# Prepare the dataset for the log transformation
ds_assign_tranformed <- ds_assign %>%
  mutate(log_CO2_emissions = log(CO2_emissions),
         log_Population = log(Population)) %>%
  filter(log_CO2_emissions > 0, log_Population > 0)

# Fit the multiple regression model
model <- lm(log_CO2_emissions ~ log_Population + Happiness_index + Life_expectancy + 
              Fertility_rate + Corruption_perception_index, data = ds_assign_tranformed)

# Output the summary of the model
model_summary <- summary(model)

# Print the summary
print(model_summary)

# Optionally, print R-squared values
cat("\nR-squared: ", model_summary$r.squared, "\n")
cat("Adjusted R-squared: ", model_summary$adj.r.squared, "\n")

# Load necessary library
library(dplyr)

# Assuming your dataset 'ds_assign' has no negative or zero values in the columns used for log transformation
# Prepare the dataset for the log transformation
ds_assign <- ds.updated %>%
  mutate(log_CO2_emissions = log(CO2_emissions),
         log_Population = log(Population),
         Income.level = factor(Income.level)) %>% # Convert Income_level to a factor if it's not already
  filter(log_CO2_emissions > 0, log_Population > 0)

# Fit the multiple regression model including the Income_level dummy variable
model <- lm(log_CO2_emissions ~ log_Population + Happiness_index + Life_expectancy + 
              Fertility_rate + Corruption_perception_index + Income.level, data = ds_assign)

# Output the summary of the model
model_summary <- summary(model)

# Print the summary
print(model_summary)

# Optionally, print R-squared values
cat("\nR-squared: ", model_summary$r.squared, "\n")
cat("Adjusted R-squared: ", model_summary$adj.r.squared, "\n")

# From this observation, we see we can remove life_expectancy and corruption_perception index 

#redo this excercise and remove life_expenctancy and corruption perception index

ds_assign_fixed <- ds.updated %>%
  mutate(log_CO2_emissions = log(CO2_emissions),
         log_Population = log(Population),
         Income.level = factor(Income.level)) %>% # Convert Income_level to a factor if it's not already
  filter(log_CO2_emissions > 0, log_Population > 0)

# Fit the multiple regression model including the Income_level dummy variable
model <- lm(log_CO2_emissions ~ log_Population + Happiness_index + 
              Fertility_rate + Income.level, data = ds_assign_fixed)

# Output the summary of the model
model_summary <- summary(model)

# Print the summary
print(model_summary)

# Optionally, print R-squared values
cat("\nR-squared: ", model_summary$r.squared, "\n")
cat("Adjusted R-squared: ", model_summary$adj.r.squared, "\n")


nwdata=ds_assign_fixed ## Just save to the data to another variable 

ggpairs(data = nwdata, columns = c("Log_CO2_emissions", "Log_Population", "Happiness_index", "Life_expectancy","Fertility_rate", "Corruption_perception_index"), lower = list(continuous = "smooth"))

#remove corruption and life_expectance

# Assuming 'dplyr' and 'GGally' libraries are loaded
library(dplyr)
library(GGally)

ds_assign_fixed <- ds.updated %>%
  mutate(log_CO2_emissions = log(CO2_emissions),
         log_Population = log(Population),
         log_Fertility_rate = log(Fertility_rate),
         Income_level = factor(Income.level)) %>%
  filter(log_CO2_emissions > 0, log_Population > 0, log_Fertility_rate > 0)

# Fit the multiple regression model including the Income_level dummy variable
model <- lm(log_CO2_emissions ~ log_Population + Happiness_index + log_Fertility_rate + Income.level, 
            data = ds_assign_fixed)

# Output and print the summary of the model
model_summary <- summary(model)
print(model_summary)
cat("\nR-squared: ", model_summary$r.squared, "\n")
cat("Adjusted R-squared: ", model_summary$adj.r.squared, "\n")

# Correct the column names for ggpairs
ggpairs(data = model_summary, 
        columns = c("log_CO2_emissions", "log_Population", "Happiness_index", "Log_Fertility_rate"), 
        lower = list(continuous = "smooth"))

# Create a new column for residuals in ds_assign and initialize with NA
ds_assign$residuals <- NA

# Assuming model_pow15 was fit using the same ds_assign data frame
# with some rows excluded due to NAs
# We now fill the residuals column only for the rows that were included in the model
fitted_rows <- complete.cases(ds_assign[c("log_Population", "Happiness_index", "pow15_Fertility_rate", "Income.level")])
ds_assign$residuals[fitted_rows] <- residuals(model_pow15)

# Proceed with plotting only rows that have residuals
ds_assign_with_residuals <- ds_assign[fitted_rows, ]

ggplot(ds_assign_with_residuals, aes(x = Income.level, y = residuals)) +
  geom_boxplot() +
  labs(title = "Residuals by Income Level", x = "Income Level", y = "Residuals") +
  theme_minimal()

library(GGally)
ggpairs(ds.updated,columns = 1:4)

require(datasets)
data("swiss")
require(GGally)
require(ggplot2)

my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=loess, fill="red", color="red", ...) +
    geom_smooth(method=lm, fill="blue", color="blue", ...)
  p
}

g = ggpairs(swiss,columns = 1:4, lower = list(continuous = my_fn))
g


# Make sure ds_assign is the same data used for the model
# If you filtered out NAs when fitting the model, do the same before adding residuals
ds_assign <- ds_assign[!is.na(ds_assign$log_CO2_emissions), ]

# Now the number of rows should match the number of residuals
ds_assign$residuals <- residuals(model_pow15)

# Now you can proceed with plotting
ggplot(ds_assign, aes(x = Income.level, y = residuals)) +
  geom_boxplot() +
  labs(title = "Residuals by Income Level", x = "Income Level", y = "Residuals") +
  theme_minimal()

library(dplyr)
library(GGally)
library(ggplot2)

ds_assign_fixed <- ds.updated %>%
  mutate(log_CO2_emissions = log(CO2_emissions),
         log_Population = log(Population),
         log_Fertility_rate = log(Fertility_rate),
         Income_level = factor(Income.level)) %>%
  filter(log_CO2_emissions > 0, log_Population > 0, log_Fertility_rate > 0)

# Fit the multiple regression model including the Income_level dummy variable
model <- lm(log_CO2_emissions ~ log_Population + Happiness_index + log_Fertility_rate + Income.level, 
            data = ds_assign_fixed)

# Output and print the summary of the model
model_summary <- summary(model)
print(model_summary)
cat("\nR-squared: ", model_summary$r.squared, "\n")
cat("Adjusted R-squared: ", model_summary$adj.r.squared, "\n")

# Use ds_assign_fixed instead of model_summary for ggpairs
# Note: ggpairs will use the actual data, not the results of the model
ggpairs(data = ds_assign_fixed, 
        columns = c("log_CO2_emissions", "log_Population", "Happiness_index", "log_Fertility_rate"), 
        lower = list(continuous = "smooth"))

model_summary
# from this, some variables appear to be skewed. So let's apply transformations 

# Now, trying to transform happiness_index 

# Apply a square root transformation to Happiness_index and create a new column
ds_assign_fixed <- ds_assign_fixed %>%
  mutate(sqrt_Happiness_index = sqrt(Happiness_index))

# You may want to update the regression model if needed
model <- lm(log_CO2_emissions ~ log_Population + sqrt_Happiness_index + log_Fertility_rate + Income.level, 
            data = ds_assign_fixed)

# Output and print the summary of the updated model
model_summary <- summary(model)
print(model_summary)
cat("\nR-squared: ", model_summary$r.squared, "\n")
cat("Adjusted R-squared: ", model_summary$adj.r.squared, "\n")

# Generate the pairs plot with the square root transformed Happiness_index
ggpairs(data = ds_assign_fixed, 
        columns = c("log_CO2_emissions", "log_Population", "sqrt_Happiness_index", "log_Fertility_rate"), 
        lower = list(continuous = "smooth"))

model <- lm(log_CO2_emissions ~ log_Population + Happiness_index + log_Fertility_rate + Income.level, 
            data = ds_assign_fixed)

# 1) checking VIF

# Calculate VIF for the model
vif_results <- vif(model)

# Print the VIF results
print(vif_results)

#2) non-normally distributed error terms 
studentized_residuals <- rstandard(model)
qqnorm(studentized_residuals)
qqline(studentized_residuals, col = "red")

#3

# Fit your regression model (if you haven't already)
model <- lm(log_CO2_emissions ~ log_Population + Happiness_index + log_Fertility_rate + Income.level, 
            data = ds_assign_fixed)

# Extract the residuals
residuals <- residuals(model)

# Perform the Shapiro-Wilk test for normality
shapiro_test <- shapiro.test(residuals)

# Output the results of the Shapiro-Wilk test
print(shapiro_test)

# Interpret the p-value
if (shapiro_test$p.value < 0.05) {
  cat("The residuals are not normally distributed (p-value:", shapiro_test$p.value, ")\n")
} else {
  cat("The residuals are normally distributed (p-value:", shapiro_test$p.value, ")\n")
}

# Assuming 'model' is the model object you created using lm()
# Add the studentized residuals to the model frame
model_frame <- model.frame(model)
model_frame$studentized_residuals <- rstandard(model)

# Now you can plot the studentized residuals
ggplot(model_frame, aes(x = fitted(model), y = studentized_residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Studentized Residuals vs. Fitted Values",
       x = "Fitted Values",
       y = "Studentized Residuals") +
  theme_minimal()

#4 
crPlots(model)
#crPlots(model_hyperbolic)
crPlots(model_sqrt)
crPlots(model_pow15)

#multi variabte output for pow15 model

library(GGally)

# Assume ds_assign is your original dataset and you've created model_pow15 from it.
# First, let's create a subset with only the variables used in model_pow15.
# You need to replace the names of the variables with the actual ones used in your dataset.

# Names of the variables used in the model.
variables_in_model <- c("log_Population", "Happiness_index", "pow15_Fertility_rate", "Income.level")

# Subset the dataset with only these variables.
data_for_plot <- ds_assign[, variables_in_model]

# Ensure that 'Income.level' is a factor if it's a dichotomous variable.
data_for_plot$Income.level <- as.factor(data_for_plot$Income.level)

# Now, generate the ggpairs plot.
ggpairs(data_for_plot)

#ggpairs for model 1 
library(GGally)

# Assuming ds_assign is your original dataset and Fertility_rate is a column in this dataset.

# Add a new column for the log transformation of Fertility_rate, if not already present
if (!"Log_Fertility_Rate" %in% names(ds_assign)) {
  ds_assign$Log_Fertility_Rate <- log(ds_assign$Fertility_rate)
}

# Names of the variables used in the previous model.
# Make sure these names match the output from print(names(ds_assign)) exactly.
variables_in_model1 <- c("Log_CO2_emissions", "log_Population", "Happiness_index", "Life_expectancy", "Log_Fertility_Rate", "Corruption_perception_index", "Income.level")

# Ensure that 'Income.level' is a factor if it's a dichotomous variable.
ds_assign$Income.level <- as.factor(ds_assign$Income.level)

# Subset the dataset with only these variables.
data_model1 <- ds_assign[, variables_in_model1]

# Now, generate the ggpairs plot.
ggpairs(variables_in_model1)

ggpairs(data_model1)

# Assuming ds_assign is your original dataset and Fertility_rate is a column in this dataset.

# Add a new column for the log transformation of Fertility_rate, if not already present
if (!"Log_Fertility_Rate" %in% names(ds_assign)) {
  ds_assign$Log_Fertility_Rate <- log(ds_assign$Fertility_rate)
}

# Names of the variables used in the previous model.
# Make sure these names match the output from print(names(ds_assign)) exactly.
variables_in_model1 <- c("log_CO2_emissions", "log_Population", "Happiness_index", "Life_expectancy", "Log_Fertility_Rate", "Corruption_perception_index", "Income.level")

# Ensure that 'Income.level' is a factor if it's a dichotomous variable.
ds_assign$Income.level <- as.factor(ds_assign$Income.level)

# Verify all variable names are correct and exist in ds_assign
print(names(ds_assign))
print(variables_in_model1 %in% names(ds_assign))

# Subset the dataset with only these variables.
data_model1 <- ds_assign[, variables_in_model1]

# Check if data_model1 has been created
print(head(data_model1))

# Now, generate the ggpairs plot using the subsetted data frame.
library(GGally)
ggpairs(data_model1)


# Custom function for adding trendlines
add_trendline <- function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method = "lm", ...)
}

# Now, generate the ggpairs plot with trendlines.
ggpairs(data_model1,
        lower = list(continuous = add_trendline))


# Add a new column for the log transformation of Fertility_rate
ds_assign$Log_Fertility_Rate <- log(ds_assign$Fertility_rate)

# Names of the variables used in the previous model, matching the screenshot
variables_in_model1 <- c("Log_CO2_emissions", "log_Population", "Happiness_index", "Life_expectancy", "Log_Fertility_Rate", "Corruption_perception_index", "Income.level")

# Ensure that 'Income.level' is a factor if it's a dichotomous variable.
ds_assign$Income.level <- as.factor(ds_assign$Income.level)

# Subset the dataset with only these variables.
data_for_previous_model_plot <- ds_assign[, variables_in_model1]

# Now, generate the ggpairs plot.
ggpairs(data_for_previous_model_plot)


# Now, generate the ggpairs plot using the subsetted data frame.
ggpairs(data_model1)


# Load the necessary libraries
library(corrplot)

# Assuming ds_assign is your data frame and model_pow15 is your final model object
# Calculate the correlation matrix for the variables in the model
variables <- c("log_Population", "Happiness_index", "pow15_Fertility_rate", "Income.level")
cor_matrix <- cor(ds_assign[, variables], use = "complete.obs")

# Plot the correlation matrix
corrplot(cor_matrix, method = "circle", type = "upper", 
         title = "Correlation Matrix of Model Variables", 
         tl.col = "black", tl.srt = 45)

# Calculate the correlation matrix for the numeric variables in the model
numeric_variables <- c("log_Population", "Happiness_index", "pow15_Fertility_rate")
cor_matrix <- cor(ds_assign[, numeric_variables], use = "complete.obs")

# Plot the correlation matrix
corrplot(cor_matrix, method = "circle", type = "upper", 
         title = "Correlation Matrix of Numeric Model Variables", 
         tl.col = "black", tl.srt = 45)

library(ggplot2)
library(car)

# Assuming 'model' is your lm model object
# For example, creating a component plus residual plot for 'log_Population'
# Extracting the values needed for the plot
df <- data.frame(
  log_Population = model$model$log_Population,
  std_Residuals = rstandard(model),
  fitted_Values = fitted(model)
)

# Plotting with ggplot2
ggplot(df, aes(x = log_Population, y = std_Residuals)) +
  geom_point() +
  geom_smooth(method = "loess", color = "blue") +
  labs(x = "Log of Population", y = "Standardized Residuals") +
  theme_minimal()

# Calculate VIF for the model
vif_results <- vif(model_pow15)

# Print the VIF results
print(vif_results)

summary(model)

model <- lm(log_CO2_emissions ~ log_Population + Happiness_index + log_Fertility_rate + Income.level, 
            data = ds_assign_fixed)

# Apply the power 3/2 transformation to fertility_rate
ds_assign$pow15_Fertility_rate <- (ds_assign$Fertility_rate)^(3/2)

# Now fit a linear model using the power 3/2 transformed fertility_rate
model_pow15 <- lm(log_CO2_emissions ~ log_Population + Happiness_index + pow15_Fertility_rate + Income.level, 
                  data = ds_assign)

summary(model_pow15)

formula(model_pow15)


# Fit a model with interaction terms
model_interaction_1 <- lm(log_CO2_emissions ~ log_Population + Happiness_index + pow15_Fertility_rate + Income.level +
                            log_Population * Income.level + Happiness_index * Income.level +
                            pow15_Fertility_rate * Income.level,
                          data = ds_assign)

# Check the summary of the model to see the effect of the interaction terms
summary(model_interaction_1)

model_interaction_2 <- lm(log_CO2_emissions ~ log_Population + Income.level +
                            Happiness_index + pow15_Fertility_rate + log_Population * Income.level +
                          Happiness_index * Income.level + pow15_Fertility_rate * Income.level,
                          data = ds_assign)


model_full <- lm(log_CO2_emissions ~ log_Population + Income.level +
                            Happiness_index + pow15_Fertility_rate,
                          data = ds_assign)


summary(model_full)

summary(model_interaction_2)

model_interaction_3 <- lm(log_CO2_emissions ~ log_Population + Income.level +
                            Happiness_index + pow15_Fertility_rate + log_Population * Income.level +
                            Happiness_index * Income.level,
                          data = ds_assign)

summary(model_interaction_3)


model_interaction_4 <- lm(log_CO2_emissions ~ log_Population + Income.level +
                            Happiness_index + pow15_Fertility_rate + log_Population * Income.level,
                          data = ds_assign)

summary(model_interaction_4)


model_interaction_5 <- lm(log_CO2_emissions ~ log_Population +
                            Happiness_index + pow15_Fertility_rate + log_Population * Income.level,
                          data = ds_assign)

summary(model_interaction_5)

model_interaction_6<- lm(log_CO2_emissions ~ log_Population +
                            Happiness_index + pow15_Fertility_rate + log_Population,
                          data = ds_assign)

summary(model_interaction_6)

# Full model with all interactions
model_full <- lm(log_CO2_emissions ~ log_Population + Income.level +
                   Happiness_index + pow15_Fertility_rate + log_Population:Income.level +
                   Happiness_index:Income.level + pow15_Fertility_rate:Income.level,
                 data = ds_assign)


model_full_2 <- lm(log_CO2_emissions ~ log_Population + Happiness_index + Life_expectancy +
                     log_Fertility_rate + Corruption_perception_index + Income.level,
                   data = ds_assign)

summary(model_full_2)


vif(model_full_2)


# Stepwise model selection using both forward and backward selection
model_step <- step(model_full, direction="both", trace=0)  # trace=0 to minimize log output

# Once we have the stepwise selected model, we can look at summary for R-squared and Adjusted R-squared
summary(model_step)


# Check the summary of the new model
summary(model_pow15)

model_pow15

head(model_pow15)

# sqrt transformation of fertility_rate
ds_assign$sqrt_Fertility_rate <- sqrt(ds_assign$Fertility_rate)

model_sqrt <- lm(log_CO2_emissions ~ log_Population + Happiness_index + sqrt_Fertility_rate + Income.level, 
                 data = ds_assign)

# Log transformation of Corruption_perception_index
ds_assign$log_Corruption_perception_index <- log(ds_assign$Corruption_perception_index)

# Ensure that there are no zero values in the fertility_rate column
ds_assign_fixed <- ds_assign_fixed %>% filter(Fertility_rate > 0)

# Create the reciprocal of fertility_rate in the dataframe
ds_assign_fixed$reciprocal_Fertility_rate <- 1 / ds_assign_fixed$Fertility_rate

# Fit the model using the reciprocal of fertility_rate
model_hyperbolic <- lm(log_CO2_emissions ~ log_Population + Happiness_index + reciprocal_Fertility_rate + Income.level, 
                       data = ds_assign_fixed)

# Check the summary of the new model
summary(model_hyperbolic)



#Cook's distance 


plot(cooks.distance(model_pow15)) # index plot
abline(h=4/178)   # cutoff
max(cooks.distance(model_pow15))
which.max(cooks.distance(model_pow15))

influenceIndexPlot(model_pow15)

# Calculate Cook's distance for the model
cooks_d <- cooks.distance(model_pow15)  # Assuming model_sqrt is your final model

# Convert it to a data frame for plotting
cooks_d_df <- data.frame(Index = 1:length(cooks_d), CooksD = cooks_d)

# Plot Cook's distance
ggplot(cooks_d_df, aes(x = Index, y = CooksD)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_hline(yintercept = 4/(nrow(ds_assign) - length(model_sqrt$coefficients) - 1), linetype = "dashed", color = "red") +
  labs(title = "Cook's Distance Plot", x = "Observation Index", y = "Cook's Distance") +
  theme_minimal()

# Identifying observations with high Cook's distance
high_cooks_d <- cooks_d_df[cooks_d_df$CooksD > 4/(nrow(ds_assign) - length(model_sqrt$coefficients) - 1), ]
print(high_cooks_d)





# Outliers test 

# Ensure the car package is installed
if (!require(car)) {
  install.packages("car")
}
library(car)

# Assuming model_sqrt is your final model
outlier_test_results <- outlierTest(model_pow15)

# Print the results
print(outlier_test_results)


# Load necessary library
library(ggplot2)

# Assuming 'ds_assign' contains 'CO2_emissions', 'Population', and 'Income.level'
# and assuming the CO2_emissions and Population are not already log-transformed


#checking income.level 

ggplot(ds_assign, aes(x = log(Population), y = log(CO2_emissions), color = Income.level)) +
  geom_point() +  # Add points
  scale_color_manual(values = c("Low-income" = "red", "High-income" = "green")) +
  labs(title = "CO2 Emissions vs. Population by Income Level",
       x = "Log of Population",
       y = "Log of CO2 Emissions",
       color = "Income Level") +  # Corrected here
  theme_minimal()


ggplot(ds_assign, aes(x = log(Population), y = log(CO2_emissions), color = Income.level)) +
  geom_point() +  # Add the data points
  geom_smooth(data = subset(ds_assign, Income.level == "Low-income"), 
              method = "lm", se = FALSE, color = "red") +  # Add line for Low-income
  geom_smooth(data = subset(ds_assign, Income.level == "High-income"), 
              method = "lm", se = FALSE, color = "green") +  # Add line for High-income
  scale_color_manual(values = c("Low-income" = "red", "High-income" = "green")) +
  labs(title = "CO2 Emissions vs. Population by Income Level",
       x = "Log of Population",
       y = "Log of CO2 Emissions",
       color = "Income Level") +
  theme_minimal()




# Load necessary libraries
library(ggplot2)

# Create a scatter plot with a regression line using the reciprocal of fertility_rate
ggplot(ds_assign_fixed, aes(x = reciprocal_Fertility_rate, y = log_CO2_emissions)) +
  geom_point() +  # Add points to represent the data
  geom_smooth(method = "lm", formula = y ~ x, color = "blue") +  # Add a linear regression line
  labs(title = "Hyperbolic Relationship between Fertility Rate and CO2 Emissions",
       x = "Reciprocal of Fertility Rate",
       y = "Log of CO2 Emissions") +
  theme_minimal()





# Create the histogram with ggplot
ggplot(ds_assign, aes(x = log_Corruption_perception_index)) +
  geom_histogram(fill = "skyblue", color = "black") +  # Adjust fill and color as needed
  labs(title = "Histogram of Log Transformed Corruption Perception Index", x = "Log Corruption Perception Index", y = "Frequency") +  # Add title and axis labels
  stat_bin(geom = "text", aes(label = ..count.., y = ..count..), vjust = -0.5) +  # Add counts over each bin
  scale_x_continuous(labels = scales::comma)  # Format the numbers with commas




# Load necessary library
library(ggplot2)

# Make sure your ds_assign_fixed data frame has the reciprocal_Fertility_rate column
# If not, you would need to create it with the following line:
# ds_assign_fixed$reciprocal_Fertility_rate <- 1 / ds_assign_fixed$fertility_rate

# Create a scatter plot with a regression line
ggplot(ds_assign_fixed, aes(x = reciprocal_Fertility_rate, y = log_CO2_emissions)) +
  geom_point() +  # Add the actual data points
  geom_smooth(method = "lm", color = "blue") +  # Add a linear regression line
  labs(title = "Relationship between Reciprocal of Fertility Rate and Log CO2 Emissions",
       x = "Reciprocal of Fertility Rate",
       y = "Log of CO2 Emissions") +
  theme_minimal()  # Use a minimal theme for the plot


# Plot of log_Population vs. studentized residuals
ggplot(model_frame, aes(x = log_Population, y = studentized_residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Studentized Residuals vs. log_Population",
       x = "log_Population",
       y = "Studentized Residuals") +
  theme_minimal()

# Plot of log_Population vs. studentized residuals with a non-parametric regression line
ggplot(model_frame, aes(x = log_Population, y = studentized_residuals)) +
  geom_point() +
  geom_smooth(method = "loess", color = "blue") +  # Add a loess smoothed line
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Studentized Residuals vs. log Population",
       x = "log Population",
       y = "Studentized Residuals") +
  theme_minimal()



library(stats)

library(dplyr)
library(ggplot2)
library(e1071) # for skewness

# Apply different transformations to Happiness_index
ds_assign_fixed <- ds_assign_fixed %>%
  mutate(log_Happiness_index = log(Happiness_index),
         inverse_Happiness_index = 1 / Happiness_index,
         cube_root_Happiness_index = Happiness_index^(1/3))

# Calculate skewness for each transformation
skewness_sqrt <- skewness(ds_assign_fixed$sqrt_Happiness_index)
skewness_log <- skewness(ds_assign_fixed$log_Happiness_index)
skewness_inverse <- skewness(ds_assign_fixed$inverse_Happiness_index)
skewness_cube_root <- skewness(ds_assign_fixed$cube_root_Happiness_index)

# Output the skewness
cat("Skewness - Square Root:", skewness_sqrt, "\n")
cat("Skewness - Log:", skewness_log, "\n")
cat("Skewness - Inverse:", skewness_inverse, "\n")
cat("Skewness - Cube Root:", skewness_cube_root, "\n")

# Plot the distributions
ggplot(ds_assign_fixed) +
  geom_density(aes(x = sqrt_Happiness_index), fill = "blue", alpha = 0.5) +
  geom_density(aes(x = log_Happiness_index), fill = "red", alpha = 0.5) +
  geom_density(aes(x = inverse_Happiness_index), fill = "green", alpha = 0.5) +
  geom_density(aes(x = cube_root_Happiness_index), fill = "purple", alpha = 0.5) +
  labs(title = "Comparing Distributions of Happiness Index Transformations",
       x = "Transformed Value",
       y = "Density")

