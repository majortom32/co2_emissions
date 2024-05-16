## README

# CO2 Emissions Analysis

## Introduction
This repository contains the code and data for analyzing potential causes of CO2 emissions. The objective was to apply various data analytics techniques to develop a model explaining potential causes of CO2 emissions.

## Dataset
The dataset was sourced from [Our World in Data](https://ourworldindata.org/). It includes variables such as CO2 emissions, population, happiness index, fertility rate, and income level.

## Methodology
The analysis followed a structured approach:

1. **Data Transformation**: Applied logarithmic transformations to normalize the data.
2. **Exploratory Data Analysis (EDA)**: Used bivariate and multivariate visualizations to identify key relationships.
3. **Correlation and Relationship Analysis**: Measured how variables related to CO2 emissions and isolated their individual effects.
4. **Identification of Unusual Observations**: Used Cook's Distance and standardized residuals to pinpoint outliers.
5. **Regression Modeling**: Constructed a series of regression models, refining them based on statistical significance, Adjusted R-squared values, Residual Standard Error, and Akaike Information Criterion (AIC).

### Final Regression Model
The final model included the following variables:
- Log-transformed Population
- Happiness Index
- Log-transformed Fertility Rate
- Income Level

The model achieved an adjusted R-squared of 0.85, indicating that 85% of the variability in CO2 emissions is explained by the model.

### Results
#### Key Findings
- **Population**: Strong positive correlation with CO2 emissions.
- **Happiness Index**: Moderate positive correlation with CO2 emissions.
- **Fertility Rate**: Negative correlation with CO2 emissions.
- **Income Level**: High-income countries generally have higher emissions.

### Visualizations
#### Correlation Plot
Below is the correlation plot showing the relationships between the log-transformed variables:

![Correlation Plot](correlation_plot.png)

#### Q-Q Plot of Residuals
![Q-Q Plot](qq_plot.png)

#### Residuals vs. Log Population Plot
![Residuals vs. Log Population](residuals_vs_log_population.png)

#### Component Plus Residual Plots
- **Log Population**: Strong linear relationship with CO2 emissions.
- **Happiness Index**: Moderate positive linear trend.
- **Transformed Fertility Rate (1.5 power)**: Negative linear relationship with CO2 emissions.

![Component Plus Residual Plot - Population](cp_residual_population.png)
![Component Plus Residual Plot - Happiness Index](cp_residual_happiness.png)
![Component Plus Residual Plot - Fertility Rate](cp_residual_fertility.png)

### Model Performance
The final model's performance was validated using various diagnostic tests:
- **Variance Inflation Factor (VIF)**: Ensured no multicollinearity among predictors.
- **Cook's Distance**: Identified and assessed influential data points.
- **Normality of Residuals**: Q-Q plot indicated that residuals are approximately normally distributed.
- **Homoscedasticity**: Residuals vs. Log Population plot indicated constant variance.

### Getting Started
To get started with this project, clone the repository and ensure you have the necessary dependencies installed.

## Collaboration notice
This project was developed with my colleagues Shurui Chen and Emmanuel Okon. 

```bash
git clone https://github.com/yourusername/CO2_Emissions_Analysis.git

<p xmlns:cc="http://creativecommons.org/ns#" xmlns:dct="http://purl.org/dc/terms/"><span property="dct:title">CO2_emissions</span> by <a rel="cc:attributionURL dct:creator" property="cc:attributionName" href="https://www.linkedin.com/in/tommyayala/">Tommy Ayala</a> is licensed under <a href="https://creativecommons.org/licenses/by/4.0/?ref=chooser-v1" target="_blank" rel="license noopener noreferrer" style="display:inline-block;">Creative Commons Attribution 4.0 International<img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/cc.svg?ref=chooser-v1" alt=""><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/by.svg?ref=chooser-v1" alt=""></a></p>
