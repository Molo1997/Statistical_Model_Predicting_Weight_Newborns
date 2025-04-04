# Statistical Analysis of Newborn Data

## Project Overview
This project performs a comprehensive statistical analysis of newborn data, examining various factors that influence newborn characteristics such as weight, length, and head circumference. Using both descriptive and inferential statistics, the analysis explores relationships between maternal factors, birth conditions, and newborn measurements to identify significant predictors and patterns.

### Dataset Description
The dataset contains information about newborns including:
* **Maternal age (Anni.madre)**
* **Number of pregnancies (N.gravidanze)**
* **Gestation period in weeks (Gestazione)**
* **Newborn weight in grams (Peso)**
* **Newborn length in mm (Lunghezza)**
* **Head circumference in mm (Cranio)**
* **Smoking status of mother (Fumatrici) - categorical**
* **Type of delivery (Tipo.parto) - categorical**
* **Hospital (Ospedale) - categorical**
* **Gender of newborn (Sesso) - categorical**

## Key Analysis Components

### Descriptive Statistics
1. **Frequency Analysis**:
   * **Frequency distributions for categorical variables**
   * **Summary statistics for numerical variables**
   * **Calculation of the Gini index for inequality measurement**

2. **Correlation Analysis**:
   * **Pearson correlation coefficients**
   * **Spearman correlation coefficients**
   * **Correlation matrix visualization**

3. **Data Visualization**:
   * **Boxplots for categorical comparisons**
   * **Scatter plots for relationship examination**
   * **Histograms for distribution analysis**

### Inferential Statistics
1. **Hypothesis Testing**:
   * **One-sample t-tests comparing means to population values**
   * **Two-sample t-tests comparing groups (e.g., by gender)**
   * **Chi-square tests for independence between categorical variables**

2. **Regression Analysis**:
   * **Univariate regression for individual predictors**
   * **Multiple linear regression models**
   * **Analysis of multicollinearity using VIF**
   * **Model selection using AIC and BIC criteria**
   * **Interaction terms to capture complex relationships**

3. **Residual Analysis**:
   * **Checking for normality of residuals**
   * **Testing for heteroscedasticity**
   * **Identifying influential observations and outliers**
   * **Calculating leverage and Cook's distance**

## Key Findings
* **Significant correlation between gestation period and newborn weight**
* **Gender differences in newborn measurements (weight, length)**
* **The interaction between gestation period and head circumference is a significant predictor of weight**
* **Model diagnostics identified potential outliers that influenced regression results**
* **After outlier removal, the refined model showed improved diagnostics and predictive accuracy**

## Usage
The analysis script can be run in R and requires the following packages:
* **ggplot2** - for data visualization
* **corrplot** - for correlation matrix visualization
* **GGally** - for creating matrix of scatterplots
* **car** - for VIF calculation and outlier testing
* **MASS** - for stepwise regression
* **lmtest** - for residual diagnostics

## Model Application
The final model can be used to predict newborn weight based on:
* **Number of pregnancies**
* **Gestation period**
* **Newborn length**
* **Head circumference**
* **Gender**
* **Interaction between gestation and head circumference**

## Future Improvements
Potential enhancements include:
* **Non-linear regression models to capture complex relationships**
* **Bayesian hierarchical models to account for hospital-level differences**
* **Machine learning approaches for improved prediction**
* **Longitudinal analysis to track newborn development over time**
