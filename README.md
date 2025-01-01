---
title: "Missing Data Imputation"
author: 'Ajinkya Kothavale'
date: "12/1/2024"
output: html_document
---

## Introduction:
The issue of missing data is a common challenge in data analysis, and handling such missingness is crucial for the validity and reliability of statistical models. In this project, we focus on imputing missing values in a dataset collected from a study of diabetes among African American subjects. The dataset contains various variables, including demographic and health-related features, with some values missing. This report investigates several imputation methods, including complete case analysis, mean imputation, and multiple imputation via chained equations (MICE), to address missing data. The goal is to compare the performance of these imputation techniques by fitting linear models to the imputed datasets and evaluating the resulting standard errors and coefficient estimates. The analysis provides insight into the accuracy and reliability of imputation methods in handling missing values in a real-world dataset.

## Objectives:
### The primary objectives of this analysis are:

1. To explore the extent and nature of missing data in the diabetes dataset using exploratory data analysis (EDA) techniques such as visualizations and summary statistics.

2. To apply different imputation methods (complete case analysis, mean imputation, and multiple imputation by chained equations) to handle missing data.

3. To compare the results of these imputation methods by fitting linear regression models and examining the estimates and standard errors of the coefficients.

4. To assess the performance of multiple imputation using MICE through diagnostic plots and comparisons of the imputation methods based on pooled results.

5. To quantify the variability between imputations and evaluate the fraction of information lost due to missing data, providing insights into the effectiveness of imputation strategies.

## Methods Detail:

This project follows a structured approach to handle missing data, beginning with exploratory data analysis (EDA) and progressing through various imputation techniques. The methods used are described in detail as follows:

Data Preprocessing:

The dataset is first read into R and cleaned by removing the stabilized glucose variable, as it directly indicates the onset of diabetes and is excluded from imputation models.
The extent and pattern of missingness are analyzed using functions like aggr() and matrixplot() from the VIM package to visualize missing data patterns.

Correlation Analysis:

A correlation plot is generated using the corrplot() function to examine relationships between variables in the dataset, helping inform the imputation process.

Imputation Methods:

1. Complete Case Analysis: This method involves analyzing only the cases with no missing data. The analysis is performed by removing rows with missing values (na.omit()), and linear regression models are fitted to the complete data.

2. Mean Imputation: Missing values are replaced by the mean of the observed values for each variable. The imputation is performed using the mice() function with the method set to "mean." Linear regression models are then fitted to the imputed dataset.

3. Multiple Imputation by Chained Equations (MICE): The mice() function is also used for multiple imputation, where 20 imputed datasets are generated. Linear regression models are fitted to each imputed dataset, and the results are pooled using the pool() function. This method accounts for uncertainty in the missing data and provides more reliable estimates than single imputation methods.

## Model Comparison:

Linear models are fitted on the imputed datasets, and the resulting estimates and standard errors are compared across the three imputation methods (complete case analysis, mean imputation, and multiple imputation).

Diagnostic plots (density plots, strip plots, etc.) are used to assess the distributions of the imputed values and the overall convergence of the MICE algorithm.

Model comparison is performed using the pool.compare() function to assess the differences between models fitted on the various imputed datasets.

## Assessment of Variability:

The variability within and between imputations is calculated by examining the coefficients and variances of the imputed datasets. The total variability is obtained by combining the within-imputation and between-imputation variances. The fraction of information lost due to missing data is also computed.
Visualization and Results:

Bar plots, density plots, and box plots are used to visually compare the results of the three imputation methods in terms of coefficient estimates and standard errors.

The analysis concludes with a comparison of the estimates and standard errors from the different imputation methods and a final interpretation of the results.

## Step 1: Setup Libraries

```{r, message=FALSE}
# Install necessary packages (uncomment if not installed)
# install.packages(c("VIM", "corrplot", "mice", "xtable", "Matrix"))

# Load libraries
library(VIM)
library(corrplot)
library(mice)
library(xtable)
library(Matrix)

```

## Step 2: Loading the Data

```{r}

# Set seed for reproducibility
set.seed(1234)

# Import the dataset
DM = read.csv("C:/Users/Ajinkyaa/OneDrive/Stata to R/New folder/Missing data analysis/Missing-Data-Imputation/diabetes_C.csv")

```

## Step 3: Preprocessing and Initial Exploration

```{r}
# Remove 'stab.glu' column as it indicates diabetes onset
DM = DM[,-c(2)]

# Display dataset structure
names(DM)
head(DM)

```

## Step 4: Missing Data Visualization

```{r}
# Visualize missing data
aggr(DM, numbers = TRUE, main = "Missing Data Overview")
matrixplot(DM) # Identify patterns in missingness

```

## Step 5: Correlation Analysis

```{r}
# Extract numeric data (excluding columns 6 and 9)
numeric_data <- DM[, sapply(DM, is.numeric)]

# Compute the correlation matrix
cor_matrix <- cor(numeric_data[, -c(6, 9)], use = "pairwise.complete.obs")

# Plot Correlation Heatmap
corrplot(cor_matrix, 
         method = "color", 
         type = "upper", 
         addCoef.col = "black", 
         tl.cex = 0.8, 
         number.cex = 0.7, 
         main = "Correlation Matrix of Diabetes Dataset")

```

## Step 6:  Complete Case Analysis

```{r}

# Retain only rows with complete data
DM1 = na.omit(DM)
y = DM1[,4]  # Response variable
X = DM1[,-4] # Predictor variables

# Fit a linear model
Model1 = lm(y ~ ., data = X)
Model2 = step(Model1) # Stepwise model selection

# Summarize results
summary(Model1)
summary(Model2)
complete_case = summary(Model2)

```

## Step 7: Imputation Methods

### A. Mean Imputation

```{r, echo = T, results = 'hide'}

#simple model, imputation with mean
M.imp = mice(DM,method = "mean",m=20)
names(M.imp)
M.imp$imp

```

```{r}

y=DM[,4]
X=DM[,-4]
Model3=with(M.imp,lm(y~ratio+age+waist,data=X))

```

```{r}
#Model3=step(Model3)
# Run the pooled model summary
pool_summary <- summary(pool(Model3))

# Extract the coefficient estimates and standard errors
mean_coef <- round(pool_summary[, c("estimate", "std.error")], 2)
mean_imp_results = summary(pool(Model3))

# Access estimates and standard errors
mean_coef[, 1] # estimates
mean_coef[, 2] # standard errors

```

```{r}
# Check structure of the mean_imp_results data frame to identify numeric columns
str(mean_imp_results)

# Extract only the numeric columns for rounding
numeric_columns <- sapply(mean_imp_results, is.numeric)

# Round the numeric columns to 3 decimal places
mean_coef <- mean_imp_results
mean_coef[, numeric_columns] <- round(mean_imp_results[, numeric_columns], 3)

# View the rounded results
mean_coef

```

```{r}
xyplot(M.imp,glyhb~chol+hdl+ratio+age+height
       +weight+frame+bp.1s+bp.1d+waist+hip,
       main="Single Imputation")
```

### B. Multiple Imputation Using MICE

```{r}
# Multiple imputation using MICE
C.imp = mice(DM, m = 20, method = "pmm") # Predictive Mean Matching

# Fit models on imputed datasets
model = with(data = C.imp, exp = lm(glyhb ~ chol + hdl + ratio + age + height + weight + frame + bp.1s + bp.1d + waist + hip))
multiple_imp_results = summary(pool(model))

# Model refinement (sequential reduction)
model8 = with(data = C.imp, exp = lm(glyhb ~ ratio + age + waist))
model8_results = summary(pool(model8))

```

```{r}
# Check the structure of the multiple_imp_results data frame to identify numeric columns
str(multiple_imp_results)

# Identify numeric columns (i.e., exclude non-numeric columns like 'term')
numeric_columns <- sapply(multiple_imp_results, is.numeric)

# Round only the numeric columns to 3 decimal places
multiple_coef <- multiple_imp_results
multiple_coef[, numeric_columns] <- round(multiple_imp_results[, numeric_columns], 3)

# View the rounded results
multiple_coef

```

```{r}
# xyplot for Multiple Imputation Using MICE
xyplot(C.imp, glyhb ~ chol + hdl + ratio + age + height + weight + frame + bp.1s + bp.1d + waist + hip,
       main = "Multiple Imputation Using MICE: Glyhb vs Predictors",
       pch = 20, 
       cex = 0.7, 
       col = c("darkblue", "red"), 
       xlab = "Predictors",
       ylab = "Glycosylated Hemoglobin (glyhb)",
       scales = list(x = list(rot = 45)))  # Rotate x-axis labels for better visibility

```

### C. Regression Imputation

```{r}
# Fit a regression model for prediction
reg_model = lm(glyhb ~ ratio + age + waist + bp.1s + bp.1d, data = DM, na.action = na.omit)

# Predict missing values and update the dataset
predicted_values = predict(reg_model, newdata = DM[is.na(DM$glyhb), ])
DM$glyhb[is.na(DM$glyhb)] = predicted_values

```

```{r}
DM$source <- ifelse(is.na(DM$glyhb), "Imputed", "Observed")  # Tag observed vs. imputed
DM$source <- factor(DM$source, levels = c("Observed", "Imputed"))

# Plot for Regression Imputation
library(lattice)
xyplot(glyhb ~ chol + hdl + ratio + age + height + weight + frame + bp.1s + bp.1d + waist + hip | source, 
       data = DM,
       groups = source,
       auto.key = list(space = "right", points = TRUE, lines = FALSE),
       pch = 20, 
       cex = 0.7,
       col = c("darkgreen", "orange"),
       main = "Regression Imputation: Glyhb vs Predictors",
       xlab = "Predictors",
       ylab = "Glycosylated Hemoglobin (glyhb)",
       layout = c(2, 1))  # Layout for Observed and Imputed

```

### D. k-Nearest Neighbor (kNN) Imputation

```{r}
# Perform kNN imputation
library(VIM)
DM_knn = kNN(DM, k = 5, variable = "glyhb")

# Check imputed values
head(DM_knn)

```

```{r}
DM$source <- ifelse(is.na(DM$glyhb), "Imputed", "Observed")  # Tag observed vs. imputed
DM$source <- factor(DM$source, levels = c("Observed", "Imputed"))

# Plot for kNN Imputation
xyplot(glyhb ~ chol + hdl + ratio + age + height + weight + frame + bp.1s + bp.1d + waist + hip | source, 
       data = DM,
       groups = source,
       auto.key = list(space = "right", points = TRUE, lines = FALSE),
       pch = 20, 
       cex = 0.7,
       col = c("blue", "red"),
       main = "kNN Imputation: Glyhb vs Predictors",
       xlab = "Predictors",
       ylab = "Glycosylated Hemoglobin (glyhb)",
       layout = c(2, 1))  # Layout for Observed and Imputed

```

## Step 7: Model Comaprison

```{r}
# Extract coefficients and standard errors
complete_case_coef = round(complete_case$coefficients, 3)
mean_coef[, numeric_columns] <- round(mean_imp_results[, numeric_columns], 3)
multiple_coef[, numeric_columns] <- round(multiple_imp_results[, numeric_columns], 3)

# Visualize comparisons
par(mfrow = c(1, 3))

# Coefficients comparison
barplot(complete_case_coef[,1], col = "blue", names.arg = rownames(complete_case_coef), main = "Complete Case Estimates")

barplot(mean_coef$estimate, col = "green", names.arg = rownames(mean_coef$term), main = "Mean Imputation Estimates", ylab = "Estimate", las = 2)

barplot(multiple_coef$estimate, col = "red", names.arg = rownames(multiple_coef$term), main = "Multiple Imputation Estimates", ylab = "Estimate", las = 2)

# Standard error comparison
barplot(complete_case_coef[,2], col = "blue", names.arg = rownames(complete_case_coef), main = "Complete Case SE")

barplot(mean_coef[,2], col = "green", names.arg = rownames(mean_coef), main = "Mean Imputation SE")

barplot(multiple_coef[,2], col = "red", names.arg = rownames(multiple_coef), main = "Multiple Imputation SE")

```

## Step 8: Diagnostics

```{r}
# Convergence plot
plot(C.imp, main = "Convergence Plot for MICE Imputation")

# Diagnostic plots

## Density plot
# Calculate density for the complete case analysis
complete_case_density <- density(DM1$glyhb, na.rm = TRUE)

# Calculate density for mean imputation
mean_imputed_data <- complete(M.imp, 1) # Extract the mean-imputed dataset
mean_imp_density <- density(mean_imputed_data$glyhb, na.rm = TRUE)

# Calculate density for multiple imputation using MICE
mice_imputed_data <- complete(C.imp, "long") # Extract the first complete imputed dataset
multiple_imp_density <- density(mice_imputed_data$glyhb, na.rm = TRUE)

# Calculate density for regression imputation
reg_imputed_data <- DM # This is the dataset where regression imputation has replaced missing glyhb
reg_imp_density <- density(reg_imputed_data$glyhb, na.rm = TRUE)

# Calculate density for kNN imputation
knn_imputed_data <- DM_knn # Dataset with kNN imputation applied
knn_imp_density <- density(knn_imputed_data$glyhb, na.rm = TRUE)

# Create the plot
plot(complete_case_density, col = "black", lwd = 2, lty = 1, 
     main = "Density Plot Comparison of Imputation Methods",
     xlab = "Glycosylated Hemoglobin (glyhb)",
     ylab = "Density",
     ylim = c(0, max(c(complete_case_density$y, mean_imp_density$y, 
                       multiple_imp_density$y, reg_imp_density$y, 
                       knn_imp_density$y))))
lines(mean_imp_density, col = "blue", lwd = 2, lty = 2)      # Mean Imputation
lines(multiple_imp_density, col = "red", lwd = 2, lty = 3)   # Multiple Imputation (MICE)
lines(reg_imp_density, col = "green", lwd = 2, lty = 4)      # Regression Imputation
lines(knn_imp_density, col = "purple", lwd = 2, lty = 5)     # kNN Imputation

# Add a legend
legend("topright", legend = c("Complete Case", "Mean Imputation", 
                              "Multiple Imputation (MICE)", "Regression Imputation", 
                              "kNN Imputation"),
       col = c("black", "blue", "red", "green", "purple"),
       lty = c(1, 2, 3, 4, 5),
       lwd = 2, bty = "n", cex = 0.8)

## Strip plot
# Combine data into a single dataset for visualization
glyhb_combined <- data.frame(
  glyhb = c(DM1$glyhb,                       # Complete Case
            mean_imputed_data$glyhb,         # Mean Imputation
            mice_imputed_data$glyhb,         # Multiple Imputation (MICE)
            reg_imputed_data$glyhb,          # Regression Imputation
            knn_imputed_data$glyhb),         # kNN Imputation
  Method = factor(rep(c("Complete Case", "Mean Imputation", 
                        "Multiple Imputation (MICE)", "Regression Imputation", 
                        "kNN Imputation"),
                      times = c(length(DM1$glyhb), 
                                length(mean_imputed_data$glyhb), 
                                length(mice_imputed_data$glyhb), 
                                length(reg_imputed_data$glyhb), 
                                length(knn_imputed_data$glyhb))))
)

# Create the strip plot
library(ggplot2)
ggplot(glyhb_combined, aes(x = Method, y = glyhb, color = Method)) +
  geom_jitter(width = 0.2, alpha = 0.6) +
  theme_minimal() +
  labs(
    title = "Strip Plot Comparison of Imputation Methods",
    x = "Imputation Method",
    y = "Glycosylated Hemoglobin (glyhb)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

```

## Step 9: Information Loss Analysis

```{r}
# Extract coefficients from multiple imputation analyses
# Model8 is the final reduced model from multiple imputations
NI <- 20  # Number of imputations
beta_list <- lapply(model8$analyses, coefficients)  # Extract coefficients from each imputed model

# Create a matrix for coefficients
beta_matrix <- do.call(rbind, beta_list)  # Combine into a matrix where each row is one imputation's coefficients

# Calculate between-imputation variability (B)
B <- cov(beta_matrix)

# Extract within-imputation variability (W)
Cov_list <- lapply(model8$analyses, vcov)  # Extract covariance matrices for each imputation
W <- Reduce("+", Cov_list) / NI  # Average covariance matrix (within-imputation variability)

# Calculate total variability (T)
T <- W + B  # Total variability

# Calculate fraction of information lost
info_loss <- diag(B) / diag(T)  # Fraction of information lost for each variable
round(info_loss, 3)  # Display rounded results

# Barplot for Information Loss
barplot(info_loss, col = rainbow(length(info_loss)), 
        main = "Fraction of Information Lost Across Variables",
        xlab = "Variables",
        ylab = "Fraction of Information Lost",
        ylim = c(0, max(info_loss) * 1.2),
        names.arg = names(info_loss),
        las = 2, cex.names = 0.8)

```





