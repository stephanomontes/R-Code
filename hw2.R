#=============================================================================
# PROGRAMMER: Stephano Montes
# PANTHER ID: 6229338
#
# CLASS: COP2210
# SECTION: Your class section: Fundamentals of Modeling & Simulations online
# SEMESTER: The current semester: example Summer 2023
# CLASSTIME: Your CAP4830 course meeting time : Monday 6:00 pm - 7:00pm  zoom

# CERTIFICATION: I understand FIU’s academic policies, and I certify that this 
#                work is my own and that none of it is the work of any other person.

#=============================================================================

library("xlsx")

getwd()



carData <- read.xlsx(file.choose(), 1)
#1 Read the excel file "CAP4830_HW2_Data.xlsx" data into R and store the imported data in a variable named "modelData".
modelData <- readxl::read_excel("CAP4830_HW2_Data.xlsx")

#2 Output the names of the modelData dataframe.
names(modelData)

#3 Create a variable with name "model1" that stores the estimate of the linear model.
model1 <- lm(UNRATE_PCH ~ DFII10_PCH + CPILFESL_PCH + XTEITT01CNM156S_PCH + DCOILWTICO_PCH + PCOPPUSDM_PCH + PCE_PCH + WPU101_PCH + GPDIC1_PCH + RRVRUSQ156N_PCH, data = modelData)
summary(model1)

#4 List all the estimate parameters from step 3 that are statistically significant for all "α ≤ 0.05".
significant_params <- summary(model1)$coefficients[summary(model1)$coefficients[, "Pr(>|t|)"] <= 0.05, ]
significant_params

#5 Plot the model1's residual Density Function.
plot(density(model1$residuals), main = "Residual Density Function")

#6 Check the model1's residual normality using the Shapiro test.
# The Shapiro test checks the normality assumption of the residuals. If the p-value is greater than the significance level (e.g., 0.05), we fail to reject the null hypothesis, 
#indicating that the residuals are normally distributed. You can interpret the result by looking at the p-value of the Shapiro test
shapiro.test(model1$residuals)

#7 Create model2 by removing regressors that are statistically insignificant with α = 0.55.
model2 <- lm(UNRATE_PCH ~ DFII10_PCH + CPILFESL_PCH + PCE_PCH + WPU101_PCH + GPDIC1_PCH, data = modelData)
summary(model2)

#8 Calculate the difference in Adjusted R-squared between model1 and model2.
adjusted_r_diff <- summary(model1)$adj.r.squared - summary(model2)$adj.r.squared
adjusted_r_diff

#9 Calculate prediction accuracy and error rates of model2.
prediction <- predict(model2, testdata= modelData)
actual_predictions <- modelData$UNRATE_PCH

#corr accuracy
correlation_accuracy <- cor(actual_predictions, prediction)^2

#mean absolute error
model2_pac <-mean(abs(actual_predictions - prediction))
model2_pac


#10 Create model3 with only three chosen regressors.
model3 <- lm(UNRATE_PCH ~ DFII10_PCH + CPILFESL_PCH + PCE_PCH, data = modelData)
summary(model3)

#11 Create model4 using a manual sampling technique with a training set of 60% and a testing set of 40%.
set.seed(42)  # Set seed for reproducibility
train_indices <- sample(1:nrow(modelData), 0.6 * nrow(modelData))
trainData <- modelData[train_indices, ]
testData <- modelData[-train_indices, ]
model4 <- lm(UNRATE_PCH ~ DFII10_PCH + CPILFESL_PCH + XTEITT01CNM156S_PCH + DCOILWTICO_PCH + PCOPPUSDM_PCH + PCE_PCH + WPU101_PCH + GPDIC1_PCH + RRVRUSQ156N_PCH, data = trainData)
summary(model4)

#12 Use model4 to predict values on the testing set.
distPred <- predict(model4, newdata = testData)
head(distPred)

#13 Calculate prediction accuracy and error rates, and create a ggplot to show actual vs. predicted values.
accuracy <- 1 - mean((testData$UNRATE_PCH - distPred)^2) / var(testData$UNRATE_PCH)
mse <- mean((testData$UNRATE_PCH - distPred)^2)
rmse <- sqrt(mse)
library(ggplot2)
ggplot(data = testData, aes(x = UNRATE_PCH, y = distPred)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(x = "Actual Values", y = "Predicted Values") +
  ggtitle("Actual vs. Predicted Values")

#14 Run a k-fold cross-validation with k = 10.
library(caret)

set.seed(42)  # Set seed for reproducibility
cv_model <- train(UNRATE_PCH ~ DFII10_PCH + CPILFESL_PCH + XTEITT01CNM156S_PCH + DCOILWTICO_PCH + PCOPPUSDM_PCH + PCE_PCH + WPU101_PCH + GPDIC1_PCH + RRVRUSQ156N_PCH, data = modelData, method = "lm", trControl = trainControl(method = "cv", number = 10))
print(cv_model)

