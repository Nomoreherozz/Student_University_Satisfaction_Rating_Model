##### Read the data
data = read.csv("Rating_data_2_demo.csv")
data

# data[data == "First"] <- 1
# data[data == "Second"] <- 2
# data[data == "Third"] <- 3
# data[data == "Fourth"] <- 4
# data[data == "Other"] <- 0

##### Group data into smaller class types
data$Sex[data$Sex == "Male"] = 1
data$Sex[data$Sex == "Female"] = 0
data$Sex[data$Sex == "Prefer not to say (Không muốn chia sẻ)"] = -1

data["Program"][data["Program"] == 1 | data["Program"] == 2] = 0
data["Program"][data["Program"] == 3 | data["Program"] == 4] = 1
data["Program"][data["Program"] == 5 | data["Program"] == 6] = 2

data["Facilities"][data["Facilities"] == 1 | data["Facilities"] == 2] = 0
data["Facilities"][data["Facilities"] == 3 | data["Facilities"] == 4] = 1
data["Facilities"][data["Facilities"] == 5 | data["Facilities"] == 6] = 2

data["Club"][data["Club"] == 1 | data["Club"] == 2] = 0
data["Club"][data["Club"] == 3 | data["Club"] == 4] = 1
data["Club"][data["Club"] == 5 | data["Club"] == 6] = 2

data["Environment"][data["Environment"] == 1 | data["Environment"] == 2] = 0
data["Environment"][data["Environment"] == 3 | data["Environment"] == 4] = 1
data["Environment"][data["Environment"] == 5 | data["Environment"] == 6] = 2

data["Opportunities"][data["Opportunities"] == 1 | data["Opportunities"] == 2] = 0
data["Opportunities"][data["Opportunities"] == 3 | data["Opportunities"] == 4] = 1
data["Opportunities"][data["Opportunities"] == 5 | data["Opportunities"] == 6] = 2

data["Faculties"][data["Faculties"] == 1 | data["Faculties"] == 2] = 0
data["Faculties"][data["Faculties"] == 3 | data["Faculties"] == 4] = 1
data["Faculties"][data["Faculties"] == 5 | data["Faculties"] == 6] = 2

data["Overall"][data["Overall"] == 1 | data["Overall"] == 2 | data["Overall"] == 3] = 0
data["Overall"][data["Overall"] == 4 | data["Overall"] == 5 | data["Overall"] == 6] = 1

##### Create dummy data - Only for correlation matrix

data$Program_0 = ifelse(data$Program == 1 | data$Program == 2, 1, 0)
data$Program_1 = ifelse(data$Program == 3 | data$Program == 4, 1, 0)
data$Program_2 = ifelse(data$Program == 5 | data$Program == 6, 1, 0)

data$Facilities_0 = ifelse(data$Facilities == 1 | data$Facilities == 2, 1, 0)
data$Facilities_1 = ifelse(data$Facilities == 3 | data$Facilities == 4, 1, 0)
data$Facilities_2 = ifelse(data$Facilities == 5 | data$Facilities == 6, 1, 0)

data$Club_0 = ifelse(data$Club == 1 | data$Club == 2, 1, 0)
data$Club_1 = ifelse(data$Club == 3 | data$Club == 4, 1, 0)
data$Club_2 = ifelse(data$Club == 5 | data$Club == 6, 1, 0)

data$Environment_0 = ifelse(data$Environment == 1 | data$Environment == 2, 1, 0)
data$Environment_1 = ifelse(data$Environment == 3 | data$Environment == 4, 1, 0)
data$Environment_2 = ifelse(data$Environment == 5 | data$Environment == 6, 1, 0)

data$Faculties_0 = ifelse(data$Faculties == 1 | data$Faculties == 2, 1, 0)
data$Faculties_1 = ifelse(data$Faculties == 3 | data$Faculties == 4, 1, 0)
data$Faculties_2 = ifelse(data$Faculties == 5 | data$Faculties == 6, 1, 0)

data$Opportunities_0 = ifelse(data$Opportunities == 1 | data$Opportunities == 2, 1, 0)
data$Opportunities_1 = ifelse(data$Opportunities == 3 | data$Opportunities == 4, 1, 0)
data$Opportunities_2 = ifelse(data$Opportunities == 5 | data$Opportunities == 6, 1, 0)

data["Overall"][data["Overall"] == 1 | data["Overall"] == 2 | data["Overall"] == 3] = 0
data["Overall"][data["Overall"] == 4 | data["Overall"] == 5 | data["Overall"] == 6] = 1

##### Factorize
### DO NOT FACTORIZE THE VARIABLE IF ONLY DRAWING CORRELATION MATRIX
data$Program = as.factor(data$Program)
data$Facilities = as.factor(data$Facilities)
data$Club = as.factor(data$Club)
data$Environment = as.factor(data$Environment)
data$Opportunities = as.factor(data$Opportunities)
data$Faculties = as.factor(data$Faculties)
data$Overall = as.factor(data$Overall)

##### Summary the data types in data
str(data)

##### Split into training samples and test samples
set.seed(101)
sample = sample(c(TRUE,FALSE),nrow(data),replace = TRUE, prob = c(0.8,0.2))
train = data[sample, ]
test = data[!sample, ]

##### Summary of classes of the samples
str(train)
str(test)

##### Set the option to disable scientific notion
options(scipen=999)

##### Initial fitting model to evaluate the variables
logistic_full = glm(Overall ~ 
                      Program + 
                      Facilities + 
                      Club + 
                      Environment +
                      Opportunities +
                      Faculties
                    , data = train, family = "binomial")
summary(logistic_full)
logistic_full
confint(logistic_full)

##### Facilities and Environment have biggest std errors -> Removed from the model and re-train
logistic_full_no_big_std_error = glm(Overall ~ 
                      Program + 
                      Club + 
                      Opportunities +
                      Faculties
                    , data = train, family = "binomial")
summary(logistic_full_no_big_std_error)
logistic_full_no_big_std_error
confint(logistic_full_no_big_std_error)

##### Program is not significant -> Removed from the model and re-train
logistic_full_no_program = glm(Overall ~ 
                                       Club + 
                                       Opportunities +
                                       Faculties
                                     , data = train, family = "binomial")
summary(logistic_full_no_program)
logistic_full_no_program
confint(logistic_full_no_program)

##### Testing the model with test sample
logitModelPred = predict(logistic_full_no_program,test,type = "response")
logitModelPred

##### Draw the graph
library(ggplot2)

predicted.data_1 <- data.frame(
  probability.of.Overall=logistic_full_no_program$fitted.values,
  Overall=data$Overall
)
predicted.data_1

predicted.data_2 <- predicted.data_1[
  order(predicted.data_1$probability.of.Overall, decreasing=FALSE),]

predicted.data_2$rank <- 1:nrow(predicted.data_2)

ggplot(data=predicted.data_2, aes(x=rank, y=probability.of.Overall)) +
  geom_point(aes(color=Overall), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of satisfaction")
# ggsave("satisfaction_probabilities.pdf")

##### Prepare the data for writing correlation matrix by remove non-numeric values
data_correlation = subset(data, select = -c(Year, age, Sex))
data_correlation

summary(data)

str(data_correlation)

##### Allow to draw two plots next to each other in one frame
par(mfrow = c(1,2))

##### Draw the correlation matrix using Pearson method 
library(corrplot)
corrplot(cor(data_correlation), method = ("pie"), title = "Pearson Correlation")

data_correlation_2 = subset(data_correlation, select = -c(Program_2, 
                                                          Facilities_2, 
                                                          Club_2, 
                                                          Environment_2, 
                                                          Opportunities_2, 
                                                          Faculties_2))
corrplot(cor(data_correlation_2), method = ("pie"), title = "Pearson Correlation - Without rating 2")

data_correlation_1 = subset(data_correlation, select = -c(Program_1, 
                                                          Facilities_1, 
                                                          Club_1, 
                                                          Environment_1, 
                                                          Opportunities_1, 
                                                          Faculties_1))
corrplot(cor(data_correlation_1), method = ("pie"), title = "Pearson Correlation - Without rating 1")

##### Calculate p-value of the models based on Chi-Square and degrees of freedom
X_2 = logistic_full_no_big_std_error$null.deviance - logistic_full_no_big_std_error$deviance
X_2
degreeFreedom = logistic_full_no_big_std_error$df.null - logistic_full_no_big_std_error$df.residual
degreeFreedom

##### Preparation for the labels and predicted 
classify <- ifelse(logitModelPred > 0.5,1,0)
classify <- ordered(classify, levels = c(1, 0))
testOrderedOverall <- ordered(test$Overall, level = c(1, 0))

##### Form the confusion matrix
confusionMatrix <- table(Predicted = classify, Actual = testOrderedOverall)
confusionMatrix

help(ordered)

##### Repeat the testing 100 times
for (x in 1:5) {
  ### Sampling 30 samples as testing dataset from the reponses
  test_Loop = data[sample(nrow(data), 30), ]
  
  ### Predict the Overall variable for 30 samples using final model (no non-significant value)
  prediction = predict(logistic_full_no_program,test_Loop,type = "response")
  
  ### Label class 1 or 0 based on the position of the above prediction table
  ## Predicted over 50% -> Label 1 and vice versa
  classification <- ifelse(prediction > 0.5,1,0)
  
  ### Rearrange the sample orders from table of predicted value into ascending order
  classification <- ordered(classification, levels = c(1, 0))
  
  ### Rearrange the sample orders from testing sample into ascending order
  testOrderedOverall_Loop <- ordered(test_Loop$Overall, level = c(1, 0))
  
  ### Form and print a confusion matrix
  confusionMatrix_Loop <- table(Predicted = classification, Actual = testOrderedOverall_Loop)
  print(confusionMatrix_Loop)
}
