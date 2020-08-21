# Join clusters onto Modeling Data
modDF <- modDF %>% 
  left_join(finClust, by = c("native_country" = "native_country")) %>% 
  mutate(income = ifelse(income == ">50K", "high", "low") %>% as.factor())

# Model building ----------------------------------------------------------

# Key variables -----------------------------------------------------------
target <- "income"
featOrig  <- c("age", "workclass", "fnlwgt", "education", "education_num", "marital_status", 
               "occupation", "relationship", "race", "sex", "capital_gain", "capital_loss",
               "hours_per_week", "native_country")
featClust <- c("age", "workclass", "fnlwgt", "education", "education_num", "marital_status", 
               "occupation", "relationship", "race", "sex", "capital_gain", "capital_loss",
               "hours_per_week", "cluster")

# Split data into 80 train and 20 test 
set.seed(123)
splitIndex <- caret::createDataPartition(modDF[, income],
                                         p     = 0.8,
                                         list  = FALSE,
                                         times = 1)
train <- modDF[splitIndex,  ] %>% as.tibble()
trainTarg <- train[[target]]
train[[target]] <- NULL

test  <- modDF[-splitIndex, ] %>% as.tibble()
testTarg <- test[[target]]
test[[target]] <- NULL


# Prep the data for modelling ---------------------------------------------

# Train -------------------------------------------------------------------
trainOrig  <- train[, c(featOrig)]
trainClust <- train[, c(featClust)]

# Dummify variables for train set
trainOrigDummy <- trainOrig %>% dummyVars(formula = "~.", fullRank = F)
trainOrig      <- predict(trainOrigDummy, trainOrig) %>% as.data.frame()
trainOrig[[target]] <- trainTarg

trainClustDummy <- trainClust %>% dummyVars(formula = "~.", fullRank = F)
trainClust      <- predict(trainClustDummy, trainClust) %>% as.data.frame()
trainClust[[target]] <- trainTarg


# Test --------------------------------------------------------------------
testOrig  <- test[, c(featOrig)]
testClust <- test[, c(featClust)]

# Dummify variables for test set
testOrigDummy                 <- testOrig %>% dummyVars(formula = "~.", fullRank = F)
testOrig                      <- predict(testOrigDummy, testOrig) %>% as.data.frame()
testOrig[[target]]            <- testTarg

testClustDummy                  <- testClust %>% dummyVars(formula = "~.", fullRank = F)
testClust                       <- predict(testClustDummy, testClust) %>% as.data.frame()
testClust[[target]]             <- testTarg

# Setting Parameters ------------------------------------------------------
objControl <- trainControl(method = 'cv', 
                           number = 3,
                           summaryFunction = twoClassSummary, 
                           classProbs = TRUE)
set.seed(123)
gbmGrid <- data.frame(interaction.depth = runif(10, 0 ,   5)    %>% round(0),
                      n.trees           = runif(10, 50,   1000) %>% round(0),
                      n.minobsinnode    = runif(10, 1,    100)  %>% round(0),
                      shrinkage         = runif(10, 0.01, 0.2)  %>% round(4)) %>%
  distinct()


# Build model using countries ---------------------------------------------
# Training the train model
formFeats  <- names(trainOrig)[names(trainOrig) != target]
modFormula <- formula(paste0(target, " ~ ", paste0(formFeats, collapse = " + ")))
set.seed(123)
trainOrigObjModel <- caret::train(modFormula,
                                  data         = trainOrig,
                                  distribution = "bernoulli",
                                  method       = "gbm",
                                  metric       = "ROC",
                                  trControl    = objControl,
                                  tuneGrid     = gbmGrid)

# Look at which  variables are important
summary(trainOrigObjModel)

# Which tuning parameters were most important
print(trainOrigObjModel)

# Adding missing dummy variables to test set ------------------------------
# Original countries
# Coefficient names used in the model
coefNames <- data.frame(names = trainOrigObjModel$coefnames) %>% 
  mutate(n = 1)

testOrigNames <- data.frame(names = names(testOrig)) %>% 
  mutate(n = 1)

# Check difference in Model Coefficient names and test dataset
nameDiff <- dplyr::anti_join(coefNames, testOrigNames)

# Add variables that are missing from test
for (nm in nameDiff$names) {
  testOrig[[nm]] <- 0
}


# Build model using clusters ----------------------------------------------

# Training the model with Clusters
formFeats  <- names(trainClust)[names(trainClust) != target]
modFormula <- formula(paste0(target, " ~ ", paste0(formFeats, collapse = " + ")))
set.seed(123)
trainClustObjModel <- caret::train(modFormula,
                                   data         = trainClust,
                                   distribution = "bernoulli",
                                   method       = "gbm",
                                   metric       = "ROC",
                                   trControl    = objControl,
                                   tuneGrid     = gbmGrid)

# Look at which  variables are important
summary(trainClustObjModel)

# Adding missing dummy variables to test set ------------------------------
# Clustered countries
# Coefficient names used in the model
coefNames <- data.frame(names = trainClustObjModel$coefnames) %>% 
  mutate(n = 1)

testClustNames <- data.frame(names = names(testClust)) %>% 
  mutate(n = 1)

# Check difference in Model Coefficient names and test dataset
nameDiff <- dplyr::anti_join(coefNames, testClustNames)

# Add variables that are missing from test
for (nm in nameDiff$names) {
  testClust[[nm]] <- 0
}

  

# Model evaluation --------------------------------------------------------
# Original with Countries
# Get predictions and probabilities on your test data
origPredRaw <- predict(object = trainOrigObjModel, testOrig, type = 'raw')
origPredProb <- predict(object = trainOrigObjModel, testOrig, type = 'prob')

head(origPredRaw)
head(origPredProb)

# Overall Accuracy
print(caret::postResample(pred = origPredRaw, obs = as.factor(testOrig[,"income"])))

aucOrig <- pROC::roc(ifelse(testOrig[,"income"] == "high", 1, 0), origPredProb[[1]])

print(aucOrig$auc)

# Original with Countries
# Get predictions and probabilities on your test data
clustPredRaw <- predict(object = trainClustObjModel, testClust, type = 'raw')
clustPredProb <- predict(object = trainClustObjModel, testClust, type = 'prob')

head(clustPredRaw)
head(clustPredProb)

# Overall Accuracy
print(caret::postResample(pred = clustPredRaw, obs = as.factor(testClust[,"income"])))

aucClust <- pROC::roc(ifelse(testClust[,"income"] == "high", 1, 0), clustPredProb[[1]])

print(aucClust$auc)




# Other
factoextra::fviz_nbclust(clusters, hcut, method = "gap_stat") +
  geom_vline(xintercept = 3, linetype = 2)

