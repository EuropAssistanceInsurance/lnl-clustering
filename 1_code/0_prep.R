# Load train and test
train <- data.table::fread("0_data/Census Income/adult.data.csv") %>%
  eaR::cleanNames(sep = "_")

test <- data.table::fread("0_data/Census Income/adult.test.csv") %>% 
  eaR::cleanNames(sep = "_")

# Fill column names
fillColNames <- c("age", "workclass", "fnlwgt", "education", "education_num", "marital_status", "occupation", "relationship", "race", "sex", "capital_gain", "capital_loss", "hours_per_week", "native_country", "income")

base::names(train) <- fillColNames
base::names(test) <- fillColNames

# Join train and test
dat <- train %>%
  dplyr::bind_rows(test)

# Remove df's from memory
rm(test, train)

# Names of columns
base::names(dat)

# Character columns
cols <- c("workclass", "education", "marital_status", "occupation", "relationship", "race", "sex", "native_country", "income")

# Loop through columns and get the unique contents and counts
finRes <- c()
finDf <- base::data.frame()
for (vr in cols) {
  tempresult <- dat %>% 
    dplyr::group_by(!!as.name(vr)) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::arrange(-n)
  if(nrow(tempresult) > base::nrow(finDf)) {
    for (ii in 1:(base::nrow(tempresult) - base::nrow(finDf)))  {
      finDf <- finDf %>% dplyr::add_row()
    }
  } else {
    for (ii in 1:(base::nrow(finDf) - base::nrow(tempresult)))  {
      tempresult <- tempresult %>% dplyr::add_row()
    }
  }
  
  finDf[[vr]] <- tempresult[[vr]]
  finDf[[paste0(vr, "_n")]] <- tempresult$n
}

# Check out the head
head(finDf)

# Use the above to clean columns
dat <- dat %>% 
  dplyr::mutate(workclass      = dplyr::if_else(workclass == "?", "other", workclass) %>% eaR::cleanNames(sep = "_"),
                education      = education %>% eaR::cleanNames(sep = "_"),
                marital_status = marital_status %>% eaR::cleanNames(sep = "_"),
                occupation     = dplyr::if_else(occupation == "?", "other", occupation) %>% eaR::cleanNames(sep = "_"),
                relationship   = relationship %>% eaR::cleanNames(sep = "_"),
                race           = race %>% eaR::cleanNames(sep = "_"),
                sex            = sex %>% eaR::cleanNames(sep = "_"),
                native_country = native_country %>% eaR::cleanNames(sep = "_"),
                native_country = dplyr::if_else(native_country %in% c("", "holand_netherlands"), "other", native_country),
                income         = income %>% gsub(pattern = "\\.", replacement = ""))

# Remove finDf from memory
rm(finDf)

# Stratified sampling separate data into 20 for cluster, 80 train, 20 test
set.seed(123)
splitIndex <- caret::createDataPartition(dat[, native_country],
                                         p     = 0.2,
                                         list  = FALSE,
                                         times = 1)
clusterDF <- dat[splitIndex,  ]
modDF     <- dat[-splitIndex, ]

# Clustering Scaling/Standardising of variables/ Mean and Standard deviation to create info 
clustPrep <- clusterDF %>% 
  dplyr::group_by(native_country) %>% 
  dplyr::summarise(meanAge     = age %>% mean(na.rm = TRUE),
                   sdAge       = age %>% sd(na.rm = TRUE),
                   meanEdu     = education_num %>% mean(na.rm = TRUE),
                   sdEdu       = education_num %>% sd(na.rm = TRUE),
                   meanWgt     = fnlwgt %>% mean(na.rm = TRUE),
                   sdWgt       = fnlwgt %>% sd(na.rm = TRUE),
                   meanhours   = hours_per_week %>% mean(na.rm = TRUE),
                   sdhours     = hours_per_week %>% sd(na.rm = TRUE),
                   meanCapGain = capital_gain %>% mean(na.rm = TRUE),
                   sdCapGain   = capital_gain %>% sd(na.rm = TRUE),
                   meanCapLoss = capital_loss %>% mean(na.rm = TRUE),
                   sdCapLoss = capital_loss %>% sd(na.rm = TRUE),
                   sexMale     = sum(sex == "male", na.rm = TRUE),
                   sexAll      = n(),
                   incomeLT50  = sum(income == "<=50K", na.rm = TRUE),
                   incomeAll   = n()) %>% 
  dplyr::mutate(incomePropLT50 = incomeLT50 / incomeAll,
                sexMaleProp    = sexMale / sexAll) %>% 
  dplyr::select(-c(incomeLT50, incomeAll, sexMale, sexAll)) %>% 
  base::as.data.frame()

# Keep a copy of countries to prep finClust
finClust <- clustPrep %>%
  dplyr::select(native_country)

# Drop categorical variable
rownames(clustPrep) <- clustPrep$native_country
clustPrep$native_country <- NULL

# Plot kmeans using elbow method and within sum of squared to find optimal clusters
# k-means clustering, is to define clusters such that the total intra-cluster variation 
# [or total within-cluster sum of square (WSS)] is minimized. 
# The total WSS measures the compactness of the clustering and we want it to be as small as possible.
set.seed(123)
factoextra::fviz_nbclust(clustPrep, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)
# Elbow method looks at the total WSS as a function of the number of clusters: 
# One should choose a number of clusters so that adding another cluster doesn’t 
# improve much better the total WSS.

# Build clusters
clusters <- stats::kmeans(clustPrep, centers = 8)

# Look at the output of kmeans
str(clusters)

# Extract clusters and join back to clusterDF
finClust$cluster <- clusters$cluster

# Join back original variables for analysis
clustAnalysis <- clustPrep
clustAnalysis$clusters <- clusters$cluster
rownames(clustAnalysis) <- NULL

# Analysis
clustAnalysis %>% group_by(clusters) %>% summarise_all(.funs = mean)

# Join clusters onto Modeling Data
modDF2 <- modDF %>% 
  left_join(finClust, by = c("native_country" = "native_country")) %>% 
  mutate(income = ifelse(income == ">50K", "high", "low") %>% as.factor())

# Model building ----------------------------------------------------------
# Key variables
target <- "income"
featOrig  <- c("age", "workclass", "fnlwgt", "education", "education_num", "marital_status", 
               "occupation", "relationship", "race", "sex", "capital_gain", "capital_loss",
               "hours_per_week", "native_country")
featClust <- c("age", "workclass", "fnlwgt", "education", "education_num", "marital_status", 
               "occupation", "relationship", "race", "sex", "capital_gain", "capital_loss",
               "hours_per_week", "cluster")

# Split data into train and test 
set.seed(123)
splitIndex <- caret::createDataPartition(modDF2[, income],
                                         p     = 0.8,
                                         list  = FALSE,
                                         times = 1)
train <- modDF2[splitIndex,  ] %>% as.tibble()
trainTarg <- train[[target]]
train[[target]] <- NULL

test  <- modDF2[-splitIndex, ] %>% as.tibble()
testTarg <- test[[target]]
test[[target]] <- NULL

# Modeling train data
trainOrig  <- train[, c(featOrig)]
trainClust <- train[, c(featClust)]

# Dummify variables for train set
trainOrigDummy <- trainOrig %>% dummyVars(formula = "~.", fullRank = F)
trainOrig      <- predict(trainOrigDummy, trainOrig) %>% as.data.frame()
trainOrig[[target]] <- trainTarg
  
trainClustDummy <- trainClust %>% dummyVars(formula = "~.", fullRank = F)
trainClust      <- predict(trainClustDummy, trainClust) %>% as.data.frame()
trainClust[[target]] <- trainTarg


# Build model using countries ---------------------------------------------
# Setting Parameters
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

# Get predictions on your testing data
trainOrig$pred <- predict(object = trainObjModel, trainOrig)


# Build model using clusters ----------------------------------------------

# Modeling test data
# Training the train model
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

# Get predictions on your testing data
trainClust$pred <- predict(object = trainClustObjModel, trainClust)

# Evaluating
predictions <- predict(object = trainClustObjModel, test, type='raw')

head(predictions)

# build model using clusters


















factoextra::fviz_nbclust(clusters, hcut, method = "gap_stat") +
  geom_vline(xintercept = 3, linetype = 2)










