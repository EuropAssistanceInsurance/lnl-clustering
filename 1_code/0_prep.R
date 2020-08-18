# Load train and test
train <- data.table::fread("0_data/Census Income/adult.data.csv") %>%
  eaR::cleanNames(sep = "_")

test <- data.table::fread("0_data/Census Income/adult.test.csv") %>% 
  eaR::cleanNames(sep = "_")

# Fill column names
fillColNames <- c("age", "workclass", "fnlwgt", "education", "education_num", "marital_status", "occupation", "relationship", "race", "sex", "capital_gain", "capital_loss", "hours_per_week", "native_country", "income")

names(train) <- fillColNames
names(test) <- fillColNames

# Join train and test
dat <- train %>%
  dplyr::bind_rows(test)

# Remove df's from memory
rm(test, train)

# Names of columns
names(dat)

# Character columns
cols <- c("workclass", "education", "marital_status", "occupation", "relationship", "race", "sex", "native_country", "income")

# Loop through columns and get the unique contents and counts
finRes <- c()
finDf <- data.frame()
for (vr in cols) {
  tempresult <- dat %>% 
    dplyr::group_by(!!as.name(vr)) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::arrange(-n)
  if(nrow(tempresult) > nrow(finDf)) {
    for (ii in 1:(nrow(tempresult) - nrow(finDf)))  {
      finDf <- finDf %>% dplyr::add_row()
    }
  } else {
    for (ii in 1:(nrow(finDf) - nrow(tempresult)))  {
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


# Clustering Scaling/Standardising of variables/ Mean and Standard deviation 
clusters <- clusterDF %>% 
  group_by(native_country) %>% 
  summarise(meanAge     = age %>% mean(na.rm = TRUE),
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
  mutate(incomePropLT50 = incomeLT50 / incomeAll,
         sexMaleProp    = sexMale / sexAll) %>% 
  select(-c(incomeLT50, incomeAll, sexMale, sexAll)) %>% 
  as.data.frame()

# Drop categorical variable
rownames(clusters) <- clusters$native_country
clusters$native_country <- NULL

# Plot elbow 
set.seed(123)
factoextra::fviz_nbclust(clusters, kmeans, method = "gap_stat") +
  geom_vline(xintercept = 3, linetype = 2)
factoextra::fviz_nbclust(clusters, hcut, method = "gap_stat") +
  geom_vline(xintercept = 3, linetype = 2)










