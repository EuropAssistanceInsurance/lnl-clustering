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
dat2 <- dat %>% 
  dplyr::mutate(workclass = dplyr::if_else(workclass == "?", "NA", workclass) %>% eaR::cleanNames(sep = "_"),
                education = education %>% eaR::cleanNames(sep = "_"),
                marital_status = marital_status %>% eaR::cleanNames(sep = "_"),
                occupation = dplyr::if_else(occupation == "?", "NA", occupation) %>% eaR::cleanNames(sep = "_"),
                relationship = relationship %>% eaR::cleanNames(sep = "_"),
                race = race %>% eaR::cleanNames(sep = "_"),
                sex = sex %>% eaR::cleanNames(sep = "_"),
                native_country = native_country %>% eaR::cleanNames(sep = "_"),
                native_country = dplyr::if_else(native_country %in% c("", "holand_netherlands"), "other", native_country))

# Stratified sampling
# Separate data into 20 for cluster, 80 train, 20 test

