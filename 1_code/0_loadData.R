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
