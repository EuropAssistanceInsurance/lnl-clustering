# Names of columns - just for inspection
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

# Inspect the "distribution" of the categorical variables
View(finDf)

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
