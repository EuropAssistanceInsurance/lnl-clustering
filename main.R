# Setup and Load Packages -------------------------------------------------
options(scipen = 999)
if (!is.element("yaml",     .packages(all.available = TRUE))) install.packages("yaml")
if (!is.element("devtools", .packages(all.available = TRUE))) install.packages("devtools")

for (func in list.files(path = "1_code/0_functions/", full.names = TRUE)) source(func)

# Load Meta data
metaData <- yaml::read_yaml(file = "metadata.yaml")

devtools::install_github(repo = metaData$connectionDetails$github$repo, auth_token = metaData$connectionDetails$github$token)
eaR::pkgInstaller(libs = c("tidyverse", "lubridate", "caret"), destinationFolder = "C:/Users/a009831/Documents/rlibraries")
