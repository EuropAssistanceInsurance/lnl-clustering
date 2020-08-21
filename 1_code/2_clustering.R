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

# Keep a copy of countries to use later, we keep this to later join our clusters back on to it
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

# Build clusters with selected k
clusters <- stats::kmeans(clustPrep, centers = 8)

# Look at the output of kmeans
str(clusters)

# Extract clusters and join back to finClust which holds country data
finClust$cluster <- clusters$cluster

# Join back original variables for analysis
clustAnalysis <- clustPrep
clustAnalysis$clusters <- clusters$cluster
rownames(clustAnalysis) <- NULL
# Summary analysis
clustAnalysis %>% group_by(clusters) %>% summarise_all(.funs = mean)



