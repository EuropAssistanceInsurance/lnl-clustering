# lnl


Kmeans explanation
Step 1: Initialization
The first thing k-means does, is randomly choose K examples (data points) from the dataset (the 4 green points) as initial centroids and that’s simply because it does not know yet where the center of each cluster is. (a centroid is the center of a cluster).
Step 2: Cluster Assignment
Then, all the data points that are the closest (similar) to a centroid will create a cluster. If we’re using the Euclidean distance between data points and every centroid, a straight line is drawn between two centroids, then a perpendicular bisector (boundary line) divides this line into two clusters.
Image for post
from Introduction to Clustering and K-means Algorithm
Step 3: Move the centroid
Now, we have new clusters, that need centers. A centroid’s new value is going to be the mean of all the examples in a cluster.
We’ll keep repeating step 2 and 3 until the centroids stop moving, in other words, K-means algorithm is converged.
https://towardsdatascience.com/clustering-using-k-means-algorithm-81da00f156f6

Hierarchical clustering
https://www.r-bloggers.com/hierarchical-clustering-in-r-2/