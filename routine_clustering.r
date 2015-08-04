# Routine that clusters data.
#
# author: tomasz.kusmierczyk(at)gmail.com

if (!exists("clustering.method")) {
  print("Clustering routine requires following parameters to be set:")
  print(" clustering.method (cmeans/kmeans/hierarchical), k, m, train.features, test.features")
}

# Cluster using c-means:
if (clustering.method == "cmeans") {
  if (!exists("clustering.reported")) {
    clustering.reported = T

    print("RUNNING C-MEANS CLUSTERING")
    print(paste("k =", k))
    print(paste("m =", m))
    print("StartCMeansClustering:")
    print(StartCMeansClustering)
  }
  
  clustering.structure = StartCMeansClustering(train.features, k, iter.max = 50000, verbose = F, dist = "euclidean",
                                               method = "cmeans", m = m, rate.par = NULL, weights = 1, control = list())
  clustering.results = ApplyClustering(clustering.structure, k)
  
  clustering.assignment = as.matrix(clustering.results$cluster)
  membership.matrix = as.matrix(clustering.results$membership)
  
  test.membership.matrix = PredictCMeans(clustering.results$centers, test.features, m = m)
  test.clustering.assignment = MembershipMatrixToAssignment(test.membership.matrix)
  
  # sharpening membership matrices (optional)
  #membership.matrix = AssignmentToMembershipMatrix(clustering.assignment)
  #test.membership.matrix = AssignmentToMembershipMatrix(test.clustering.assignment)
}  



# Cluster using k-means:
if (clustering.method == "kmeans") {
  if (!exists("clustering.reported")) {
    clustering.reported = T
    
    print("RUNNING K-MEANS CLUSTERING")
    print(paste("k =", k))
    print("StartKMeansClustering:")
    print(StartKMeansClustering)
  }
  
  clustering.structure = StartKMeansClustering(train.features, k, iter.max = 50000)
  clustering.results = ApplyClustering(clustering.structure, k)
  
  clustering.assignment = as.matrix(clustering.results$cluster)
  membership.matrix = AssignmentToMembershipMatrixK(clustering.assignment, k)
  
  clustering.results$membership =  membership.matrix
  
  test.clustering.assignment = PredictKnn(clustering.assignment, train.features, test.features, k = 1)
  test.membership.matrix = AssignmentToMembershipMatrixK(test.clustering.assignment, k)
}  


# Hierarchical clustering:
if (clustering.method == "hierarchical") {
  if (!exists("clustering.reported")) {
    clustering.reported = T
  
    print("RUNNING HIERARCHICAL CLUSTERING")
    print(paste("k =", k))
    print("StartHierarchicalClustering:")
    print(StartHierarchicalClustering)
  }
  
  clustering.structure = StartHierarchicalClustering(train.features)
  clustering.results = ApplyClustering(clustering.structure, k)
  
  membership.matrix = clustering.results
  clustering.assignment = MembershipMatrixToAssignment(membership.matrix)
  
  clustering.results = list(
    membership =  membership.matrix,
    size = colSums(membership.matrix),
    cluster = clustering.assignment,
    centers = ClusterCenters(clustering.assignment, train.features, k))
  
  test.clustering.assignment = PredictKnn(clustering.assignment, train.features, test.features, k = 1)
  test.membership.matrix = AssignmentToMembershipMatrixK(test.clustering.assignment, k)
}


