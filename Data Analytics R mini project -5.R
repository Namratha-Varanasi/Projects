
library(dplyr)
library(ggplot2)

alda_calculate_sse <- function(data_df, cluster_assignments){
  # Calculate overall SSE
  # Input:
    # data_df: data frame that has been given to you by the TA (x,y attributes)
    # cluster_assignments: cluster assignments that have been generated using any of the clustering algorithms
  # Output:
    # A single value of type double, which is the total SSE of the clusters.
  centroids<-data.frame(x_mean=numeric(),y_mean=numeric(),count=numeric(),stringsAsFactors = FALSE)
  for(i in 1:max(cluster_assignments))
    {
    centroids<-rbind(centroids,c(data_df[i,1],data_df[i,2],0))
  }
  for(i in 1:length(cluster_assignments))
  {
      centroids[cluster_assignments[i],1]<-centroids[cluster_assignments[i],1]+data_df[i,1]
      centroids[cluster_assignments[i],2]<-centroids[cluster_assignments[i],2]+data_df[i,2]
      centroids[cluster_assignments[i],3]<-centroids[cluster_assignments[i],3]+1
  }
  for(i in 1:NROW(centroids))
  {
    centroids[i,1]<-centroids[i,1]/centroids[i,3]
    centroids[i,2]<-centroids[i,2]/centroids[i,3]
  }
  sse<-0
  for(i in 1:NROW(data_df))
  {
    x1=data_df[i,1]
    y1=data_df[i,2]
    x2=centroids[cluster_assignments[i],1]
    y2=centroids[cluster_assignments[i],2]
    distance <- function(x1,y1,x2,y2) (((x1 - x2) ^ 2) + ((y1 - y2) ^ 2))
    sse<-sse+distance(x1,y1,x2,y2)
 }
 return(sse)

  
}



alda_kmeans_elbow_plot <- function(data_df, k_values){
  # ~ 8-10 lines
  # Input:
    # data_df: Original data frame supplied to you by the TA
    # k_values: A vector of values of k for k means clustering
  
  # General Information:
    # Run k means for all the values specified in k_values, generate elbow plot
    # Use alda_cluster with kmeans as your clustering type
    # (you can see an example this function call in hw4_checker.R for k = 2, now repeat it for all k_values)
  
  # Output:
    # Nothing, simply generate a plot and save it to disk as "GroupNumber_elbow.png"
  elbowframe<-data.frame(kvalue=numeric(),ssevalue=numeric(),stringsAsFactors = FALSE)
  for(k in k_values)
  {
    kcluster<-alda_cluster(data_df = clustering_data, n_clusters = k, clustering_type = "kmeans")
    ksse<-alda_calculate_sse(data_df = clustering_data, cluster_assignments = kcluster)
    elbowframe<-rbind(elbowframe,c(k,ksse))
    ggsave("19_elbow.png")
   
  }
  print(elbowframe)
  ggplot(elbowframe, aes(elbowframe[,1],elbowframe[,2]))+geom_line()+geom_point()+ ggtitle("k values vs SSE") +
    xlab("k values") +
    ylab("SSE for each K") 
  
 
  
}


alda_cluster <- function(data_df, n_clusters, clustering_type)
{
  set.seed(100) # this is v. important from reproducibility point of view
  # Perform specified clustering
  
  # Inputs:
  # data_df: The dataset provided to you, 2-dimensional (x1,x2)
  # n_clusters: number of clusters to be created
  # clustering_type: can be one of "kmeans" or "single-link" or "complete-link"
  
  # Outputs:
  # Cluster assignments for the specified method (vector, with length = nrow(data_df) and values ranging from 1 to n_clusters)
  if(clustering_type == "kmeans")
    {
    # ~ 1-2 lines
    # allowed packages for kmeans: R-base, stats, dplyr
    # set the max number of iterations to 100, number of random restarts = 1 (let's not break the TA's computer! )
    # choose "Lloyd" as the algorithm 
    fit.km<-kmeans(data_df,centers=n_clusters,iter.max=100,nstart=1,algorithm="Lloyd")
    cluster<- fit.km$cluster
    return(cluster)
  
    
    
    
    
  }
  else if(clustering_type == "single-link")
    {
    # ~ 3-5 lines
    # Allowed packages for single-link: R-base, stats, dplyr
    # Use euclidean distance for distance calculation (Hint: Look at dist method from stats package)
    # Note 1: Can you use the data_df directly for hclust, or do you need to compute something first?
            # What does 'd' mean in hclust? 
    # Note 2: Does hclust return the clusters assignments directly, or does it return a dendrogram? 
            # Hint 2: Look up the stats package for a method to cut the tree at n_clusters
            # Visualize the dendrogram - paste this dendrogram in your PDF 
    
    d<-dist(data_df,method="euclidean")
    hc<-hclust(d,method="single")
    plot(hc)
    #hc1<-identify(hc, FUN = NULL,N=2, MAXCLUSTER=2,DEV.FUN = NULL)
    hc_aftercut=cutree(hc,k=n_clusters)
    return(hc_aftercut)

    
    
    
  }
  else
    { #complete link clustering is default
    # ~ 3-5 lines
    # Allowed packages for single-link: R-base, stats, dplyr
    # Use euclidean distance for distance calculation (Hint: Look at dist method from stats package)
    # Note 1: Can you use the data_df directly for hclust, or do you need to compute something first?
    # What does 'd' mean in hclust? 
    # Note 2: Does hclust return the clusters assignments directly, or does it return a dendrogram? 
    # Hint 2: Look up the stats package for a method to cut the dendrogram at n_clusters
    # Visualize the dendrogram - paste this dendrogram in your PDF 
      d<-dist(data_df,method="euclidean")
      hc<-hclust(d,method="complete")
      plot(hc)
      hc_aftercut=cutree(hc,k=n_clusters)
      return(hc_aftercut)
    
    
      
  }
}



alda_nn <- function(x_train, x_test, y_train, parameter_grid){
  set.seed(100) # this is v. important from reproducibility point of view
  # ~4-7 lines
  # Perform classification using artificial neural networks using the nnet library
  
  # Inputs:
  # x_train: training data frame(4 variables, x1-x4)
  # x_test: test data frame(4 variables, x1-x4)
  # y_train: dependent variable for training data (can be one of the following classes: 0,1,2)
  # parameter_grid: grid of parameters has already been given to you in hw4_checker
  
  # General information
  # Both training data and test data have already been scaled - so you don't need to scale it once again.
  # 1. Use the nnet library 
  # 2. Perform 10 fold cross validation without replacement using caret and nnet
  # 3. Note that I am giving you x_train and x_test as separate variables - do you need to combine them like you did in the previous hws?
  # 4. Perform training using 10-fold cross validation without replacement:
    # 4.1  Use accuracy as your metric
    # 4.2  Use nnet as your method
    # 4.3  Use parameter_grid as your grid
  # 5. Predict using the trained model on the test dataset (x_test)
  
  # Output:
  # A list with two elements, first element = model generated, 
  # second element = predictions on test data (factor)
  
  # NOTE 1: doing as.vector(as.factor(...)) turns it into numeric datatype.
  # NOTE 2: Best way to ensure that your output is of type factor, with the same levels is factor(your_variable, levels=c(specify all your levels here))
  # NOTE 3: If you want to know the best parameters caret chose for you, you can just do print(your trained model using caret), which will print out the final values used for the model
  # NOTE 4: Setting trace = TRUE could help you get insight into how the procedure is done, but set it to FALSE when submitting to reduce clutter 
  # NOTE 5: Remember, there is a penalty for unnecessary print/View function calls in your code.
  # Methods you need to read about:
  # train() (from caret), predict(), nnet()
  
  # allowed packages: R-base, nnet, caret, dplyr
  df<-cbind(x_train,y_train)
  model<-train(y_train~.,data=df,method="nnet",trControl = trainControl(method = "cv",number=10),tuneGrid = parameter_grid)
  predicted<-predict(model,newdata=x_test)
  t<-as.factor(predicted)
  return(list (model,t))
  
}


