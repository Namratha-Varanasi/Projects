
require(ggplot2)
require(reshape2)
require(data.table)
require(philentropy)
require(plyr)
require(utils)

# read data matrix
read_data <- function(path = './hw1_word_frequency.csv') {
  # Note 1: DO NOT change the function arguments
  # Input: path: type: string, output: a matrix containing data from hw1_word_frequency.csv
  # Write code here to read the csv file as a matrix and return it.
  
  #########DOCUMENTATION###########
  
  #The data from the given path is read as a cSV file and then converted to data_matrix f which is returned as output
  
  data_input=read.csv(path,header=FALSE)
  f=data.matrix(data_input)
  return(f)
  
}

calculate_matrix <- function(data_matrix, method_name){
  # NOTE: This function has already been implemented for you.
  # DO NOT modifiy this function.
  # Input: data_matrix: type: matrix n_sentences x sentence_length,
  # where n_sentences is total # sentences (155 in the dataset supplied to you) and
  # sentence_length is the total length of each sentence (200 in the dataset supplied to you).
  # Input: method_name: type: string, can be one of the following values: ('calculate_euclidean', 'calculate_cosine', 'calculate_manhattan', 'calculate_chebyshev')
  # output: a 155 x 155 matrix of type double, containing the distance/similarity calculation using method_name between 
  # every pair of sentences 
  # This function has already been implemented for you. It takes the data matrix and method name, outputs the distance
  # matrix based on the method name.
  # Programming logic for selecting every pair wise rows from the data matrix has already been provided for you
  # for euclidean, cosine and manhattan. You are only required to write the logic to calculate the actual distances for a pair of vectors p and q
  # in the corresponding functions listed above.
  # for chebyshev, you have been tasked to use the library. You will not need to use loops here, since the 
  # library is already optimized. Read the documentation and figure out how to compute the distance matrix
  # without loops for chebyshev
  distance_matrix = matrix(0L, nrow = nrow(data_matrix), ncol = nrow(data_matrix))
  if(method_name %in% c("calculate_euclidean", "calculate_cosine", "calculate_manhattan")){
    # the looping logic for pairwise distances is already provided for you
    for(i in seq(1, nrow(data_matrix))){
      for(j in seq(i, nrow(data_matrix))){
        distance_matrix[i,j] <- do.call(method_name, list(unlist(data_matrix[i,]), unlist(data_matrix[j,])))
        distance_matrix[j,i] <- distance_matrix[i,j]
      }
    }
  }else if(method_name == "calculate_chebyshev"){
    # for chebyshev, you have been tasked to use the library. You will not need to use loops here, since the 
    # library is already optimized. Read the documentation and figure out how to compute the distance matrix
    # without loops for chebyshev
    distance_matrix <- calculate_chebyshev(data_matrix)
  }
  return(distance_matrix)
}

calculate_euclidean <- function(p, q) {
  # Input: p, q are vectors of size 1 x 200, each representing a row (i.e., a sentence) from the original dataset.
  # output: a single value of type double, containing the euclidean distance between the vectors p and q
  # Write code here to calculate the euclidean distance between pair of vectors p and q
  
  ###########DOCUMENTATION############
  
  #The euclidean distance for between 2 vectors is calculated and then outputted as euclidean_distance
  
  e_sum=0
  for ( i in 1:length(p))
  {
    e_sum=e_sum+((p[i]-q[i])^2)
    
  }
  euclidean_distance=sqrt(e_sum)
  return(euclidean_distance)
  
  
}

calculate_cosine <- function(p, q) {
  # Input: p, q are vectors of size 1 x 200, each representing a row (i.e., a sentence) from the original dataset.
  # output: a single value of type double, containing the cosine distance between the vectors p and q
  # Write code here to calculate the cosine distance between pair of vectors p and q
  
  
  ###########DOCUMENTATION############
  
  # The cosine distance between two vectors is calculated using the formula and then outputted as cosine_distance
  c_sum=0
  a=0
  b=0
  for(i in 1:length(p))
  {
    a=a+(p[i]^2)
  }
  a_sq=sqrt(a)
  
  for(j in 1:length(q))
  {
    b=b+(q[j]^2)
  }
  b_sq=sqrt(b)
  
  for(k in 1:length(p))
  {
    c_sum=c_sum+(p[k]*q[k])
  }
  cosine_distance=(c_sum/(a_sq*b_sq))
  return(cosine_distance)
  
}

calculate_manhattan <- function(p, q) {
  # Input: p, q are vectors of size 1 x 200, each representing a row (i.e., a sentence) from the original dataset.
  # output: a single value of type double, containing the manhattan distance between the vectors p and q
  # Write code here to calculate the manhattan distance between pair of vectors p and q
  
  ###########DOCUMENTATION############
  
  # The manhattan distance between two vectors is calculated using the formula and then outputted as manhattan_distance
  
  m_sum=0
  for ( i in 1:length(p))
  {
    m_sum=m_sum+(abs(p[i]-q[i]))
    
  }
  manhattan_distance=m_sum
  return(manhattan_distance)l
  
}

calculate_chebyshev <- function(data_matrix){
  # Input: data_matrix: type: matrix n_sentences x sentence_length, 
  # where n_sentences is total # sentences (155 in the dataset supplied to you) and
  # sentence_length is the total length of each sentence (200 in the dataset supplied to you).
  # output: a 155 x 155 matrix of type double, containing the chebyshev distance between every pair of sentences
  # Write code here to calculate chebyshev distance given an original data matrix of size 155 x 200
  
  ###########DOCUMENTATION############
  
  # The chebyshev distance matrix is calculated directly by using the distance function of philentropy package and then using the method as 'chebyshev'
  
  library('philentropy')
  chebyshev_distance=distance(data_matrix,method='chebyshev')
  return(chebyshev_distance)
  
}

normalize_data <- function(data_matrix){
  # Input: data_matrix: type: matrix n_sentences x sentence_length, 
  # where n_sentences is total # sentences (155 in the dataset supplied to you) and
  # sentence_length is the total length of each sentence (200 in the dataset supplied to you).
  # output: a 155 x 200 matrix of type double, containing the normalized values in [0, 1] range per row.
  # Write code here to normalize data_matrix
  
  ###########DOCUMENTATION############
  
   # In this function we will calculate the minimum and maximum value of each row using the loop 
   # Then apply the formula to get the normalized value for each data item 
   # The output will be the normalized data  matrix which can be then saved to data_matrix_normalized
  
  for(i in 1 :nrow(data_matrix))
  {
    min_row=min(data_matrix[i,])
    max_row=max(data_matrix[i,])
    data_matrix[i,]=((data_matrix[i,]-min_row)/(max_row-min_row))
    
  }
  
  return(data_matrix)
  
  
  
  
}

analyze_normalization <- function(data_matrix, normalized_data_matrix){
  # Input: data_matrix, normalized_data_matrix: type: matrix n_sentences x sentence_length, 
  # where n_sentences is total # sentences (155 in the dataset supplied to you) and
  # sentence_length is the total length of each sentence (200 in the dataset supplied to you).
  # data_matrix refers to the original data_matrix, and normalized_data_matrix refers to the normalized version (i.e., output from normalize_data)
  # Output: a 155 x 155 matrix of type double containing the updated euclidean distance using the normalized_data_matrix
  # Also generate the plot(s) that were requested in the question and save them to the pdf.
  # Write code here to generate the output requested as well as any plots/analyses requested.
  
  ###########DOCUMENTAION####################
  
  # the euclidean distance on the original(non normalized ) data is calculate using the same code as that given in calculate_matrix function and by setting the method as 'calculate_euclidean'
  # these values of the euclidean distances are stored in the distance_matrix 
  
  distance_matrix = matrix(0L, nrow = nrow(data_matrix), ncol = nrow(data_matrix))
  
  # the looping logic for pairwise distances is already provided for you
  for(i in seq(1, nrow(data_matrix))){
    for(j in seq(i, nrow(data_matrix))){
      distance_matrix[i,j] <- do.call('calculate_euclidean', list(unlist(data_matrix[i,]), unlist(data_matrix[j,])))
      distance_matrix[j,i] <- distance_matrix[i,j]
    }
  }
  
  ####Comparison of the properties of the two distance matrices implemented below (mean ,standard deviation ,histogram and ggplot ) is explained in the pdf file
  
  # The range,mean and standard deviations of the euclidean distances on the original (non normalized) data calculated which is used as a measure of comparision 
  cat("non-normalized range=[" ,range(distance_matrix) ,"] \n")
  cat("non-normalised mean = " , mean(distance_matrix) , "\n")
  cat("non-normalised standard deviation =" , sd(distance_matrix) , "\n")
 
  #plot of the distance_matrix having non normnalised euclidean distance is called and saved to the current working directory as "G19_WithoutNorm.pdf"
  plot_distance_matrix(distance_matrix)
          ggsave('G19_WithoutNorm.pdf')
  #Histogram of the distance matrix having original(non normalized) euclidean distance is plotted and saved to the current working directory as "histogram_Non_Normalized"
  #pdf("Histogram_Non_Normalised.pdf ")
  hist(distance_matrix)
  #dev.off()
  
  # the euclidean distance on the  normalized  data is calculate using the same code as that given in calculate_matrix function and by setting the method as 'calculate_euclidean'
  # these values of the euclidean distances are stored in the normalized_distance_matrix 
  
  normalized_distance_matrix = matrix(0L, nrow = nrow(normalized_data_matrix), ncol = nrow(normalized_data_matrix))
  #if(method_name %in% c("calculate_euclidean", "calculate_cosine", "calculate_manhattan")){
    # the looping logic for pairwise distances is already provided for you
    for(i in seq(1, nrow(normalized_data_matrix))){
      for(j in seq(i, nrow(normalized_data_matrix))){
        normalized_distance_matrix[i,j] <- do.call('calculate_euclidean', list(unlist(normalized_data_matrix[i,]), unlist(normalized_data_matrix[j,])))
        normalized_distance_matrix[j,i] <- normalized_distance_matrix[i,j]
      }
    }
  
  
  # The range, mean and standard deviations of the euclidean distances on the normalized data is calculated which is used as a measure of comparision 
  cat("normalized range= [" ,range(normalized_distance_matrix) ," ] \n")
   cat("normalised mean =" , mean(normalized_distance_matrix), "\n")
  cat("normalised standard deviation =" , sd(normalized_distance_matrix) , "\n")
  
  #plot of the normalized_distance_matrix having normalized euclidean distance is called and saved to the current working directory as "G19_WithNorm.pdf"
  
  #pdf("Histogram_Normalised.pdf ")
  hist(normalized_distance_matrix)
  #dev.off()
  plot_distance_matrix(normalized_distance_matrix)
  ggsave('G19_WithNorm.pdf')
  ## function will return the euclidean distance matrix on normalized data as output which can be then saved to euclidean_distance_normalized
  return(normalized_distance_matrix)
  }
  


# This function visualizes a distance matrix, with color indicating distance

#############DOCUMENTATION#####################

#this fuction will generate a ggplot by taking input a distance matrix
#To get the plot of normalized distance matrix run as plot_distance_matrix(euclidean_distance_normalized)
#To get the plot of orignal euclidean distance the following steps are followed
#step 1--  euclidean_distance_non_normalized <- calculate_matix(data_matrix,'calculate_euclidean')
#step 2-- plot_distance_matrix(euclidean_distance_non_normalized)
#We get two plots by running twice and comparision is discussed in the pdf file



plot_distance_matrix <- function(distance_matrix) {
  library(ggplot2)
  library(reshape2)
  ggplot(data = melt(distance_matrix), aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile() +
    scale_x_discrete(name="Row Number") + scale_y_discrete(name="Row Number") +
    scale_fill_continuous(name="Distance")
  #ggsave("plot.pdf")
  
  
}
