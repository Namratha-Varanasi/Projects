
require(ggplot2)
set.seed(123)
# no install.packages or rm(list=ls(all=T)) in this function


intro_to_r <- function(num_values){
  set.seed(123)
  #runif(num_values)) is used to generate random sample of values of count equal to num_values and of type double 
  new_vector=runif(num_values)
  #To calculate the mean,median,max and min in-built functions are used
  new_mean=mean(new_vector)
  new_median=median(new_vector)
  new_max=max(new_vector)
  new_min=min(new_vector)
  result=list(new_vector,new_mean,new_median,new_max,new_min)
  return(result)
 
  # input: num_values type: integer, specifies the sample size of the random vector you are to generate.
  # output: a  list:  [generated vector (type: double), mean of vector (type: double), median of the vector (type: double), max value of vector (type: double), min value of the vector (type: double)]
  
}

intro_to_plotting <- function(num_values){
  set.seed(123)
  library(ggplot2)
  #runif(num_values) creates a random vector of size num_values and type double
  #Assuming the new vector is of type double
  new_vector=runif(num_values)
  # A data frame df is created with two colums both of which are new vector
  df=data.frame(new_vector,new_vector)
  # A scatter plot is plotted using this dataframe where geom_point() is used for scatter plot and labs is used to add title to the plot and labels to X and Y axis
  ggplot(df,aes(x=new_vector,y=new_vector))+geom_point()+labs(title="G19_plot01",x="new_vector",y="new_vector")
  #The scatter plot is programmatically saved to the current working directory using the ggsave in-built function
  ggsave("G19_plot01.pdf")
  
  # Data frame df2 is created with new_vector and its squared value
  df2=data.frame(new_vector,new_vector^2)
  # A scatter plot is plotted using this dataframe where geom_point() is used for scatter plot and labs is used to add title to the plot and labels to X and Y axis
  ggplot(df2,aes(x=new_vector,y=new_vector^2))+geom_point()+labs(title="G19_plot02",x="new_vector",y=expression(paste(new_vector^2)))
  #The scatter plot is programmatically saved to the current working directory using the ggsave in-built function
  ggsave("G19_plot02.pdf")
  

  # input: num_values type: integer, specifies the sample size of the random vector you are to generate.
  # output: two plots (saved to disk, no return value), descriptions for which have been provided in the hw0 document. 
  
}

# do not call either function in this script 
# this script is only for function definitions
# if you wish to test your script, call your functions separately in a separate script.