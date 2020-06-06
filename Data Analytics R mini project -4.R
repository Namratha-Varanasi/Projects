

# Write code for regression here
alda_regression <- function(x_train, x_test, y_train, regression_type){
  # Perform regression (linear/ridge/lasso)
  
  # Inputs:
    # x_train: training data frame(19 variables, x1-x19)
    # x_test: test data frame(19 variables, x1-x19)
    # y_train: dependent variable, training data (vector, continous type)
    # regression_type: specifies type of regression, string variable, can be of type 'linear', 'ridge' or 'lasso'
  
  # General Information:
    # Instructions for specific regression types:
      # linear: no cross validation
      # ridge: use 10-fold cross validation to determine optimal lambda
      # lasso: use 10-fold cross validation to determine optimal lambda
  
  # Output:
    # A list with two elements, first element = model generated, second element = predictions on test data (vector) 
  
  # allowed packages: R-base, glmnet
  
  # Function hints: Read the documentation for the functions glmnet, cv.glmnet, predict
  # Ridge and Lasso regression hints: Lambda is the hyperparameter
  if(regression_type == 'linear'){ 
    # ~ 2-3 lines of code
    # write code for building a linear regression model using x_train, y_train
    # can you use glmnet to do simple linear regression as well?
    # Explore away!  
    # Hint: Think of what the lambda value means for linear regression without regularization.
    
    # lambda is 0 for linear regression as there are no pentalty term
    linear_regression_model<-glmnet(x_train, y_train, lambda = 0);
    # predict using the model
    y_predicted<-predict.glmnet(linear_regression_model, x_test);
    #return the list of the model and its predictions
    return(list(linear_regression_model, y_predicted));
    
    
  }else if(regression_type == 'ridge'){
    # ~ 2-3 lines of code
    # write code for ridge regression here
    # 10 fold cross validation, with mse (mean squared error) as the measure
    # the hyperparameter you are tuning here is lambda
    
    # penalty alpha is 0 here
    
    ridge_regression_model <- glmnet(x = x_train, y = y_train, alpha = 0);
    cv_ridge_model <- cv.glmnet(x = x_train, y = y_train, alpha = 0, nfolds = 10);
    
    # predict on x_test using the model that gives least MSE
    y_predicted <- predict(cv_ridge__model, newx = x_test, s = "lambda.min");
    
    #return the list of the model and its predictions
    return(list(ridge_regression_model, y_predicted));
  }else{
    # ~ 2-3 lines of code
    # write code for lasso regression here
    # 10 fold cross validation, with mse (mean squared error) as the measure
    # the hyperparameter you are tuning here is lambda
    
    # penalty alpha is 1 here
    lasso_regression_model <- glmnet(x = x_train, y = y_train, alpha = 1);
    cv_lasso_model <- cv.glmnet(x = x_train, y = y_train, alpha = 1, nfolds = 10);
    
    # predict on x_test using the model that gives least MSE
    y_predicted <- predict(cv_lasso_model, newx = x_test, s = "lambda.min");
    
    #return the list of the model and its predictions
    return(list(lasso_regression_model, y_predicted));
  }
  
}

calculate_rmse <- function(y_true, y_pred){
  # DO NOT modify this code. TA has already given you code for this
  # You have already been provided this code to calculate RMSE
  
  # Inputs:
  # y_true: ground truth dependent variable values, of type vector
  # y_pred: prediction outcomes from any regression method, with the same length as y_true
  
  # Outputs:
  # a single value of type double, with the RMSE value
  return(sqrt(sum(y_true - y_pred)^2/length(y_true)));
}

regression_compare_rmse <- function(y_test, linear_regression_prediction, ridge_prediction, lasso_prediction){
  # ~ 8-10 lines of code
  # Calculate the rmse for each of the regression methods: 'linear', 'ridge', 'lasso'
  # Return the best method and its RMSE (i.e., method with least RMSE)
  
  # Inputs:
  # y_test: ground truth dependent variable from test data (vector)
  # linear_regression_prediction: predictions from linear regression (vector)
  # ridge_regression_prediction: predictions from ridge regression (vector)
  # lasso_regression_prediction: predictions from lasso regression (vector)
  
  # Returns:
  # list of three values:
  # First value, of type string, with the name of the best method
  #  'linear' if linear_regression_prediction is best
  #  'ridge' if ridge_prediction is best
  #  'lasso' if lasso_regression is best
  # Second value, of type double, with the corresponding RMSE of the best method (do not round off)
  # third value is a vector of RMSE values, in the following order: c(linear regression's RMSE, ridge regression's RMSE, lasso's RMSE)
  
  # Allowed packages: R-base
  # You are given the implementation for calculate_rmse (see above) 
  
  # compute the rmse for all the methods
  linear_regression_rmse <- calculate_rmse(y_true = y_test, y_pred = linear_regression_prediction);
  ridge_regression_rmse <- calculate_rmse(y_true = y_test, y_pred = ridge_prediction);
  lasso_regression_rmse <- calculate_rmse(y_true = y_test, y_pred = lasso_prediction);
  rmse_values <- c(linear_regression_rmse, ridge_regression_rmse, lasso_regression_rmse);
 
  best_rmse <- min(rmse_values);
  
  if(best_rmse == linear_regression_rmse)
    return(list("linear", best_rmse, rmse_values))
  
  else if(best_rmse == ridge_regression_rmse)
    return(list("ridge", best_rmse, rmse_values))
  
  else(best_rmse == lasso_regression_rmse)
  return(list("lasso", best_rmse, rmse_values))
}


alda_svm <- function(x_train, x_test, y_train, kernel_name){
  # Perform classification using support vector machines (linear/radial/sigmoid)
  
  # Inputs:
    # x_train: training data frame(4 variables, x1-x4)
    # x_test: test data frame(4 variables, x1-x4)
    # y_train: dependent variable, training data (factor)
    # kernel_name: specifies type of SVM kernel, string variable, can be of type 'linear', 'radial' or 'sigmoid' or 'polynomial'

  # General information
    # Both training data and test data have already been scaled - so you don't need to scale it once again.
  
  # Kernel specific information: using 10-fold cross-validation, perform hyperparameter tuning for each kernel as shown below:
    # Linear: 
      # 'cost' parameter: for the following values: c(0.01, 0.1, 1, 10)
    # radial: 
      # 'cost' parameter: for the following values: c(0.01, 0.1, 1, 10), 
      # 'gamma' parameter: for the following values: c(0.05, 0.5, 1, 2)
    # polynomial:
      # 'cost' parameter: for the following values: c(0.01, 0.1, 1, 10), 
      # 'gamma' parameter: for the following values: c(0.05, 0.5, 1, 2)
      # 'degree' parameter: for the following values: c(1,2,3)
    # sigmoid:
      # 'cost' parameter: for the following values: c(0.01, 0.1, 1, 10), 
      # 'gamma' parameter: for the following values: c(0.05, 0.5, 1, 2)
  
  # Output:
    # A list with two elements, first element = model generated, second element = predictions on test data (factor) 
  
  # Word of caution:
    # Make sure that you pick the best parameters after tuning
  
  # allowed packages: R-base, e1071
  
  # Hints: See if you can use the 'tune' function in e1071 for cross validation
  # create the cost, gamma and degree vectors
   cost_vector<- c(0.01, 0.1, 1, 10);
   gamma_vector<-c(0.05, 0.5, 1, 2);
   degree_vector<- c(1,2,3);
   if(kernel_name == "radial"){
    # ~1-2 lines 
    # tune to find the best cost and gamma
    tuned = tune.svm(x_train, y_train, cost = cost_vector, gamma = gamma_vector);
    best_cost = tuned$best.parameters$cost;
    best_gamma = tuned$best.parameters$gamma;
    # use the tuned parameter to build the svm model
    radial_model = svm(x_train, y_train, kernel = kernel_name, cost = best_cost, gamma = best_gamma);
    predictions = predict(radial_model, x_test);
    return(list(radial_model, predictions));
  }
   else if(kernel_name == 'polynomial'){
    #~1-2 lines
    # tune to find the best cost, gamma and degree
    tuned = tune.svm(x_train, y_train, cost = cost_vector, gamma = gamma_vector, degree = degree_vector);
    best_cost = tuned$best.parameters$cost;
    best_gamma = tuned$best.parameters$gamma;
    best_degree = tuned$best.parameters$degree;
    # use the tuned parameter to build the svm model
    poly_model = svm(x_train, y_train, kernel = kernel_name, cost = best_cost, gamma = best_gamma, degree = best_degree);
    predictions = predict(poly_model, x_test);
    return(list(poly_model, predictions));
  }
   else if(kernel_name == 'sigmoid'){
    #~1-2 lines
    # tune to find the best cost, gamma
    tuned = tune.svm(x_train, y_train, cost = cost_vector, gamma = gamma_vector);
    best_cost = tuned$best.parameters$cost;
    best_gamma = tuned$best.parameters$gamma;
    # use the tuned parameter to build the svm model
    model = svm(x_train, y_train, kernel = kernel_name, cost = best_cost, gamma = best_gamma);
    predictions = predict(model, x_test);
    return(list(model, predictions));
  }else{ # default linear kernel
    #~1-2 lines
    # tune to find the best cost
    tuned = tune.svm(x_train, y_train, cost = cost_vector);
    best_cost = tuned$best.parameters$cost;
    # use the tuned parameter to build the svm model
    model = svm(x_train, y_train, kernel = kernel_name, cost = best_cost);
    predictions = predict(model, x_test);
    return(list(model, predictions));
  }
  
}


classification_compare_accuracy <- function(y_test, linear_kernel_prediction, radial_kernel_prediction, 
                                            polynomial_kernel_prediction, sigmoid_kernel_prediction){
  # ~ 6-10 lines of code
  # Calculate the accuracy for each of the classification methods: 
    # 'svm-linear': linear kernel SVM
    # 'svm-radial': radial kernel SVM
    # 'svm-poly': polynomial kernel SVM
    # 'svm-sigmoid': sigmoid kernel SVM 
  # Return the best method and its accuracy (i.e., method with highest accuracy)
  
  # Inputs:
    # y_test: ground truth dependent variable from test data (factor)
    # linear_kernel_prediction: predictions from linear kernel SVM (factor)
    # radial_kernel_prediction: predictions from radial kernel SVM (factor)
    # polynomial_kernel_prediction: predictions from polynomial kernel SVM (factor)
    # sigmoid_kernel_prediction: predictions from sigmoid kernel SVM (factor)
    
  # Returns:
  # list of three values:
    # First value, of type string, with the name of the best method, sould be:
      # 'svm-linear' if linear_kernel_prediction is best
      # 'svm-radial' if radial_kernel_prediction is best
      # 'svm-poly' if polynomial_kernel_prediction is best
      # 'svm-sigmoid' if sigmoid_kernel_prediction is best
    # Second value, of type double, with the corresponding overall accuracy of the best method (on a scale of 100, do not round off)
    # third value, a vector with the overall accuracies of all methods in this order: c(linear-svm's accuracy, radial-svm's accuracy, poly-svm's accuracy, sigmoid-svm's accuracy)
  # Allowed packages: R-base
  # Note that I asked you to implement accuracy calculation - do not use a library for this
  
  # generate table with true and predicted value to generate a confusion matrix
  linear_svm_table = table(y_test, linear_kernel_prediction);
  # calculate the accuracy using a custom function
  linear_svm_accuracy = calculate_accuracy(linear_svm_table);
  
  # generate table with true and predicted value to generate a confusion matrix
  radial_svm_table = table(y_test, radial_kernel_prediction);
  # calculate the accuracy using a custom function
  radial_svm_accuracy = calculate_accuracy(radial_svm_table);
  
  # generate table with true and predicted value to generate a confusion matrix
  polynomial_svm_table = table(y_test, polynomial_kernel_prediction);
  # calculate the accuracy using a custom function
  polynomial_svm_accuracy = calculate_accuracy(polynomial_svm_table);
  
  # generate table with true and predicted value to generate a confusion matrix
  sigmoid_svm_table = table(y_test, sigmoid_kernel_prediction);
  # calculate the accuracy using a custom function
  sigmoid_svm_accuracy = calculate_accuracy(sigmoid_svm_table);
  
  # generate the accuracy vector
  accuracy_vector = c(linear_svm_accuracy, radial_svm_accuracy, polynomial_svm_accuracy, sigmoid_svm_accuracy);
  # generate the name vector
  name_vector = c('svm-linear', 'svm-radial', 'svm-polynomial', 'svm-sigmoid');
  # scale accuracies to 100
  accuracy_vector = accuracy_vector*100;
  # find the index of max accuracy and use that index to return the corresponding method name
  best_method_name = accuracy_vector[which.max(accuracy_vector)];
  # find the index of max accuracy and use that index to return the corresponding accuracy
  best_method_accuracy = name_vector[which.max(accuracy_vector)];
  # return a list of best method name, best method accuracy, and the vector of all accuracies
  return(list(best_method_name, best_method_accuracy, accuracy_vector));
}

# custom function to compute accuracy given the confusion table
calculate_accuracy <- function(confusionTable) {
  Total = sum(confusionTable);
  True_vals = 0;
  # sums the correct values elements
  for (i in seq(1,nrow(confusionTable))) {
    True_vals = True_vals + confusionTable[i, i];
  }
  True_vals/Total;
}

