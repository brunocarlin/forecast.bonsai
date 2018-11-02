#' Error functions
#' 
#' Functions to be used in conjunction with bonsai_calculate_errors
#' @param y A time series
#' @param error the error created by some cross-validation method
#' @param h the horizon to forecast
#' @return A list with all of the forecast means
#' @export 
Mean_Error <- function(y = NULL, error) {
  mean(error, na.rm = TRUE)
}
#' Error functions
#' 
#' Functions to be used in conjunction with bonsai_calculate_errors
#' @param y A time series
#' @param error the error created by some cross-validation method
#' @param h the horizon to forecast
#' @return A list with all of the forecast means
#' @export 
CV_Mean_Error <- function(y = NULL, error) {
  colMeans(error, na.rm = TRUE)
}
#' Error functions
#' 
#' Functions to be used in conjunction with bonsai_calculate_errors
#' @param y A time series
#' @param error the error created by some cross-validation method
#' @param h the horizon to forecast
#' @return A list with all of the forecast means
#' @export 
Percentage_Error <- function(y, error) {
  col_Names_H <- colnames(error)
  Percentage_Error_Result <- (100 * error) / y
  colnames(Percentage_Error_Result) <- col_Names_H
  return(Percentage_Error_Result)
  
}
#' Error functions
#' 
#' Functions to be used in conjunction with bonsai_calculate_errors
#' @param y A time series
#' @param error the error created by some cross-validation method
#' @param h the horizon to forecast
#' @return A list with all of the forecast means
#' @export 
Mean_Percentage_Error <- function(y, error) {
  col_Names_H <- colnames(error)
  Percentage_Error_Result <- (100 * error) / y
  colnames(Percentage_Error_Result) <- col_Names_H
  mean(Percentage_Error_Result, na.rm = TRUE)
}
#' Error functions
#' 
#' Functions to be used in conjunction with bonsai_calculate_errors
#' @param y A time series
#' @param error the error created by some cross-validation method
#' @param h the horizon to forecast
#' @return A list with all of the forecast means
#' @export 
CV_Mean_Percentage_Error <- function(y, error) {
  col_Names_H <- colnames(error)
  Percentage_Error_Result <- (100 * error) / y
  colnames(Percentage_Error_Result) <- col_Names_H
  colMeans(Percentage_Error_Result, na.rm = TRUE)
}
#' Error functions
#' 
#' Functions to be used in conjunction with bonsai_calculate_errors
#' @param y A time series
#' @param error the error created by some cross-validation method
#' @param h the horizon to forecast
#' @return A list with all of the forecast means
#' @export 
Squared_Error <- function(y = NULL, error) {
  error ^ 2
}
#' Error functions
#' 
#' Functions to be used in conjunction with bonsai_calculate_errors
#' @param y A time series
#' @param error the error created by some cross-validation method
#' @param h the horizon to forecast
#' @return A list with all of the forecast means
#' @export 
Mean_Squared_Error <- function(y = NULL, error) {
  mean(error ^ 2, na.rm = TRUE)
}
#' Error functions
#' 
#' Functions to be used in conjunction with bonsai_calculate_errors
#' @param y A time series
#' @param error the error created by some cross-validation method
#' @param h the horizon to forecast
#' @return A list with all of the forecast means
#' @export 
CV_Mean_Squared_Error <- function(y = NULL, error) {
  colMeans(error ^ 2, na.rm = TRUE)
}
#' Error functions
#' 
#' Functions to be used in conjunction with bonsai_calculate_errors
#' @param y A time series
#' @param error the error created by some cross-validation method
#' @param h the horizon to forecast
#' @return A list with all of the forecast means
#' @export 
Root_Mean_Squared_Error <- function(y = NULL, error) {
  sqrt(mean(error ^ 2, na.rm = TRUE))
}
#' Error functions
#' 
#' Functions to be used in conjunction with bonsai_calculate_errors
#' @param y A time series
#' @param error the error created by some cross-validation method
#' @param h the horizon to forecast
#' @return A list with all of the forecast means
#' @export 
CV_Root_Mean_Squared_Error <- function(y = NULL, error) {
  sqrt(colMeans(error ^ 2, na.rm = TRUE))
}
#' Error functions
#' 
#' Functions to be used in conjunction with bonsai_calculate_errors
#' @param y A time series
#' @param error the error created by some cross-validation method
#' @param h the horizon to forecast
#' @return A list with all of the forecast means
#' @export 
Absolute_Error <- function(y = NULL, error) {
  abs(error)
}
#' Error functions
#' 
#' Functions to be used in conjunction with bonsai_calculate_errors
#' @param y A time series
#' @param error the error created by some cross-validation method
#' @param h the horizon to forecast
#' @return A list with all of the forecast means
#' @export 
Mean_Absolute_Error <- function(y = NULL, error) {
  mean(abs(error), na.rm = TRUE)
}
#' Error functions
#' 
#' Functions to be used in conjunction with bonsai_calculate_errors
#' @param y A time series
#' @param error the error created by some cross-validation method
#' @param h the horizon to forecast
#' @return A list with all of the forecast means
#' @export 
CV_Mean_Absolute_Error <- function(y = NULL, error) {
  colMeans(abs(error), na.rm = TRUE)
}
#' Error functions
#' 
#' Functions to be used in conjunction with bonsai_calculate_errors
#' @param y A time series
#' @param error the error created by some cross-validation method
#' @param h the horizon to forecast
#' @return A list with all of the forecast means
#' @export 
Absolute_Percentage_Error <- function(y, error) {
  col_Names_H <- colnames(error)
  Percentage_Error_Result <- (100 * error) / y
  colnames(Percentage_Error_Result) <- col_Names_H
  Absolute_Percentage_Error_Result <- abs(Percentage_Error_Result)
  return(Absolute_Percentage_Error_Result)
}
#' Error functions
#' 
#' Functions to be used in conjunction with bonsai_calculate_errors
#' @param y A time series
#' @param error the error created by some cross-validation method
#' @param h the horizon to forecast
#' @return A list with all of the forecast means
#' @export 
Mean_Absolute_Percentage_Error <- function(y, error) {
  col_Names_H <- colnames(error)
  Percentage_Error_Result <- (100 * error) / y
  colnames(Percentage_Error_Result) <- col_Names_H
  Absolute_Percentage_Error_Result <- abs(Percentage_Error_Result)
  Mean_Absolute_Percentage_Error_Result <-
    mean(Absolute_Percentage_Error_Result, na.rm = TRUE)
  return(Mean_Absolute_Percentage_Error_Result)
}
#' Error functions
#' 
#' Functions to be used in conjunction with bonsai_calculate_errors
#' @param y A time series
#' @param error the error created by some cross-validation method
#' @param h the horizon to forecast
#' @return A list with all of the forecast means
#' @export 
CV_Mean_Absolute_Percentage_Error <- function(y, error) {
  col_Names_H <- colnames(error)
  Percentage_Error_Result <- (100 * error) / y
  colnames(Percentage_Error_Result) <- col_Names_H
  Absolute_Percentage_Error_Result <- abs(Percentage_Error_Result)
  CV_Mean_Absolute_Percentage_Error_Result <-
    colMeans(Absolute_Percentage_Error_Result, na.rm = TRUE)
  return(CV_Mean_Absolute_Percentage_Error_Result)
  
}
#' Error functions
#' 
#' Functions to be used in conjunction with bonsai_calculate_errors
#' @param y A time series
#' @param error the error created by some cross-validation method
#' @param h the horizon to forecast
#' @return A list with all of the forecast means
#' @export 
Symmetric_Absolute_Percentage_Error <- function(y, error) {
  col_Names_H <- colnames(error)
  Absolute_Error <- abs(error)
  Error_Plus_y <- error + y
  Symmetric_Absolute_Percentage_Error <- 200 * Absolute_Error / Error_Plus_y
  colnames(Symmetric_Absolute_Percentage_Error) <- col_Names_H
  return(Symmetric_Absolute_Percentage_Error)
}
#' Error functions
#' 
#' Functions to be used in conjunction with bonsai_calculate_errors
#' @param y A time series
#' @param error the error created by some cross-validation method
#' @param h the horizon to forecast
#' @return A list with all of the forecast means
#' @export 
Mean_Symmetric_Absolute_Percentage_Error <- function(y, error) {
  col_Names_H <- colnames(error)
  Absolute_Error <- abs(error)
  Error_Plus_y <- error + y
  Symmetric_Absolute_Percentage_Error <-
    200 * Absolute_Error / Error_Plus_y
  colnames(Symmetric_Absolute_Percentage_Error) <- col_Names_H
  mean(Symmetric_Absolute_Percentage_Error, na.rm = TRUE)
}
#' Error functions
#' 
#' Functions to be used in conjunction with bonsai_calculate_errors
#' @param y A time series
#' @param error the error created by some cross-validation method
#' @param h the horizon to forecast
#' @return A list with all of the forecast means
#' @export 
CV_Mean_Symmetric_Absolute_Percentage_Error <- function(y, error) {
  col_Names_H <- colnames(error)
  Absolute_Error <- abs(error)
  Error_Plus_y <- error + y
  Symmetric_Absolute_Percentage_Error <- 200 * Absolute_Error / Error_Plus_y
  colnames(Symmetric_Absolute_Percentage_Error) <- col_Names_H
  colMeans(Symmetric_Absolute_Percentage_Error, na.rm = TRUE)
}
#' Error functions
#' 
#' Functions to be used in conjunction with bonsai_calculate_errors
#' @param y A time series
#' @param error the error created by some cross-validation method
#' @param h the horizon to forecast
#' @return A list with all of the forecast means
#' @export 
Absolute_Scaled_Error <- function(y, error) {
  (abs(error) / mean(abs(diff(y, lag = frequency(
    y
  )))))
}
#' Error functions
#' 
#' Functions to be used in conjunction with bonsai_calculate_errors
#' @param y A time series
#' @param error the error created by some cross-validation method
#' @param h the horizon to forecast
#' @return A list with all of the forecast means
#' @export 
Mean_Absolute_Scaled_Error <- function(y, error) {
  ASE <-  (abs(error) / mean(abs(diff(y, lag = frequency(
    y
  )))))
  mean(ASE, na.rm = TRUE)
}
#' Error functions
#' 
#' Functions to be used in conjunction with bonsai_calculate_errors
#' @param y A time series
#' @param error the error created by some cross-validation method
#' @param h the horizon to forecast
#' @return A list with all of the forecast means
#' @export 
CV_Mean_Absolute_Scaled_Error <- function(y, error) {
  ASE <-  (abs(error) / mean(abs(diff(y, lag = frequency(
    y
  )))))
  colMeans(ASE, na.rm = TRUE)
}
#' Error functions
#' 
#' Functions to be used in conjunction with bonsai_calculate_errors
#' @param y A time series
#' @param error the error created by some cross-validation method
#' @param h the horizon to forecast
#' @return A list with all of the forecast means
#' @export 
Mean_Random_Error <- function(y, error) {
  
  MRE <- apply(error, 1, function(x) x*0 + rnorm(ncol(error)) )
  
  
  mean(MRE, na.rm = TRUE)
  
}
#' Error functions
#' 
#' Functions to be used in conjunction with bonsai_calculate_errors
#' @param y A time series
#' @param error the error created by some cross-validation method
#' @param h the horizon to forecast
#' @return A list with all of the forecast means
#' @export 
CV_Mean_Random_Error <- function(y, error) {
  
  CMRE <- apply(error, 1, function(x) x*0 + rnorm(ncol(error)) )
  
  
  rowMeans(CMRE, na.rm = TRUE)
  
}
#' Error functions
#' 
#' Functions to be used in conjunction with bonsai_calculate_errors
#' @param y A time series
#' @param error the error created by some cross-validation method
#' @param h the horizon to forecast
#' @return A list with all of the forecast means
#' @export 
Mean_Accuracy <- function(y, error) {
  Mean_Accracy_Results <- rbind(
    AME = abs(Mean_Error(y, error)),
    RMSE = Root_Mean_Squared_Error(y, error),
    MAE = Mean_Absolute_Error(y, error),
    AMPE = abs(Mean_Percentage_Error(y, error)),
    MAPE = Mean_Absolute_Percentage_Error(y, error),
    MASE = Mean_Absolute_Scaled_Error(y, error),
    AsMAPE = abs(Mean_Symmetric_Absolute_Percentage_Error(y, error)),
    AMRE = abs(Mean_Random_Error(y,error))
  )
  colnames(Mean_Accracy_Results) <- "Averaged_Time"
  return(Mean_Accracy_Results)
}
#' Error functions
#' 
#' Functions to be used in conjunction with bonsai_calculate_errors
#' @param y A time series
#' @param error the error created by some cross-validation method
#' @param h the horizon to forecast
#' @return A list with all of the forecast means
#' @export 
CV_Mean_Accuracy <- function(y, error) {
  CV_Mean_Accuracy_Results <- rbind(
    AME = abs(CV_Mean_Error(y, error)),
    RMSE = CV_Root_Mean_Squared_Error(y, error),
    MAE = CV_Mean_Absolute_Error(y, error),
    AMPE = abs(CV_Mean_Percentage_Error(y, error)),
    MAPE = CV_Mean_Absolute_Percentage_Error(y, error),
    MASE = CV_Mean_Absolute_Scaled_Error(y, error),
    AsMAPE = abs(CV_Mean_Symmetric_Absolute_Percentage_Error(y, error)),
    AMRE = abs(CV_Mean_Random_Error(y,error))
  )
  colnames(CV_Mean_Accuracy_Results) <- colnames(error)
  return(CV_Mean_Accuracy_Results)
}
