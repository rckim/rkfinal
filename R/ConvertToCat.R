#' Converting Categorical Variables to Factors
#'
#' @param df A data frame
#' @param nam A vector of names of your categorical variables
#'
#' @return Variables as factors
#' @export
#'

ConvertToCat <- function(df, nam)
{
  df[nam]<-lapply(df[nam],factor)
  return(df[nam])
}
