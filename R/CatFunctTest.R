#' Analysis of Categorical Variables
#'
#' @param dat A data frame
#' @param names A vector of names of your categorical variables
#'
#' @return A data frame with n, percentage, and p-value (from other Fisher's or Chi-Square tests)
#' for each variable and group
#' @export
#'
#' @importFrom stats chisq.test filter fisher.test sd shapiro.test t.test wilcox.test


CatFunctTest <- function(dat, names)
{
  myTab <- lapply(dat[names], function(x) table(x, dat$independent))
  myChi <- lapply(myTab, function(x) chisq.test(x))
  myFish <- lapply(myTab, function(x) fisher.test(x))
  ## p values
  i<-1
  p <- seq(1, length(names))
  for(i in 1:length(names)){
    p[i]<-myChi[[i]][["p.value"]]
    ## Extract the expected, and if less than 5, use Fisher's
    if(min(myChi[[i]][["expected"]]) < 5){
      p[i] <- myFish[[i]][["p.value"]]
    }
    i <- i+1
  }
  grpA_N <- seq(1, length(names))
  grpB_N <- seq(1, length(names))
  grpA_Pct <- seq(1, length(names))
  grpB_Pct <- seq(1, length(names))
  j<-1
  for(j in 1:length(names)){
    myCnt <- myTab[[j]]
    myPct <- prop.table(myTab[[j]],2)
    grpA_N[j] <- myCnt[1,1]
    grpB_N[j] <- myCnt[1,2]
    grpA_Pct[j] <- 100*myPct[1,1]
    grpB_Pct[j] <- 100*myPct[1,2]
    j <- j+1
  }
  newDF <- data.frame(Variable = names, GroupA_N = grpA_N, GroupA_Pct = grpA_Pct, GroupB_N = grpB_N, GroupB_Pct = grpB_Pct, pVal = p)
}
