#' Analyze Continuous Variables
#'
#' @param dat A data frame
#' @param names A vector of names of continuous variables
#'
#' @return A data fram with mean, standard deviation, and p-value (either t-Test or U-test) for each
#' group and variable
#' @export
#'
#' @importFrom stats chisq.test filter fisher.test sd shapiro.test t.test wilcox.test

ContFuncTest <- function(dat, names) {
  l <- length(names)
  #Shapiro-Wilk Normality tests, T-Tests, and Non-Parametric Mann-Whitney U tests
  myNorm <- lapply(dat[1:l], function (x) shapiro.test(x))
  myT <- lapply(dat[1:l], function (x) t.test(x ~ dat$independent))
  myU <- lapply(dat[1:l], function (x) wilcox.test(x ~ dat$independent))
  ## p values
  i <- 1
  p <- seq(1, length(names))
  for (i in 1:length(names)) {
    p[i] <- myT[[i]][["p.value"]]
    ## Check normality, if less than 0.05, then use U-Test
    if (myNorm[[i]][["p.value"]] < 0.05) {
      p[i] <- myU[[i]][["p.value"]]
    }
    i <- i+1
  }
  #Means and Standard Deviations
  grpA_mean <- seq(1, length(names))
  grpB_mean <- seq(1, length(names))
  grpA_SD <- seq(1, length(names))
  grpB_SD <- seq(1, length(names))
  j <- 1
  datagrp1 <- dat %>% dplyr::filter(independent==0)
  datagrp2 <- dat %>% dplyr::filter(independent==1)
  for (j in 1:length(names)) {
    grpA_mean[j] <- myT[[j]][["estimate"]][[1]]
    grpB_mean[j] <- myT[[j]][["estimate"]][[2]]
    grpA_SD[j] <- sd(datagrp1[[j]], na.rm = T)
    grpB_SD[j] <- sd(datagrp2[[j]], na.rm = T)
    j <- j+1
  }

  newDF <- data.frame(Variable = names, GroupA_Mean = grpA_mean, GroupA_SD = grpA_SD,
                      GroupB_Mean = grpB_mean, GroupB_SD = grpB_SD, pVal = p)
}


