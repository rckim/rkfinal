
#' Generate the Continuous Variable Table
#'
#' @param dat Your output from ContFuncTest
#'
#' @return A Table with your analysis
#' @export
#'
#' @importFrom flextable flextable
#' @importFrom dplyr mutate
#'
myContTable <- function(dat) {
  myContTab <- dat %>% mutate(pVal = ifelse(pVal < 0.001, "<0.001", round(pVal,3))) %>%
    mutate(GroupA_Mean = round(GroupA_Mean,2)) %>%
    mutate(GroupA_SD = round(GroupA_SD,2)) %>%
    mutate(GroupB_Mean = round(GroupB_Mean,2)) %>%
    mutate(GroupB_SD = round(GroupB_SD,2)) %>%
    mutate(pVal = ifelse(pVal < 0.05, paste0(pVal,"*"),pVal))
  myContTab$GroupA_MSD <- paste(myContTab$GroupA_Mean, "\u00B1", myContTab$GroupA_SD)
  myContTab$GroupB_MSD <- paste(myContTab$GroupB_Mean, "\u00B1", myContTab$GroupB_SD)

  flextable(myContTab, col_keys = c("Variable", "GroupA_MSD", "GroupB_MSD", "pVal")) %>%
    width(j=1, width = 2, unit="in") %>%
    width(j=2:3, width = 1.5, unit="in") %>%
    set_header_labels(variable = "Variable", GroupA_MSD = "Mean \u00B1 SD", GroupB_MSD = "Mean \u00B1 SD", pVal = "p-value") %>%
    add_header_row(values = c("", GroupNames, ""), colwidths = c(1,1,1,1)) %>% align(j=2:4, align = "center", part="all")
}
