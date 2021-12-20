#' Generate the Categorical Variable Table
#'
#' @param dat Your output from CatFunctTest
#'
#' @return A table with your analysis
#' @export
#'
#' @importFrom flextable flextable
#' @importFrom dplyr mutate

myCatTable <- function(dat){
  myCatTab <- dat %>% mutate(pVal = ifelse(pVal < 0.001, "<0.001", round(pVal,2))) %>%
    mutate(GroupA_Pct = round(GroupA_Pct,1)) %>% mutate(GroupB_Pct = round(GroupB_Pct,1)) %>%
    mutate(pVal = ifelse(pVal < 0.05, paste0(pVal,"*"),pVal))
  myCatTab$ANPct <- paste0(myCatTab$GroupA_N, " (", myCatTab$GroupA_Pct, ")")
  myCatTab$BNPct <- paste0(myCatTab$GroupB_N, " (", myCatTab$GroupB_Pct, ")")

  flextable(myCatTab, col_keys = c("Variable", "ANPct", "BNPct", "pVal")) %>%
    set_header_labels(variable = "Variable", ANPct = "n (%)", BNPct = "n (%)", pVal = "p-value") %>%
    add_header_row(values = c("", GroupNames, ""), colwidths = c(1,1,1,1)) %>%
    align(j=2:4, align = "center", part="all") %>% width(j=2:3, width = 1, unit="in") %>%
    width(j=1, width = 1.5, unit = "in")
}

