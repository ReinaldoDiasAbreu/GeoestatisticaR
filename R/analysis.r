
#' statistical_analysis
#'
#' Performs normality tests
#' 
#' @param data Object type data.frame
#' @param column Data variable column
#' @return data frame with the results
#' @details Perform normality tests: Kolmogorov-Smirnov and Shapiro-Wilk test
#' @importFrom stats sd
#' @importFrom stats ks.test
#' @importFrom stats shapiro.test
#' @export
statistical_analysis <- function(data, column){
  tks = ks.test(data[,column], 'pnorm', mean=mean(data[,column]), sd=sd(data[,column]))
  sht = shapiro.test(data[,column])
  tests = c("statistic", "p.value")
  statistic = c(tks$statistic, sht$statistic)
  p_value = c(tks$p.value, sht$p.value)
  table = data.frame(tests, statistic, p_value)
  names(table) = c(" ","Kolmogorov-Smirnov","Shapiro-Wilk")
  return(table)
}

