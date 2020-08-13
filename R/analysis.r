
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

#' freq_dist
#'
#' Calculates the frequency distribution
#'
#' @param vector Object type data.frame
#' @param k Number of classes
#' @param show_preview Boolean which true case displays data.frame with the frequency distribution
#' @return data.frame with the frequency distribution
#' @details Use the vector data and return the data.frame with the frequency distribution
#' @export
freq_dist <- function(vector, k=NULL, show_preview=FALSE){
  
  if(is.null(k)){
    k = round(1 + 3.3*log10(length(vector))) # Sturges Rule
  }
  
  v_max = max(vector)
  v_min = min(vector)
  
  amplitude_total = v_max - v_min
  amp = amplitude_total / (k-1)
  
  class = table(cut(vector, breaks = seq(v_min-amp/2, v_max+amp/2, by = amp), right = F))
  fi = as.vector(class)
  Fi = fi
  
  for(i in 2:length(fi)){Fi[i] = Fi[i-1] + Fi[i]}
  fr = round(fi/length(vector), 4)
  Fr = fr
  for(i in 2:length(fr)){Fr[i] = (Fr[i-1] + Fr[i])}
  table = data.frame(class, fi, Fi, fr, Fr)
  
  if(show_preview == T){
    print(table)
    plot_hist(vector, k, fi)
  }
  
  return(table)
}
