
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
#' @param data Object type data.frame
#' @param column Data variable column
#' @param k Number of classes, if null is calculated following the Sturges rule
#' @return data.frame with the frequency distribution
#' @details Use the vector data and return the data.frame with the frequency distribution
#' @importFrom fdth fdt
#' @importFrom fdth plot.fdt.default
#' @importFrom viridis viridis
#' @export
freq_dist <- function(data, column, k=NULL){
  
  vector = data[,column]
  
  if(is.null(k)){
    k = round(1 + 3.3*log10(length(vector))) # Sturges Rule
    message(paste("Defined",k,"classes using the Sturges Rule.", sep = " "))
  }

  table = fdt(x = vector, k)
  plot.fdt.default(table, type=c('fh'), col = rev(viridis(k)))

  return(table)
}

#' plot_hist
#'
#' Plot histogram
#'
#' @param data Object type data.frame
#' @param k Number of classes
#' @param fi Absolute frequency vector
#' @param title Histogram title
#' @param x_name Label for x axis
#' @param y_name Label for y axis
#' @return Plot histogram
#' @details Plots the histogram with vector data, frequency and number of classes
#' @importFrom graphics hist
#' @export
#'
plot_hist <- function(data, k, fi, title="Histogram", x_name="Data", y_name="Frequency"){
  v_min = min(data)
  v_max = max(data)
  amplitude_total = v_max - v_min
  amp = amplitude_total / (k-1)
  v_min = v_min - amp/2
  v_max = v_max + amp/2
  
  hist(data, breaks=seq(v_min, v_max, by=amp),
       labels=T, col="white", main=title, xlab=x_name, ylab=y_name, plot=TRUE,
       xlim=c(min(data)-amp, max(data)+amp),
       ylim=c(0, 1.25*max(fi))
  )
}


#' normality_test
#'
#' Runs shapiro.test on the data columns
#'
#' @param data Object type data.frame
#' @param colums Data columns
#' @return data.frame with shapiro.test for selected columns
#' @details Run shapiro.test on the data columns and return data.frame with the p_value of each column
#' @importFrom stats shapiro.test
#' @export
#'
normality_test <- function(data, colums){
  id = c()
  name = c()
  p_valor = c()
  for(i in colums){
    id[length(id)+1] = i
    name[length(name)+1] = names(data[i])
    st = shapiro.test(data[,i])
    p_valor[length(p_valor)+1] = st$p.value
  }
  norm_test = data.frame(id=id, name=name, p.value=p_valor)
  print(norm_test)
  return(norm_test)
}

#' basic
#'
#' Call the basicStats function
#'
#' @param data Object type data.frame
#' @param column Data column
#' @return data.frame with the basic statistical measures
#' @details Call the basicStats function, and plot the data boxplot
#' @importFrom fBasics basicStats
#' @export
#'
basic = function(data, column){
  estb=basicStats(data[, column], ci=0.95)
  return(estb)
}
