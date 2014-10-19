library(roxygen2)
library(devtools)

#' multiStockHistory
#'
#' Pull multiple historic stock returns series
#' @keywords stock returns
#' @param symbols char vector of stock or index symbols
#' @param symbols.names char vector of names for symbols
#' @param startday int first day of series
#' @param startmonth int first day of series
#' @param startyear int first day of series
#' @param endday int first day of series
#' @param endmonth int first day of series
#' @param endyear int first day of series
#' @param series char default "Adj.Close" {"High","Low","Close","Volume","Adj.Close"}
#' @return list: [[1]] data.frame of series attribute from all symbols, [[2]] list of data.frames of all series from all symbols
#' @author Stephen Downing
#' @details This function downloads multiple historic series for stocks and stock indices from Yahoo, returning a returns list: [[1]] data frame of given series from all symbols [[2]] full list of all series data frames from all symbols
#' @example
#' \dontrun{
#' multiStockHistory(c('AAPL','GOOG'))
#' tail(y$df)
#' }
#' @export

multiStockHistory <- function(symbols, #char vector
                              symbols.names=NULL, #char vector 
                              startday=1, #int
                              startmonth=1,
                              startyear=1970,
                              endday=19,
                              endmonth=10,
                              endyear=2014,
                              series="Adj.Close",
                              ...)
{
  #if(!require(xts)) {install.packages('xts')}
  library(xts)
  if(is.null(symbols.names)) {symbols.names <- symbols}
  
  # loop through symbols downloading series to list
  x <- list()
  for (i in 1:length(symbols)) {
    base <- "http://ichart.yahoo.com/table.csv?s="
    tail <- paste("&a=",startday,"&b=",startmonth,"&c=",startyear,"&d=",endday,"&e=",endmonth,"&f=",endyear,"&g=d&ignore=.csv",sep="")
    url <- paste(base,symbols[i],tail,sep="")
    x[[i]] <- read.table(url,sep=",",header=T, stringsAsFactors=F,flush = T,...)
    x[[i]][,1] <- as.Date(x[[i]][,1])
    x[[i]] <- xts(x[[i]][,-1], order.by=x[[i]][,1])
    xtsAttributes(x[[i]]) <- list(IndexSymbol=symbols[i], IndexName=symbols.names[i])
    
    cat("pulled series: ",symbols.names[i],"\n")
  }
  
  #build xts df with only 1 series, e.g., Adj.Close
  ind.df <- data.frame()
  for (i in 1:length(x)) {
    if (dim(ind.df)[2]==0) {
      ind.df <- x[[i]][,series]
      colnames(ind.df)[i] <- unlist(xtsAttributes(x[[i]]))[2]
    } else {
      ind.df <- merge.xts(ind.df,x[[i]][,series])
      colnames(ind.df)[i] <- unlist(xtsAttributes(x[[i]]))[2]
    }
  }
  return(list(df=ind.df,full.list=x))
} #end function

