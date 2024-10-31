#' @title Creates the quantile table output for largevar function
#' @description
#' Outputs the quantile tables from the package's corresponding vignette.
#'
#' @param r   Which partial sum the quantile table should be returned for. (Only r<=10 is available.) Default is r=1.
#' @examples
#' quantile_tables(r=3)
#' @returns A numeric matrix.
#' @export
quantile_tables <- function(r=1){

  # Stopping conditions
  if((is.numeric(r)==FALSE)|(length(r) == 1)==FALSE){
    stop("`r` must be a number.")

  }else if(((r%%1==0)==FALSE)|((r>0)==FALSE)){
    stop("`r` must be a positive integer.")

  }else if  ( r>10 ){
    stop("No quantile table is available for r>10.")
  }

  percentiles <- get("percentiles", envir = asNamespace("Largevars"))
  percentiles <- as.matrix(percentiles)
  values <- c(-Inf, percentiles[,r+1])
  quant_table_vignette <- t(matrix(data=values, nrow=10))
  colnames(quant_table_vignette) <- c("0", "1","2","3","4","5","6","7","8","9")
  rownames(quant_table_vignette) <- c("0.0", "0.1","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9")
  return(quant_table_vignette)
}


