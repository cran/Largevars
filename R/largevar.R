#' @title Cointegration test for settings of large N and T
#' @description
#' Runs the Bykhovskaya-Gorin test for cointegration. Paper can be found at: https://doi.org/10.48550/arXiv.2202.07150
#'
#' @param data A numeric matrix where the columns contain individual time series that will be examined for the presence of cointegrating relationships.
#' @param k The number of lags that we wish to employ in the vector autoregression. The default value is k = 1.
#' @param r The number of largest eigenvalues used in the test. The default value is r = 1.
#' @param fin_sample_corr A boolean variable indicating whether we wish to employ finite sample correction on our test statistic. The default value is fin_sample_corr = FALSE.
#' @param plot_output A boolean variable indicating whether we wish to generate a plot of the empirical distribution of eigenvalues. The default value plot_output = TRUE.
#' @param significance_level Specify the significance level at which the decision about H0 should be made. The default value is significance_level = 0.05.
#' @examples
#' largevar(
#'   data = matrix(rnorm(60, mean = 0.05, sd = 0.01), 20, 3),
#'   k = 1,
#'   r = 1,
#'   fin_sample_corr = FALSE,
#'   plot_output = FALSE,
#'   significance_level = 0.05
#' )
#' @returns A list that contains the test statistic, a table with theoretical quantiles presented for r=1 to r=10, and the decision about H0 at the significance level specified by the user.
#' @importFrom graphics hist curve
#' @export
largevar <- function(data=NULL,k=1,r=1, fin_sample_corr = FALSE, plot_output=TRUE, significance_level = 0.05){

# Stopping conditions
  check_input_largevar(data,k,r, fin_sample_corr, plot_output, significance_level)

#parameters based on data
  ss = dim(data)
  tau = ss[1]
  t = tau-1
  N =ss[2]

# Matrix objects to store our transformed data in
  X_tilde=matrix(nrow=N,ncol=t)
  dX=matrix(nrow=N,ncol=t)

# transform the data input
  for (i in 1:t){
    X_tilde[,i]=data[i,]-((i-1)/t)*(data[tau,]-data[1,]) #de-trend, time shift
    # changes from the (N,T) layout to (T,N) layout
    dX[,i]=(data[i+1,]-data[i,]) #differencing
  }


# Based on whether k=1 or k>1 our code is a little different for conducting the test, hence the "if" structure
  if (k==1){
    # Step 2: De-mean data
      R0=matrix(0, N, t)
      R1=matrix(0,N,t)
      meanvec_tilde <- apply(X_tilde, 1 , mean)
      R1 <- apply(X_tilde, 2, function(x) x-meanvec_tilde)
      meanvec_d <- apply(dX, 1 , mean)
      R0 <- apply(dX,2,function(x) x-meanvec_d)


    # Calculate squared sample canonical correlations between R0 and R1
      S00=R0%*%t(R0)
      Skk=R1%*%t(R1)
      S0k=R0%*%t(R1)
      Sk0=R1%*%t(R0)

  } else { # k>1
    # Create cyclic lag matrix
      m <- matrix(1,nrow=t-1,t-1)
      m <- m - lower.tri(m,diag=FALSE)-upper.tri(m,diag = FALSE)
      m <- rbind(rep(0,t-1),m)
      m <- cbind(m,rep(0,t))
      m[1,t] <- 1

    # Create variable matrices for regressions
      Z1=matrix(1,nrow=N*(k-1)+1,ncol=t)
      Zk <- X_tilde

    # Creating X_{t-k} based on VECM form
      for (i in 1:(k-1)){
        Zk <- Zk%*%t(m)
      }

    # Creating the lags with the cyclic lag operator
      cyclic_lag <- dX
      for (j in 1:(k-1)){
        cyclic_lag <- cyclic_lag%*%t(m)
        Z1[(1+N*(j-1)):(N*j),]<-cyclic_lag
      }

    # stacked regressions
      M11=Z1%*%t(Z1)/t;
      R0=dX-(dX%*%t(Z1)/t)%*%solve(M11)%*%Z1
      Rk=Zk-(Zk%*%t(Z1)/t)%*%solve(M11)%*%Z1
      S00=R0%*%t(R0)
      Skk=Rk%*%t(Rk)
      S0k=R0%*%t(Rk)
      Sk0=Rk%*%t(R0)
  }

  can_corr_mat <- solve(Skk)%*%Sk0%*%solve(S00)%*%S0k
  ev_values <- eigen(can_corr_mat)$values
  ev_values <- sort(ev_values, decreasing = TRUE)

 # Test statistic
  loglambda <- log(rep(1,length(ev_values))-ev_values)
  NT <- sum(loglambda[c(1:r)])

      if (fin_sample_corr == FALSE){
        p <- 2
        q <- t/N - k
      } else{ # if we have finite sample correction request by the user
        p <- 2-2/N
        q <- t/N - k - 2/N
      }

      lambda_m <- 1/((p+q)^2)*((sqrt(p*(p+q-1))-sqrt(q))^2)
      lambda_p <- 1/((p+q)^2)*((sqrt(p*(p+q-1))+sqrt(q))^2)

      c_1 <- log(1-lambda_p)
      c_2 <- -((2^(2/3)*lambda_p^(2/3))/(((1-lambda_p)^(1/3))*((lambda_p-lambda_m)^(1/3))))*((p+q)^(-2/3))

      # test statistic
      LR_nt <- (NT-r*c_1)/((N^(-2/3))*c_2)

 # r=1-10 test statistic for output table to aid decision making about the H0 at different H1 alternatives
      statistics <- rep(0,10)
      for (i in 1:10){
        nt <- sum(loglambda[c(1:i)])
        statistics[i] <- (nt-i*c_1)/((N^(-2/3))*c_2)
      }

 # Output table (r=1-10 H0 at 0.9, 0.95, 0.97, 0.99 percentiles)
      percentiles <- get("percentiles", envir = asNamespace("Largevars"))
      table <- cbind(t(percentiles[90,2:11]),t(percentiles[95,2:11]),t(percentiles[99,2:11]),statistics)
      colnames(table) <- c("10% Critical value", "5% Critical value", "1% Critical value", "Test stat.")
      rownames(table) <- c("r=1",  "r=2",  "r=3",  "r=4" , "r=5",  "r=6" , "r=7" , "r=8" , "r=9" , "r=10")


#guide for how to make the decision on H0
      decision <- paste('If the test statistic is larger than the quantile, reject H0 at the chosen level.')

      # makes intermediary list to merge in to the final list that is the output of the function
      list_table <- list("significance_table"=table,"Statistical decision"=decision)

    if (r<=10){
      #  statistical table output: as default give only the row corresponding to r
      significance_table_row <- cbind(t(table[r,1:3]),table[r,4])
      colnames(significance_table_row) <- c("10% Critical value", "5% Critical value", "1% Critical value", "Test stat.")
      rownames(significance_table_row) <- ""
          # we match DOWN our test statistic because we have right sided test statistic and we don't want to increase type 1 error rate
          lessthan_matrix <- as.matrix(which(percentiles[,1+r] <= LR_nt))

        #p-value
          if (dim(lessthan_matrix)[1] == 0){ #p-value greater than than 0.99
                  p_value <- 0.99
                  decision_2 <- "The p-value is greater than 0.99"

                  if (significance_level > 0.99){ #we don't know what the decision would be because we don't know the correct p-value
                            decision_3 <- NA
                  } else{
                            decision_3 <- 0
                  }

          }else if(LR_nt <= as.numeric(percentiles[99,1+r])){
                  p_value <- 1-as.numeric(percentiles[dim(lessthan_matrix)[1],1])
                  decision_2 <- paste('The p-value is',p_value)

                  if (p_value >= significance_level){
                    decision_3 <- 0
                  }else{
                    decision_3 <- 1
                  }

          }else{ #p-value smaller than than 0.01
                  p_value <- 0.01
                  decision_2 <- "The p-value is less than 0.01"

                  if (significance_level < 0.01){ #we don't know what the decision would be because we don't know the correct p-value
                    decision_3 <- NA
                  } else{
                    decision_3 <- 1
                  }
          }

          list_2 <- list("significance_row"=significance_table_row,"p_value"=p_value, "text"=decision_2, "boolean_decision" = decision_3) #intermediary list to append our final, output list with
          list_table <- append(list_table,list_2)
    }
    # if r>=10 then we don't have quantiles, so below applies
    significance_table_row <- " "
    list_2 <- list("significance_row"=significance_table_row)
    list_table <- append(list_table,list_2)


 list <- list("statistic"=LR_nt, "measure_upper_bound"=lambda_p, "measure_lower_bound"=lambda_m, "eigenvalues"=ev_values,"significance_test"=list_table,"k"=k,"r"=r,"N"=N,"t"=t)

    if (plot_output==TRUE){ # Plot the function
      my_function <- function(x){(p+q)*sqrt(pmax(0,(lambda_p-x)*(x-lambda_m)))/x/(1-x)/2/pi}
      plot <- hist(ev_values, breaks = 2*(ceiling(log2(length(ev_values)))+1), probability = TRUE, col = "lightblue", border = "white", main = paste("VAR(",k,") Eigenvalues"),xlab = "Eigenvalues", ylab = "" ,xlim=c(0,1))
      curve(my_function,add = TRUE)

      list <- append(list,plot)
    }
 t <- list # function output
 new("stat_test", t)
}
