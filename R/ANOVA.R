#' ANOVA
#'
#' ANOVA is used to obtain ANOVA table of a fitted model. It yields the same results as anova(lm( )) for sequential sums of squares
#' and car::Anova(lm( ), type="III") for partial sums of squares.
#'
#' @param formula An object of class "formula": a symbolic description of the model to be fitted. A typical model has the form outcome ~ covariates
#' where outcome is the numeric response vector (which people usually denote as Y in statistical formula) and covariates are predictor of response.
#'
#' @param data A data frame (or object coercible by as.data.frame to a data frame) containing the variables in the model.
#'
#' @param type the type of sums of squares to be obtained. Use "Sequential" for sequential sums of squares and "Partial" for partial sums of squares.
#'
#' @return ANOVA returns ANOVA table in data.frame.
#'
#' @examples
#'
#' ANOVA(mpg ~ cyl + wt, mtcars, type = "Partial") ## Get partial SS
#' ANOVA(mpg ~ cyl + wt + disp, mtcars, type = "Sequential") ## Get sequential SS
#'
#' @export
#'
#'
ANOVA = function(formula, data, type){
  covariates = attr(terms(formula(formula)), which = "term.labels")
  CompleteData = na.omit(data)
  n = nrow(CompleteData)
  p = length(covariates) + 1
  X = matrix(c(rep(1,n), as.matrix(CompleteData[covariates])), n, p)
  Y = as.matrix(CompleteData[as.character(formula[[2]])], n, 1) ## Response
  beta = solve(t(X) %*% X) %*% t(X) %*% Y
  yhat = X %*% beta
  ei = Y - yhat
  SSE = t(ei) %*% ei
  MSE = SSE / (n - p) ## Same SSE for all SS
  if(type == "Sequential"){
    ## SSE under H0
    SSE0 = rep(0, length(covariates))
    for (i in 1:length(covariates)) {
      H0 = X[,(1:i)] %*% solve(t(X[,(1:i)]) %*% X[,(1:i)]) %*% t(X[,(1:i)])
      SSE0[i] = as.numeric(t(Y) %*% Y - t(Y) %*% H0 %*% Y)
    }
    ## SSE under H1
    SSE1 = rep(0, length(covariates))
    for (i in 1:(p-1)) {
      H1 = X[,(1:(i+1))] %*% solve(t(X[,(1:(i+1))]) %*% X[,(1:(i+1))]) %*% t(X[,(1:(i+1))])
      SSE1[i] = as.numeric(t(Y) %*% Y - t(Y) %*% H1 %*% Y)
    }
    SS = SSE0 - SSE1
    fstat = SS/rep(MSE,length(SS))
    p_val = rep(0, length(fstat))
    for (i in 1:length(fstat)) {
      p_val[i] = pf(fstat[i], 1, n-p, lower.tail = FALSE)
    }
    table = cbind(rep(1,p-1), SS, SS, fstat, p_val)
    colnames(table) = c("Df", "Sum Sq", "Mean Sq", "F value", "Pr(>F)")
    res = c(n-p, SSE, MSE, " ", " ")
    summary = as.data.frame(rbind(table, res))
    rownames(summary) = c(covariates,"Residuals")
  }
  if(type == "Partial"){
    ## SSE under H0
    SSE0 = rep(0, p)
    for (i in 1:p) {
      H0 = X[,-i] %*% solve(t(X[,-i]) %*% X[,-i]) %*% t(X[,-i])
      SSE0[i] = as.numeric(t(Y) %*% Y - t(Y) %*% H0 %*% Y)
    }
    SS = SSE0 - rep(SSE, length(SSE0))
    fstat = SS/rep(MSE,length(SS))
    p_val = rep(0, length(fstat))
    for (i in 1:length(fstat)) {
      p_val[i] = pf(fstat[i], 1, n-p, lower.tail = FALSE)
    }
    table = cbind(SS, rep(1,p), fstat, p_val)
    colnames(table) = c("Sum Sq", "Df", "F value", "Pr(>F)")
    res = c(SSE, n-p, " ", " ")
    summary = as.data.frame(rbind(table, res))
    rownames(summary) = c("(Intercept)",covariates, "Residuals")
  }
  return(summary)
}

