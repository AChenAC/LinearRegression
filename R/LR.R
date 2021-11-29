#' Linear Regression
#'
#' LR is used to fit linear model. It yields the same results as lm( ), summary(lm( )), and confint( ) functions.
#'
#' @param formula An object of class "formula": a symbolic description of the model to be fitted. A typical model has the form outcome ~ covariates
#' where outcome is the numeric response vector (which people usually denote as Y in statistical formula) and covariates are predictor of response.
#'
#' @param data A data frame (or object coercible by as.data.frame to a data frame) containing the variables in the model.
#'
#' @param include.intercept If the model should fit with intercept, include.intercept = TRUE; if model should fit without intercept,
#' then include.intercept = FALSE. The default setting for include.intercept is TRUE.
#'
#' @param to.predict The default argument is set to NULL. If wants to use the current model for prediction, enter a n by p matrix (or object coercible by as.matrix to a matrix) to obtain predicted values.
#' n is the number of prediction desired, p is the number of covariates included in the model.
#'
#' @return LR does not explicitly return anything unless extract the value with $ followed with the name of desired output.
#' The returned output is a list containing at least the following components:
#' \describe{
#'   \item{coefficients}{a named vector of coefficients}
#'   \item{residuals}{the residuals (i.e. response minus fitted values)}
#'   \item{fitted.values}{the predicted value}
#'   \item{sigma}{the residual standard error}
#'   \item{df}{degrees of freedom}
#'   \item{coeff_summary}{mimic the result from using summary(lm()) which includes estimates of beta coefficients, standard error, t value, and p-value}
#'   \item{R_squared}{the proportion of the variance for a dependent variable that's explained by independent variable(s)}
#'   \item{adj_R_squared}{a penalized version of R_squared}
#'   \item{CI}{95\% confidence interval of estimates (i.e. coefficients)}
#'   \item{fstatistic}{Give the overall F statistic and its corresponding degrees of freedom of numerator and denominator}
#'   \item{p_value_F_test}{p-value for overall F test}
#'   \item{predicted}{Give the predicted value using the current fitted model}
#' }
#'
#' @examples
#'
#' LR(mpg ~ cyl + wt, mtcars)$coefficients ## Obtain beta coefficient estimates
#' LR(mpg ~ cyl + wt + disp, mtcars)$coeff_summary ## Obtain summary of beta coefficients
#' LR(mpg ~ cyl + wt, mtcars)$sigma ## Obtain residual standard error
#' LR(mpg ~ cyl + wt + qsec, mtcars)$CI ## Obtain 95% confidence interval
#' LR(mpg ~ cyl + wt, mtcars, include.intercept = FALSE) ## omitting intercept
#' LR(mpg ~ cyl + wt + qsec + disp, mtcars, include.intercept = FALSE)$df ## Extract degrees of freedom when fitting a model without an intercept
#' LR(cyl~mpg+wt, mtcars, to.predict = matrix(c(mean(mtcars$mpg), mean(mtcars$wt)), 1, 2))$predicted
#'
#' @export
#'
#'
LR = function(formula, data, include.intercept = TRUE, to.predict = NULL){
  CompleteData = na.omit(data)
  n = nrow(CompleteData)
  if(include.intercept==TRUE){
    p = length(labels(terms(formula))) + 1
    X = matrix(c(rep(1,n), as.matrix(CompleteData[labels(terms(formula))])), n, p)
  } else {
    p = length(labels(terms(formula)))
    X = as.matrix(CompleteData[labels(terms(formula))], n, p)
  }
  Y = as.matrix(CompleteData[as.character(formula[[2]])], n, 1)
  beta = solve(t(X) %*% X) %*% t(X) %*% Y ## Coefficients
  if(p == (length(labels(terms(formula))) + 1)){
    rownames(beta) = c("intercept", labels(terms(formula)))
    colnames(beta) = "Coefficients"
  } else {
    rownames(beta) = labels(terms(formula))
    colnames(beta) = "Coefficients"
  }
  yhat = X %*% beta ## Fitted Values
  ei = Y - yhat ## Residuals
  sigma = sqrt((t(ei) %*% ei) / (n-p)) ## sigma
  df = n-p
  var_cov_beta = as.vector((t(ei) %*% ei) / (n-p)) * solve(t(X) %*% X)
  std.error = sqrt(diag(var_cov_beta))
  t_value = beta / std.error
  p_value = rep(0, length(beta))
  for (i in 1:length(beta)) {
    p_value[i] = 2*pt(q=abs(t_value[i]), df=df, lower.tail=FALSE)
  }
  Coeff_summary = cbind(beta, std.error, t_value, p_value)
  colnames(Coeff_summary) = c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  SSY = sum((Y-mean(Y))^2)
  SSR = sum((yhat-mean(Y))^2)
  R2 = SSR/SSY
  SSE = t(ei) %*% ei
  R2_adj = 1 - (SSE/(n-p))/(SSY/(n-1))
  LB = beta - qt(0.05/2, df, lower.tail = FALSE)*std.error
  UB = beta + qt(0.05/2, df, lower.tail = FALSE)*std.error
  CI_95 = cbind(LB, UB)
  colnames(CI_95) = c("2.5%", "97.5%")
  numdf = p - 1
  MSR = SSR / numdf
  dendf = n - p
  MSE = SSE / dendf
  F_stat = MSR / MSE
  F_stat_df = c(F_stat, numdf, dendf)
  names(F_stat_df) = c("F statistic", "numdf", "dendf")
  p_val_F = pf(F_stat, numdf, dendf, lower.tail = FALSE)
  if(!is.null(to.predict)){
    if(include.intercept == TRUE){
      predicted = cbind(1, to.predict) %*% beta
      colnames(predicted) = "Predicted Values"
    } else {
      predicted = to.predict %*% beta
      colnames(predicted) = "Predicted Values"
    }
  } else {
    predicted = NULL
  }
  results = list(coefficients = beta,
                 residuals = ei,
                 fitted.values = yhat,
                 sigma = as.vector(sigma),
                 df = df,
                 coeff_summary = Coeff_summary,
                 R_squared = as.vector(R2),
                 adj_R_squared = as.vector(R2_adj),
                 CI = CI_95,
                 fstatistic = F_stat_df,
                 p_value_f_test = as.vector(p_val_F),
                 predicted = predicted)
  return(invisible(results))
}

