library(survival)
#library(Design)
library('rms');
library('MASS')
library('ROCR')
library('cmprsk');


#' @import survival
#' @import rms
#' @import MASS
#' @import cmprsk



#' @title Take a continous variable, and cut it into multiple points, by cut points
#' @export
#'
#' @param xvar    Variable to divide
#' @param cp:     Cut points to divide variable by, firs group is < cp[1], 2nd group is >=cp[1], <cp[2], etc.
#'
#' @return	ordinal scale 0,1,2,3,4....
#'
group_var <- function(xvar, cp) {
  n <- length(cp);
  tmp <- rep(NA, length(xvar));
  tmp[xvar < cp[1]] <- 0;
  for (a in 2:n) {
    tmp[xvar >= cp[a-1] & xvar < cp[a]] <- (a-1);
  }
  tmp[xvar >= cp[n]] <- n;
  return(tmp);
}

# Create ROC Curve (assume binary output)
create_roc <- function(output, predictor) {
  m<-glm(output~predictor,family=binomial("logit") )
  predictions <- predict.glm(m)

  pred <- prediction(predictions, output)
  perf <- performance(pred, measure = "tpr", x.measure = "fpr")
  plot(perf, col=rainbow(10))

  z<-performance(pred, measure = "auc")
  print(z)
  abline(a=0,b=1)
}

# run_all_univariate: --- need to update this ot make it exportable
#	outcome is a Survival object i.e. Surv(event_time, event)
#	predictors a list of predictors to run i.e. c('predictor1', 'predictor2', ...)
#
#  Output:   Run cox univariate for all predictors
run_all_univariate <- function(outcome, predictors) {
  n <- length(predictors);
  res <- list();
  for (a in 1:n) {

    # Display raw output from cox model and
    formula_str <- paste(outcome, "~", predictors[a]);
    formula_str <- paste("tres1234<-coxph(", formula_str, ")");
    print(formula_str);
    eval(parse(text=formula_str))
    print(tres1234)
    print(summary(tres1234))

    # Create KM model to test by log-rank
    formula_str <- paste(outcome, "~", predictors[a]);
    formula_str <- paste("tkm1234<-survdiff(", formula_str, ")");
    eval(parse(text=formula_str))
    #print(tkm1234);

    # Store key data for this variable in a list
    tmp <- paste("mod",a,sep="");
    lr <- 1-pchisq(tkm1234$chisq, length(tkm1234$n)-1);
    wld<-summary(tres1234)$wald
    cf<-	summary(tres1234)$conf.int
    cpv <- summary(tres1234)$coefficients
    vals <- list(name=predictors[a],wald=wld, conf=cf, co_pv=cpv, logrank=lr);

    # Append created list, to list of all models
    junk<-list()
    junk[[tmp]] <- vals;
    res <- c(junk,res)
  }

  # Display abridge summary of univariate results
  # Variable Name
  # Log Rank
  # Confidence interval
  # Wald Stats for Cox model
  cat("\n\n########################################\n")
  for (a in 1:n) {
    tmp <- paste("mod",a,sep="");
    l <- res[[tmp]];
    cat(sprintf("### %s ###", l[['name']])); cat("\n");
    cat("Log rank (not valid for contious variables): ");
    cat(l[['logrank']]); cat("\n");
    cat("Confidence Interval for HR (cox)\n");
    print(l[['conf']]) ;cat("\n");
    print(l[['co_pv']]) ;cat("\n");
    cat("Wald Statistic\n");
    print(l[['wald']]); cat("\n\n");
  }

  return(res);
}




# Create a graph with DF betas with each person removed for each variable
cox_dfbeta <- function(model) {
  n <- length(model$coefficients);
  if ( n <= 4 ) {
    tmp <- c(2,2);
  } else if ( n <= 9 ) {
    tmp <- c(3,3);
  } else {
    tmp <- c(4,4);
  }
  par(mfrow=tmp);
  dfbeta <- residuals(model, type='dfbeta');
  for (a in 1:n) {
    plot(dfbeta[,a], ylab=names(coef(model)[a]))
    abline(h=0, lty=2);
  }
}


