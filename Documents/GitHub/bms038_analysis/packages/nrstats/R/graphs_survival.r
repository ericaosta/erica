
######################### SIMPLE KAPLAN MEIER CURVES #########################

#' Create a Kaplan Meier Survival Curve, print table of results
#' @export
#'
#' @param event_time   Time to Event
#' @param event        Event occured or not (0 or 1)
#' @param max          Maximum time to display on graph (default = 5)
#' @param tit       Title of graph (default 'Suvival')
#' @param xlabv X-axis label (default: 'Years')
#' @param NNT Display number at risk (boolean)
#'
#' @return Model form survfit
#'
#' @seealso \code{\link{qkm2}}, \code{\link{ggkm}}, \code{\link{qkm_var_med}}, \code{\link{qkm_var_anal}}
#'
qkm <- function(event_time, event, max=5, tit="Survival", xlabv="Years", NNT=FALSE) {
  so <- Surv(event_time, event)~1
  mfit <- survfit(so)
  print(mfit)
  print(summary(mfit, seq(0,10,1)))
  tmp_ttl <- "KM";

  if ( NNT ) {
    tfit <- npsurv(so);
    survplot(tfit, conf="none",n.risk=TRUE, col=c(1,2,3), xlab=xlabv, label.curves=FALSE, xlim=c(0,max), time.inc=1)
  } else {
    plot(mfit, xlim=c(0,max), conf.int=FALSE, xlab=xlabv, ylab="Fraction")
  }

  title(tit)

  medtime <- summary(mfit)$table[5]
  str <- sprintf("median = %.2f", medtime)
  print(str);
  stext(str, 3, pos=0)
  return(mfit)
}

#' Survival Curves by a Variable. displays p-values by log-rank & cox model
#' @export
#'
#' @param event_time   Time to Event
#' @param event        Event occured or not (0 or 1)
#' @param Max          Maximum time to display on graph
#' @param Var          Variable to split Survival Curves by (should be categorical or ordinal)
#' @param lgnd         Legend text (optional)
#' @param tit         Title of graph  (default = 'Survival')
#'
#' @return P-value from log-rank test
#'
#' @seealso \code{\link{qkm}}, \code{\link{ggkm}}, \code{\link{qkm_var_med}}, \code{\link{qkm_var_anal}}
#'
qkm2 <- function(event_time, event, max, var, lgnd, tit="Survival") {
  lbls = c("1","2","3","4","5","6","7","8","9");
  so <- Surv(event_time, event)~var
  colors<-seq(1,9,1);
  mfit<-survfit(so)
  #print(mfit)
  #print(survdiff(so))
  plot(mfit, xlim=c(0,max), conf.int=FALSE, xlab="Years", ylab="Fraction", col=colors)
  if ( missing(lgnd) ) {
    lnum = length(unique(na.omit(var)));
    ltxt = lbls[1:lnum];
    legend("bottomright", ltxt, lty=1, col=colors)
  } else {
    legend("bottomright", lgnd, lty=1, col=colors)

  }
  m <- coxph(so)
  #print(summary(m))
  title(tit)

  # Get annotations to add to graph for survival curve
  annot <- km_get_pval(so);
  if ( is.null(annot) ) { return (NA ) }
  stext(annot$str1, pos=0, side=3);
  stext(annot$str2, pos=1,side=3);

  return(annot$pv);
}


#' GGPlot based Survival Curves by a Variable. Report p-values by log-rank & Cox Model
#' @export
#'
#' @param event_time   Time to Event
#' @param event        Event occured or not (0 or 1)
#' @param max          Maximum time to display on graph
#' @param var          Variable to split Survival Curves by (should be categorical or ordinal)
#' @param lgnd         Legend text (optional)
#' @param tit          Title of graph  (default = "Survival")
#'
#' @return p-value from log-rank test
#'
#' @seealso \code{\link{qkm}}, \code{\link{qkm2}}, \code{\link{qkm_var_med}}, \code{\link{qkm_var_anal}}
#'
ggkm<- function(event_time, event, max, var, lgnd, tit="Survival") {
  so <- Surv(event_time, event)~var
  colors<-seq(1,9,1);
  mfit<-survfit(so);

  p <- ggsurv(mfit)
  p <- p+myggplotdefaults();
  p <- p+ggtitle(tit);


  # set max on x axis; make x-axis start at 0
  p<-p+scale_x_continuous(limits=c(0,max), expand=c(0,0))+xlab("Years")
  # Change legend (first remove old legend, then add new legend)
  p <- p + guides(linetype = FALSE)
  grps <- unique(var)
  p <- p + scale_color_discrete(name="",breaks=grps,labels=lgnd);

  # Get annotations to add to graph for survival curve
  annot <- km_get_pval(so);
  if ( is.null(annot) ) { reutrn (NA ) }
  fstr <- sprintf("\n\n%s\n%s\n", annot$str1, annot$str2)

  p2 <- arrangeGrob(p,
                    sub = textGrob(fstr, x = 0, hjust = -0.1, vjust=0.2, gp = gpar(fontface = "italic", fontsize = 14)),
                    heights=c(0.9, 0.1)
  )
  grid.draw(p2)

  return(p2);
}




######################### FANCIER KAPLAN MEIER ANALYSIS #########################

#' Display Survival curve of quantitiative variable split @ median
#' @export
#'
#' @param event_time   Time to Event
#' @param event        Event occured or not (0 or 1)
#' @param max          Maximum time to display on graph
#' @param var          Variable to split Survival Curves by (should be continous)
#' @param tit          title of graph  (optional)
#'
#' @return P-value from log-rank test
#'
#' @seealso \code{\link{qkm}}, \code{\link{qkm2}}, \code{\link{ggkm}}, \code{\link{qkm_var_anal}}
#'

qkm_var_med <- function(event_time, event, max, var, tit) {
  # Create variable that splits value at median and plot survival curve
  m <- median(var, na.rm=TRUE);
  m_val <- var >= m;
  pv<-qkm2(event_time, event, max, m_val, c("<median", ">=median"), tit);
  return(pv)
}


#' Display Survival curve of quantitiative variable split by quantiles
#' @export
#'
#' @param event_time:   Time to Event
#' @param event        Event occured or not (0 or 1)
#' @param max          Maximum time to display on graph
#' @param var          Variable to split Survival Curves by (should be continous)
#' @param tit          title of graph  (optional)
#'
#' @return P-value from log-rank test
#'
#' @seealso \code{\link{qkm}}, \code{\link{qkm2}}, \code{\link{ggkm}}, \code{\link{qkm_var_anal}}
#'
qkm_var_quant <- function(event_time, event, max, var, tit) {
  # Create variable that splits value at median and plot survival curve
  q1 <- quantile(var, 0.25, na.rm=TRUE);
  m <- median(var, na.rm=TRUE);
  q2 <- quantile(var, 0.75, na.rm=TRUE);
  quants <- group_var(var, c(q1,m, q2));

  qkm2(event_time, event, max, quants, c("q1", "q2", "q3","q4"), tit);
}


#' Display 4 panels with analysis of a quantiative variable and survival
#' @export
#'
#' @param event_time   Time to Event
#' @param event        Event occured or not (0 or 1)
#' @param max          Maximum time to display on graph
#' @param var          Variable to split Survival Curves by (should be continous)
#' @param tit          title of graph  (optional)
#'
#' @return P-value from log-rank test
#'
#' @seealso \code{\link{qkm}}, \code{\link{qkm2}}, \code{\link{ggkm}}, \code{\link{qkm_var_med}}
#'
qkm_var_anal <- function(event_time, event, max, var, tit) {
  if ( ! is.numeric(var) ) {
    stop("qkm_var_anal: var is not numeric")
  }
  #png(file, height=1500, width=1500);
  par(mfrow=c(2,2), cex=2.0)
  histwStats(var, tit);
  print("#### Report for median ####")
  qkm_var_med(event_time, event, max, var, "Split at Median");
  print("#### Report for quantiles ####")
  qkm_var_quant(event_time, event, max ,var, "Split by Quartiles");
  print("#### Report for cut points ####")
  qkm_cut_point(event_time, event, var, "Different Cutpoints");
  #dev.off();
}

######################### MISC GRAPHS  #########################

#' Create a forest plot of hazard ratios
#' @export
#'
#' @param vars Names of variables
#' @param HR Hazard ratio point estimate for each variable (HR)
#' @param lowerCI Lower CI estiate for each HR
#' @param upperCI Upper CI estimate for each HR
#'
#'
forestPlot <- function(vars, HRs, lowerCI, upperCI) {
  d<- data.frame(x=vars, y=HRs, ylo=lowerCI, yhi=upperCI);
  # X-axis labels
  xlb <- c(0.1,0.25,0.5,0.75,1,2,3,4,10);

  p <- ggplot(d, aes(x=x, y=y, ymin=ylo, ymax=yhi))+geom_pointrange()+coord_flip()
  p<- p+ scale_y_log10(labels=xlb, breaks=xlb)+geom_hline(aes(x=1), lty=2)
  p <- p+xlab("")+ylab("Hazard Ratio")
  p<- p + myggplotdefaults()+  theme(axis.line.y=element_blank())

  return(p)
}




######################### UTILITY FUNCTIONS  #########################

# qkm_cut_point:  Display p-values for different cut points of continous variables
qkm_cut_point <- function(event_time, event, var, tit) {
  sorted_var <- unique(sort(var));
  mn <- sorted_var[2];   # need to pick second to min value, not minimum
  n <- length(sorted_var)
  mx <- sorted_var[n-1];  # need to pick second larged val, not max.

  vals <- sorted_var[2:(n-1)];
  n <- length(vals);
  res <- rep(NA, n);

  for (x in 1:n) {
    tmp <- var >= vals[x];
    so <- Surv(event_time, event)~tmp
    m <- tryCatch({survdiff(so)},
                  error = function(e) {
                    warning("Error computing survival difference for group");
                  });
    if ( is.list(m) == FALSE ) {
      next;
    }
    pv <- 1- pchisq(m$chisq, 1);
    res[x] <- -1 * log10(pv);
  }
  plot(vals, res, xlab="Cut points", ylab="-log(P-Value)", main=tit, type="b");
  abline(h= -1*log10(0.05), col="red")
  m <- coxph(Surv(event_time, event)~var)
  t <- summary(m);
  str <- sprintf("HR (Continous) = %.2f p-val = %.2e", exp(m$coefficients[1]), t$logtest[3]);
  stext(str, pos=1, side=3);
  print(summary(m));
}


# Utility funciton to get strings to annotate graphs
# returns pvalue, and a couple of string annotations for graphs (ggkm, qkm2)
# input is a survival function object
km_get_pval <- function(so) {
  tmp <- tryCatch({survdiff(so)},
                  error = function(e) {
                    warning("Error computing survival difference for group");
                  }
  );

  # some problem computing survival difference
  if ( is.list(tmp) == FALSE ) {
    return(NULL);
  }
  # add log-rank p-value to top of graph
  df <- length(tmp$obs)-1;
  pv <- 1 - pchisq(tmp$chisq, df);
  if ( pv < 0.01) {
    str <- sprintf("log-rank p=%.2e", pv);
  } else {
    str <- sprintf("log-rank p=%.2f", pv);
  }


  # add N in each group to bottom of graph
  str2 <- "";
  for (j in 1:length(tmp$n)) {
    str2 <- paste(str2, "N",j,"= ",tmp$n[j], " ",sep="");
  }

  annot <- list();
  annot$str1 <- str;
  annot$str2 <- str2
  annot$pv <- pv;
  return(annot);
}
