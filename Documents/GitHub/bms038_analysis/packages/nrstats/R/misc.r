# MISC CODE To look at

#' write data frame to temporary csv and quickly open in excel
#' @export
#'
#' @param dat Data frame to write to a temporary csv to open up in excel
#'
vexcel <- function(dat) {
  #TDIR <- "/Users/nadeemriaz/tmp";
  tfile <- tempfile()
  fl <- paste(tfile, ".csv",sep="");
  write.csv(dat, file=fl);
  system(paste("open", fl))
  cat(sprintf("Temporary file = %s\n", fl))
}

#' Convienence function to concatenate in shorter format
#' @export
cc <- function(...) {
		str1 <- paste(...,sep="");
		return(str1);
}

#' mac paste
#' @export
macpaste <- function() {
	tmp <- -read.table(pipe("pbpaste"), sep="\t")
	return(tmp)
}

#' Take a list of vectors and convert it to a matrix
#' @export
#'
#' @param lst Variable that is a list of vectors
#'
#' @return a matrix with same data
list2matrix <- function(lst) {
  mat <- do.call(rbind, lst)
  return(mat)
}


# sample elastic net code
#myglmnet_crap <- function() {
#	      #glmnet stuff
#	      dt<-matrix(NA, length(psa),7);
#	      dt[,1]<-psa;
#	      dt[,2]<-gleason_grouped;
#	      dt[,3]<-t_stage;
#	      dt[,4]<-adt;
#	      dt[,5]<-ece_marker
#	      dt[,6]<-mr_stage;
#	      dt[,7]<-diam_marker;

#	      cv.fit <- cv.glmnet(dt, Surv(event_time, event), family = "cox", maxit = 1000)
#	      fit <- glmnet(dt, Surv(event_time, event), family = "cox", maxit = 1000)
#}

