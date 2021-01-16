#' Output results of a cox model to Excel (CSV file)
#' @export
#'
#' @param filename file to save results to
#' @param model Cox model from coxph
#'
#' @seealso \code{\link{write_uva_to_csv}}
#'
write_cox_to_csv <- function(filename, model) {
    fd <- my_safe_open(filename);
    if ( fd == -1 ) {
        return(0);
    }
    s <- summary(model);
    print("here 1")
    n <- dim(s$coefficients)[1];
    varnames <- rownames(s$coefficients);
    cat("Results of multi-variate Cox model\n", file=fd);
    cat(paste(as.character(s$call), collapse=" "), file=fd);
    cat("\nVariable, HR, Conf Interval, p-value\n", file=fd);
    print("here 2")
    for (a in 1:n) {
        str<- sprintf("%s, %.2f, %.2f -- %.2f, %f\n", varnames[a],
                      s$coefficients[a,2],
                      s$conf.int[a,3],
                      s$conf.int[a,4],
                      s$coefficients[a,5]);
        cat(str, file=fd);
    }
    print("here 3")
    cat("\n\n", file=fd);
    str <- sprintf("Log rank test: %f  (p-value: %f)\n",s$logtest[1], s$logtest[3]);
    cat(str, file=fd);
    str <- sprintf("N = %d\n",s$n);
    cat(str, file=fd);
    str <- sprintf("Num Events = %d\n",s$nevent);
    cat(str, file=fd);
    close(fd);
    #shell.exec2(filename);
    system(paste("open", filename))
}

# write results of a UVA analysis  (deprecated -- not currently using?)
#
#       Arguments: filename
#                  model:     Cox model from coxph
#       Return:    Nothing

write_uva_to_csv <- function(filename, uva) {
    fd <- my_safe_open(filename);
    if ( fd == -1 ) {
        return(0);
    }

    cat("Results of Uni-Variate Analysis\n\n", file=fd);
    cat("Variable Name, HR, Conf Int, Cox P-value, Log Rank P-Value\n", file=fd)
    n<-length(uva);
    for (a in 1:n) {
        str<- sprintf("%s,%.2f, %.2f -- %.2f, %f, %f\n", uva[[a]]$name,
                      uva[[a]]$conf[1],
                      uva[[a]]$conf[3],
                      uva[[a]]$conf[4],
                      uva[[a]]$co_pv[5],
                      uva[[a]]$logrank);
        cat(str, file=fd);
    }
    close(fd);
    #wrishell.exec(filename);
    system(paste("open", filename))
}

# check if file exists, if exists prompt user whether to over write or not
# otherwise fails
# utility function
my_safe_open <- function(filename) {
    if ( file.exists(filename) ) {
        cat(paste("File exists: ", filename, "\n"));
        cat("Overwrite? y/n\n");
        res <- scan(what=character(), nmax=1)
        if ( res != 'y' ) {
            print("Quiting");
            return(-1);
        }
    }
    fd <- file(filename, "w");
    return(fd);
}


