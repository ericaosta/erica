library(ggplot2);
library(grid);
library(gridExtra);
library(R.utils);
library(GGally)

#' @import ggplot2
#' @import grid
#' @import gridExtra
#' @import R.utils
#' @import GGally
#'

######################### GRAPHS TO COMPARE CONTINOUS VARIABLES BETWEEN GROUPS  #########################

#' @title Test for differences in a variable between two groups
#' @export
#'
#' @param dat Data matrix (contains Group Column; Val Column)
#' @param groupCol Name of column in dat, that contains information on group id
#' @param valCol Column with continous values to compare between two groups
#' @param groupLabels Labels to use for graphing
#' @param tit Title for graph (default "")
#' @param colCol Column to use for coloring data points (optinal)
#' @param swarm display indiivudal points
#' @param devoff TBD
#' @param dodraw TBD
#'
#' @return conglomerate graph object (from arrangeGrob)
#'
#' @seealso \code{\link{myggboxplot}},  \code{\link{myswarmplot}}
#'
#' @description Compares two groups for differences in a variable using a T-test and Wilcox test
#' @description Display corresponding graph with multiple display options
#'
tTestandGraph <- function(dat, groupCol, groups, 	valCol, groupLabels,
							tit ="", colCol="", swarm = FALSE, devoff=TRUE, dodraw=TRUE
							) {
	i1 <- which(dat[,groupCol] == groups[1]);
	i2 <- which(dat[,groupCol] == groups[2]);

	# Create new matrix that has only rows with groups
	# that we are comparing
	cdat <- rbind(dat[i1,], dat[i2,]);


  if ( swarm == FALSE ) {
	   p <- myggboxplot(cdat, groupCol, groups, valCol, groupLabels, tit, colCol);
	} else {
	   p <- myswarmplot(dat, groupCol, groups, valCol, groupLabels, tit, colCol);
	}

	print("made it here");
	# Compute t-test, wilcox test, and run some stats to diplay
	val1 <- dat[i1, valCol];
	val2 <- dat[i2, valCol];
	res<-t.test(val1, val2 )
	resw <- wilcox.test(val1, val2);
	n1 <- length(na.omit(dat[i1, valCol]));
	n2 <- length(na.omit(dat[i2, valCol]));
	m1 <- mean(dat[i1, valCol], na.rm=TRUE);
	m2 <- mean(dat[i2, valCol], na.rm=TRUE)
	print(sprintf("p-value = %.2e", res$p.value));
	print(sprintf("wilcox p-value = %.2e", resw["p.value"]));
	print(sprintf("n1 = %d, n2=%d",n1,n2));
	pv <- sprintf("t-test = %.2e wilcox = %.2e", res$p.value, resw["p.value"]);
	pv2 <- sprintf("Avg %s = %.2f (n=%d) ;  Avg %s = %.2f (n=%d)", groupLabels[1], m1,n1, groupLabels[2], m2, n2);
	pv <- sprintf("\n%s\n%s", pv, pv2);
	print(res);

	# adding an annotation below the axis, is a pain in the ass.
	# this is from http://bigdata-analyst.com/best-way-to-add-a-footnote-to-a-plot-created-with-ggplot2.html
	p2 <- arrangeGrob(p, sub = textGrob(pv, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontface = "italic", fontsize = 12)),
					 heights=c(0.9, 0.1)
					  )

	# old method of annotating
	#maxVal <- max(cdat[,valCol], na.rm=TRUE);
	#p <- p + annotate("text", label=pv, x=1, y=maxVal-2)
	if ( devoff ) {
	  dev.off()
	}
	if ( dodraw ) {
	  grid.draw(p2);
	}
	return(p2);

}

#' Create a box plot from a data frame
#' @export
#'
#' @param dat Data matrix (contains Group Column; Val Column)
#' @param groupCol Name of column in dat, that contains information on group id
#' @param valCol Column with continous values to compare between two groups
#' @param groupLabels Labels to use for graphing
#' @param tit Title for graph (default "")
#' @param colCol Column to use for coloring data points (optinal)
#' @param violen display violen plot instead of boxplots
#'
#' @return ggplot graph
#'
#' @seealso \code{\link{tTestandGraph}}, \code{\link{myswarmplot}}
#'
myggboxplot <- function(dat, groupCol, groups, valCol, groupLabels,
							tit ="", 	colCol = "",	violin = 0    ) {
  checkPlotError(dat, groupCol, valCol, colCol);
	cdat <- dat;
	cdat[,"groupCol"] <- factor(cdat[,groupCol], ordered=TRUE, levels=groups)
	cdat[,"valCol"] <- cdat[,valCol]
	if ( nchar(colCol) > 0 ) {
		cdat[,"colCol"] <- cdat[,colCol];
	}

	tmpN <- length(unique(sort(cdat[,"groupCol"])));
	cat(sprintf("Groups = %d", tmpN));
	cat(sprintf("Levels = %s\n", paste(levels(cdat$groupCol), collapse=" ")))

	# Create box plot
	p <- ggplot(cdat, aes(groupCol, valCol))
	xax <- paste("\n", groupCol, sep="");
	yax <- paste(valCol, "\n", sep="");
	p <- p + xlab(xax) + ylab(yax) +ggtitle(tit)
	p <- p + scale_x_discrete(labels=groupLabels)

	if ( violin == 1) {
	  p <- p +geom_violin()
	} else {
	  p<- p + geom_boxplot(outlier.colour="transparent")
	}

	if ( nchar(colCol) > 0 ) {
		p<- p + geom_jitter(aes(colour=factor(colCol)), position=position_jitter(width=.1, height=0));
	} else{
		p<- p + geom_jitter(position=position_jitter(width=.1, height=0));
	}
	#p <- p + geom_jitter(position=position_jitterdodge());

	p <- p + myggplotdefaults()
	return(p);
}


#' Create a swarm plot to compare different groups
#' @export
#'
#' @param dat Data matrix (contains Group Column; Val Column)
#' @param groupCol Name of column in dat, that contains information on group id
#' @param valCol Column with continous values to compare between two groups
#' @param groupLabels Labels to use for graphing
#' @param tit Title for graph (default "")
#' @param colCol Column to use for coloring data points (optinal)
#'
#' @return ggplot graph
#'
#' @seealso \code{\link{myggboxplot}},  \code{\link{tTestandGraph}}
#'
myswarmplot <- function(dat, groupCol, 	groups, valCol, groupLabels, tit ="", colCol = "") {
  # Make sure groupCol, valCol, and colCol exist in data frame
  checkPlotError(dat, groupCol, valCol, colCol);

	i1 <- which(dat[,groupCol] == groups[1]);
	i2 <- which(dat[,groupCol] == groups[2]);

	m1 <- mean(dat[i1, valCol], na.rm=TRUE);
	n1<- length(na.omit(dat[i1, valCol]));
	e1 <- sd(dat[i1, valCol], na.rm=TRUE)/sqrt(n1);
	m2 <- mean(dat[i2, valCol], na.rm=TRUE);
	n2 <- length(na.omit(dat[i2, valCol]));
	e2 <- sd(dat[i2, valCol], na.rm=TRUE)/sqrt(n2);

	# Create new matrix that has only rows with groups
	# that we are comparing
	if ( nchar(colCol) > 0 ) {
	  cdat <- rbind(dat[i1,c(groupCol, valCol, colCol)], dat[i2,c(groupCol, valCol, colCol)]);
	} else {
	  cdat <- rbind(dat[i1,c(groupCol, valCol)], dat[i2,c(groupCol, valCol)]);
	}
	cdat <- na.omit(cdat)

	# Going to change group col to numeric to make life simpler
	cdat[,"groupCol"] <- c(rep(1,n1), rep(2,n2))
	cdat[,"valCol"] <- cdat[,valCol]

	cat(sprintf("Group 1 = %s (n=%d) Group 2 = %s (n=%d)\n", groupLabels[1], n1, groupLabels[2], n2))

	# load up beeswarm
	library(beeswarm);
	new_pts <- beeswarm(valCol~groupCol, dat=cdat, do.plot=FALSE);
	ndat <- data.frame(x=new_pts[,"x"], y=new_pts[,"y"])
	if ( nchar(colCol) > 0 ) {
		ndat[,"colCol"] <- cdat[,colCol];
	}
	i <- which(new_pts[,"x.orig"] == 2);
	ndat[i,"x"]<-ndat[i,"x"]-0.30
	ctr2 <- 1.7;


	# Create  plot
	p <- ggplot(ndat, aes(x=x, y=y))+geom_point(size=2.5);
	print("here");


	if ( nchar(colCol) > 0 ) {
		p<- p + geom_point(aes(colour=factor(colCol)), size=2.5);
	} else{
		p<- p + geom_point(size=2.5);
	}



	xax <- paste("\n", groupCol, sep="");
	yax <- paste(valCol, "\n", sep="");
	p <- p + xlab(xax) + ylab(yax) +ggtitle(tit)
	p <- p + scale_x_continuous(breaks=c(1,ctr2), labels=groupLabels, lim=c(0.65,ctr2+0.4))

	# Mean lines
	p<-p+geom_segment(x=0.75, xend=1.25, y=m1, yend=m1, linetype="dashed");
	p<-p+geom_segment(x=ctr2-0.25, xend=ctr2+0.25, y=m2, yend=m2, linetype="dashed");

	# error top lines
	p<-p+geom_segment(x=0.9, xend=1.1, y=m1+e1, yend=m1+e1);
	p<-p+geom_segment(x=ctr2-0.1, xend=ctr2+0.1, y=m2+e2, yend=m2+e2);

	# error bottom lines
	p<-p+geom_segment(x=0.9, xend=1.1, y=m1-e1, yend=m1-e1);
	p<-p+geom_segment(x=ctr2-0.1, xend=ctr2+0.1, y=m2-e2, yend=m2-e2);

	# error cross bar
	p<-p+geom_segment(x=1, xend=1, y=m1-e1, yend=m1+e1);
	p<-p+geom_segment(x=ctr2, xend=ctr2, y=m2-e2, yend=m2+e2);

	print("here");

	p <- p + myggplotdefaults()
	return(p);
}

# input error check for myggboxplot and myswarmplot
checkPlotError <- function(dat, groupCol, valCol, colCol) {
  if ( !(groupCol %in% colnames(dat)) ) {
    stop(sprintf("Group column is not in data frame: %s", groupCol))
  }

  if ( !(valCol %in% colnames(dat)) ) {
    stop(sprintf("val column is not in data frame: %s", valCol))
  }

  if ( nchar(colCol) > 0 && !(colCol %in% colnames(dat)) ) {
    stop(sprintf("col column is not in data frame: %s", colCol))
  }
}

######################### UTILITY GRAPHS  #########################

#' Quick scatter plot with linear regression line, and annotation at bottom of graph
#' @export
qscat <- function(x, y, xax="x", yax="y", main="", bw=0, displayStat=1, dodraw=TRUE) {
  xax <- paste("\n",xax,sep="");
  yax <- paste(yax,"\n",sep="");
  p <- qplot(x,y) + xlab(xax) + ylab(yax) + ggtitle(main)

  # Make it Black and white
  if (bw == 1) {
    p <- p + myggplotdefaults()
    p <- p + stat_smooth(method="lm", col="black")
  } else {
    p <- p  + stat_smooth(method="lm")
  }

  # Just return back ggplot object
  if (displayStat == 0 ) {
    return(p)
  }

  # compute complete pairs
  tdat <- cbind(y,x)
  tdat<-na.omit(tdat)
  n <- dim(tdat)[1]

  mod <- lm(y~x)
  mod2 <- cor.test(x,y, use="complete.obs")
  str1 <- sprintf("r = %.2f (p = %.2e; n=%d)\ny = %.2e + %.2e * x", mod2$estimate, mod2$p.value, n, mod$coefficients[1], mod$coefficients[2])
  p2 <- arrangeGrob(p,
                    sub = textGrob(str1, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontface = "italic", fontsize = 14)),
                    heights=c(0.9, 0.1)
  )
  if ( dodraw ) {
    grid.draw(p2)
  }
  return(p2);
}

#' Scatter with density plots
#' @export
qscat_density <- function(x, y, xax, yax, title="", bw=0) {
  p <- qplot(x, y) + xlab(xax) + ylab(yax)
  if (bw == 1) {
    p <- p + myggplotdefaults()
  }
  p2<-gghist(x)+xlab("")+ylab("")
  p3<-gghist(y)+xlab("")+ylab("")+coord_flip()
  p4 <- textGrob(title)

  g1 <- ggplotGrob(p)
  g2 <- ggplotGrob(p2)
  g3 <- ggplotGrob(p3)
  # line up widths
  maxWidth = grid::unit.pmax(g1$widths[2:5], g2$widths[2:5])
  g1$widths[2:5] <- as.list(maxWidth)
  g2$widths[2:5] <- as.list(maxWidth)

  #line up hights
  maxHeight = grid::unit.pmax(g3$heights[2:5], g3$heights[2:5])
  g1$heights[2:5] <- as.list(maxHeight)
  g3$heights[2:5] <- as.list(maxHeight)

  layout <- cbind(c(2,1,1,1), c(2,1,1,1), c(2,1,1,1), c(4,3,3,3))
  grid.arrange(g1, g2, g3, p4, layout_matrix = layout)
}

#' Displays histogram of variable x, with some basic statistics on graph
#' @export
#'
#' @param	x		Variable to create histogram from
#' @param	tit	Optional title of graph
#'
#' @seealso \code{\link{gghist}}
#'
histwStats <- function(x, tit="", xtit="") {
  if ( nchar(tit) == 0 ) {
    hist(x, col="green", ylab="Count", xlab=xtit);
  } else {
    hist(x, col="green", ylab="Count", main=tit, xlab=xtit);
  }
  mn <- mean(x, na.rm=TRUE);
  std <- sd(x, na.rm=TRUE);
  med <- median(x, na.rm=TRUE);
  n <- sum(! is.na(x));
  str <- sprintf("mean = %.2f median = %.2f sd = %.2f n = %d", mn, med, std,n);
  stext(str, pos=0, side=3);
}

#' GGPlot histogram of variable x, with some basic statistics on graph
#' @export
#'
#' @param	x		Variable to create histogram from
#' @param	tit	Optional title of graph
#'
#' @seealso \code{\link{histwStats}}
#'
gghist <- function(x, tit="", bw=NA, displayStats=FALSE) {
  p <- qplot(x)+myggplotdefaults()
  if ( is.na(bw) ) {
    p <- p+geom_histogram(fill="black", color="white")
  } else {
    p <- p+geom_histogram(binwidth=bw, fill="black", color="white")
  }
  p <- p + ggtitle(tit)
  p<-p+scale_y_continuous(expand=c(0,0))
  p<-p+scale_x_continuous(expand=c(0,0))

  if ( displayStats == FALSE ) {
    return(p)
  }

  mn <- mean(x, na.rm=TRUE);
  std <- sd(x, na.rm=TRUE);
  med <- median(x, na.rm=TRUE);
  n <- sum(! is.na(x));
  str <- sprintf("mean = %.2f median = %.2f sd = %.2f n = %d", mn, med, std,n);


  p2 <- arrangeGrob(p,
                    sub = textGrob(str, x = 0, hjust = -0.1, vjust=0.2, gp = gpar(fontface = "italic", fontsize = 14)),
                    heights=c(0.9, 0.1)
  )
  grid.draw(p2)

  return(p2)
}


#' createAnnotationGrid
#' @export
#' @description This funciton is meant to create a grid with an annotation
#' @description to be display concurrently with another type of graphic
#'
#' @seealso \code{\link{waterfallPlot}}
#'
createAnnotationGrid <-function(cases, vals) {
  df <- list()
  df$x <- cases
  df$fill <- as.factor(vals)
  n<-length(cases)
  df$y <- rep(0.5, n)

  df <- as.data.frame(df)
  p<-ggplot(df, aes(x=x, y=y, fill=fill))+geom_tile(colour="black")

  p <- p+theme(panel.grid.major = element_blank(),
               panel.grid.minor=element_blank(),
               panel.border=element_blank(),
               panel.background=element_blank(),
               axis.title.x= element_blank(),
               axis.title.y= element_blank(),
               axis.text.y=element_blank(),
               axis.ticks.y = element_blank(),
               axis.text.x=element_text(angle=90)
  )
  return(p)
}


#' Create a waterfall plot for cases, using values
#' @export
#'
#' @param cases List of cases for waterfall plot
#' @param vals values to use in waterfall plot
#' @param dec sort cases in decreasing order
#'
#' @seealso \code{\link{createAnnotationGrid}}
#'
waterfallPlot <- function(cases, vals, dec=FALSE) {
  df <- list()

  df$cases <- cases
  df$vals <- vals

  # remove missing cases
  df <- as.data.frame(df)
  df <- na.omit(df);

  # Now need to sort
  sort_ids <- sort(df$vals, index.return=TRUE, decreasing=dec)$ix;
  df$cases <- factor(x=df$cases, levels=df$cases[sort_ids], ordered=TRUE)

  p<-ggplot(df, aes(x=cases, y=vals))+geom_bar(stat="identity")
  p<-p+theme(axis.text.x=element_text(angle=90))

  return(p)
}

######################### UTILITY FUNCTIONS  #########################

#' My default preferences for ggplots
#' @export
#'
#' @description BW / remove major/minor panels, bold lines, etc
#'
myggplotdefaults <- function() {
  p <- theme_bw() + theme(	panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(),
                           panel.background = element_blank(),
                           panel.border = element_blank(),
                           axis.line.x = element_line(colour = "black", size=1.0),
                           axis.line.y = element_line(colour = "black", size=1.0),
                           title=element_text(face="bold", size=20),
                           axis.title=element_text(face="bold", size=18),
                           axis.title.x=element_text(vjust=0.3),
                           axis.text=element_text(face="bold", size=14),
                           legend.text=element_text(size=14)
  );
  return(p)
}

ggFootNote <- function(fnStr) {
  require(grid)
  pushViewport(viewport())
  grid.text(label = fnStr ,
            x = unit(1,"npc") - unit(2, "mm"),
            y = unit(2, "mm"),
            just = c("right", "bottom"),
            gp = gpar(cex = 0.7, col = "black")
  )
  popViewport()
}

#' Create a traditional R graphic object that just displays text
#' @export
#'
#' @param txt Text to display in graphical object
#' @param size Size of text
#'
mytextplot <- function(txt, size=1.0) {
  plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
  text(x = 0.5, y = 0.5, txt, cex =size, col = "black")
}
