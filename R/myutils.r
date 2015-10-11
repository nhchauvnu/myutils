#' naValueGraph function helps us to display NA values
#' map of a data frame.
#'
#' @param data is a data frame
#' @param percent is the percentage of row values to display
#' The percentage must be a number between 1 and 100
#' @keywords NA values, data frames
#' @export
#' @examples
#' naValueGraph(data=df, percent=50)

naValueGraph <- function(data, percent=50, msg="NA values map of") {
	dy <- max(nchar(names(data)))
	dx <- max(nchar(rownames(data)))
	nr <- nrow(data)
	nc <- ncol(data)
	tmp <- seq(1, nr, by=100/percent)
	if (tmp[length(tmp)] != nr)
		tmp = c(tmp, nr)
	lbl <- rownames(data)[tmp]
	nr <- length(lbl)
	dfl <- par("mar")
	dfl[1] <- dx/2+3
	dfl[2] <- dy/2
	par(mar=dfl, cex=0.8)
	image(is.na(data), axes=FALSE, col=c('gray', 'black'), xlab="Observation")
	title(paste0(msg, " '", deparse(substitute(data)), "'"))
	axis(side=1, at=0:(nr-1)/(nr-1), labels=lbl, las=2)
	axis(side=2, at=0:(nc-1)/(nc-1), labels=colnames(data), las=1)
}

