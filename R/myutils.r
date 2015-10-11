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

naValueGraph <- function(data, percent=100) {
	nr <- nrow(data)
	nc <- ncol(data)
	image(is.na(data), axes=FALSE, col=c('gray', 'black'))
	axis(1, at=0:(nr-1)/(nr/100*percent), labels=rownames(data), las=2)
	axis(2, at=0:(nc-1)/nc, labels=colnames(data), las=2)
}

