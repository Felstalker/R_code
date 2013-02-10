# TODO: Add comment
#   to alter: Preferences->StatET->Code Generation
# Author: OGOREKB
###############################################################################
setInternet2(TRUE)

library(RUnit)
foo <- function(x) {
	x <- x * x
	x <- 2 * x
	return(x)
}
test.foo <- function() {
	checkTrue(is.numeric(foo(1:10)))
	checkEquals(length(foo(1:10)), 10)
	checkEqualsNumeric(foo(1), 2)
}
bar <- function(x, y = NULL) {
	if (is.null(y)) {
		y <- x
	}
	if (all(y > 100)) {
		y <- y - 100
	}
	res <- x^y
	return(res)
}

track <- tracker()
track$init()
a <- 1:10
d <- seq(0, 1, 0.1)
resFoo <- inspect(foo(a), track = track)
resBar <- inspect(bar(d), track = track)
resTrack <- track$getTrackInfo()
printHTML.trackInfo(resTrack, "C:/devl/output")
