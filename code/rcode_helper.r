.helpers <- new.env()

.helpers$loadpkg <- function(x) {
   x <- as.character(substitute(x))
   if (suppressWarnings(!require(x, character.only=TRUE))) {
      install.packages(x)
      base::library(x, character.only=TRUE)
   }
}

.helpers$ridgeline <- function(x, grp, xlim, ylim, xlab, adjust=1, n=TRUE, height=0.8, col, ...) {

   if (!is.factor(grp))
      grp <- factor(grp)

   grps <- levels(grp)
   p <- length(grps)

   if (missing(col)) {
      col <- rainbow(p, alpha=0.2)
   } else {
      if (length(col) == 1L)
         col <- rep(col, p)
   }

   if (length(col) != p)
      stop("Length of 'col' does not match number of groups (", p, ").")

   if (missing(xlim))
      xlim <- range(x, na.rm=TRUE)

   if (missing(ylim))
      ylim <- c(0.8, p+height)

   if (missing(xlab))
      xlab <- ""

   plot(NA, xlim=xlim, ylim=ylim, xlab=xlab, ylab="", yaxt="n", bty="n", ...)
   axis(side=2, at=1:p, labels=grps, las=1, tick=FALSE, ...)

   for (i in p:1) {

      abline(h = i - 0.15, col="lightgray")
      res <- density(x[grp == grps[i]], na.rm=TRUE, adjust=adjust)
      res$y <- res$y / max(res$y) * height
      lines(res$x, res$y + i - 0.15, ...)
      polygon(res$x, res$y + i - 0.15, col=col[i], ...)
      if (n)
         text(xlim[2] + (xlim[2]-xlim[1])*.04, i, paste("n =", length(x[grp == grps[i]])), pos=2, cex=0.8)

   }

}

attach(.helpers, warn.conflicts=FALSE)
