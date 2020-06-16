#' Colored dendrogram
#'
#' This function plot an dendrogram with different colors to each cluster
#' for a given number of classes. See examples.
#'
#' @param x an \code{\link[stats]{hclust}} object to plot
#' @param k the number of clusters
#' @param col.up color for the upper part
#' @param col.down a vector of colors of length \code{k}, one color per cluster
#' @param lty.up line type for the upper part (see \code{\link{par}})
#' @param lty.down line type for the clusters part (see \code{\link{par}})
#' @param lwd.up line width for the upper part (see \code{\link{par}})
#' @param lwd.down line width for the clusters part (see \code{\link{par}})
#' @param type type of link (\code{"rectangle"} or \code{"triangle"})
#' @param knot.pos position of the knots: \code{"mean"} mean between the two tree sons, \code{"bary"} weighted mean relative to the number of observations in the left and the right branch, \code{"left"}, \code{"right"}, \code{"random"} just for fun actually
#' @param criteria vector of a criteria to draw on the left of the tree
#' @param fact.sup a factor to categorize the observations
#' @param show.labels	\code{TRUE} if the labels should be drawn
#' @param only.tree	\code{TRUE} if only the tree should be drawn (use that to include the tree in a more complicated layout)
#' @param main title of the plot
#' @param boxes	\code{TRUE} to draw the bow around the plots
#' @param members	members of each terminal node (see \code{\link{hclust}} for more details)
#' @author Romain Francois
#' @note
#' The A2R package has not been updated since January 2006 and cannot be installed
#' anymore with a recent version of R. It's why this function has been copied here.
#' @source \url{http://addictedtor.free.fr/packages/A2R/lastVersion/}
#' @seealso \url{http://rpubs.com/gaston/dendrograms}, \code{\link{plot.hclust}}
#' @export A2Rplot
#' @examples
#' # Example with iris data
#' d <- dist(iris[,1:4],method="euc")
#' h <- hclust(d)
#' Species <- iris[,5]
#' A2Rplot(h, k=3, fact.sup=Species, knot.pos="bary", show.labels=FALSE)
#'
#' # Examples from http://rpubs.com/gaston/dendrograms
#'
#' bg.def <- par()$bg
#'
#' hc <- hclust(dist(mtcars))
#' par(bg = "#EFEFEF")
#' A2Rplot(hc, k = 3, boxes = FALSE, col.up = "gray50",
#'        col.down = c("#FF6B6B", "#4ECDC4", "#556270"))
#'
#' par(bg = "gray15")
#' cols = hsv(c(0.2, 0.57, 0.95), 1, 1, 0.8)
#' A2Rplot(hc, k = 3, boxes = FALSE, col.up = "gray50", col.down = cols)
#'
#' par(bg = bg.def)
#'
#' # Examples with state.x77
#' d77 <- dist(state.x77)
#' h77 <- hclust(d77)
#' A2Rplot(h77, k=4, knot.pos="mean", type="tri")
#' A2Rplot(h77, k=4, lty.up=1,lwd.down=1,
#' 				col.down=c("purple","black","green3","orange"),
#' 				col.up="gray", boxes=FALSE)
#'
#' A2Rplot(h77, k=4, knot.pos="left", type="tri")
#'
#' # Example showing how to include this in an other layout with only.tree
#' op <- par(no.readonly=TRUE)
#' par(mfrow = c(3,3))
#' par(mar=c(3,3,3,3))
#' plot(rnorm(50)) # one plot
#' plot(rnorm(50)) # one plot
#' plot(rnorm(50)) # one plot
#' plot(rnorm(50)) # one plot
#' par(mar=c(1,1,1,1))
#' A2Rplot(h77, k=4, only.tree=TRUE, boxes=FALSE)
#' par(mar=c(3,3,3,3))
#' plot(rnorm(50)) # one plot
#' plot(rnorm(50)) # one plot
#' plot(rnorm(50)) # one plot
#' plot(rnorm(50)) # one plot
#' par(op)
#'
#' # Example using members
#' hc <- hclust(dist(USArrests)^2, "cen")
#' memb <- cutree(hc, k = 10)
#' cent <- NULL
#' for(k in 1:10){
#' 	cent <- rbind(cent, colMeans(USArrests[memb == k, , drop = FALSE]))
#' }
#' hc1 <- hclust(dist(cent)^2, method = "cen", members = table(memb))
#' hc1$labels <- paste('g',1:10)
#' A2Rplot(hc1,
#' 				members = table(memb),
#' 				k=4,
#' 				lwd.up   =  2, lty.up=1, col.up = "gray",
#' 				lwd.down =  1, lty.down='twodash',
#' 				col.down = c("orange", "brown", "green3", "royalblue"),
#' 				knot.pos = "bary"
#' )
#' @importFrom grDevices rainbow
#' @importFrom utils tail
#' @importFrom graphics axis box layout par plot points polygon rect segments text title
#' @importFrom stats as.dendrogram as.formula as.hclust chisq.test cutree heatmap prop.test runif xtabs

`A2Rplot` <- function(
  x, # an hclust object to draw
  k        = 2, # the number of groups
  col.up   = "black",
  col.down = rainbow(k),
  lty.up   = 2,
  lty.down = 1,
  lwd.up   = 1,
  lwd.down = 2,
  type     = c("rectangle", "triangle"),
  knot.pos = c("mean", "bary", "left", "right", "random"),
  criteria,
  fact.sup,
  show.labels=TRUE,
  only.tree=FALSE,
  main     = paste("Colored Dendrogram (", k, " groups)"),
  boxes    = TRUE,
  members) {
  if (!inherits(x, "hclust")) x <- as.hclust(x)
  if (missing(members)) members <- NULL
  opar <- par(no.readonly = TRUE)
  knot.pos <- match.arg(knot.pos)
  type <- match.arg(type)
  # tests
  if (k < 2) {
    stop("k must be at least 2")
  }
  
  ._a2r_counter <<- 0
  ._a2r_hclu <<- x
  
  ._a2r_envir <<- environment()
  nn <- length(x$order) - 1
  
  ._a2r_height_cut <<- mean(x$height[nn - k + 1:2])
  ._a2r_group <<- 0
  
  n.indiv <- length(x$order)
  groups.o <- cutree.order(x, k = k)[x$order]
  
  bottom <- if (is.null(members)) 0 else x$height[nn] * -.2
  
  if (only.tree) {
    if (is.null(members)) {
      plot(0, type = "n", xlim = c(0.5, n.indiv + .5), ylim = c(bottom, x$height[nn]), xaxs = "i", axes = FALSE, xlab = "", ylab = "")
    } else {
      plot(0, type = "n", xlim = c(0.5, sum(members) + .5), ylim = c(bottom, x$height[nn]), xaxs = "i", axes = FALSE, xlab = "", ylab = "")
    }
    # call to the ** recursive function ** .rec.hclust
    .rec.hclust(nn, col = col.up, lty = lty.up, lwd = lwd.up)
    
    if (boxes) {
      axis(2)
      box()
    }
    return(NULL)
  }
  
  # prepare the layout
  matlayout <- matrix(c(2, 4, 6, 1, 3, 5), nc = 2, nr = 3)
  widths <- c(1, 9)
  heights <- c(8, 1, 1)
  if (!show.labels) {
    matlayout <- matrix(c(2, 4, 1, 3), nc = 2, nr = 2)
    widths <- c(1, 9)
    heights <- c(9, 1)
  }
  if (!missing(fact.sup)) {
    heights <- c(8, 1, 1)
  }
  if (missing(criteria) & missing(fact.sup)) {
    matlayout <- matrix(c(2, 4, 1, 3), nc = 2, nr = 2)
    widths <- c(1, 9)
    heights <- c(9, 1)
  }
  layout(matlayout, width = widths, height = heights)
  
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ The tree (1)
  par(mar = c(0, 0, 3, 4))
  if (is.null(members)) {
    plot(0, type = "n", xlim = c(0.5, n.indiv + .5), ylim = c(bottom, x$height[nn]), xaxs = "i", axes = FALSE, xlab = "", ylab = "")
  } else {
    plot(0, type = "n", xlim = c(0.5, sum(members) + .5), ylim = c(bottom, x$height[nn]), xaxs = "i", axes = FALSE, xlab = "", ylab = "")
  }
  # call to the ** recursive function ** .rec.hclust
  .rec.hclust(nn, col = col.up, lty = lty.up, lwd = lwd.up)
  title(main)
  if (boxes) {
    box()
    axis(4)
  }
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Criteria (2)
  if (!missing(criteria)) {
    par(mar = c(0, 0, 3, 0))
    plot(0,
         type = "n",
         xlim = range(criteria),
         ylim = c(0, x$height[nn]),
         axes = FALSE,
         xlab = "",
         ylab = ""
    )
    par(las = 2)
    n.crit <- length(criteria)
    heights.cut <- (tail(x$height, n.crit) +
                      tail(x$height, n.crit + 1)[-(n.crit + 1)]) / 2
    heights.cut <- rev(heights.cut)
    
    points(criteria, heights.cut, pch = 21, bg = "red", type = "o")
    points(criteria[k - 1], heights.cut[k - 1], pch = 21, cex = 2, bg = "blue", xpd = NA)
    if (boxes) {
      axis(3)
      box()
    }
  }
  else {
    par(mar = c(0, 0, 3, 0))
    plot(0,
         type = "n",
         xlim = c(0, 1),
         ylim = c(0, 1),
         axes = FALSE,
         xlab = "",
         ylab = ""
    )
  }
  
  if (show.labels) {
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Name of the observations (3)
    par(mar = c(0, 0, 0, 4))
    par(srt = 90)
    obs.labels <- substr(x$labels[x$order], 1, 8)
    if (is.null(members)) {
      plot(0, type = "n", xlim = c(1, n.indiv + 0.8), ylim = c(0, 1.5), xaxs = "i", axes = FALSE, xlab = "", ylab = "")
      text(1:n.indiv, 0, obs.labels, pos = 4, col = col.down[groups.o])
    }
    
    
    
    
    
    else {
      plot(0, type = "n", xlim = c(-0.2, sum(members) + .5), ylim = c(0, 1), xaxs = "i", axes = FALSE, xlab = "", ylab = "")
      xo <- members[x$order]
      text(cumsum(xo) - xo / 2, 0, obs.labels, pos = 4, cex = 2, col = col.down[groups.o])
    }
    par(srt = 0)
    if (boxes) {
      box()
    }
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Labels (4)
    par(mar = c(0, 0, 0, 0))
    plot(0, type = "n", xlim = c(0, 1), ylim = c(0, 1), xaxs = "i", axes = FALSE, xlab = "", ylab = "")
    text(.5, .5, "Labels")
    if (boxes) {
      box()
    }
  }
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Quali (5,6)
  if (!missing(fact.sup)) {
    quali <- as.factor(fact.sup)[x$order]
    quanti <- as.numeric(quali)
    
    par(mar = c(1, 0, 0, 4))
    n.levels <- length(levels(quali))
    plot(0,
         type = "n",
         xlim = c(0.5, n.indiv + .5),
         ylim = c(0, n.levels),
         xaxs = "i", yaxs = "i", axes = FALSE, xlab = "", ylab = ""
    )
    
    rect(
      xleft = (1:n.indiv) - .5,
      xright = (1:n.indiv) + .5,
      ybottom = quanti - 1,
      ytop = quanti,
      col = col.down[groups.o]
    )
    par(las = 1)
    axis(4, (1:n.levels) - .5, levels(quali), tick = FALSE)
    
    if (boxes) {
      box()
    }
    
    
    par(mar = c(1, 0, 0, 0))
    plot(0, type = "n", xlim = c(0, 1), ylim = c(0, 1), xaxs = "i", axes = FALSE, xlab = "", ylab = "")
    text(.5, .5, deparse(substitute(fact.sup)))
    if (boxes) {
      box()
    }
  }
  
  
  par(opar) # reset parameter
}

# ===============================================================================

`.rec.hclust` <- function(
  index, # index of the current tree to draw
  lwd = 1,
  lty = 1,
  col = "black") {
  members <- get("members", envir = ._a2r_envir)
  bottom <- get("bottom", envir = ._a2r_envir)
  if (index < 0) { # it is a leaf
    if (is.null(members)) {
      ._a2r_counter <<- ._a2r_counter + 1
      return(list(
        x = ._a2r_counter,
        n = 1
      ))
    }
    else {
      cc <- ._a2r_counter
      mm <- members[-index]
      polygon(
        x = c(cc, cc + mm / 2, cc + mm),
        y = c(bottom, 0, bottom),
        col = col,
        border = col,
        lwd = lwd
      )
      ._a2r_counter <<- ._a2r_counter + mm
      return(list(
        x = cc + mm / 2,
        n = mm
      ))
    }
  }
  
  h.m <- ._a2r_hclu$height[index]
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~ do left
  index.l <- ._a2r_hclu$merge[index, 1]
  
  h.l <- if (index.l < 0) 0 else ._a2r_hclu$height[index.l]
  if (h.l < ._a2r_height_cut & h.m > ._a2r_height_cut) {
    ._a2r_group <<- ._a2r_group + 1
    col.l <- get("col.down", envir = ._a2r_envir)[._a2r_group]
    lwd.l <- get("lwd.down", envir = ._a2r_envir)
    lty.l <- get("lty.down", envir = ._a2r_envir)
  }
  else {
    col.l <- col
    lwd.l <- lwd
    lty.l <- lty
  }
  out.l <- .rec.hclust(index.l, col = col.l, lty = lty.l, lwd = lwd.l)
  x.l <- out.l$x
  n.l <- out.l$n
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~ do right
  index.r <- ._a2r_hclu$merge[index, 2]
  h.r <- if (index.r < 0) 0 else ._a2r_hclu$height[index.r]
  if (h.r < ._a2r_height_cut & h.m > ._a2r_height_cut) {
    ._a2r_group <<- ._a2r_group + 1
    col.r <- get("col.down", envir = ._a2r_envir)[._a2r_group]
    lwd.r <- get("lwd.down", envir = ._a2r_envir)
    lty.r <- get("lty.down", envir = ._a2r_envir)
  }
  else {
    col.r <- col
    lwd.r <- lwd
    lty.r <- lty
  }
  out.r <- .rec.hclust(index.r, col = col.r, lty = lty.r, lwd = lwd.r)
  x.r <- out.r$x
  n.r <- out.r$n
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~ draw what you have to draw
  
  type <- get("type", envir = ._a2r_envir)
  x.m <- (x.r + x.l) / 2
  n <- n.r + n.l
  x.b <- (n.r * x.r + n.l * x.l) / n
  
  
  knot.pos <- get("knot.pos", envir = ._a2r_envir)
  
  x <- switch(knot.pos,
              mean = x.m,
              left = x.l,
              right = x.r,
              random = x.l + runif(1) * (x.r - x.l),
              bary = x.b
  )
  
  
  
  if (type == "rectangle") {
    segments(
      x0 = c(x.l, x.l, x.r),
      x1 = c(x.l, x.r, x.r),
      y0 = c(h.l, h.m, h.r),
      y1 = c(h.m, h.m, h.m),
      col = col,
      lty = lty,
      lwd = lwd
    )
  }
  if (type == "triangle") {
    segments(
      x0 = c(x.l, x.r),
      x1 = c(x, x),
      y0 = c(h.l, h.r),
      y1 = c(h.m, h.m),
      col = col,
      lty = lty,
      lwd = lwd
    )
  }
  
  
  list(x = x, n = n)
}

#' Cut a tree into groups of data in the order of the tree
#'
#' Cut a tree (result from \link{hclust}) into groups of data.
#' Groups are in the order of the tree leafs.
#'
#' @param tree an \code{\link{hclust}} object
#' @param k the desired number of groups
#' @param h height where the tree is to be cut
#' @author Romain Francois
#' @note
#' The A2R package has not been updated since January 2006 and cannot be installed
#' anymore with a recent version of R. It's why this function has been copied here.
#' @source \url{http://addictedtor.free.fr/packages/A2R/lastVersion/}
#' @seealso \code{\link{cutree}}
#' @export cutree.order
#' @examples
#' h77 <- hclust(dist(state.x77))
#' ct.o <- cutree.order(h77, k=4)
#' ct.n <- cutree(h77,k=4)
#' plot(h77)
#' rect.hclust(h77, 4)
#' cbind(ct.o, ct.n)

`cutree.order` <-
  function(tree, k = NULL, h = NULL) {
    coupe <- cutree(tree, k = k, h = h)
    
    coupe.or <- coupe[tree$order]
    coupe.out <- rep(NA, length(coupe))
    j <- 1 #
    k <- coupe.or[1]
    for (i in 1:length(coupe)) {
      if (coupe.or[i] == k) {
        next
      } else {
        coupe.out[which(coupe == k)] <- j
        j <- j + 1
        k <- coupe.or[i]
      }
    }
    coupe.out[is.na(coupe.out)] <- j
    names(coupe.out) <- names(coupe)
    coupe.out
  }
# ===============================================================================
