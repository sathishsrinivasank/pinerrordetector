#' legend2
#'
#' This is a modified function of legend found in graphics package. Three lines
#' are commented out to prevent xy.coords from determining the legend coordinates.
#' All parameters are the same as given in the legend function of graphics
#' package of version 3.1.2.
#'
#' Arguments x, y, legend are interpreted in a non-standard way to allow the coordinates
#' to be specified via one or two arguments. If legend is missing and y is not numeric,
#' it is assumed that the second argument is intended to be legend and that the first
#' argument specifies the coordinates.
#'
#' The coordinates can be specified in any way which is accepted by xy.coords. If this
#' gives the coordinates of one point, it is used as the top-left coordinate of the
#' rectangle containing the legend. If it gives the coordinates of two points, these
#' specify opposite corners of the rectangle (either pair of corners, in any order).
#'
#' The location may also be specified by setting x to a single keyword from the list
#' "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right"
#' and "center". This places the legend on the inside of the plot frame at the given
#' location. Partial argument matching is used. The optional inset argument specifies
#' how far the legend is inset from the plot margins. If a single value is given, it
#' is used for both margins; if two values are given, the first is used for x- distance,
#' the second for y-distance.
#'
#' Attribute arguments such as col, pch, lty, etc, are recycled if necessary: merge is
#' not. Set entries of lty to 0 or set entries of lwd to NA to suppress lines in
#' corresponding legend entries; set pch values to NA to suppress points.
#'
#' Points are drawn after lines in order that they can cover the line with their
#' background color pt.bg, if applicable.
#'
#' See the examples for how to right-justify labels.
#'
#' Since they are not used for Unicode code points, values -31:-1 are silently omitted,
#' as are NA and "" values.
#'
#' @param x,y	the x and y co-ordinates to be used to position the legend.
#' They can be specified by keyword or in any way which is accepted by xy.coords: See ‘Details’.
#' @param legend	a character or expression vector of length ≥ 1 to appear in the legend.
#'  Other objects will be coerced by as.graphicsAnnot.
#' @param fill	if specified, this argument will cause boxes filled with the specified colors
#'  (or shaded in the specified colors) to appear beside the legend text.
#' @param col	the color of points or lines appearing in the legend.
#' @param border	the border color for the boxes (used only if fill is specified).
#' @param lty,lwd	the line types and widths for lines appearing in the legend.
#' One of these two must be specified for line drawing.
#' @param pch	the plotting symbols appearing in the legend, as numeric vector or a vector
#'  of 1-character strings (see points). Unlike points, this can all be specified as a
#'  single multi-character string. Must be specified for symbol drawing.
#' @param angle angle of shading lines.
#' @param density the density of shading lines, if numeric and positive. If NULL or negative
#'  or NA color filling is assumed.
#' @param bty	the type of box to be drawn around the legend. The allowed values are "o"
#'  (the default) and "n".
#' @param bg	the background color for the legend box. (Note that this is only used if bty != "n".)
#' @param box.lty,box.lwd,box.col	the line type, width and color for the legend box (if bty = "o").
#' @param pt.bg	the background color for the points, corresponding to its argument bg.
#' @param cex	character expansion factor relative to current par("cex"). Used for text, and
#'  provides the default for pt.cex and title.cex.
#' @param pt.cex	expansion factor(s) for the points.
#' @param pt.lwd	line width for the points, defaults to the one for lines, or if that is
#'  not set, to par("lwd").
#' @param xjust	how the legend is to be justified relative to the legend x location.
#'   A value of 0 means left justified, 0.5 means centered and 1 means right justified.
#' @param yjust	the same as xjust for the legend y location.
#' @param x.intersp	character interspacing factor for horizontal (x) spacing.
#' @param y.intersp	the same for vertical (y) line distances.
#' @param adj	numeric of length 1 or 2; the string adjustment for legend text.
#'  Useful for y-adjustment when labels are plotmath expressions.
#' @param text.width	the width of the legend text in x ("user") coordinates. (Should be
#'  positive even for a reversed x axis.) Defaults to the proper value computed by strwidth(legend).
#' @param text.col	the color used for the legend text.
#' @param text.font	the font used for the legend text, see text.
#' @param merge	logical; if TRUE, merge points and lines but not filled boxes.
#'  Defaults to TRUE if there are points and lines.
#' @param trace	logical; if TRUE, shows how legend does all its magical computations.
#' @param plot	logical. If FALSE, nothing is plotted but the sizes are returned.
#' @param ncol	the number of columns in which to set the legend items (default is 1, a vertical legend).
#' @param horiz	logical; if TRUE, set the legend horizontally rather than vertically
#' (specifying horiz overrides the ncol specification).
#' @param title	a character string or length-one expression giving a title to be placed at the
#' top of the legend. Other objects will be coerced by as.graphicsAnnot.
#' @param inset	inset distance(s) from the margins as a fraction of the plot region when
#' legend is placed by keyword.
#' @param xpd	if supplied, a value of the graphical parameter xpd to be used while
#' the legend is being drawn.
#' @param title.col	color for title.
#' @param title.adj	horizontal adjustment for title: see the help for par("adj").
#' @param seg.len	the length of lines drawn to illustrate lty and/or lwd (in units
#' of character widths).
#'
#' @return returns legend symbol and text
#'
legend2 <- function (x, y = NULL, legend, fill = NULL, col = par("col"),
                     border = "black", lty, lwd, pch, angle = 45, density = NULL,
                     bty = "o", bg = par("bg"), box.lwd = par("lwd"), box.lty = par("lty"),
                     box.col = par("fg"), pt.bg = NA, cex = 1, pt.cex = cex,
                     pt.lwd = lwd, xjust = 0, yjust = 1, x.intersp = 1, y.intersp = 1,
                     adj = c(0, 0.5), text.width = NULL, text.col = par("col"),
                     text.font = NULL, merge = do.lines && has.pch, trace = FALSE,
                     plot = TRUE, ncol = 1, horiz = FALSE, title = NULL, inset = 0,
                     xpd, title.col = text.col, title.adj = 0.5, seg.len = 2)
{
  if (missing(legend) && !missing(y) && (is.character(y) ||
                                         is.expression(y))) {
    legend <- y
    y <- NULL
  }
  mfill <- !missing(fill) || !missing(density)
  if (!missing(xpd)) {
    op <- par("xpd")
    on.exit(par(xpd = op))
    par(xpd = xpd)
  }
  title <- as.graphicsAnnot(title)
  if (length(title) > 1)
    stop("invalid 'title'")
  legend <- as.graphicsAnnot(legend)
  n.leg <- if (is.call(legend))
    1
  else length(legend)
  if (n.leg == 0)
    stop("'legend' is of length 0")
  auto <- if (is.character(x))
    match.arg(x, c("bottomright", "bottom", "bottomleft",
                   "left", "topleft", "top", "topright", "right", "center"))
  else NA
  if (is.na(auto)) {
#     xy <- xy.coords(x, y)
#     x <- xy$x
#     y <- xy$y
    nx <- length(x)
    if (nx < 1 || nx > 2)
      stop("invalid coordinate lengths")
  }
  else nx <- 0
  xlog <- par("xlog")
  ylog <- par("ylog")
  rect2 <- function(left, top, dx, dy, density = NULL, angle,
                    ...) {
    r <- left + dx
    if (xlog) {
      left <- 10^left
      r <- 10^r
    }
    b <- top - dy
    if (ylog) {
      top <- 10^top
      b <- 10^b
    }
    rect(left, top, r, b, angle = angle, density = density,
         ...)
  }
  segments2 <- function(x1, y1, dx, dy, ...) {
    x2 <- x1 + dx
    if (xlog) {
      x1 <- 10^x1
      x2 <- 10^x2
    }
    y2 <- y1 + dy
    if (ylog) {
      y1 <- 10^y1
      y2 <- 10^y2
    }
    segments(x1, y1, x2, y2, ...)
  }
  points2 <- function(x, y, ...) {
    if (xlog)
      x <- 10^x
    if (ylog)
      y <- 10^y
    points(x, y, ...)
  }
  text2 <- function(x, y, ...) {
    if (xlog)
      x <- 10^x
    if (ylog)
      y <- 10^y
    text(x, y, ...)
  }
  if (trace)
    catn <- function(...) do.call("cat", c(lapply(list(...),
                                                  formatC), list("\n")))
  cin <- par("cin")
  Cex <- cex * par("cex")
  if (is.null(text.width))
    text.width <- max(abs(strwidth(legend, units = "user",
                                   cex = cex, font = text.font)))
  else if (!is.numeric(text.width) || text.width < 0)
    stop("'text.width' must be numeric, >= 0")
  xc <- Cex * xinch(cin[1L], warn.log = FALSE)
  yc <- Cex * yinch(cin[2L], warn.log = FALSE)
  if (xc < 0)
    text.width <- -text.width
  xchar <- xc
  xextra <- 0
  yextra <- yc * (y.intersp - 1)
  ymax <- yc * max(1, strheight(legend, units = "user", cex = cex)/yc)
  ychar <- yextra + ymax
  if (trace)
    catn("  xchar=", xchar, "; (yextra,ychar)=", c(yextra,
                                                   ychar))
  if (mfill) {
    xbox <- xc * 0.8
    ybox <- yc * 0.5
    dx.fill <- xbox
  }
  do.lines <- (!missing(lty) && (is.character(lty) || any(lty >
                                                            0))) || !missing(lwd)
  n.legpercol <- if (horiz) {
    if (ncol != 1)
      warning(gettextf("horizontal specification overrides: Number of columns := %d",
                       n.leg), domain = NA)
    ncol <- n.leg
    1
  }
  else ceiling(n.leg/ncol)
  has.pch <- !missing(pch) && length(pch) > 0
  if (do.lines) {
    x.off <- if (merge)
      -0.7
    else 0
  }
  else if (merge)
    warning("'merge = TRUE' has no effect when no line segments are drawn")
  if (has.pch) {
    if (is.character(pch) && !is.na(pch[1L]) && nchar(pch[1L],
                                                      type = "c") > 1) {
      if (length(pch) > 1)
        warning("not using pch[2..] since pch[1L] has multiple chars")
      np <- nchar(pch[1L], type = "c")
      pch <- substr(rep.int(pch[1L], np), 1L:np, 1L:np)
    }
    if (!is.character(pch))
      pch <- as.integer(pch)
  }
  if (is.na(auto)) {
    if (xlog)
      x <- log10(x)
    if (ylog)
      y <- log10(y)
  }
  if (nx == 2) {
    x <- sort(x)
    y <- sort(y)
    left <- x[1L]
    top <- y[2L]
    w <- diff(x)
    h <- diff(y)
    w0 <- w/ncol
    x <- mean(x)
    y <- mean(y)
    if (missing(xjust))
      xjust <- 0.5
    if (missing(yjust))
      yjust <- 0.5
  }
  else {
    h <- (n.legpercol + (!is.null(title))) * ychar + yc
    w0 <- text.width + (x.intersp + 1) * xchar
    if (mfill)
      w0 <- w0 + dx.fill
    if (do.lines)
      w0 <- w0 + (seg.len + x.off) * xchar
    w <- ncol * w0 + 0.5 * xchar
    if (!is.null(title) && (abs(tw <- strwidth(title, units = "user",
                                               cex = cex) + 0.5 * xchar)) > abs(w)) {
      xextra <- (tw - w)/2
      w <- tw
    }
    if (is.na(auto)) {
      left <- x - xjust * w
      top <- y + (1 - yjust) * h
    }
    else {
      usr <- par("usr")
      inset <- rep_len(inset, 2)
      insetx <- inset[1L] * (usr[2L] - usr[1L])
      left <- switch(auto, bottomright = , topright = ,
                     right = usr[2L] - w - insetx, bottomleft = ,
                     left = , topleft = usr[1L] + insetx, bottom = ,
                     top = , center = (usr[1L] + usr[2L] - w)/2)
      insety <- inset[2L] * (usr[4L] - usr[3L])
      top <- switch(auto, bottomright = , bottom = , bottomleft = usr[3L] +
                      h + insety, topleft = , top = , topright = usr[4L] -
                      insety, left = , right = , center = (usr[3L] +
                                                             usr[4L] + h)/2)
    }
  }
  if (plot && bty != "n") {
    if (trace)
      catn("  rect2(", left, ",", top, ", w=", w, ", h=",
           h, ", ...)", sep = "")
    rect2(left, top, dx = w, dy = h, col = bg, density = NULL,
          lwd = box.lwd, lty = box.lty, border = box.col)
  }
  xt <- left + xchar + xextra + (w0 * rep.int(0:(ncol - 1),
                                              rep.int(n.legpercol, ncol)))[1L:n.leg]
  yt <- top - 0.5 * yextra - ymax - (rep.int(1L:n.legpercol,
                                             ncol)[1L:n.leg] - 1 + (!is.null(title))) * ychar
  if (mfill) {
    if (plot) {
      if (!is.null(fill))
        fill <- rep_len(fill, n.leg)
      rect2(left = xt, top = yt + ybox/2, dx = xbox, dy = ybox,
            col = fill, density = density, angle = angle,
            border = border)
    }
    xt <- xt + dx.fill
  }
  if (plot && (has.pch || do.lines))
    col <- rep_len(col, n.leg)
  if (missing(lwd) || is.null(lwd))
    lwd <- par("lwd")
  if (do.lines) {
    if (missing(lty) || is.null(lty))
      lty <- 1
    lty <- rep_len(lty, n.leg)
    lwd <- rep_len(lwd, n.leg)
    ok.l <- !is.na(lty) & (is.character(lty) | lty > 0) &
      !is.na(lwd)
    if (trace)
      catn("  segments2(", xt[ok.l] + x.off * xchar, ",",
           yt[ok.l], ", dx=", seg.len * xchar, ", dy=0, ...)")
    if (plot)
      segments2(xt[ok.l] + x.off * xchar, yt[ok.l], dx = seg.len *
                  xchar, dy = 0, lty = lty[ok.l], lwd = lwd[ok.l],
                col = col[ok.l])
    xt <- xt + (seg.len + x.off) * xchar
  }
  if (has.pch) {
    pch <- rep_len(pch, n.leg)
    pt.bg <- rep_len(pt.bg, n.leg)
    pt.cex <- rep_len(pt.cex, n.leg)
    pt.lwd <- rep_len(pt.lwd, n.leg)
    ok <- !is.na(pch)
    if (!is.character(pch)) {
      ok <- ok & (pch >= 0 | pch <= -32)
    }
    else {
      ok <- ok & nzchar(pch)
    }
    x1 <- (if (merge && do.lines)
      xt - (seg.len/2) * xchar
      else xt)[ok]
    y1 <- yt[ok]
    if (trace)
      catn("  points2(", x1, ",", y1, ", pch=", pch[ok],
           ", ...)")
    if (plot)
      points2(x1, y1, pch = pch[ok], col = col[ok], cex = pt.cex[ok],
              bg = pt.bg[ok], lwd = pt.lwd[ok])
  }
  xt <- xt + x.intersp * xchar
  if (plot) {
    if (!is.null(title))
      text2(left + w * title.adj, top - ymax, labels = title,
            adj = c(title.adj, 0), cex = cex, col = title.col)
    text2(xt, yt, labels = legend, adj = adj, cex = cex,
          col = text.col, font = text.font)
  }
  invisible(list(rect = list(w = w, h = h, left = left, top = top),
                 text = list(x = xt, y = yt)))
}
