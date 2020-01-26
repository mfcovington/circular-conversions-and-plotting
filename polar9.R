# Polar coordinate plot.
# It is assumed that "r" is given in radians.
# Taken from R. Ihaka's post to R-help

library(circular)

polar.plot <- function(r, theta, grp = NULL, pch = NULL, col = NULL,
                       hours = TRUE, avg = TRUE, angle.axis = -90,
                       reverse = TRUE, simple.radius = FALSE, bg = NULL) {

  # r = radius
  # theta = hours in a 24 hour clock (Default) or radian (if hours = FALSE)
  # grp = a vector the same length as r and theta, describing the grouping structure
  # pch = a single pch or up to one pch per group
  # col = a single color, or up to one color per group
  # bg = a single color, or up to one color per group (only for pch 21-25)
  # hours: is theta given in hours (Default) or radians?
  # avg: should a arrow be plotted corresponding to the average of each group?
  # reverse: should lower r values be plotted to the inside or outside of the circle?

  if (is.null(pch) & !is.null(grp)) pch <- 1:length(levels(grp))

  if (length(pch) < nlevels(grp)) pch <- rep(pch, length.out = nlevels(grp))

  if (!is.null(col)) col <- as.character(col)
  if (!is.null(bg)) bg <- as.character(bg)

  if (length(col) < nlevels(grp)) col <- rep(col, length.out = nlevels(grp))
  if (length(bg) < nlevels(grp)) bg <- rep(bg, length.out = nlevels(grp))

  pplot <- function(r, theta, pch = NULL, col = NULL, bg = NULL, width=1){
    # plot points on a polar chart
    # put the points on the graph
    points(r * cos(theta), r * sin(theta), col = col, bg = bg, pch = pch, cex=2, lwd=width)
  }

  parrow <- function(r, theta, pch = NULL, col = NULL, width=4) {
    #plots arrows that have the mean radius and angle of the group sent
    theta <- circular(theta,units="radians")
    if (simple.radius) {
      r.mean <- mean(r)
    } else {
      r.mean <- sqrt(mean(sin(theta)) ^ 2 + mean(cos(theta)) ^ 2)
    }
    arrows(0, 0, r.mean * cos(mean(theta)), r.mean * sin(mean(theta)),
      lwd = width, col = col)
  }

  # convert hours to radians in a polar coordinate system
  convert.phase <- function(hours) (-hours / 24 * 2 * pi) + pi / 2

  if (hours==TRUE) theta <- convert.phase(theta)

  ## Determine the range of data value.
  ## Choose pretty cutpoints which include the range.


  # special kluge for Stacey
  if ((max(r) <= 1) & (max(r) > 0.05)) {
    r.range <- c(0, 1)
  } else {
    r.range <- range(abs(r), 0, na.rm = TRUE)
  }



  rpretty <- pretty(r.range, n = 3)
  rmax <- rpretty[length(rpretty)]

  # reverse = T means that low R will near the outside of the circle

  if (reverse == TRUE) r <- r.range[2] - r

  # print(c("r.range",r.range))
  # print(c("rpretty",rpretty))
  # print(c("rmax",rmax))
  # print(c("r",r))
  # print(by(r,grp,mean))
  # print(by(rae,grp,mean))


  ## Begin a new plot.

  plot.new()

  ## Set up the plot coordinates.
  ## We choose a square region which includes a circle
  ## large enough to include all the data.
  ## Note the use of asp = 1, so that circles will
  ## appear as circles, even if a graphics window is resized.

  plot.window(xlim = c(-rmax, rmax), ylim = c(-rmax, rmax), asp = 1)

  ## Draw a circular grid.
  ## - The circles are really polygons with many vertices.
  grid <- seq(0, 2 * pi, length = 360 )

  grid.color <- 'gray60'
  night.color <- 'gray30'

  night.start <- 13.25
  night.end <- 24

  nightshade <- seq(0.5 * pi - 2 * pi * (- 24 + night.end) / 24, 0.5 * pi - 2 * pi * (- 24 + night.start) / 24, length = 360 )
  polygon(c(0, cos(nightshade)), c(0, sin(nightshade)), col = night.color)

  for(rad in rpretty) {
    if(rad > 0) lines(rad * cos(grid), rad * sin(grid), col = grid.color)
  }

  # Draw a radial grid in "gray".
  ## The use of "12" gives divisions into 30 degree slices.

  rad <- seq(0, 2 * pi, length = 12 + 1)[-1]
  segments(0, 0, rmax * cos(rad), rmax * sin(rad), col = grid.color)

  ## Basic axis labelling.
  ## Labels appear along the ray at angle "angle.axis" to the x axis.

  # text(rpretty[-1] * cos(angle.axis * pi / 180), rpretty[-1] * sin(angle.axis * pi / 180), rpretty[-1])

  if (is.null(grp)){ # no groups
    pplot(r, theta, pch, col, bg) #plot the points
    if (avg == TRUE) parrow (r, theta, pch, col) # plot an arrow if required
  } else { # grps exist
    for (i in 1: nlevels(grp)) {#plot backlighting for points, group by group
      level <- levels(grp)[i]
      pplot(r[grp == level], theta[grp == level], pch = pch[i], col = 'white', bg = 'white', width=2)
    }
    for (i in 1: nlevels(grp)) {#plot points, group by group
      level <- levels(grp)[i]
      pplot(r[grp == level], theta[grp == level], pch = pch[i], col = col[i], bg = bg[i])
    }
    #arrow are plotted after all points, so that the arrows are "on top"
    if (avg == TRUE) {
      for (i in 1:nlevels(grp)) {#plot arrow, group by group
        level <- levels(grp)[i]
        parrow(r[grp == level], theta[grp == level], pch = pch[i], col = 'white', width=5)
        parrow(r[grp == level], theta[grp == level], pch = pch[i], col = col[i])
      } # for i
    } # if avg
  } # else
} # polar.plot

# end R. Ihaka section

