# Polar coordinate plot.
# It is assumed that "r" is given in radians.
# Taken from R. Ihaka's post to R-help

library(circular)

polar.plot <- function(r, theta, grp = NULL, pch = NULL, col = NULL,
                       hours = TRUE, avg = TRUE, angle.axis = -90,
                       reverse = TRUE, simple.radius = FALSE) {

  # r = radius
  # theta = hours in a 24 hour clock (Default) or radian (if hours = FALSE)
  # grp = a vector the same length as r and theta, describing the grouping structure
  # pch = a single pch or up to one pch per group
  # col = a single color, or up to one color per group
  # hours: is theta given in hours (Default) or radians?
  # avg: should a arrow be plotted corresponding to the average of each group?
  # reverse: should lower r values be plotted to the inside or outside of the circle?

  if (is.null(pch) & !is.null(grp)) pch <- 1:length(levels(grp))

  if (length(pch) < nlevels(grp)) pch <- rep(pch, length.out = nlevels(grp))

  if (!is.null(col)) col <- as.character(col)

  if (length(col) < nlevels(grp)) col <- rep(col, length.out = nlevels(grp))

  pplot <- function(r,theta,pch=NULL,col=NULL){
    # plot points on a polar chart
    # put the points on the graph
    points(r * cos(theta), r * sin(theta), col = col, pch = pch,cex=2)
  }

  parrow <- function(r,theta,pch=NULL,col=NULL) {
    #plots arrows that have the mean radius and angle of the group sent
    theta <- circular(theta,units="radians")
    if (simple.radius) {
      r.mean <- mean(r)
    } else {
      r.mean <- sqrt(mean(sin(theta)) ^ 2 + mean(cos(theta)) ^ 2)
    }
    arrows(0, 0, mean.r * cos(mean(theta)), mean.r * sin(mean(theta)),
      lwd = 4, col = col)
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
  for(rad in rpretty) {
    if(rad > 0) lines(rad * cos(grid), rad * sin(grid), col = "gray30")
  }

  # Draw a radial grid in "gray".
  ## The use of "12" gives divisions into 30 degree slices.

  rad <- seq(0, 2 * pi, length = 12 + 1)[-1]
  segments(0, 0, rmax * cos(rad), rmax * sin(rad), col = "gray30")

  ## Basic axis labelling.
  ## Labels appear along the ray at angle "angle.axis" to the x axis.

  # text(rpretty[-1] * cos(angle.axis * pi / 180), rpretty[-1] * sin(angle.axis * pi / 180), rpretty[-1])

  if (is.null(grp)){ # no groups
    pplot(r, theta, pch, col) #plot the points
    if (avg == TRUE) parrow (r, theta, pch, col) # plot an arrow if required
  } else { # grps exist
    for (i in 1: nlevels(grp)) {#plot points, group by group
      level <- levels(grp)[i]
      pplot(r[grp == level], theta[grp == level], pch = pch[i], col = col[i])
    }
    #arrow are plotted after all points, so that the arrows are "on top"
    if (avg == TRUE) {
      for (i in 1:nlevels(grp)) {#plot arrow, group by group
        level <- levels(grp)[i]
        parrow(r[grp == level], theta[grp == level], pch = pch[i], col = col[i])
      } # for i
    } # if avg
  } # else
} # polar.plot

# end R. Ihaka section


hours <- c(rnorm(10, 3, 2), rnorm(10, 6, 2.5), rnorm(10, 15))

rae <- c(rnorm(10, 0.3, 0.2), rnorm(10, 0.5, 0.2), rnorm(10, 0.2, 0.1))

grp <- factor(rep(c("A", "B", "C"), each = 10))

polar.plot(rae, hours, pch = 20, grp = grp, col = c("blue", "red", "green"))



# stacey notes:

# pch specifies:
# 15 = filled square
# 17 = solid triangle, upright
# 18 = solid diamond (way too small)
# 19 = solid circle (too small)
# 20 = solid diamond (too small)

polar.plot(RAE, CT_phase, pch = 20, grp = construct, col = c("black", "gray"))

polar.plot(RAE, CT_phase, pch = c(15, 17), grp = construct, col = c("black", "gray60"))

polar.plot(RAE, CT_phase, pch = c(15, 17), grp = construct, col = c("black", "gray50"

polar.plot(RAE, CT_phase, pch = c(15, 17), grp = construct, col = c("black", "gray50"))

polar.plot(RAE, CT_phase, pch = c(20, 17), grp = construct, col = c("black", "gray50"))

# I like that last one.


polar.plot(RAE, CT_phase, pch = c(3, 20), grp = construct, col = c("black", "gray50"))

# used this one for TL1EE and mt

# how do I load a text file?
# > flanking<-read.table("flanking_phases_083004.txt",header=T)

# how do I get everything where it needs to be?
# > attach(flanking)

# then plot
# polar.plot(rae,pos_phase,pch=20,grp=construct,col=c("green","blue","gray","red"))
