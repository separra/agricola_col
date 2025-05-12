pairs.cor <- function (x,y,smooth=TRUE, digits=2,  ...)
{
  panel.cor <- function(x, y, ...)
  {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r.obj = cor.test(x, y,use="pairwise",...)
    r = as.numeric(r.obj$estimate)
    p = r.obj$p.value
    mystars <- ifelse(p < .05, "* ", " ")
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(txt, mystars, sep="")
    text(0.5, 0.5, txt)
  }
  panel.hist <- function(x)
  {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col="cyan")
  }
  pairs(x,diag.panel=panel.hist,lower.panel=panel.cor,upper.panel=panel.smooth, ...)
} 

