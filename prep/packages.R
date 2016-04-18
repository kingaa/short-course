## check to see that the version of R is at least 3.2.1
print(R.version.string)
stopifnot(getRversion()>="3.2.1")

op <- options(warn=2)

## get list of packages to install
pkglist <- scan(
  what=character(0),
  text="
bbmle
coda
colorspace
deSolve
foreach
doParallel
ggplot2
gridExtra
gtable
knitr
lhs
magrittr
mvtnorm
nloptr
plyr
RColorBrewer
reshape2
sos
stringr
subplex
xtable
"
)

pkglist <- setdiff(pkglist,rownames(installed.packages()))

if (length(pkglist)>0) install.packages(pkglist)

options(op)

cat("all packages installed successfully!\n")
