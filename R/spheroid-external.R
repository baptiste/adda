## ----loading, echo=FALSE-------------------------------------------------
require(knitr)
opts_knit$set(tidy=FALSE)
library(dielectric)
library(plyr)

## ----function------------------------------------------------------------


adda_run <- function(ii=1, wavelength=500, 
                     shape = "ellipsoid", AR = 1.3,
                     n = 1.5+0i,
                     medium.index = 1.0,
                     dpl = ceiling(min(50, 20 * abs(n))),
                     radius = 10, grid=10, ..., 
                     run="", params = "params.txt", comment="",
                     adda = "../adda/src/seq/adda"){

  dir <- paste0(" -dir tmp_", ii)
    
  command <- paste("-shape", shape, 1/AR, 1/AR,
                   "-size ", 2 * radius*1e-3,
                   "-lambda ", wavelength*1e-3 / medium.index ,
                   "-dpl ", dpl,
                   "-m ", Re(n) / medium.index ,
                   Im(n) / medium.index , ...)
  
  ## append the parameters for this run to a file
  cat(file=params, paste(ii, dir, command, comment, "\n"), append=TRUE)
  # append command to be run in the shell
  cmd <- paste(adda, command, dir, "\n")
  cat(file=run, cmd, append=TRUE)
  
  # display string during testing phase
  if(run != "" && interactive()) 
    return(cmd)
}

adda_run()

## ----params--------------------------------------------------------------
params <- expand.grid(wavelength = seq(400, 600, length=30),
                      AR=seq(1, 2, by=0.2), dpl = c(1000, 3000, 5000))

params <- transform(params, ii=seq_along(wavelength), 
                    n=sqrt(epsAu(wavelength)$epsilon))
head(params)

## ----files---------------------------------------------------------------
## remove previous runs
unlink("tmp_*", recursive=TRUE)

## create the files
cat("#!/bin/bash\n", file="test.sh")
cat("# run parameters", format(Sys.time(), "%a %b %d %X %Y"), "\n", file="params.txt")
m_ply(params, adda_run, run="test.sh", int=" -int fcd -pol fcd ", comment="testing")

## ---- engine='bash', echo=FALSE------------------------------------------
system("head params.txt")

## ---- engine='bash', echo=FALSE------------------------------------------
system("head test.sh")

## ----run, engine='bash'--------------------------------------------------
system("chmod a+rwx test.sh ")# give exec permissions
system("./test.sh > log ")

## ----log-----------------------------------------------------------------
read_log <- function(f){
  file <- paste0(f, "/CrossSec-Y")
  tmp <- read.table(file, sep="=", head=FALSE, colClasses=c("NULL", "numeric"))
  xsecy <- data.frame(extinction = tmp[1,1], absorption=tmp[3,1])
  xsecy$ii <- as.numeric(gsub("tmp_([0-9]+)", "\\1", f))
  xsecy$pol <- "y"
  
  file <- paste0(f, "/CrossSec-X")
  if(file.exists(file)){
    tmp <- read.table(file, sep="=", head=FALSE, colClasses=c("NULL", "numeric"))
    xsecx <- data.frame(extinction = tmp[1,1], absorption=tmp[3,1])
    xsecx$ii <- as.numeric(gsub("tmp_([0-9]+)", "\\1", f))
    xsecx$pol <- "x"
    
  } else {
    xsecx <- mutate(xsecy, pol="x") # sphere: just copy the data
  }
  rbind(xsecx, xsecy)
}

results <- ldply(list.files(pattern="tmp_"), read_log)
results <- arrange(results, ii) # reorder by index
results <- merge(results, params, by.y="ii")
str(results)


## matalb

library(R.matlab)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(plyr)


tmp <- readMat('comp_adda_rho-1-2-auto.mat')

toplot <- function(tmp){
  nl <- length(c(tmp$lambda))
  input <- as.data.frame(setNames(lapply(tmp$input, c), 
                                  attr(tmp$input, "dimnames")[[1]]))
  nvar = NROW(input)
#   print(nvar)
  lambda <- rep(c(tmp$lambda), nvar)
  res <- cbind(data.frame(wavelength = lambda,
                    Cext = c(tmp$Cext),
                    Cabs = c(tmp$Cabs),
                    Csca = c(tmp$Csca)),
                    input[rep(seq_len(nvar), each=nl),])
  
  melt(res, meas=c('Cext','Cabs', 'Csca'))
  
}

dp <- toplot(tmp)

p <- ggplot(dp, aes(wavelength, value, colour=factor(rho),
                    linetype=variable)) +
  geom_line() + 
  scale_x_continuous(expand=c(0,0))+
  #   scale_y_continuous(expand=c(0,0))+
  labs(x = "wavelength /nm", y=expression(sigma/nm^2), 
       colour="aspect ratio", linetype='variable')+
  theme_bw() +
  theme(strip.background=element_rect(fill=NA))


## ----comparison, message=FALSE,warning=FALSE-----------------------------
library(mie)
library(ggplot2)

fine <- epsAu(seq(400, 600))
exact <- mie(fine$wavelength, fine$epsilon,
             radius = 10, medium = 1.0,
             efficiency = T)
str(exact)
exact$pol <- "y"

bm <- mutate(subset(dp, variable=="Cext"), AR=rho, extinction=value*1e-6, pol="x")

ggplot(exact, aes(wavelength, extinction)) +
  facet_wrap(~pol)+
  geom_line(data=bm, aes(colour=factor(AR))) +
#   geom_line(col="black") +
  geom_line(data=subset(results, dpl==3000), aes(colour=factor(AR),
                              group=interaction(dpl, AR)), lty=2)


