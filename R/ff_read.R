setwd("~/Documents/plasmonics/spheroids/code/Studies")
library(R.matlab)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(plyr)


tmp <- readMat('a0-15-20-rho-1-2-material-1-2-medium-1-1.33-auto.mat')

toplot <- function(tmp){
  nl <- nrow(tmp$lambda)
  input <- as.data.frame(setNames(lapply(tmp$input, c), 
                                  attr(tmp$input, "dimnames")[[1]]))
  nvar = nrow(input)
  lambda <- rep(c(tmp$lambda), nvar)
  res <- data.frame(wavelength = lambda,
                    Cext = c(tmp$Cext),
                    Cabs = c(tmp$Cabs),
                    Csca = c(tmp$Csca),
                    input[rep(seq_len(nvar), each=nl),])
  
  melt(res, meas=c('Cext','Cabs', 'Csca'))
  
}

dp <- toplot(tmp)

silver <- subset(dp,material == 1)
gold <- subset(dp,material == 2)

p <- ggplot(dp, aes(wavelength, value, colour=factor(rho),
                    linetype=variable)) +
  facet_wrap(~material+medium+a0, scales="free", ncol=2)+
  geom_line() + 
  scale_x_continuous(expand=c(0,0))+
#   scale_y_continuous(expand=c(0,0))+
  labs(x = "wavelength /nm", y=expression(sigma/nm^2), 
       colour="aspect ratio", linetype='variable')+
  theme_bw() +
  theme(strip.background=element_rect(fill=NA))

grid.arrange(p %+% silver, p %+% gold, ncol=2)
