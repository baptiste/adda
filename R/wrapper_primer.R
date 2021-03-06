
## @knitr invisible, echo=FALSE, results='hide'
library(knitr)
opts_chunk$set(cache=TRUE, fig.width=10, tidy=FALSE)
library(ggplot2)
theme_set(theme_minimal() + theme(panel.background=element_rect(fill=NA)))


## @knitr setup
library(dielectric) # dielectric function of Au and Ag
library(plyr) # convenient functions to loop over parameters
library(reshape2) # reshaping data from/to long format
library(ggplot2) # plotting framework


## @knitr wrapper
adda_spectrum <- function(shape = "ellipsoid",
                         euler = c(0, 0, 0),
                         AR = 1.3,
                         wavelength = 500,
                         radius = 20,
                         n = 1.5 + 0.2i ,
                         medium.index = 1.46,
                         dpl = ceiling(min(50, 20 * abs(n))),
                         test = TRUE, verbose=TRUE, ...) {
  
  command <- paste("echo ../adda/src/seq/adda -shape ", shape, 1/AR, 1/AR,
                   "-orient ", paste(euler, collapse=" "),
                   "-lambda ", wavelength*1e-3 / medium.index ,
                   "-dpl ", dpl,
                   "-size ", 2 * radius*1e-3,
                   "-m ", Re(n) / medium.index ,
                   Im(n) / medium.index , ...)
  
  if(verbose)
    message(system(command, intern=TRUE))
  
  if(test) return() # don't actually run the command
  
  # extract the results of interest
  resultadda <- system(paste(command, "| bash"), intern = TRUE)
  Cext <- as.numeric(unlist(strsplit(grep("Cext",resultadda,val=T)[1:2],s="="))[c(2,4)])
  Cabs <- as.numeric(unlist(strsplit(grep("Cabs",resultadda,val=T)[1:2],s="="))[c(2,4)])
  Csca <- Cext - Cabs
  
  c(Cext[1], Cext[2], Cabs[1], Cabs[2], Csca[1], Csca[2])
}

# testing that it works
adda_spectrum(test = FALSE)


## @knitr basic
gold <- epsAu(seq(400, 700, length=100))
str(gold)

## empty matrix to store the results
results <- matrix(ncol=6, nrow=nrow(gold))

## loop over the wavelengths
for( ii in 1:nrow(gold) ){
  results[ii, ] <- adda_spectrum(wavelength = gold$wavelength[ii],
                                 n = sqrt(gold$epsilon[ii]),
                                 radius = 20, AR = 1.3, dpl=50,
                                 test = FALSE, verbose = FALSE)  
}

str(results)

## basic plot
matplot(gold$wavelength, results, 
        type = "l", col = rep(1:3, each=2),
        lty = rep(1:2, 3),
        xlab = "Wavelength /nm",
        ylab = expression("Cross-sections /"*nm^2),
        main = "Au ellipsoid")

legend("topleft", legend=expression(sigma[ext],sigma[abs],sigma[sca], "",
                                     "x-polarisation", "y-polarisation"),
       inset=0.01, col=c(1:3, NA, 1, 1), lty=c(1,1,1, NA, 1, 2), 
       bg  = "grey95", box.col=NA)


## @knitr simulation, fig.height=4
gold <- epsAu(seq(400, 700, length=200))
         
simulation <- function(radius = 20, AR = 1.3, ..., material=gold){
  params <- data.frame(wavelength = material$wavelength,
                       n = sqrt(material$epsilon),
                       radius = radius,
                       AR = AR)
  results <- mdply(params, adda_spectrum, ..., test=FALSE)
  
  m <- melt(results, measure.vars = c("V1","V2","V3","V4","V5","V6"))
  m$polarisation <- m$type <- factor(m$variable)
  
  levels(m$polarisation) <- list(x = c('V1','V3','V5'), 
                                 y = c('V2','V4','V6'))
  levels(m$type) <- list(extinction = c('V1','V2'), 
                         absorption = c('V3','V4'),
                         scattering = c('V5','V6'))
  m
}

test <- simulation(radius = 20, AR = 1.3, verbose = FALSE)
str(test)

qplot(wavelength, value, colour = polarisation, 
      facets = ~ type, data = test, geom = 'line')


## @knitr multiple
params <- expand.grid(radius = c(20, 22),
                      AR = c(1.2, 1.3))

all <- mdply(params, simulation, verbose = FALSE)

ggplot(all, aes(wavelength, value, 
                linetype = polarisation, colour = factor(AR),
                group = interaction(polarisation, AR))) +
  facet_grid(type~radius, scales='free') +
  geom_line() + labs(x = 'wavelength /nm', 
                     y = expression(sigma/nm^2),
                     colour = 'aspect ratio') +
  scale_colour_brewer(palette = 'Set1')


