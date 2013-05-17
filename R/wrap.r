adda_spectrum = function(shape = "ellipsoid",
                         AR = 1.1,
                         wavelength = 500,
                         radius = 10,
                         n = 1.5 + 0.2i ,
                         medium.index = 1.46,
                         dpl = ceiling(10 * abs(n)),
                         test = TRUE, verbose=TRUE) {
  command <-paste("echo ../src/seq/adda -shape ", shape, AR, AR,
                  "-lambda ", wavelength*1e-3 / medium.index ,
                  "-dpl ", dpl,
                  "-size ", 2 * radius*1e-3,
                  "-m ", Re(n) / medium.index ,
                  Im(n) / medium.index )
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

