require(dielectric)
require(plyr)
require(ggplot2)
adda_run <- function(ii=1, wavelength=500, m=sqrt(epsAu(wavelength)$epsilon), 
                     size = 20, grid=10, ..., 
                     run="", params = "params.txt", comment="",
                     adda = "/Users/baptiste/Documents/github/adda/src/seq/adda"){
  
  part <- paste0(" -size ", size*1e-3, " -grid ", grid)
  dispersion <- paste(" -lambda ", wavelength*1e-3, "-m ", Re(m), Im(m), collapse="")
  dir <- paste0(" -dir tmp_", ii)
  
  cat(file=params, paste(ii, dir, part, dispersion, ..., comment, "\n"), append=TRUE)
  cmd <- paste0(adda, part, dir, dispersion, ..., "\n")
  cat(file=run, cmd, append=TRUE)
  if(run != "") 
    cmd
}

adda_run()

params <- data.frame(wavelength = seq(400, 600, length=50))
params <- transform(params, ii=seq_along(wavelength), m=sqrt(epsAu(wavelength)$epsilon))
unlink("tmp_*", recursive=TRUE)

cat("#!/bin/bash\n", file="test.sh")
cat("# run parameters\n", file="params.txt")
m_ply(params, adda_run, run="test.sh", int=" -int fcd ", comment="testing")

system("chmod a+rwx test.sh")
system("./test.sh")

read_log <- function(f){
  file <- paste0(f, "/CrossSec-Y")
  tmp <- read.table(file, sep="=", head=FALSE, colClasses=c("NULL", "numeric"))
  xsec <- data.frame(extinction = tmp[2,1], absorption=tmp[4,1])
  xsec$folder <- as.numeric(gsub("tmp_([0-9]+)", "\\1", f))
  xsec
}

results <- ldply(list.files(pattern="tmp_"), read_log)
results <- arrange(results, folder)
results$wavelength <- params$wavelength

library(mie)

simulation <- function(radius = 5, material="silver", 
                       wavelength=seq(300, 400, length=200), ...){
  
  metal <- if(material == "gold") epsAu(wavelength) else epsAg(wavelength)
  
  res <- mie_bh(metal$wavelength, metal$epsilon, epsilon.coating = 1.0^2,
                radius = radius, thickness = 0, medium = 1.0,
                efficiency = TRUE)
  
  res
}

params2 <- expand.grid(radius=10, wavelength=seq(400, 600),
                      material = c("gold"), stringsAsFactors=FALSE)

all <- mdply(params2, simulation, .progress="text")

str(all)
p <- ggplot(all, aes(wavelength, extinction))+
  geom_line() +
  geom_line(data=results, linetype=2)

print(p)