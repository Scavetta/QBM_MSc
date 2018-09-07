# Peak calling in a BAM file
# Rick Scavetta
# 00.09.2018

# Download and initalize package:
# source("http://bioconductor.org/biocLite.R")
# biocLite("Rsamtools") # Bioconductors version of install.packages
library(Rsamtools) # Initialize package
library(ggplot2)

# Define the parameters to import
parameters <- ScanBamParam(what = "isize")

data <- scanBam("sample.bam", param = parameters)
class(data)
names(data)
length(data)
class(data[[1]]$isize)

# Save insert sizes as a vector
raw_size <- data[[1]]$isize
typeof(raw_size)
class(raw_size)
rm(data)
rm(parameters)

# fulldata <- scanBam("sample.bam", param = parameters)

# Count positive and negative:
length(which(raw_size > 0))
sum(raw_size > 0) # because TRUE == 1
length(which(raw_size < 0))

# Tally positive and negative, different way:
table(cut(raw_size, c(-Inf, 0 , Inf)))

# Get only the positive insert size values:
raw_size <- raw_size[raw_size > 0]

# Tally the count sizes:
dist <- plyr::count(raw_size)

# Remove this large, unnecessary object
rm(raw_size)

# A little plotting trick:
plot(dist, type = "l")

# Save this data frame as a .txt file:
# write.table(dist, "dist.txt", sep = "\t", row.names = F)

#### part 1: Find peaks and minimia
# how to find peaks in an ordered vector:
# e.g.
# yy <- c(1:5,0:-4,0:7,3:1)
# 
# myDF <- data.frame(xx = seq_along(yy),
#                    yy = yy)
# plot(myDF)

# find peaks in
diff(sign(diff(dist$freq)))
# Peaks produce -2
# Minima produce 2

# So where are they? Which row in the data set are peak or minima rows?
which(diff(sign(diff(dist$freq))) == -2) + 1 # peaks
which(diff(sign(diff(dist$freq))) == 2) + 1 # minima

#### part 2: Plot with ggplot2:
ggplot(dist, aes(x, freq)) +
  geom_line() +
  geom_vline(xintercept = dist$x[lines],
             col = "grey80", linetype = 3) +
  scale_x_continuous(limits = c(20, 150)) +
  theme_classic()

# Generalize plotting and peak calling:
myPlot <- function(x, xRange = c(0,1000), type = NULL) {
  
  g <- ggplot(x, aes(x, freq)) +
    geom_line() +
    scale_x_continuous(limits = xRange) +
    theme_classic()
  
  if (is.null(type)) {
    g
  } else if (type == "peaks") {
    lines <- which(diff(sign(diff(x$freq))) == -2) + 1 # peaks
    
    g + 
      geom_vline(xintercept = x$x[lines],
                 col = "grey80", linetype = 3)
    
  } else if (type == "troughs") {
    lines <- which(diff(sign(diff(x$freq))) == 2) + 1 # peaks
    
    g + 
      geom_vline(xintercept = x$x[lines],
                 col = "grey80", linetype = 3)
    
  } else {
    message("Not this time :/")
  }
}

myPlot(dist, c(20, 150))
myPlot(dist, c(20, 150), "peaks")
myPlot(dist, c(20, 150), "troughs")
myPlot(dist, c(20, 150), "hello")

# Can we make it easier?
myPlot_v2 <- function(x, xRange = c(0,1000), type = NULL) {
  stopifnot(type %in% c("peaks", "troughs"))
  
  if (is.null(type)) {
    lines <- NULL
  } else if (type == "peaks") {
    lines <- which(diff(sign(diff(x$freq))) == -2) + 1 # peaks
  } else if (type == "troughs") {
    lines <- which(diff(sign(diff(x$freq))) == 2) + 1 # peaks
  } else {
    message("Not this time :/")
  }

  ggplot(x, aes(x, freq)) +
    geom_line() +
    scale_x_continuous(limits = xRange) +
    geom_vline(xintercept = x$x[lines],
               col = "grey80", linetype = 3) +
    theme_classic() 
}

myPlot_v2(dist, c(20, 150))
myPlot_v2(dist, c(20, 150), "peaks")
myPlot_v2(dist, c(20, 150), "troughs")
myPlot_v2(dist, c(20, 150), "hello")






