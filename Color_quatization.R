# Color quatization for DVN SS18
# Required libraries
library(jpeg)
library(grid)
library(gridExtra)

# Internal functions
# CQ == Color quantization, retreives the n main solid colors in the picture
CQ <- function(vimage, n){
        col_vctr <- vector()
        xdf <- data.frame(
                red = matrix(vimage[,,1], ncol=1),
                green = matrix(vimage[,,2], ncol=1),
                blue = matrix(vimage[,,3], ncol=1))
        # Estimate the K means
        K = kmeans(xdf,n)
        col_vctr <- rgb(K$centers)
        col_vctr
}
# heat plots a heatmap with the colors in each outfit
heat <- function(vA){
        x <- rep(1:dim(vA)[2], 10)
        y <- rep(1:dim(vA)[2], each=dim(vA)[1])
        plot(x,y, pch=15, cex=8, col=vA)
}

# The script
setwd("~/Desktop/A/Tela/Designers/DVN/SS18_M/Looks_cut/")
files <- dir()
looks_df <- matrix(nrow=length(dir()), ncol=10)
for(f in seq_along(files)){
        xlook <- readJPEG(files[f])
        looks_df[f,] <- sort(CQ(xlook, 10))
}
heat(looks_df)

