# get designer's collections
# It obtaines the images of the looks for each collection
### Libraries ####
library(rvest)
library(xml2)
library(RCurl)
library(httr)

### Variables ####
year <- 2018
# Seasons for menswear are: spring and fall
season <- c("spring", "fall")
designer <- c("hermes", "gucci", "prada", "dries-van-noten", "thom-browne", 
              "junya-watanabe", "lanvin", "fendi", "marni", "n21", "lemaire",
              "valentino", "neil-barrett", "haider-ackermann", "berluti", 
              "wooyoungmi")

### internal functions ####
get.looks <- function(vURL, vDes){
xsrc_all <- vector()
for(l in 1:100){
        xURL <- paste(vURL, l, sep="")
        xhtml <- read_html(xURL)
        xsrc <- xml_attr(xml_find_all(xhtml, ".//img"), "src")[3]
        if(!is.element(xsrc, xsrc_all)){
                xlook <- formatC(l, width = 2, format = "d",flag = "0")
                xfile <- paste(paste(vDes, "_", sep=""), xlook, ".jpeg", sep="")
                download.file(xsrc, destfile = xfile)
                xsrc_all <- c(xsrc_all, xsrc)
        }else{
                break()
        }
}
}
### Main function ####
get.collections.BOF <- function(Season, Year, Designer){
# The parts of the URL
# Base of the URL << string >>
vB <- "https://www.businessoffashion.com/fashion-week/"
# Season, either spring or fall << string >>
vS <-season
# Year, the << numeric>>
vY <- year
# Gender, either menswear of womenswear << string >>
vG <- "menswear" 
# The designer, please check that is spelled correctly << string >>
vD <- designer
# End of the URL << string >>
vT <- "/collection/look/"
#Assemble the URL
xgrid <- expand.grid(vS, vY, vD)
for (x in 1:nrow(xgrid)){
        vS <- as.character(xgrid[x,1])
        vY <- xgrid[x,2]
        vD <- as.character(xgrid[x,3])
        xmiddle <- paste(vG, paste(vY, vS, sep=""),  sep="-")
        URL <- paste(vB, xmiddle , paste("/", vD, sep=""), vT, sep="")
        xdir <- paste(vD, paste(substr(vS,1,1), substr(vY,3,4), sep=""), sep="_")
        xURL <- paste(URL, 1, sep="")
        xget <- GET(xURL)
        if(!(xget$status_code == 404)){
                setwd("~/Desktop/A/Tela/Designers/")
                if(!dir.exists(xdir)){
                        dir.create(xdir)
                                
                }
                setwd(xdir)
                get.looks(URL, xdir)
        }
}
}
get.collections.BOF(Season, Year, designer[1:4])        

