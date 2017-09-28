# Other possibilites:
#       Using Vogue as the source for the images
#       internal function
#       vURL is the URL for all the looks
#       vDes is the designer
vDes <- xdir
get.looks.vogue <- function(vURL, vDes){
        xsrc_all <- vector()
        n <- 2
        for(l in 1:100){
                xURL <- paste(vURL, l, sep="")
                xhtml <- read_html(xURL)
                xsrc <- xml_attr(xml_nodes(xhtml, xpath = '//img'), "srcset")
                        if(!is.element(xsrc, xsrc_all)){
                                xlook <- formatC(l, width = 2, format = "d",flag = "0")
                                xfile <- paste(paste(vDes, "_", sep=""), xlook, ".jpeg", sep="")
                                download.file(xsrc[n], destfile = xfile)
                                xsrc_all <- c(xsrc_all, xsrc[n])
                                n <- n + 1
                        }else{
                                break()
                        }
        }
}

Year <- year <- 2018
# Seasons for menswear are: spring and fall
Season <- season <- c("spring", "fall")
Dedigner <- designer <- c("hermes", "gucci", "prada", "dries-van-noten", "thom-browne", 
              "junya-watanabe", "lanvin", "fendi", "marni", "n21", "lemaire",
              "valentino", "neil-barrett", "haider-ackermann", "berluti", 
              "wooyoungmi")



#       Season, Year and Designer are vectors indicating the respective values
get.collections.Vogue <- function(Season, Year, Designer){
        # The parts of the URL
        # Base of the URL << string >>
        #http://www.vogue.com/fashion-shows/spring-2015-menswear/prada/slideshow/collection#1
        vB <- "http://www.vogue.com/fashion-shows/"
        # Season, either spring or fall << string >>
        vS <- Season
        # Year, the << numeric>>
        vY <- Year
        # Gender, either menswear of womenswear << string >>
        vG <- "menswear" 
        # The designer, please check that is spelled correctly << string >>
        vD <- Designer
        # End of the URL << string >>
        vT <- "/slideshow/collection#"
        #Assemble the URL
        xgrid <- expand.grid(vS, vY, vD)
        for (x in 1:nrow(xgrid)){
                vS <- as.character(xgrid[x,1])
                vY <- xgrid[x,2]
                vD <- as.character(xgrid[x,3])
                xmiddle <- paste(vS, vY, vG, sep="-")
                URL <- paste(vB, xmiddle , paste("/", vD, sep=""), vT, sep="")
                xdir <- paste(vD, paste(substr(vY,3,4), substr(vS,1,1), sep="_"), sep="_")
                xURL <- paste(URL, 1, sep="")
                xget <- GET(xURL)
                if(!(xget$status_code == 404)){
                        setwd("~/Desktop/A/Tela/Designers/")
                        if(!dir.exists(xdir)){
                                dir.create(xdir)
                        }
                        setwd(xdir)
                        get.looks.vogue(URL, xdir)
                }
        }
}
get.collections.Vogue(Season="spring", Year=2018, Designer="prada")        
