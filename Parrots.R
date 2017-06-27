library(imager)
# upload the the image of the parrots
im <- load.example('parrots')
plot(im)
# transform the image to a grayscale
im_grey <- im %>% grayscale
plot(im_grey)

#Select pixels with high luminance
px <- im > .7
plot(px)


im <- grayscale(boats)
px <- im > .85
plot(im)
highlight(bbox(px))
highlight(px,col="green")
crop.bbox(im,px) %>% plot
