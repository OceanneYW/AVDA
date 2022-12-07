# Set working directory to path of this source file
tryCatch({
  setwd(getSrcDirectory()[1])
}, error= function (e) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
})

source("mds.R")
source("parse.R")


setwd("//Users/Oceanne.W/Desktop/ BINF ADVA /testdata")
tdf = read.csv(file = "countDFeByg.csv", TRUE, ",", na.strings = "") 
tdf2 = read.csv(file = "Test Hops Terpene.csv", TRUE, ",", na.strings = "") 
typeof(tdf)
str(tdf)
tdf <- parse(tdf)
tdf2 <- parse(tdf2)
p <- plotMds(tdf)
p

nms <- calcMds(tdf,"bray")

nmsPoints <- as.data.frame(nms$points)
nmsPoints$samp <- row.names(nmsPoints)


p <- ggplot() + geom_point(data = nmsPoints, aes(x = MDS1, y = MDS2), size = 2) +
  geom_text_repel(data = nmsPoints, aes(x = MDS1, y = MDS2,label = samp), size = 2)+
  theme_bw()
p

hc <- calcDendro(tdf)
hcB <- calcDendro(tdf, binary = T)

fit <- calcEnvfit(nms, tdf)
p <- plotEnvfit(fit, nms)
# pp <- appendEnvfitToPlot(fit, nms, )
p

pp <- ggplot()+ geom_segment(data = fit_df, aes(x=0, y=0, xend=NMDS1, yend=NMDS2), 
                   arrow = arrow())+
  geom_text_repel(aes(label=species))
pp
