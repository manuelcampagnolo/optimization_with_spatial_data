#################################################################################################
#
# sessão 10: optimização (conclusão); interpolação espacial
#
##################################################################################################

################################################################################
#
# problema caixeiro viajante para as áreas protegidas
#
# pasta de trabalho
wd<-"Y:\\Aulas\\sigs_com_R\\dados_aulas"
aulas<-"Y:\\Aulas\\sigs_com_R"
setwd(wd)

library(sp)
library(rgeos)
library(rgdal)
library(raster) #contour
library(TSP)
library(RColorBrewer) # para criar paletas
#library(gdalUtils) #gdalwarp
#library(rasterVis) # plot3D
#library(rgl)
# ler shapefile de areas protegidas (ICNF)
icnf<-readOGR(dsn=getwd(),layer="AP_JUL_2014",encoding="ISO8859-1")
valid<-gIsValid(icnf,byid=TRUE,reason=TRUE)
icnfv<-icnf[which(valid=="Valid Geometry"),]
# matrix com colunas "x" e "y"
xy <- coordinates(gPointOnSurface(icnfv,byid=TRUE))
N<-nrow(xy)
etrs<-"+proj=tmerc +lat_0=39.6682583 +lon_0=-8.1331083 +k=1 +x_0=0 +y_0=0 +ellps=GRS80 +units=m"
xy.sp <- SpatialPoints(xy, proj4string=CRS(etrs))

#points(xy.sp,col="red")
# devolve matrix
d<-gDistance(xy.sp,xy.sp,byid=TRUE) # demora uns 2mn a calcular
colnames(d)<-as.character(icnfv@data$NOME)
rownames(d)<-as.character(icnfv@data$NOME)
# criar STSP
d.tsp<-as.TSP(d)
# resolver; devolve objecto de classe TOUR
ap.tsp<-solve_TSP(x=d.tsp)
# vector de indices da solução 
tour<-c(as.integer(ap.tsp),as.integer(ap.tsp)[1])
# plot
plot(icnfv,col=brewer.pal(n=9, "Spectral"))
lines(x=xy[tour,"x"],y=xy[tour,"y"],col="blue")
text(x=mean(range(xy[,"x"])), y=quantile(range(xy[,"y"]),.3), paste(round(attr(ap.tsp,"tour_length")/1000,2),"Km"),pos=4)
