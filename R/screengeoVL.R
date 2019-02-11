#' Calculate degrees of visual angle
#' 
#' @param stimX Stimulus width in pixels
#' @param stimY Stimulus height in pixels
#' @param dist Distance from screen to user
#' @param distUnit Measurement unit for dist
#' @param scrX screen width
#' @param scrY Screen height
#' @param scrUnit Measurement unit for scrX and scrY
#' @param resX Screen resolution (horizontal)
#' @param resY Screen resolution (vertical)
#' @return A list with the visual angles in radian and in degrees
#' @export

DVA_VL<-function(stimX,stimY,dist,distUnit="cm",scrX,scrY,scrUnit="mm",resX,resY){
  switch(scrUnit,
         mm={
           scrXmm<-scrX
           scrYmm<-scrY
         },cm={
           scrXmm<-scrX*10
           scrYmm<-scrY*10
         },inch={
           scrXmm<-scrX*10*2.54
           scrYmm<-scrY*10*2.54
         },{
           scrXmm<-scrX
           scrYmm<-scrY})
  switch(distUnit,
         mm={
           distmm<-dist
         },cm={
           distmm<-dist*10
         },m={
           distmm<-dist*1000
         },inch={
           distmm<-dist*10*2.54
         },feet={
           distmm<-dist*10*2.54*12
         },{distmm<-dist*10})
  stimXmm<-stimX*scrXmm/resX
  vaX<-2*atan(x = stimXmm/(2*distmm))
  vaXdeg<-vaX*360/(2*pi)
  stimYmm<-stimY*scrYmm/resY
  vaY<-2*atan(x = stimYmm/(2*distmm))
  vaYdeg<-vaY*360/(2*pi)
  return(list(stimXmm=stimXmm,stimYmm=stimYmm,vaX=vaX,vaXdeg=vaXdeg,vaY=vaY,vaYdeg=vaYdeg))
}

#' Calculate area of the complex hull envelopping a visual stimulus
#' 
#' @param x vector of x coordinates of pixels
#' @param y vector of y coordinates of pixels
#' @return a list with the points component containing the points defining the hull and a scalar component containing the area of the hull.
#' @export
AreaHull<-function(x,y){
  getSteps <- function(x,y) {
    d <- complex(real = x-x[1], imaginary = y-y[1])
    Arg(d) %% (2*pi)
  }
  ccw<-function(p1,p2,p3){
    (p2[1] - p1[1])*(p3[2] - p1[2]) - (p2[2] - p1[2])*(p3[1] - p1[1])
  }
  
  NN<-length(x)
  uu<-matrix(data = c(x,y),nrow = length(x),byrow = F)
  uu<-uu[order(uu[,2]),]
  uu<-uu[order(getSteps(uu[,1],uu[,2])),]
  uu<-rbind(uu[NN,],uu)
  MM<-2
  for(ii in 3:(NN+1)){
    itemp<-ii
    continueFlag<-T
    while(!(ccw(p1 = uu[MM-1,],p2 = uu[MM,],p3 = uu[itemp,])>0)&&continueFlag){
      if(MM>2){
        MM<-MM-1
      }else{
        if(itemp==NN+1){
          continueFlag<-F
        }else{
          itemp<-itemp+1
        }
      }
    }
    MM<-MM+1
    tempi<-uu[MM,]
    uu[MM,]<-uu[itemp,]
    uu[itemp,]<-tempi
  }
  points<-uu[2:(MM),]
  areaPol<-sum((points[,1]*c(points[2:(MM-1),2],points[1,2]))-(points[,2]*c(points[2:(MM-1),1],points[1,1])))/2
  list(points=points,area=areaPol)
}