#' skewT.plot
#'
#' plot an empty skewT graph 
#' @param p_min
#' @param p_max
#' @param T_min
#' @param T_max
#' @keywords skewT
#' @export
#' @examples
#' skewT.plot()
skewT.plot<-function(p_min,p_max,T_min,T_max){

   CTE<-skewT.ab(T_min,T_max,p_max,p_min)
   
   
   # Empty plot
   plot(1, log="y",xlim=c(T_min,T_max),ylim=c(p_max,p_min),axe=FALSE,xlab="",ylab="")
   axis(1,ylim=c(T_min,T_max))
   mtext("T (C)",side=1,line=2.5)
   
   #isobars 
   p_plot<-c(1050, 1000, 850, 700, 500, 400, 300, 250, 200, 150, 100)
   abline(h=p_plot,lty=2,col="darkorange")
   axis(2,labels = p_plot,at = p_plot)
   mtext("P (hPa)",side=2,line=2.5,srt = 0)
   
   
   #isotherms
   T_plot<-seq(-80,80,10)
   for (i in 1:length(T_plot)) {
      tmp_x<-skewT.x(CTE$a,CTE$b,CTE$origin,T_plot[i],p_plot)
      lines(tmp_x,p_plot,col="darkorange") 
   }
   #isotherm 0 in black
   tmp_x<-skewT.x(CTE$a,CTE$b,CTE$origin,0,p_plot)
   lines(tmp_x,p_plot,col="black") 
   
   # iso saturation mr 
   mr_plot<-c(20,12,8,5,3,2,1)
   for (i in 1:length(mr_plot)) {
      tmp<-meteorology.Tmrp(mr_plot[i], p_plot[1:6])
      tmp_x<-skewT.x(CTE$a,CTE$b,CTE$origin,tmp,p_plot[1:6])
      lines(tmp_x,p_plot[1:6],lty=2,col="darkorange")   
      y_text<-1050
      x_text <- skewT.x(CTE$a,CTE$b,CTE$origin,meteorology.Tmrp(mr_plot[i],y_text),y_text)
      text(x_text, y_text, labels = as.character(mr_plot[i]),col="darkorange",srt = 45,adj=0.5,cex=0.75)
   }

   #iso dry adiabiatic
   tmp_p <- seq(p_min,p_max,10)
   for (i in 1:length(T_plot)) {
      tmp_x<-skewT.x(CTE$a,CTE$b,CTE$origin,meteorology.Tda(T_plot[i],tmp_p),tmp_p)
      lines(tmp_x,tmp_p, lty = 1, col = "brown")
	  y_text<-200
      x_text <- skewT.x(CTE$a,CTE$b,CTE$origin,meteorology.Tda(T_plot[i],y_text),y_text)
      text(x_text, y_text, labels = as.character(T_plot[i]),col="brown",srt =-45,adj=0.5,cex=0.75)

   }

   #iso moist adiabatic
   tmp_p <- seq(p_min,p_max,10)
   Tpseudo<-c(5,10,15,20,25,30)
   tmp_x<-matrix(NA,nr=length(tmp_p),nc=length(Tpseudo))
   for(j in 1:length(tmp_p)) {
      moist<-meteorology.moistadiab(Tpseudo, tmp_p[j])
      tmp_x[j,]<-skewT.x(CTE$a,CTE$b,CTE$origin,moist,tmp_p[j])
	  
   }
   for (i in 1:length(Tpseudo)) {
      lines(tmp_x[,i],tmp_p, lty = 1, col = "forestgreen")
      y_text<-tmp_p[which(tmp_p==250)]
      x_text<-tmp_x[which(tmp_p==250),i]
      text(x_text, y_text, labels = as.character(Tpseudo[i]), col = "forestgreen",srt=0,adj=0.5,cex=0.75)
   }	  
}
