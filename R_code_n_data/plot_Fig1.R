## function required
fig_label <- function(text, region="figure", pos="topleft", offset0=0, cex=NULL, ...) {
  
  region <- match.arg(region, c("figure", "plot", "device"))
  pos <- match.arg(pos, c("topleft", "top", "topright", 
                          "left", "center", "right", 
                          "bottomleft", "bottom", "bottomright"))
  
  if(region %in% c("figure", "device")) {
    ds <- dev.size("in")
    # xy coordinates of device corners in user coordinates
    x <- grconvertX(c(0, ds[1]), from="in", to="user")
    y <- grconvertY(c(0, ds[2]), from="in", to="user")
    
    # fragment of the device we use to plot
    if(region == "figure") {
      # account for the fragment of the device that 
      # the figure is using
      fig <- par("fig")
      dx <- (x[2] - x[1])
      dy <- (y[2] - y[1])
      x <- x[1] + dx * fig[1:2]
      y <- y[1] + dy * fig[3:4]
    } 
  }
  
  # much simpler if in plotting region
  if(region == "plot") {
    u <- par("usr")
    x <- u[1:2]
    y <- u[3:4]
  }
  
  sw <- strwidth(text, cex=cex) * 60/100
  sh <- strheight(text, cex=cex) * 60/100
  
  x1 <- switch(pos,
               topleft     =x[1] + sw, 
               left        =x[1] + sw,
               bottomleft  =x[1] + sw,
               top         =(x[1] + x[2])/2,
               center      =(x[1] + x[2])/2,
               bottom      =(x[1] + x[2])/2,
               topright    =x[2] - sw,
               right       =x[2] - sw,
               bottomright =x[2] - sw)
  
  y1 <- switch(pos,
               topleft     =y[2] - sh + offset0,
               top         =y[2] - sh,
               topright    =y[2] - sh,
               left        =(y[1] + y[2])/2,
               center      =(y[1] + y[2])/2,
               right       =(y[1] + y[2])/2,
               bottomleft  =y[1] + sh,
               bottom      =y[1] + sh,
               bottomright =y[1] + sh)
  
  old.par <- par(xpd=NA)
  on.exit(par(old.par))
  
  text(x1, y1, text, cex=cex, ...)
  return(invisible(c(x,y)))
}

## plot the flu case overview
png("fig1_v6.png", width=886*2.5, height=558*2.5*1.5, res=300)
par(mfrow = c(2,1),
    oma = c(5,4,0,0) + 0.1,
    mar = c(0,0,1,1) + 0.1,
    mgp = c(2, 1, 0),
    xpd = NA)

plot(train0$dates,train0$I,type="l",ylab="Weekly influenza cases",lwd=2,xlab=NA)

rect(xleft=range(train0_2016$dates)[1],xright = range(train0_2016$dates)[2],ybottom=par("usr")[3], ytop=par("usr")[4], col = rgb(1,0.65,0,alpha=0.2),border = NA)
rect(xleft=range(train0_2018$dates)[1],xright = range(train0_2018$dates)[2],ybottom=par("usr")[3], ytop=par("usr")[4], col = rgb(1,0.65,0,alpha=0.2),border = NA)
rect(xleft=range(train0_2019$dates)[1],xright = range(train0_2019$dates)[2],ybottom=par("usr")[3], ytop=par("usr")[4], col = rgb(1,0.65,0,alpha=0.2),border = NA)
rect(xleft=range(train0_2020$dates)[1],xright = range(train0_2020$dates)[2],ybottom=par("usr")[3], ytop=par("usr")[4], col = rgb(1,0.65,0,alpha=0.2),border = NA)
lines(train0_2020$dates[9:15],train0_2020$I[9:15],lwd=3,col="red")
legend("topleft",legend = c("Number of influenza cases","Influenza seasons considered","Period with behavioural changes"),col=c(1,rgb(1,0.65,0,alpha=0.2),"red"),lwd=c(2,15,3),lty=1,cex=0.8)
fig_label(LETTERS[1],cex = 2)

##### plot severe cases situation
plot(dat0$date,dat0$severe_case,type="l",xlab="Time",ylab="Weekly severe influenza cases")#,main="Severe influenza case situation")
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "white")
lines(dat0$date,dat0$severe_case)
fig_label(LETTERS[2],cex = 2,offset0 = 20)

dev.off()

