library(plotrix)

mask0 = c(45.5,75,98)
socialD0 = c(37.9, 61, 90)



testdf<-data.frame(rbind(socialD0 = c(37.9, 61, 90),mask0 = c(45.5,75,98)))
row.names(testdf) <- c("Avoiding               \ncrowded place (%)", "      Using face masks (%)")
colnames(testdf)<-c("Phase 1\n(baseline)\n", "Phase 2\n(survey 1 of\nCowling et al.)", "Phase 3\n(survey 2 of\nCowling et al.)")



png("behav_ch_v4.png", width=886*2.5, height=640*2.5, res=300)

plot(mask0,type="b",xaxt="n",xlab="Phase",ylab="Percentage of adoption (%)",ylim=c(range(c(mask0,socialD0))))#,main="Behavioural changes across different phases \nin 2020 winter influenza season")
# points(socialD0,pch = 2)
lines(socialD0,lty=2,type="b",pch=2)
axis(1,at=1:3,labels = c(1,2,3))
legend("bottomright",legend = c("Avoiding crowded place", "Using face masks"),lty=c(2,1),pch=c(24,21),pt.bg = 'white',cex=0.8)

addtable2plot(x="topleft",table = testdf,bty="n",display.rownames=TRUE,hlines=T,vlines=T,cex = 0.6,xpad = 0.1,ypad = 0.2,xjust = 0)

# legend(
#   'topleft', ncol = 3,
#   legend = c(
# 
#     "Phase 1 \n(baseline)", testdf[1,1],testdf[2,1] ,
#     "Phase 2\n(survey 1 of \nCowling et al.)", testdf[1,2], testdf[2,2],
#     "Phase 3\n(survey 2 of \nCowling et al.)", testdf[1,3], testdf[2,3]
#   ), cex=0.6 , bty = "n", x.intersp = 0,inset=c(0.2,-.03)
# )

# legend( "topleft",   legend = c('   ', "Avoiding \ncrowded place (%)", "Using face masks (%)"),cex=0.6,bty="n",inset = c(0,-.011),x.intersp = 0,y.intersp = 2)

dev.off()




# show most of the options
addtable2plot(1 ,90,testdf,bty="o",display.rownames=TRUE,hlines=F,
              vlines=T,title="The table",bty="n")
