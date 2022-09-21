png("flu_pred_2020_new17.png", width=886*2.5, height=558*2.5, res=300)

## prepare date variable
temp0 = train0_2020$dates[1:7]
for (i in 1:(length(new_dat0_origin_model_INLA$I)-7)){
  temp0 = c(temp0, tail(temp0,1) + 7)
}

plot(train0_2020$dates,train0_2020$I,ylim=c(0,5300),xlim=c(range(temp0)[1],range(temp0)[2]+0),type="p",xlab="Date",ylab="Weekly influenza cases reported",xaxt="n",lwd=2)
lines(train0_2020$dates[1:8],train0_2020$I[1:8],lwd=2)

lines(temp0[8:29],new_dat0_origin_model_INLA$I,col=8,lty=1,lwd=2)
lines(temp0[8:29],new_dat0_origin_model_0.025quan$I,col=8,lty=2,lwd=2)
lines(temp0[8:29],new_dat0_origin_model_0.975quan$I,col=8,lty=2,lwd=2)
axis(1, temp0, format(temp0, "%b %d \n%Y"), cex.axis = .8)
lines(temp0[8:11],c(train0_2020$I[8],exp(m.pois_new_second$summary.fitted.values$mean[4])*mean(adj_report_rate1_all),exp(tail(m.pois_new_second$summary.fitted.values$mean,2))*mean(adj_report_rate1_all)),col=2,lwd=2)
### simulated CI
lines(temp0[8:11],c(train0_2020$I[8],exp(m.pois_new_second$summary.fitted.values$`0.025quant`[4])*mean(adj_report_rate1_all),exp(tail(m.pois_new_second$summary.fitted.values$`0.025quant`,2))*mean(adj_report_rate1_all)),col=2,lwd=3,lty=2)
lines(temp0[8:11],c(train0_2020$I[8],exp(m.pois_new_second$summary.fitted.values$`0.975quant`[4])*mean(adj_report_rate1_all),exp(tail(m.pois_new_second$summary.fitted.values$`0.975quant`,2))*mean(adj_report_rate1_all)),col=2,lwd=3,lty=2)

lines(temp0[11:14],c(exp(tail(m.pois_new_second$summary.fitted.values$mean,1))*mean(adj_report_rate1_all),exp(tail(m.pois_new_third$summary.fitted.values$mean,3))*mean(adj_report_rate2_all)),col=3,lwd=2)
## simulated CI
lines(temp0[11:14],c(exp(tail(m.pois_new_second$summary.fitted.values$"0.025quant",1))*mean(adj_report_rate1_all),exp(tail(m.pois_new_third$summary.fitted.values$"0.025quant",3))*mean(adj_report_rate2_all)),col=3,lwd=3,lty=2)
lines(temp0[11:14],c(exp(tail(m.pois_new_second$summary.fitted.values$"0.975quant",1))*mean(adj_report_rate1_all),exp(tail(m.pois_new_third$summary.fitted.values$"0.975quant",3))*mean(adj_report_rate2_all)),col=3,lwd=3,lty=2)


abline(v=as.Date("2019-11-24"),lty=2)
abline(v=as.Date("2020-01-12"),lty=2)
abline(v=as.Date("2020-02-02"),lty=2)
abline(v=as.Date("2020-02-23"),lty=2)
arrows(x0 = as.Date("2019-11-25"),x1 = as.Date("2020-01-11"),y0 = 5200,y1 = 5200,length = 0.1,code = 3)
text(x = as.Date("2019-12-15"),y = 5000,labels = "Phase 1",cex=0.75)
arrows(x0 = as.Date("2020-01-13"),x1 = as.Date("2020-02-01"),y0 = 5200,y1 = 5200,length = 0.1,code = 3)
text(x = as.Date("2020-01-23"),y = 5000,labels = "Phase 2",cex=0.75)
arrows(x0 = as.Date("2020-02-03"),x1 = as.Date("2020-02-22"),y0 = 5200,y1 = 5200,length = 0.1,code = 3)
text(x = as.Date("2020-02-13"),y = 5000,labels = "Phase 3",cex=0.75)
text(x=as.Date("2020-01-22"),y = c(4500),labels = bquote(Phi[2] == .((1-round(R0_2nd_mean/mean(R0_1st_mean_all),digits = 3))*100)*"%"),cex=0.7,col=1)
text(x=as.Date("2020-01-22"),y = c(4200),labels = paste(" (",(1-round(R0_2nd_975/quantile(R0_1st_mean_all,probs = 0.025),digits = 3))*100,"%",",",(1-round(R0_2nd_025/quantile(R0_1st_mean_all,probs = 0.975),digits = 3))*100,"%",")",sep = ""),cex=0.7,col=1)
text(x=as.Date("2020-02-13"),y = c(4500),labels = bquote(Phi[3] == .((1-round(R0_3rd_mean/mean(R0_1st_mean_all),digits = 3))*100)*"%"),cex=0.7,col=1)
text(x=as.Date("2020-02-13"),y = c(4200),labels = paste(" (",(1-round(R0_3rd_975/quantile(R0_1st_mean_all,probs = 0.025),digits = 3))*100,"%",",",(1-round(R0_3rd_025/quantile(R0_1st_mean_all,probs = 0.975),digits = 3))*100,"%",")",sep = ""),cex=0.7,col=1)
text(x=as.Date("2019-11-26"),y = 600,labels = bquote(""*R[t]*" = "*.(round(Rt_all_over_p_all[1,1],digits = 2))),cex=0.7,col=1)
points(train0_2020$dates[c(1,8,11,14)],train0_2020$I[c(1,8,11,14)],pch=16,lwd=2,cex=1.4)
legend("topright",legend = c("Reported influenza cases","Model prediction in Phase 2","Model prediction in Phase 3","Model projection without intervention"),col=c(1,2,3,8),lty = c(1,1,1,1),pch = c(1,NA,NA,NA),cex=0.64,lwd = c(1,2,2,2))


### Rt during ordinary period to predict cases at Jan 12, 2020
text(x = as.Date("2020-01-07"),2320,round((1-new_dat0_origin_model_INLA[2,"cumsum_yminus_over_N"]/mean(ori_report_rate_all))*exp((m.pois$summary.fixed$mean[1])/tscal0),digits = 2),cex=0.7,col=1)
text(x = as.Date("2019-12-31"),2300,bquote(R[t]*" = "),cex=0.7,col=1)

arrows(x0 = as.Date("2020-01-11"),y0 = 2300,x1 = as.Date("2020-01-14"),y1 = 2300,length = 0.05)

## Rt to predict cases at Jan 12, 2020 with COVID-19
text(x = as.Date("2020-01-17"),2320,round(Rt_all_over_p[8],digits = 2),cex=0.7,col=1)

## Rt to predict cases at Feb 9, 2020 based on phase 2 situation
text(x = as.Date("2020-01-29"),1520,round((1-new_2020_dat0$cumsum_yminus_over_N_p[4])*mean(R0_1st_mean_all)*R0_2nd_mean/mean(R0_1st_mean_all),digits = 2),cex=0.7,col=1)
text(x = as.Date("2020-01-23"),1500,bquote(R[t]*" = "),cex=0.7,col=1)

arrows(x0 = as.Date("2020-02-01"),y0 = 1500,x1 = as.Date("2020-02-04"),y1 = 1500,length = 0.05)

## Rt to predict cases at Feb 9, 2020 based on phase 3 situation
text(x = as.Date("2020-02-09"),1520,round(Rt_all_over_p[11],digits = 2),cex=0.7,col=1)
text(x = as.Date("2020-02-20"),420,round(Rt_all_over_p[14],digits = 2),cex=0.7,col=1)
text(x = as.Date("2020-02-15"),400,bquote(R[t]*" = "),cex=0.7,col=1)
text(x = as.Date("2019-12-15"),y = 5800,labels = "Ordinary",cex=0.75,xpd=T)
text(x = as.Date("2020-01-23"),y = 5800,labels = "Awareness",cex=0.75,xpd=T)
text(x = as.Date("2020-02-13"),y = 5800,labels = "Spreading",cex=0.75,xpd=T)


dev.off()
