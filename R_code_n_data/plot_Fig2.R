png("flu_pred_2020_new16.png", width=886*2.5, height=558*2.5, res=300)

# palette("R3")

## prepare date variable
temp0 = train0_2020$dates[1:7]
for (i in 1:(length(new_dat0_origin_model_INLA$I)-7)){
  temp0 = c(temp0, tail(temp0,1) + 7)
}

plot(train0_2020$dates,train0_2020$I,ylim=c(0,5300),xlim=c(range(temp0)[1],range(temp0)[2]+0),type="p",xlab="Date",ylab="Weekly influenza cases reported",xaxt="n",lwd=2)
lines(train0_2020$dates[1:8],train0_2020$I[1:8],lwd=2)

# legend("topright",legend = c("real influenza cases reported","model prediction for ordinary influenza without COVID-19",expression("              adjusting C"[]*" to obtain best fit line (two phases)")),col=c(1,8,3),lty = c(1,1,1),pch = c(1,NA,NA),cex=0.64,lwd = c(1,2,2))
# legend(x = temp0[15],y=4710,legend = "",col = 4,lty = 1,pch = 4,cex = 0.65,lwd=2,bty = "n")

# legend("topright",legend = c("real flu cases reported","model prediction for ordinary flu without COVID-19"),col=c(1,2),lty = c(1,1),pch = c(1,1),cex=0.7,lwd = c(1,1))

### use date as axis instead of week number
# axis(side = 1,at = seq(1,28,by = 5),labels = date00[seq(1,28,by = 5)])

# lines(new_dat0_origin_model_INLA$row_name,new_dat0_origin_model_INLA$I,col=2,lty=1)
# points(new_dat0_origin_model_INLA$row_name,new_dat0_origin_model_INLA$I,col=2)
# lines(new_dat0_origin_model_0.025quan$row_name,new_dat0_origin_model_0.025quan$I,col=2,lty=2)
# lines(new_dat0_origin_model_0.975quan$row_name,new_dat0_origin_model_0.975quan$I,col=2,lty=2)

lines(temp0[8:29],new_dat0_origin_model_INLA$I,col=8,lty=1,lwd=2)
# points(temp0[8:29],new_dat0_origin_model_INLA$I,col=8,lwd=2)

lines(temp0[8:29],new_dat0_origin_model_0.025quan$I,col=8,lty=2,lwd=2)
lines(temp0[8:29],new_dat0_origin_model_0.975quan$I,col=8,lty=2,lwd=2)

# text(x=temp0[20],y = 3200,labels = bquote(""*R[0]*" = "*.(round(exp((m.pois$summary.fixed$mean[1])/tscal0),digits = 3))*" ("*.(round(exp((m.pois$summary.fixed$"0.025quant"[1])/tscal0),digits = 3))*","*.(round(exp((m.pois$summary.fixed$"0.975quant"[1])/tscal0),digits = 3))*")"),cex=0.7,col=8)
# text(x=temp0[20],y = 3000,labels = paste("ordinary reporting rate = ",round(tscal0/(m.pois$summary.fixed$mean[3]*-1),digits = 4)," (",round(tscal0/(m.pois$summary.fixed$"0.025quant"[3]*-1),digits = 4),",",round(tscal0/(m.pois$summary.fixed$"0.975quant"[3]*-1),digits = 4),")",sep = ""),cex=0.7,col=2)

axis(1, temp0, format(temp0, "%b %d \n%Y"), cex.axis = .8)



lines(temp0[8:11],c(train0_2020$I[8],exp(m.pois_new_second$summary.fitted.values$mean[4])*mean(adj_report_rate1_all),exp(tail(m.pois_new_second$summary.fitted.values$mean,2))*mean(adj_report_rate1_all)),col=2,lwd=2)

### simulated CI
lines(temp0[8:11],c(train0_2020$I[8],exp(m.pois_new_second$summary.fitted.values$`0.025quant`[4])*mean(adj_report_rate1_all),exp(tail(m.pois_new_second$summary.fitted.values$`0.025quant`,2))*mean(adj_report_rate1_all)),col=2,lwd=3,lty=2)
lines(temp0[8:11],c(train0_2020$I[8],exp(m.pois_new_second$summary.fitted.values$`0.975quant`[4])*mean(adj_report_rate1_all),exp(tail(m.pois_new_second$summary.fitted.values$`0.975quant`,2))*mean(adj_report_rate1_all)),col=2,lwd=3,lty=2)

# for (i in 1:500){
#   lines(temp0[8:11],c(train0_2020$I[8],exp(m.pois_new_second_all[[i]]$summary.fitted.values$`0.025quant`[4])*(adj_report_rate1_all[i]),exp(tail(m.pois_new_second_all[[i]]$summary.fitted.values$`0.025quant`,2))*adj_report_rate1_all[i]),col=3,lwd=2,lty=2)
#   lines(temp0[8:11],c(train0_2020$I[8],exp(m.pois_new_second_all[[i]]$summary.fitted.values$`0.975quant`[4])*(adj_report_rate1_all[i]),exp(tail(m.pois_new_second_all[[i]]$summary.fitted.values$`0.975quant`,2))*adj_report_rate1_all[i]),col=2,lwd=2,lty=2)
# }
# 
# dat0_025_temp=NULL
# dat0_975_temp=NULL
# dat0_mean_temp=NULL
# for (i in 1:500){
#   dat0_025_temp = rbind(dat0_025_temp,c(train0_2020$I[8],exp(m.pois_new_second_all[[i]]$summary.fitted.values$`0.025quant`[4])*(adj_report_rate1_all[i]),exp(tail(m.pois_new_second_all[[i]]$summary.fitted.values$`0.025quant`,2))*adj_report_rate1_all[i]))
#   dat0_975_temp = rbind(dat0_975_temp,c(train0_2020$I[8],exp(m.pois_new_second_all[[i]]$summary.fitted.values$`0.975quant`[4])*(adj_report_rate1_all[i]),exp(tail(m.pois_new_second_all[[i]]$summary.fitted.values$`0.975quant`,2))*adj_report_rate1_all[i]))
#   dat0_mean_temp = rbind(dat0_mean_temp,c(train0_2020$I[8],exp(m.pois_new_second_all[[i]]$summary.fitted.values$mean[4])*(adj_report_rate1_all[i]),exp(tail(m.pois_new_second_all[[i]]$summary.fitted.values$mean,2))*adj_report_rate1_all[i]))
# }

# text(x=temp0[4],y = 1650,labels = bquote("second-phase "*C[]*" of best fit line"),cex=0.7,col=4)
# text(x=temp0[4],y = 1460,labels = paste(" = ",round(mean(R0_2nd_mean_all/R0_1st_mean_all),digits = 3)," (",round(quantile(R0_2nd_mean_all/R0_1st_mean_all,probs = 0.025),digits = 3),",",round(quantile(R0_2nd_mean_all/R0_1st_mean_all,probs = 0.975),digits = 3),")",sep = ""),cex=0.7,col=4)
# text(x=temp0[4],y = 2200,labels = paste("adjusted reporting rate\n = ",round(adj_report_rate1,digits = 4)," (",round(adj_report_rate1_low,digits = 4),",",round(adj_report_rate1_up,digits = 4),")",sep = ""),cex=0.7,col=4)

lines(temp0[11:14],c(exp(tail(m.pois_new_second$summary.fitted.values$mean,1))*mean(adj_report_rate1_all),exp(tail(m.pois_new_third$summary.fitted.values$mean,3))*mean(adj_report_rate2_all)),col=3,lwd=2)

## simulated CI
lines(temp0[11:14],c(exp(tail(m.pois_new_second$summary.fitted.values$"0.025quant",1))*mean(adj_report_rate1_all),exp(tail(m.pois_new_third$summary.fitted.values$"0.025quant",3))*mean(adj_report_rate2_all)),col=3,lwd=3,lty=2)
lines(temp0[11:14],c(exp(tail(m.pois_new_second$summary.fitted.values$"0.975quant",1))*mean(adj_report_rate1_all),exp(tail(m.pois_new_third$summary.fitted.values$"0.975quant",3))*mean(adj_report_rate2_all)),col=3,lwd=3,lty=2)


# for (i in 1:500){
#   lines(temp0[11:14],c(exp(tail(m.pois_new_second_all[[i]]$summary.fitted.values$"0.025quant",1))*adj_report_rate1_all[i],exp(tail(m.pois_new_third_all[[i]]$summary.fitted.values$"0.025quant",3))*adj_report_rate2_all[i]),col=2,lwd=2,lty=2)
#   lines(temp0[11:14],c(exp(tail(m.pois_new_second_all[[i]]$summary.fitted.values$"0.975quant",1))*adj_report_rate1_all[i],exp(tail(m.pois_new_third_all[[i]]$summary.fitted.values$"0.975quant",3))*adj_report_rate2_all[i]),col=3,lwd=2,lty=2)
# }

# text(x=temp0[8],y = 250,labels = bquote("third-phase "*C[]),cex=0.7,col=3)
# text(x=temp0[8],y = 60,labels = paste("of best fit line = ",round(mean(R0_3rd_mean_all/R0_1st_mean_all),digits = 3)," (",round(quantile(R0_3rd_mean_all/R0_1st_mean_all,probs = 0.025),digits = 3),",",round(quantile(R0_3rd_mean_all/R0_1st_mean_all,probs = 0.975),digits = 3),")",sep = ""),cex=0.7,col=3)
# text(x=temp0[15],y = 450,labels = paste("adjusted reporting rate\n = ",round(tscal0/((new_coef0)*-1),digits = 4)," (",round(tscal0/((new_coef0_lcl)*-1),digits = 4),",",round(tscal0/((new_coef0_ucl)*-1),digits = 4),")",sep = ""),cex=0.7,col=3)

abline(v=as.Date("2019-11-24"),lty=2)
abline(v=as.Date("2020-01-12"),lty=2)
abline(v=as.Date("2020-02-02"),lty=2)
abline(v=as.Date("2020-02-23"),lty=2)
arrows(x0 = as.Date("2019-11-25"),x1 = as.Date("2020-01-11"),y0 = 5200,y1 = 5200,length = 0.1,code = 3)
# text(x = as.Date("2019-12-15"),y = 5000,labels = expression("T"[1]),cex=0.8)
text(x = as.Date("2019-12-15"),y = 5000,labels = "Phase 1",cex=0.75)
arrows(x0 = as.Date("2020-01-13"),x1 = as.Date("2020-02-01"),y0 = 5200,y1 = 5200,length = 0.1,code = 3)
# text(x = as.Date("2020-01-23"),y = 5000,labels = expression("T"[2]),cex=0.8)
text(x = as.Date("2020-01-23"),y = 5000,labels = "Phase 2",cex=0.75)
arrows(x0 = as.Date("2020-02-03"),x1 = as.Date("2020-02-22"),y0 = 5200,y1 = 5200,length = 0.1,code = 3)
# text(x = as.Date("2020-02-14"),y = 5000,labels = expression("T"[3]),cex=0.8)
text(x = as.Date("2020-02-13"),y = 5000,labels = "Phase 3",cex=0.75)

# text(x=as.Date("2019-12-18"),y = c(4500),labels = bquote(C[1] == 1),cex=0.7,col=1)

# text(x=as.Date("2020-01-21"),y = 4000,labels = bquote(C[2]*" = "*.(round(mean(R0_2nd_mean_all/R0_1st_mean_all),digits = 3))*" ("*.(round(quantile(R0_2nd_mean_all/R0_1st_mean_all,probs = 0.025),digits = 3))*","*.(round(quantile(R0_2nd_mean_all/R0_1st_mean_all,probs = 0.975),digits = 3))*")"),cex=0.7,col=4)
text(x=as.Date("2020-01-22"),y = c(4500),labels = bquote(Phi[2] == .((1-round(R0_2nd_mean/mean(R0_1st_mean_all),digits = 3))*100)*"%"),cex=0.7,col=1)
text(x=as.Date("2020-01-22"),y = c(4200),labels = paste(" (",(1-round(R0_2nd_975/quantile(R0_1st_mean_all,probs = 0.025),digits = 3))*100,"%",",",(1-round(R0_2nd_025/quantile(R0_1st_mean_all,probs = 0.975),digits = 3))*100,"%",")",sep = ""),cex=0.7,col=1)

text(x=as.Date("2020-02-13"),y = c(4500),labels = bquote(Phi[3] == .((1-round(R0_3rd_mean/mean(R0_1st_mean_all),digits = 3))*100)*"%"),cex=0.7,col=1)
text(x=as.Date("2020-02-13"),y = c(4200),labels = paste(" (",(1-round(R0_3rd_975/quantile(R0_1st_mean_all,probs = 0.025),digits = 3))*100,"%",",",(1-round(R0_3rd_025/quantile(R0_1st_mean_all,probs = 0.975),digits = 3))*100,"%",")",sep = ""),cex=0.7,col=1)

text(x=as.Date("2019-11-26"),y = 600,labels = bquote(""*R[t]*" = "*.(round(exp((m.pois$summary.fixed$mean[1])/tscal0),digits = 2))),cex=0.7,col=1)

points(train0_2020$dates[c(1,8,11,14)],train0_2020$I[c(1,8,11,14)],pch=16,lwd=2,cex=1.4)

# legend("topright",legend = c("reported influenza cases",expression("cases in T"[2]*" period"),expression("cases in T"[3]*" period"),"model projection without intervention"),col=c(1,4,3,8),lty = c(1,1,1,1),pch = c(1,NA,NA,NA),cex=0.64,lwd = c(1,2,2,2))
legend("topright",legend = c("Reported influenza cases","Model prediction in Phase 2","Model prediction in Phase 3","Model projection without intervention"),col=c(1,2,3,8),lty = c(1,1,1,1),pch = c(1,NA,NA,NA),cex=0.64,lwd = c(1,2,2,2))


### Rt during ordinary period to predict cases at Jan 12, 2020
# text(x = as.Date("2020-01-09"),2000,bquote(""*R[t]*" = "*.(round((1-new_dat0_origin_model_INLA[2,"cumsum_yminus_over_N"]/mean(ori_report_rate_all))*exp((m.pois$summary.fixed$mean[1])/tscal0),digits = 3))),cex=0.7,col=8)
# text(x = as.Date("2020-01-03"),2300,bquote(R[t]*" = "*.(round((1-new_dat0_origin_model_INLA[2,"cumsum_yminus_over_N"]/mean(ori_report_rate_all))*exp((m.pois$summary.fixed$mean[1])/tscal0),digits = 2))),cex=0.7,col=8)
text(x = as.Date("2020-01-07"),2320,round((1-new_dat0_origin_model_INLA[2,"cumsum_yminus_over_N"]/mean(ori_report_rate_all))*exp((m.pois$summary.fixed$mean[1])/tscal0),digits = 2),cex=0.7,col=1)
text(x = as.Date("2019-12-31"),2300,bquote(R[t]*" = "),cex=0.7,col=1)

arrows(x0 = as.Date("2020-01-11"),y0 = 2300,x1 = as.Date("2020-01-14"),y1 = 2300,length = 0.05)

## Rt to predict cases at Jan 12, 2020 with COVID-19
# text(x = as.Date("2020-01-20"),800,bquote(""*R[t]*" = "*.(round(mean(Rt_all_over_p_all[,8]),digits = 3))),cex=0.7,col=4)
# text(x = as.Date("2020-01-17"),800,bquote(R[t]*" = "*.(round(mean(Rt_all_over_p_all[,8]),digits = 3))),cex=0.7,col=4)
text(x = as.Date("2020-01-17"),2320,round(Rt_all_over_p[8],digits = 2),cex=0.7,col=1)

## Rt to predict cases at Feb 9, 2020 based on phase 2 situation
# text(x = as.Date("2020-02-04"),1400,bquote(""*R[t]*" = "*.(round((1-new_2020_dat0$cumsum_yminus_over_N_p[4])*mean(R0_1st_mean_all)*mean(R0_2nd_mean_all/R0_1st_mean_all),digits = 3))),cex=0.7,col=4)
# text(x = as.Date("2020-01-25"),1500,bquote(R[t]*" = "*.(round((1-new_2020_dat0$cumsum_yminus_over_N_p[4])*mean(R0_1st_mean_all)*mean(R0_2nd_mean_all/R0_1st_mean_all),digits = 2))),cex=0.7,col=4)
text(x = as.Date("2020-01-29"),1520,round((1-new_2020_dat0$cumsum_yminus_over_N_p[4])*mean(R0_1st_mean_all)*R0_2nd_mean/mean(R0_1st_mean_all),digits = 2),cex=0.7,col=1)
text(x = as.Date("2020-01-23"),1500,bquote(R[t]*" = "),cex=0.7,col=1)

arrows(x0 = as.Date("2020-02-01"),y0 = 1500,x1 = as.Date("2020-02-04"),y1 = 1500,length = 0.05)

## Rt to predict cases at Feb 9, 2020 based on phase 3 situation
# text(x = as.Date("2020-02-14"),900,bquote(""*R[t]*" = "*.(round(mean(Rt_all_over_p_all[,11]),digits = 3))),cex=0.7,col=3)
# text(x = as.Date("2020-02-13"),900,bquote(R[t]*" = "*.(round(mean(Rt_all_over_p_all[,11]),digits = 3))),cex=0.7,col=3)
text(x = as.Date("2020-02-09"),1520,round(Rt_all_over_p[11],digits = 2),cex=0.7,col=1)

text(x = as.Date("2020-02-20"),420,round(Rt_all_over_p[14],digits = 2),cex=0.7,col=1)
text(x = as.Date("2020-02-15"),400,bquote(R[t]*" = "),cex=0.7,col=1)


text(x = as.Date("2019-12-15"),y = 5800,labels = "Ordinary",cex=0.75,xpd=T)
text(x = as.Date("2020-01-23"),y = 5800,labels = "Awareness",cex=0.75,xpd=T)
text(x = as.Date("2020-02-13"),y = 5800,labels = "Spreading",cex=0.75,xpd=T)


dev.off()

##############################################
tscal0 = 2  ## time scale (Tc)

# m.pois <- inla(train0_combine$I ~ train0_combine$logyminus_one + train0_combine$cumsum_yminus_over_N + train0_combine$gp2016 + train0_combine$gp2018 + train0_combine$gp2019 ,family = "poisson",data = train0_combine,control.predictor = list(compute=T))

m.pois <- inla(I ~ logyminus_one + cumsum_yminus_over_N + gp2016 + gp2018 + gp2019 ,family = "poisson",data = train0_combine,control.predictor = list(compute=T),control.fixed = list(mean=list(logyminus_one=1, default=0),prec=list(logyminus_one=100000000000)))


summary(m.pois)

m.pois$summary.fixed$mean


## rho
# ori_report_rate_all = c(ori_report_rate_all,ori_report_rate)

ori_report_rate = tscal0/(m.pois$summary.fixed$mean[3]*-1)

# ori_report_rate_low = tscal0/(m.pois$summary.fixed$"0.025quant"[3]*-1)
# ori_report_rate_up = tscal0/(m.pois$summary.fixed$"0.975quant"[3]*-1)

## R0

# plot(y=m.pois$marginals.fixed$`(Intercept)`[,2],x=m.pois$marginals.fixed$`(Intercept)`[,1])
# plot(density(nval00))

R0_1st_mean = exp((m.pois$summary.fixed$mean[1])/tscal0)

# R0_1st_low = exp((m.pois$summary.fixed$"0.025quant"[1])/tscal0)
# R0_1st_up = exp((m.pois$summary.fixed$"0.975quant"[1])/tscal0)



## get correction factor for reporting rate from severe cases

#####################
########### to get the correction factor for reporting rate from severe cases
# abc <- read.csv("dat0.csv",header = T)
# abc$date = as.Date(abc$date, origin = "1899-12-30")
# 
# cf_2016 = sum(abc$severe_case[match(train0_2016$dates,abc$date)])/sum(train0_2016$I) ## 2016
# cf_2018 = sum(abc$severe_case[match(train0_2018$dates,abc$date)])/sum(train0_2018$I)  ## 2018
# cf_2019 = sum(abc$severe_case[match(train0_2019$dates,abc$date)])/sum(train0_2019$I)  ## 2019

# train0_2020_second_half = train0_2020[9:14,]
# cf_2020_second_half = sum(abc$severe_case[match(train0_2020_second_half$dates,abc$date)])/sum(train0_2020_second_half$I)  ## 2020 second half
# mean(c(cf_2016,cf_2018,cf_2019))
# cf_covid = cf_2020_second_half/mean(c(cf_2016,cf_2018,cf_2019))

# train0_2020_first_half = train0_2020[1:8,]
# cf_2020_first_half = sum(abc$severe_case[match(train0_2020_first_half$dates,abc$date)])/sum(train0_2020_first_half$I)  ## 2020 first half

train0_2020_second_half = train0_2020[9:11,]
cf_2020_second_half = sum(abc$severe_case[match(train0_2020_second_half$dates,abc$date)])/sum(train0_2020_second_half$I)  ## 2020 second half
mean(c(cf_2016,cf_2018,cf_2019))
cf_covid1 = cf_2020_second_half/mean(c(cf_2016,cf_2018,cf_2019))

new_coef0 = tscal0/(ori_report_rate/cf_covid1)*(-1)
# new_coef0_lcl = tscal0/(ori_report_rate_low/cf_covid1)*(-1)
# new_coef0_ucl = tscal0/(ori_report_rate_up/cf_covid1)*(-1)

## adjusted reporting rate
adj_report_rate1 = tscal0/((new_coef0)*-1)

# adj_report_rate1_low = tscal0/((new_coef0_lcl)*-1)
# adj_report_rate1_up = tscal0/((new_coef0_ucl)*-1)

train0_2020_second_half = train0_2020[12:14,]
cf_2020_second_half = sum(abc$severe_case[match(train0_2020_second_half$dates,abc$date)])/sum(train0_2020_second_half$I)  ## 2020 second half
mean(c(cf_2016,cf_2018,cf_2019))
cf_covid2 = cf_2020_second_half/mean(c(cf_2016,cf_2018,cf_2019))

new_coef0 = tscal0/(ori_report_rate/cf_covid2)*(-1)
# new_coef0_lcl = tscal0/(ori_report_rate_low/cf_covid2)*(-1)
# new_coef0_ucl = tscal0/(ori_report_rate_up/cf_covid2)*(-1)
## adjusted reporting rate
adj_report_rate2 = tscal0/((new_coef0)*-1)


# adj_report_rate2_low = tscal0/((new_coef0_lcl)*-1)
# adj_report_rate2_up = tscal0/((new_coef0_ucl)*-1)



## use INLA to fit the true 2020 cases

## adjusted lm0$coef[3]
# new_coef0 = tscal0/(ori_report_rate/cf_covid)*(-1)
#
# new_coef0_lcl = tscal0/(ori_report_rate_low/cf_covid)*(-1)
#
# new_coef0_ucl = tscal0/(ori_report_rate_up/cf_covid)*(-1)



# prior0_prec_x2 = 1/((new_coef0_lcl - new_coef0)/-1.96)^2 ## assume normal distribution and back-calculate the standard deviation, and then calculate the precision = 1/((sd)^2)

prior0_prec_x2 = 1000000000  ## try to fix the Tc, as Tc should be varying
# prior0_prec_x2 = 1/(0.05)^2

# prior0_prec_x1 = 1/((m.pois$summary.fixed$"0.975quant"[2]-m.pois$summary.fixed$mean[2])/1.96)^2
# prior0_prec_x1 = 1/(0.001)^2

prior0_prec_x1 = 1000000000


##################################    week number: 9-11
# y0 = c(train0_2020$I[9])
# x1 = c(train0_2020$logyminus_one[9])
# x2 = c(train0_2020$cumsum_yminus_over_N[9]/ori_report_rate)

new_2020_dat0 = data.frame(train0_2020$I[9:11],train0_2020$logyminus_one[9:11],train0_2020$cumsum_yminus_over_N[9:11])
names(new_2020_dat0)=c("I","logyminus_one","cumsum_yminus_over_N")

new_2020_dat0$cumsum_yminus_over_N*train0_2020$N[1]
new_2020_dat0$cumsum_yminus_over_N_p = new_2020_dat0$cumsum_yminus_over_N
new_2020_dat0$cumsum_yminus_over_N_p[1] = new_2020_dat0$cumsum_yminus_over_N[1]/ori_report_rate
new_2020_dat0$cumsum_yminus_over_N_p[2] = new_2020_dat0$cumsum_yminus_over_N_p[1] + new_2020_dat0$I[1]/(train0_2020$N[1]*adj_report_rate1)
new_2020_dat0$cumsum_yminus_over_N_p[3] = new_2020_dat0$cumsum_yminus_over_N_p[2] + new_2020_dat0$I[2]/(train0_2020$N[1]*adj_report_rate1)

new_2020_dat0$logyminus_one_over_p = new_2020_dat0$logyminus_one
new_2020_dat0$logyminus_one_over_p[1] =  new_2020_dat0$logyminus_one[1] - log(ori_report_rate)
new_2020_dat0$logyminus_one_over_p[2] =  new_2020_dat0$logyminus_one[2] - log(adj_report_rate1)
new_2020_dat0$logyminus_one_over_p[3] =  new_2020_dat0$logyminus_one[3] - log(adj_report_rate1)

new_2020_dat0$I_over_p = round(new_2020_dat0$I/adj_report_rate1,digits = 0)

#### to visualize the regression fit
y0 = c(new_2020_dat0$I_over_p,rep(NA,length(new_2020_dat0$I)))
x1 = rep(new_2020_dat0$logyminus_one_over_p,2)
x2 = rep(new_2020_dat0$cumsum_yminus_over_N_p,2)

dat0_poi = data.frame(y0,x1,x2)

# set.seed(1992)
m.pois_new <- inla(y0 ~ x1 + x2  ,family = "poisson",data = dat0_poi,control.predictor = list(compute=T),control.fixed = list(mean=list(x1=m.pois$summary.fixed$mean[2], x2=-tscal0, default=0),prec=list(x1=prior0_prec_x1, x2=prior0_prec_x2))) ## sd 0.05 for x1; sd 1 for x2

# m.pois_new <- inla(y0 ~ x1 + x2  ,family = "poisson",data = dat0_poi,control.predictor = list(compute=T),control.fixed = list(mean=list(x1=m.pois$summary.fixed$"0.975quant"[2], x2=new_coef0, default=0),prec=list(x1=400, x2=0.04))) ## sd 0.05 for x1; sd 5 for x2


summary(m.pois_new)
m.pois_new_second = m.pois_new
# lines(temp0[8:11],c(train0_2020$I[8],exp(m.pois_new$summary.fitted.values$mean[4])*adj_report_rate1,exp(tail(m.pois_new$summary.fitted.values$mean,2))*adj_report_rate1),col=4,lwd=2)


## new R0
R0_2nd_mean = exp((m.pois_new_second$summary.fixed$mean[1])/tscal0)


# R0_2nd_up = exp((m.pois_new_second$summary.fixed$"0.975quant"[1])/tscal0)
# R0_2nd_low = exp((m.pois_new_second$summary.fixed$"0.025quant"[1])/tscal0)


# text(x=temp0[4],y = 1650,labels = bquote("second-phase "*C[]*" of best fit line"),cex=0.7,col=4)
# text(x=temp0[4],y = 1460,labels = paste(" = ",round(R0_2nd_mean/R0_1st_mean,digits = 3)," (",round(R0_2nd_low/R0_1st_up,digits = 3),",",round(R0_2nd_up/R0_1st_low,digits = 3),")",sep = ""),cex=0.7,col=4)
# 
# text(x=temp0[4],y = 2200,labels = paste("adjusted reporting rate\n = ",round(adj_report_rate1,digits = 4)," (",round(adj_report_rate1_low,digits = 4),",",round(adj_report_rate1_up,digits = 4),")",sep = ""),cex=0.7,col=4)


##################################    week number: 12-14

### prepare training data from wk 12-14 since the reporting rate in cumsum is different
new_2020_dat0 = data.frame(rbind(new_2020_dat0, NA))
new_2020_dat0 = data.frame(rbind(new_2020_dat0, NA))
new_2020_dat0 = data.frame(rbind(new_2020_dat0, NA))

new_2020_dat0$I[4:6] = train0_2020$I[12:14]
new_2020_dat0$logyminus_one[4:6] = log(new_2020_dat0$I[3:5])

for (i in 4:4){
  new_2020_dat0$cumsum_yminus_over_N[i] = new_2020_dat0$cumsum_yminus_over_N[i-1] + (new_2020_dat0$I[i-1]/train0_2020$N[1])
  new_2020_dat0$cumsum_yminus_over_N_p[i] = new_2020_dat0$cumsum_yminus_over_N_p[i-1] + (new_2020_dat0$I[i-1]/(train0_2020$N[1]*adj_report_rate1))
  new_2020_dat0$logyminus_one_over_p[i] = log(exp(new_2020_dat0$logyminus_one[i])/adj_report_rate1)
  new_2020_dat0$I_over_p[i] = round(new_2020_dat0$I[i]/adj_report_rate2,digits = 0)
  print(i)
}
for (i in 5:6){
  new_2020_dat0$cumsum_yminus_over_N[i] = new_2020_dat0$cumsum_yminus_over_N[i-1] + (new_2020_dat0$I[i-1]/train0_2020$N[1])
  new_2020_dat0$cumsum_yminus_over_N_p[i] = new_2020_dat0$cumsum_yminus_over_N_p[i-1] + (new_2020_dat0$I[i-1]/(train0_2020$N[1]*adj_report_rate2))
  new_2020_dat0$logyminus_one_over_p[i] = log(exp(new_2020_dat0$logyminus_one[i])/adj_report_rate2)
  new_2020_dat0$I_over_p[i] = round(new_2020_dat0$I[i]/adj_report_rate2,digits = 0)
}

# y0 = c(train0_2020$I[12:14])
# x1 = c(train0_2020$logyminus_one[12:14])
# x2 = c(train0_2020$cumsum_yminus_over_N[12:14])

y0 = c(tail(new_2020_dat0$I_over_p,3),rep(NA,length(tail(new_2020_dat0$I,3))))
x1 = rep(tail(new_2020_dat0$logyminus_one_over_p,3),2)
x2 = rep(tail(new_2020_dat0$cumsum_yminus_over_N_p,3),2)

dat0_poi = data.frame(y0,x1,x2)


# prior0_prec_x2 = 1/((new_coef0_lcl - new_coef0)/-1.96)^2 ## assume normal distribution and back-calculate the standard deviation, and then calculate the precision = 1/((sd)^2)

prior0_prec_x2 = 1000000000

# prior0_prec_x1 = 1/((m.pois$summary.fixed$"0.975quant"[2]-m.pois$summary.fixed$mean[2])/1.96)^2
# prior0_prec_x2 = 1/(0.05)^2
# prior0_prec_x1 = 1/(0.001)^2
prior0_prec_x1 = 1000000000

# set.seed(1992)
m.pois_new <- inla(y0 ~ x1 + x2  ,family = "poisson",data = dat0_poi,control.predictor = list(compute=T),control.fixed = list(mean=list(x1=m.pois$summary.fixed$mean[2], x2=-tscal0, default=0),prec=list(x1=prior0_prec_x1, x2=prior0_prec_x2))) ## sd 0.05 for x1; sd 5 for x2

summary(m.pois_new)

m.pois_new_third = m.pois_new

# lines(temp0[11:14],c(exp(tail(m.pois_new_second$summary.fitted.values$mean,1))*adj_report_rate1,exp(tail(m.pois_new$summary.fitted.values$mean,3))*adj_report_rate2),col=3,lwd=2)



## new R0

# R0_3rd_low = exp((m.pois_new_third$summary.fixed$"0.025quant"[1])/tscal0)
# R0_3rd_up = exp((m.pois_new_third$summary.fixed$"0.975quant"[1])/tscal0)
R0_3rd_mean = exp((m.pois_new_third$summary.fixed$mean[1])/tscal0)

# text(x=temp0[8],y = 250,labels = bquote("third-phase "*C[]),cex=0.7,col=3)
# text(x=temp0[8],y = 60,labels = paste("of best fit line = ",round(R0_3rd_mean/R0_1st_mean,digits = 3)," (",round(R0_3rd_low/R0_1st_up,digits = 3),",",round(R0_3rd_up/R0_1st_low,digits = 3),")",sep = ""),cex=0.7,col=3)
# 
# 
# text(x=temp0[15],y = 450,labels = paste("adjusted reporting rate\n = ",round(tscal0/((new_coef0)*-1),digits = 4)," (",round(tscal0/((new_coef0_lcl)*-1),digits = 4),",",round(tscal0/((new_coef0_ucl)*-1),digits = 4),")",sep = ""),cex=0.7,col=3)


# dev.off()



