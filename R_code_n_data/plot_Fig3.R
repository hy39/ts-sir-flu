library(ggplot2)
library(scales)
library(lubridate)
library(grid)
library(EpiEstim)

## function for labelling the figure with letters
fig_label <- function(text, region="figure", pos="topleft", cex=NULL, offset0=0, ...) {
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


###########
png("timeline_covid_v14.png", width=886*2.5, height=558*2.5*2, res=300)
par(mfrow=c(2,1))

df = data.frame(date=c("2020-01-14","2020-01-23","2020-01-23","2020-01-25","2020-01-29","2020-01-30","2020-02-04","2020-02-04","2020-02-09"),item = c("Jan14: Limited human-to-human \ntransmission message announced from WHO (a)","Jan23: Wuhan lockdown;","First imported COVID-19 case (b)","Jan25: Lunar new year holiday begins","Jan29: Work from home policy for civil servants","Jan30: Partial closure of borders","Feb04: More closure of borders;  \nFirst death due to COVID-19;","First local transmission (c)","Feb09: More COVID-19 cases \n(local transmission cluster)"))
df$date = as.Date(df$date)

df$status = c("important","less important","important","less important","less important","less important","less important","important","less important")
status_levels <- c("important", "less important")
status_colors <- c("#c00000", "#00B050")
df$status <- factor(df$status, levels=status_levels, ordered=TRUE)


positions <- c(0.25, -0.35,0.6,-0.7,0.85,-1.1, 1.1, -1.5, 1.5, -2)
directions <- c(1, -1)

line_pos <- data.frame(
  "date"=unique(df$date),
  "position"=rep(positions, length.out=length(unique(df$date))),
  "direction"=rep(directions, length.out=length(unique(df$date)))
)

df <- merge(x=df, y=line_pos, by="date", all = TRUE)
df <- df[with(df, order(date)), ]

head(df)

text_offset <- 0.05

df$month_count <- ave(df$date==df$date, df$date, FUN=cumsum)
df$text_position <- (df$month_count * text_offset * df$direction) + df$position
head(df)
df$text_position[3]=-0.5   ### to deal with the overlapping problem of one item 
df$text_position[8]=df$text_position[8]+0.01   ### to deal with the overlapping problem of one item 
df$text_position[9]=df$text_position[9]+0.05
df$text_position[7]=df$text_position[7]+0.1
df$text_position[5]=df$text_position[5]+0.035

month_buffer <- 2

month_date_range <- seq(min(df$date) - months(month_buffer), max(df$date) + months(1), by='day')
month_format <- format(month_date_range, '%d')
month_df <- data.frame(month_date_range, month_format)
month_df = month_df[match(as.Date(c("2019-11-24","2019-12-08","2019-12-22","2020-01-05","2020-01-19","2020-02-02","2020-02-16","2020-02-23")),month_df$month_date_range),]

year_date_range <- seq(min(df$date) - months(month_buffer), max(df$date) + months(1), by='month')
year_date_range <- as.Date(
  intersect(
    ceiling_date(year_date_range, unit="month"),
    floor_date(year_date_range, unit="month")
  ),  origin = "1970-01-01"
)
year_format <- format(year_date_range, '%b')
year_df <- data.frame(year_date_range, year_format)

## tick mark
df_tick = data.frame(date=c("2019-11-24","2019-12-01"),position=-0.08)
df_tick$date = as.Date(df_tick$date)
for (k in c(1:12)){
  df_tick = rbind(df_tick,df_tick[nrow(df_tick),])
  df_tick$date[nrow(df_tick)]=df_tick$date[nrow(df_tick)]+7
}
row.names(df_tick)=NULL
df_tick$item = "less important"

#### PLOT ####
timeline_plot<-ggplot(df,aes(x=date,y=0, col=status, label=item))
timeline_plot<-timeline_plot+labs(col="Event")
timeline_plot<-timeline_plot+scale_color_manual(values=status_colors, labels=c("important incident", "Hong Kong less important incident"), drop = FALSE)
timeline_plot<-timeline_plot+theme_classic()

# Plot horizontal black line for timeline
timeline_plot<-timeline_plot+geom_hline(yintercept=0, 
                                        color = "black", size=0.3)

## add vertical tick mark
timeline_plot<-timeline_plot+geom_segment(data=df_tick, aes(y=position,yend=0,xend=date), color='black', size=0.2)


# Plot vertical segment lines for milestones
timeline_plot<-timeline_plot+geom_segment(data=df[df$month_count == 1,], aes(y=position,yend=0,xend=date), color='black', size=0.5)

# Plot scatter points at zero and date
timeline_plot<-timeline_plot+geom_point(aes(y=0), size=3)

# Don't show axes, appropriately position legend
timeline_plot<-timeline_plot+theme(axis.line.y=element_blank(),
                                   axis.text.y=element_blank(),
                                   axis.title.x=element_blank(),
                                   axis.title.y=element_blank(),
                                   axis.ticks.y=element_blank(),
                                   axis.text.x =element_blank(),
                                   axis.ticks.x =element_blank(),
                                   axis.line.x =element_blank(),
                                   legend.position = "none"
)

## show the date
year_df = month_df
names(year_df) = c("year_date_range", "year_format")
year_df$year_format <- format(year_df$year_date_range, '%b %d\n %Y')
timeline_plot<-timeline_plot+geom_text(data=year_df, aes(x=year_date_range,y=-0.2,label=year_format),size=3, color='white')
timeline_plot<-timeline_plot+geom_text(data=year_df[-dim(year_df)[1],], aes(x=year_date_range,y=-0.2,label=year_format),size=3, color='black')

# Show text for each milestone
timeline_plot<-timeline_plot+geom_text(aes(x=as.Date(c("2019-12-31","2020-01-15","2020-01-12","2020-01-11","2020-01-12","2020-01-20","2020-01-22","2020-01-22","2020-02-10")),y=text_position,label=item),size=3)  ## size = 2.5)

########################################################
########################################################
########################################################
### to produce plot with ben cowling survey
train0_backup = train0
temp0 = tail(train0_backup$dates,18)
temp0 = temp0[1:14]
date0 = temp0


##################### without reporting rate
dat00 = train0_backup

###################  convert weekly data to 3.5days data
temp00 = NULL

for (i in 1:nrow(dat00)){
  temp0 = dat00[i,]
  
  for (j in 1:2){
    if (j==1){
      temp0$dates = dat00[i,]$dates-(3)
      temp0$I=dat00[i,"I"]/2
    } else {
      temp0$dates = dat00[i,]$dates-0
      temp0$I=dat00[i,"I"]/2
    }
    temp00 = rbind(temp00,temp0)
  }
}

dat0_daily_3dot5days = temp00

dat00 = dat0_daily_3dot5days
dat00$dates = as.numeric(1:length(dat00$dates))


i=4
win_size0 = i   ## i= 56
t_start <- seq(2, nrow(dat00)-(win_size0-1))   
t_end <- t_start + (win_size0-1) 


res0 <- estimate_R(dat00, method = "parametric_si",
                   config = make_config(list(mean_si = 1.0001, std_si = 1/3.5,t_start=t_start,t_end=t_end)))

plot(date0,Rt_all_over_p_all[1,],col=2,lwd=2,ylim=c(0.35,1.65),type="o",xlab="Date",ylab=expression("R"[t]),xaxt="n")
lines(date0,Rt_all_over_p_all[2,],col=2,lty=2,lwd=2)
lines(date0,Rt_all_over_p_all[3,],col=2,lty=2,lwd=2)

## correct the date
Rt_epi = data.frame(cbind(dat0_daily_3dot5days$dates,c(rep(NA,win_size0),res0$R$`Mean(R)`),c(rep(NA,win_size0),res0$R$`Quantile.0.025(R)`),c(rep(NA,win_size0),res0$R$`Quantile.0.975(R)`)))
names(Rt_epi)=c("dates","mean_Rt","LCL","UCL")
Rt_epi$dates = dat0_daily_3dot5days$dates
Rt_epi$mean_Rt = c(Rt_epi$mean_Rt[-1],NA)
Rt_epi$LCL = c(Rt_epi$LCL[-1],NA)
Rt_epi$UCL = c(Rt_epi$UCL[-1],NA)
Rt_epi = Rt_epi[-nrow(Rt_epi),]

range0 = Rt_epi$dates>="2019-11-24" & Rt_epi$dates<="2020-02-23"
lines(Rt_epi$dates[range0],Rt_epi$mean_Rt[range0],col="dodgerblue1",type="o",lwd=2)
lines(Rt_epi$dates[range0],Rt_epi$LCL[range0],col="dodgerblue1",type="l",lty=2,lwd=2)
lines(Rt_epi$dates[range0],Rt_epi$UCL[range0],col="dodgerblue1",type="l",lty=2,lwd=2)

###################################################
### prepare I with reporting rate considered
dat0_with_report_rate = train0_backup
dat0_with_report_rate$I[match(train0_2020$dates[9],train0_backup$dates):nrow(dat0_with_report_rate)] = NA
dat0_with_report_rate$I = round(dat0_with_report_rate$I/mean(ori_report_rate_all),digits = 0)
dat0_with_report_rate$I[match(train0_2020$dates[9],train0_backup$dates):match(train0_2020$dates[11],train0_backup$dates)] = round(train0_backup$I[match(train0_2020$dates[9],train0_backup$dates):match(train0_2020$dates[11],train0_backup$dates)]/mean(adj_report_rate1_all),digits = 0)
dat0_with_report_rate$I[match(train0_2020$dates[12],train0_backup$dates):nrow(dat0_with_report_rate)] = round(train0_backup$I[match(train0_2020$dates[12],train0_backup$dates):nrow(dat0_with_report_rate)]/mean(adj_report_rate2_all),digits = 0)

temp00 = NULL

for (i in 1:nrow(dat0_with_report_rate)){
  temp0 = dat0_with_report_rate[i,]
  
  for (j in 1:2){
    if (j==1){
      temp0$dates = dat0_with_report_rate[i,]$dates-(3)
      temp0$I=dat0_with_report_rate[i,"I"]/2
    } else {
      temp0$dates = dat0_with_report_rate[i,]$dates-0
      temp0$I=dat0_with_report_rate[i,"I"]/2
    }
    
    temp00 = rbind(temp00,temp0)
  }
}

dat0_3dot5days_with_report_rate = temp00

dat00 = dat0_3dot5days_with_report_rate

dat00$dates = as.numeric(1:length(dat00$dates))

i=4
win_size0 = i   ## i= 56
t_start <- seq(2, nrow(dat00)-(win_size0-1))   
t_end <- t_start + (win_size0-1) 

res0 <- estimate_R(dat00, method = "parametric_si",
                   config = make_config(list(mean_si = 1.0001, std_si = 1/3.5,t_start=t_start,t_end=t_end)))


## correct the date
Rt_epi = data.frame(cbind(dat0_3dot5days_with_report_rate$dates,c(rep(NA,win_size0),res0$R$`Mean(R)`),c(rep(NA,win_size0),res0$R$`Quantile.0.025(R)`),c(rep(NA,win_size0),res0$R$`Quantile.0.975(R)`)))
names(Rt_epi)=c("dates","mean_Rt","LCL","UCL")
Rt_epi$dates = dat0_3dot5days_with_report_rate$dates
Rt_epi$mean_Rt = c(Rt_epi$mean_Rt[-1],NA)
Rt_epi$LCL = c(Rt_epi$LCL[-1],NA)
Rt_epi$UCL = c(Rt_epi$UCL[-1],NA)
Rt_epi = Rt_epi[-nrow(Rt_epi),]

range0 = Rt_epi$dates>="2019-11-24" & Rt_epi$dates<="2020-02-23"
lines(Rt_epi$dates[range0],Rt_epi$mean_Rt[range0],col=8,type="o",lwd=2)
lines(Rt_epi$dates[range0],Rt_epi$LCL[range0],col=8,type="l",lty=2,lwd=2)
lines(Rt_epi$dates[range0],Rt_epi$UCL[range0],col=8,type="l",lty=2,lwd=2)
# points(Rt_epi$dates[range0],Rt_epi$Rt[range0],col=6)

abline(v=as.Date("2019-11-24"),lty=2)
abline(v=as.Date("2020-01-12"),lty=2)
abline(v=as.Date("2020-02-02"),lty=2)
abline(v=as.Date("2020-02-23"),lty=2)
arrows(x0 = as.Date("2019-11-25"),x1 = as.Date("2020-01-11"),y0 = 1.6,y1 = 1.6,length = 0.1,code = 3)
text(x = as.Date("2019-12-20"),y = 1.5,labels = "Phase 1")
arrows(x0 = as.Date("2020-01-13"),x1 = as.Date("2020-02-01"),y0 = 1.6,y1 = 1.6,length = 0.1,code = 3)
text(x = as.Date("2020-01-23"),y = 1.5,labels = "Phase 2")
arrows(x0 = as.Date("2020-02-03"),x1 = as.Date("2020-02-22"),y0 = 1.6,y1 = 1.6,length = 0.1,code = 3)
text(x = as.Date("2020-02-14"),y = 1.5,labels = "Phase 3")

text(x = as.Date("2019-12-20"),y = 1.77,labels = "Ordinary",xpd=T)
text(x = as.Date("2020-01-23"),y = 1.77,labels = "Awareness",xpd=T)
text(x = as.Date("2020-02-14"),y = 1.77,labels = "Spreading",xpd=T)

arrows(x0 = as.Date("2020-01-14"),x1 = as.Date("2020-01-14"),y0 = 1.85,y1 = 1.75,length = 0.1,code = 2,xpd=T,lwd=2)
text(x = as.Date("2020-01-14"),y = 1.9,labels = "a",xpd=T,col=2)
arrows(x0 = as.Date("2020-01-23"),x1 = as.Date("2020-01-23"),y0 = 1.9,y1 = 1.8,length = 0.1,code = 2,xpd=T,lwd=2)
text(x = as.Date("2020-01-23"),y = 1.95,labels = "b",xpd=T,col=2)
arrows(x0 = as.Date("2020-02-04"),x1 = as.Date("2020-02-04"),y0 = 1.85,y1 = 1.75,length = 0.1,code = 2,xpd=T,lwd=2)
text(x = as.Date("2020-02-04"),y = 1.9,labels = "c",xpd=T,col=2)


axis(1, train0_backup$dates, format(train0_backup$dates, "%b %d \n%Y"), cex.axis = .8)

### to produce plot with ben cowling survey
legend("bottomleft",legend = c("Model predicted", "EpiEstim (with reporting rate adjusted)","EpiEstim (without reporting rate adjusted)"),lty=1,col=c(2,8,"dodgerblue1",rgb(1,1,0,alpha=0.2),rgb(0,1,0,alpha=0.2)),cex=0.75,pch=c(1,1,1,NA,NA),lwd=c(2,2,2,11,11))
rect(xleft=as.Date("2020-01-13"),xright = as.Date("2020-01-23"),ybottom=par("usr")[3], ytop=par("usr")[4], col = rgb(1,1,0,alpha=0.2),border = NA)
rect(xleft=as.Date("2020-02-04"),xright = as.Date("2020-02-14"),ybottom=par("usr")[3], ytop=par("usr")[4], col = rgb(0,1,0,alpha=0.2),border = NA)

text(x=as.Date("2020-01-18"),y = 0.35,labels = "Survey 1",cex=0.8)
text(x=as.Date("2020-02-09"),y = 0.35,labels = "Survey 2",cex=0.8)

fig_label(LETTERS[1],cex = 2)

### attach the ggplot at bottom
vp.Bottom <- viewport(height=unit(.5, "npc"), width=unit(0.871, "npc"), 
                           just=c("top"), 
                           y=0.5, x=0.525)
print(timeline_plot,vp = vp.Bottom)

fig_label(LETTERS[2],cex = 2,offset0 = -2.3)

dev.off()



