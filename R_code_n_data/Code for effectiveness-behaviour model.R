############  Code for effectiveness-behaviour model

res0_rank=NULL   ## data.frame hosting the effectiveness of individual behaviours
R0_zero_rank = NULL ## data.frame hosting the estimated R0 when nobody wears mask or adopts social distancing 
percentage_for_one_rank = NULL ## data.frame hosting the percentage of people required to adopt precautionary behaviours in order to lower R0 below 1
sam_1 = mean(R0_1st_mean_all) ## R0
run0 = 10000  ## bootstrap with 10000 samples

set.seed(1992)

## Sample the Phase 2 contact-rate reduction and behaviours
df0_p1 = NULL
for (i in 1:run0){
  df0_p1 = rbind(df0_p1, c(1-exp(sample(m.pois_new_second$marginals.fixed$`(Intercept)`[,1],prob = m.pois_new_second$marginals.fixed$`(Intercept)`[,2]/sum(m.pois_new_second$marginals.fixed$`(Intercept)`[,2]),size = 1)/tscal0)/sam_1,rbinom(n = 1,size = 7395278,prob = 627/1008)/7395278,rbinom(n = 1,size = 7395278,prob = 778/1008)/7395278))
}
df0_p1 = data.frame(df0_p1)
names(df0_p1) = c("y0","crowd","mask")


## sample the Phase 1 behaviours
sam_0_sd_all = NULL
for (i in 1:run0){
  sam_0_sd = rbinom(n = 1,size = 7395278,prob = 25/66)/7395278
  sam_0_sd_all = c(sam_0_sd_all,sam_0_sd)
}
sam_0_mask_all = NULL
for (i in 1:run0){
  sam_0_mask = rbinom(n = 1,size = 7395278,prob = 30/66)/7395278
  sam_0_mask_all = c(sam_0_mask_all,sam_0_mask)
}


## rearrange the Phase 2 contact-rate reduction and behaviours in ascending order
df0_p1$y0 = df0_p1$y0[order(df0_p1$y0)]

df0_p1$crowd = df0_p1$crowd - sam_0_sd_all ### social distancing
df0_p1$crowd = df0_p1$crowd[order(df0_p1$crowd)]

df0_p1$mask = df0_p1$mask - sam_0_mask_all  ### mask
df0_p1$mask = df0_p1$mask[order(df0_p1$mask)]


## Sample the Phase 3 contact-rate reduction and behaviours
df0_p2 = NULL
for (i in 1:run0){
  df0_p2 = rbind(df0_p2,c(1-exp(sample(m.pois_new_third$marginals.fixed$`(Intercept)`[,1],prob = m.pois_new_third$marginals.fixed$`(Intercept)`[,2]/sum(m.pois_new_third$marginals.fixed$`(Intercept)`[,2]),size = 1)/tscal0)/sam_1,rbinom(n = 1,size = 7395278,prob = 920/1000)/7395278,rbinom(n = 1,size = 7395278,prob = 976/1000)/7395278))
}
df0_p2 = data.frame(df0_p2)
names(df0_p2) = c("y0","crowd","mask")

## rearrange the Phase 3 contact-rate reduction and behaviours in ascending order
df0_p2$y0 = df0_p2$y0[order(df0_p2$y0)]

df0_p2$crowd = df0_p2$crowd - sam_0_sd_all ### social distancing
df0_p2$crowd = df0_p2$crowd[order(df0_p2$crowd)]

df0_p2$mask = df0_p2$mask - sam_0_mask_all  ### mask
df0_p2$mask = df0_p2$mask[order(df0_p2$mask)]


## fit the effectiveness-behaviour model
for (i in 1:run0){
  df0 = rbind(df0_p1[i,],df0_p2[i,])
  lm0 <- lm(y0~crowd+mask+0,data = df0)
  res0_rank = rbind(res0_rank,lm0$coefficients)
  R0_zero_rank = c(R0_zero_rank, (lm0$coefficients[1]*sam_0_sd_all[i]+lm0$coefficients[2]*sam_0_mask_all[i]+1)*sam_1)
  percentage_for_one_rank = c(percentage_for_one_rank,((1-1/sam_1)+lm0$coefficients[1]*sam_0_sd_all[i]+lm0$coefficients[2]*sam_0_mask_all[i])/sum(lm0$coefficients))
}

#### print the results
print(paste("Effectiveness of wearing face masks (95% CI):", mean(res0_rank[,2]),"(",quantile(res0_rank[,2],probs = c(0.025)),", ",quantile(res0_rank[,2],probs = c(0.975)),")"))

print(paste("Effectiveness of avoiding crowded places (95% CI):", mean(res0_rank[,1]),"(",quantile(res0_rank[,1],probs = c(0.025)),", ",quantile(res0_rank[,1],probs = c(0.975)),")"))

print(paste("Estimated R0 when nobody wears mask or adopts social distancing:", mean(R0_zero_rank),"(",quantile(R0_zero_rank,probs = c(0.025)),", ",quantile(R0_zero_rank,probs = c(0.975)),")"))

print(paste("Percentage of people required to adopt precautionary behaviours in order to lower R0 below 1:", mean(percentage_for_one_rank),"(",quantile(percentage_for_one_rank,probs = c(0.025)),", ",quantile(percentage_for_one_rank,probs = c(0.975)),")"))
