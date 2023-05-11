## ED2 - allometry figures

#---------------------------------------------------------------------#
# run ED2_allometry.R to line ~475
#---------------------------------------------------------------------#
setwd("G:/My Drive") # Google Drive
#datDNM <- census_css_dnm
#---------------------------------------------------------------------#
#---------------------------------------------------------------------#

hite <- dat$hite
hmax = 70
rho_low <- 0.4
rho_med <- 0.6
rho_high <- 0.8
#---------------------------------------------------------------------#
#---------------------------------------------------------------------#

#---------------------------------------------------------------------#
#---------------------------------------------------------------------#
#              Calculate Crown Area from DBH and plot
#---------------------------------------------------------------------#
# Marcos changed IALLOM=3 & 4 based on Sustainable Landscapes Data: 
# (i.e. CA = b1Ca * (DBH^2 * Hgt)^b2Ca
#---------------------------------------------------------------------#
b1Ht_4_2 = 0.6348
b1Ht_4_3 = 0.6054
b1Ht_4_4 = 0.5815
b2Ht_4 = 0.6734
dbh_crit <- 124.21

b1Ht_SEA      = 0.5279284 * log(10)  # Use for dbh2h_01
b2Ht_SEA      = 0.5782               #"coefficient of ln(D)" # Use for dbh2h_01

dbh <- rnorm(100, 50, 60)
dbh <- ifelse(dbh < 0, NA, dbh)
hist(dbh)

#dbh2h tropical default: 
mdbh = ifelse(dbh < dbh_crit, dbh, dbh_crit)
hist(mdbh)
ht_2 = exp(b1Ht_4_2 + b2Ht_4 * log(dbh))
ht_3 = exp(b1Ht_4_3 + b2Ht_4 * log(dbh))
ht_4 = exp(b1Ht_4_4 + b2Ht_4 * log(dbh))
ht_sea = exp(b1Ht_SEA + b2Ht_SEA * log(dbh))

plot(dbh, ht_2, pch=19, ylim=c(0,90), col="darkgreen")
points(dbh, ht_3, pch=19, col="blue")
points(dbh, ht_4, pch=19, col="darkred")
points(dbh, ht_sea, pch=19)


#--------------------------------
b1Ca = 0.3700000048
b2Ca = 0.4639999866

hgt <- ht_4

CA_baseline = b1Ca * (dbh^2 * hgt)^b2Ca ## Line 3817 (Crow area in m2)
CA_SEA = b1Ca * (dbh^2 * ht_sea)^b2Ca ## Line 3817 (crown area in m2)

baad_southeast_asia$dbh <- baad_southeast_asia$d.bh*100

fit1 <- nls(a.cp ~ I(dbh^power), data = baad_southeast_asia, start = list(power = 1), trace = T)
#fit <- lm(a.cp ~ dbh, data = baad_southeast_asia)
summary(fit1)
s <- seq(1:max(baad_southeast_asia$dbh, na.rm=T))
length(s)

s_ca <- s**coef(fit1)[[1]] # == 1.209017

plot(baad_southeast_asia$dbh, baad_southeast_asia$a.cp, pch=19, col="grey60", ylim=c(0,550), 
     xlab = "DBH (cm)", ylab = "Crown area (m2)")
#abline(fit1, col="red", lwd=2)
#lines(s, predict(fit1, list(x = s)), col = "red")
lines(s, s_ca, col = "red", lwd=2)
points(dbh, CA_baseline, pch=19, col="darkgreen", add=T)
points(dbh, CA_SEA, pch=19)
# see EVS4_traits.R - run to BAAD database


#---------------------------------------------------------------------#
#---------------------------------------------------------------------#
#              Calculate Height from DBH and plot
#---------------------------------------------------------------------#
# Marcos changed IALLOM=3 so the function is similar to Chave: 
# (i.e. Bdead = b1bd * (rho * DBH^2 * H)^b2bd)
#---------------------------------------------------------------------#
#h50_chave <- dbh2h_34(dbh_samp,hgt_max=50,hgt_ref_34,b1Ht_34,b2Ht_34)
#h65_chave <- dbh2h_34(dat$dbh,hgt_max=65,hgt_ref_34,b1Ht_34,b2Ht_34)
h80_chave <- dbh2h_34(dat$dbh,hgt_max=hmax,hgt_ref_34,b1Ht_34,b2Ht_34)

#h50_chaveE <- dbh2h_ChaveE(dbh_samp,hgt_max=50,hgt_ref_34,b1Ht_34,b2Ht_34,E_DNM)
#h65_chaveE_SEA <- dbh2h_ChaveE(dat$dbh,hgt_max=65,hgt_ref_34,b1Ht_34,b2Ht_34,E_DNM)
h80_chaveE_SEA <- dbh2h_ChaveE(dat$dbh,hgt_max=hmax,hgt_ref_34,b1Ht_34,b2Ht_34,E_DNM); summary(h80_chaveE_SEA)

#h65_chaveE_WAm <- dbh2h_ChaveE(dat$dbh,hgt_max=65,hgt_ref_34,b1Ht_34,b2Ht_34,E_ALP)
h80_chaveE_WAm <- dbh2h_ChaveE(dat$dbh,hgt_max=hmax,hgt_ref_34,b1Ht_34,b2Ht_34,E_ALP); summary(h80_chaveE_WAm)

#h50_feldSEA <- dbh2h_01(dat$dbh,hgt_max=50,b1Ht_SEA,b2Ht_SEA)
#h65_feldSEA <- dbh2h_01(dat$dbh,hgt_max=65,b1Ht_SEA,b2Ht_SEA)
#h80_feldSEA <- dbh2h_01(dat$dbh,hgt_max=80,b1Ht_SEA,b2Ht_SEA)

#h65_feldSEA <- dbh2h_01(dat$dbh,hgt_max=65,hgt_ref_SEA,b1Ht_SEA,b2Ht_SEA)
h80_feldSEA <- dbh2h_01(dat$dbh,hgt_max=hmax,hgt_ref_SEA,b1Ht_SEA,b2Ht_SEA)

# height_dat <- data.frame(cbind(dat$dbh,h65_chave,h80_chave,
#                                h65_chaveE_SEA,h80_chaveE_SEA,
#                                h65_chaveE_WAm,h80_chaveE_WAm,
#                                h65_feldSEA,h80_feldSEA))
height_dat <- data.frame(cbind(dat$dbh,h80_chave,
                               h80_chaveE_SEA,
                               h80_chaveE_WAm,
                               h80_feldSEA))
dat_long <- tidyr::gather(height_dat,"Allometry","H",2:ncol(height_dat)); head(dat_long)
colnames(dat_long) <- c("dbh","Allometry","H")

#---------------------------------------------------------------------#
# find specific dbh for a given height
h_test <- subset(height_dat, h80_feldSEA >= 29.9 & h80_feldSEA <= 31); h_test
#---------------------------------------------------------------------#

ggplot(dat_long, aes(dbh, H)) + 
  geom_point(aes(col=Allometry, shape=Allometry), alpha=0.7, size=4) + 
  scale_color_manual("Allometry", 
#                     values=c("#7fcdbb","black","blue","#a50f15","#7fcdbb","black","blue","#a50f15"),
                     values=c("#7fcdbb","black","blue","#a50f15"),
                     labels=c("Chave (70 m)", "Chave + E (DNM, 70 m)", 
                     "Chave + E (ALP, 70 m)", "Feldpausch (SEA, 70 m)")) +
                     # labels=c("Chave (65 m)", "Chave + E (DNM, 65 m)", "Chave + E (ALP, 65 m)",
                     #          "Feldpausch (SEA, 65 m)", "Chave (70 m)", "Chave + E (DNM, 70 m)", 
                     #          "Chave + E (ALP, 70 m)", "Feldpausch (SEA, 70 m)")) +
  scale_shape_manual("Allometry", values=c(19,19,19,19),
                     labels=c("Chave (70 m)", "Chave + E (DNM, 70 m)", 
                              "Chave + E (ALP, 70 m)", "Feldpausch (SEA, 70 m)")) + 
  labs(x="DBH (cm)", y="Height (m)") + 
  theme(legend.position = c(0.06,0.8))
# tiff 700x700: Allometry_comparison_D_H & Allometry_comparison_D_H_dbh_crit

new_dat <- data.frame(cbind(dat$dbh,h80_chave,h80_chaveE_SEA,h80_feldSEA))
dat_long <- tidyr::gather(new_dat,"Allometry","H",2:ncol(new_dat)); head(dat_long)
colnames(dat_long) <- c("dbh","Allometry","H")

ggplot(dat_long, aes(dbh, H)) + 
  geom_point(aes(col=Allometry), alpha=0.7, size=4) + 
  scale_color_manual("Allometry",
                     values=c("#7fcdbb","blue","#a50f15"),#"#a50f15","#7fcdbb","black","blue"),
                     labels=c("Chave et al. 2014 Pantropical",
                              "Chave et al. 2014 Pantropical + E",
                              "Feldpausch et al. 2011 SE Asia Regional")) +
  labs(x="DBH (cm)", y="Height (m)") + 
  theme(legend.position = c(0.06,0.85))
# pdf 8x5: Allometry_comparison_D_H_Chave_Feld_full_DNM

#---------------------------------------------------------------------#


#---------------------------------------------------------------------#
# D:H with tentative SEA parameters
# Default ED 2.1 -- log-log
#---------------------------------------------------------------------#
# dbh2h_pan = exp(b1Ht_pan + b2Ht_pan * log((mdbh/10)))
# dbh2h_SEA = exp(b1Ht_SEA + b2Ht_SEA * log((mdbh/10)))
#dbh2h_WAM = exp(b1Ht_WAM + b2Ht_WAM * log((mdbh/10))) # need to figure this out... not main intercept??

dbh2h_pan = exp(b1Ht_pan + b2Ht_pan * log((dat$dbh)))
dbh2h_SEA = exp(b1Ht_SEA + b2Ht_SEA * log((dat$dbh)))
#dbh2h_WAM = exp(b1Ht_WAM + b2Ht_WAM * log((mdbh/10))) # need to figure this out... not main intercept??

# dat2 <- as.data.frame(rbind(cbind(mdbh,dbh2h),cbind(mdbh,dbh2h_P)))
# dat2$method <- rep(c("ED2.1_loglog_dbh2h","ED2.1_Poorter_dbh2h"),each=length(mdbh))
# colnames(dat2) <- c("dbh","h","method")

dat3 <- as.data.frame(rbind(cbind(dat$dbh,dbh2h_pan),cbind(dat$dbh,dbh2h_SEA)))
dat3$method <- rep(c("pantropical","SEA"),each=length(dat$dbh))
colnames(dat3) <- c("dbh","h","method")

ggplot() + #geom_line(aes(x=dbh,y=h, col=method), data=dat) + 
  #  geom_line(aes(x=dbh,y=h, col=method, linetype=method), data=dat2, lwd=2) + 
  geom_line(aes(x=dbh,y=h, col=method, linetype=method), data=dat3, lwd=2) + 
  scale_linetype_manual("Allometry",values=c(1,2,2,1)) +
  scale_color_manual("Allometry",values=colors()[c(153,26,153,50)]) +
  #  xlim(0,210) + ylim(0,100) +
  labs(y="height (m)", x="DBH (cm)") +
  guides(linetype=guide_legend(keywidth = 2, keyheight = 1),
         colour=guide_legend(keywidth = 2, keyheight = 1)) +
  theme_classic() + 
  theme(legend.position = c(0.25,0.85)) 











#---------------------------------------------------------------------#
#---------------------------------------------------------------------#
# calculate AGB using Feldpausch & Chave AGB equations
#---------------------------------------------------------------------#
# BOTH use Feldpausch et al 2011 SE Asia Regional D:H eq
#---------------------------------------------------------------------#

agb_1_Low <- size2bd_1(hgt_max=hmax,hgt_ref_SEA,b1Ht_SEA,b2Ht_SEA,dbh,C2B,rho=rho_low,
                       odead_small_1,odead_small_2,odead_small_3,odead_large_1,odead_large_2,odead_large_3)
agb_1_Med <- size2bd_1(hgt_max=hmax,hgt_ref_SEA,b1Ht_SEA,b2Ht_SEA,dbh,C2B,rho=rho_med,
                       odead_small_1,odead_small_2,odead_small_3,odead_large_1,odead_large_2,odead_large_3)
agb_1_High <- size2bd_1(hgt_max=hmax,hgt_ref_SEA,b1Ht_SEA,b2Ht_SEA,dbh,C2B,rho=rho_high,
                        odead_small_1,odead_small_2,odead_small_3,odead_large_1,odead_large_2,odead_large_3)

#h80_chave <- dbh2h_34(dbh,hgt_max=hmax,hgt_ref_34,b1Ht_SEA,b2Ht_34)
agb_34_Low <- size2bd_34(hgt_max=hmax,hgt_ref_SEA,b1Ht_SEA,b2Ht_SEA,dbh,hite,C2B,rho=rho_low)
agb_34_Med <- size2bd_34(hgt_max=hmax,hgt_ref_SEA,b1Ht_SEA,b2Ht_SEA,dbh,hite,C2B,rho=rho_med)
agb_34_High <- size2bd_34(hgt_max=hmax,hgt_ref_SEA,b1Ht_SEA,b2Ht_SEA,dbh,hite,C2B,rho=rho_high)

dbh_crit <- exp((log(hmax)-b1Ht_SEA)/b2Ht_SEA); dbh_crit

#---------------------------------------------------------------------#
# total plot AGB (kg C m-2)
# sum kg C for all stems / area (50-ha plot)
sum(agb_1_Med)/plot_area  # IALLOM = 1 = 21.01 kg C m-2 for SPKS
# CAO ACD estimate for total biomass in 50-ha plot = 
sum(agb_34_Med)/plot_area # IALLOM = 3/4 = 22.62 kg C m-2 for SPKS
#---------------------------------------------------------------------#
#---------------------------------------------------------------------#
#sum(agb_1)/nplant
#sum(agb_34)/nplant

#---------------------------------------------------------------------#
# combine BDEAD data into data frame & plot
#---------------------------------------------------------------------#
agb_dat <- data.frame(cbind(dbh,agb_1_Low,agb_1_Med,agb_1_High,agb_34_Low,agb_34_Med,agb_34_High))
dat_long <- tidyr::gather(agb_dat,"Allometry","bdead",2:ncol(agb_dat)); head(dat_long)
# agb_dat_samp <- agb_dat[sample(nrow(agb_dat), 40000), ]
# dat_long <- tidyr::gather(agb_dat_samp,"Allometry","bdead",2:ncol(agb_dat_samp)); head(dat_long)

# takes a really long time to plot....

ggplot(dat_long, aes(x=dbh,y=bdead)) + 
  geom_point(aes(col=Allometry), size=3) +
  geom_line(aes(col=Allometry)) +
  scale_color_manual("Allometry", values=c("#800026","#fb6a4a","#e31a1c","#253494","#7bccc4","#1d91c0"), 
                     labels=c("IALLOM = 1, High WD","IALLOM = 1, Low WD","IALLOM = 1, Med WD",
                              "IALLOM = 3/4, High WD","IALLOM = 3/4, Low WD","IALLOM = 3/4, Med WD")) +
  geom_point(data=baad_sub, aes(x=DBH,y=m.st), size=4, fill="grey80", shape=21) +
  #  scale_color_manual(col="black", fill="grey80") + 
  annotate("point", x = 20, y = 40000, pch=21, fill="grey80", size = 4) +
  annotate("text", x = 60, y = 40000, label = "BAAD measured stem biomass") +
  labs(x="DBH (cm)", y="BDEAD (kg C)") +
  theme(legend.position = c(0.1,0.8))
# TIFF 650x700 Allometry_comparison_D_Bdead_DNM_2019
#---------------------------------------------------------------------#
#---------------------------------------------------------------------#


#---------------------------------------------------------------------#
# bin DBH data and plot AGB ~ binned DBH + line for cumulative
#---------------------------------------------------------------------#


#---------------------------------------------------------------------#
plot_dat <- agb_dat %>% group_by(dbh_bin) %>% summarise(n=n(),
                                                        t_AGB_1 = sum(agb_1_Med)/plot_area,
                                                        t_AGB_34 = sum(agb_34_Med)/plot_area)
plot_dat$cmsm_AGB_1 = cumsum(plot_dat$t_AGB_1)
plot_dat$cmsm_AGB_34 = cumsum(plot_dat$t_AGB_34)

p1 <- ggplot(plot_dat, aes(x = dbh_bin)) +
  geom_col(aes( y = t_AGB_1, fill="grey70"), col="black") +
  geom_line(aes(y = cmsm_AGB_1 / 5, group = 1, color = 'blackline')) +
  geom_point(aes(y = cmsm_AGB_1 / 5), color = 'black', size=2) +
  scale_y_continuous(sec.axis = sec_axis(trans = ~ . * 5, name = expression(Cumulative~Bdead~(Kg~C~m^{-2})))) +
  scale_fill_manual('', labels = 'Bdead distribution\n(Feld 2012)', values = "grey70") +
  scale_color_manual('', labels = 'Cumulative Bdead', values = 'black') + 
  labs(x="", y=expression(Bdead~(Kg~C~m^{-2}))) +
  theme(legend.position = c(0.4,0.9), legend.spacing.y = unit(0.000000001, 'cm')) 
p2 <- ggplot(plot_dat, aes(x = dbh_bin)) +
  geom_col(aes( y = t_AGB_34, fill="grey70"), col="black") +
  geom_line(aes(y = cmsm_AGB_34 / 5, group = 1, color = 'blackline')) +
  geom_point(aes(y = cmsm_AGB_34 / 5), color = 'black', size=2) +
  scale_y_continuous(sec.axis = sec_axis(trans = ~ . * 5, name = expression(Cumulative~Bdead~(Kg~C~m^{-2})))) +
  scale_fill_manual('', labels = 'Bdead distribution\n(Chave 2014)', values = "grey70") +
  scale_color_manual('', labels = 'Cumulative Bdead', values = 'black') + 
  labs(x="DBH (cm)", y=expression(Bdead~(Kg~C~m^{-2}))) +
  theme(legend.position = c(0.4,0.9), legend.spacing.y = unit(0.000000001, 'mm')) 
plot_grid(p1, p2, ncol = 1, align = 'v')
#---------------------------------------------------------------------#
#---------------------------------------------------------------------#




