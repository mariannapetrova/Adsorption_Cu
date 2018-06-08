install.packages("extrafont")

library(readr)
library(ggplot2)

library(extrafont)
font_import()
loadfonts(device="win")       #Register fonts for Windows bitmap output
fonts() 

cu <- read_csv("D:/Data Science/R1/Adsorption/Book2.csv")
cu1 <- read_csv("D:/Data Science/R1/Adsorption/Book21.csv")
xrd <- read_csv("D:/Data Science/R1/Adsorption/Book3.csv")
unm <- read_csv("D:/Data Science/R1/Adsorption/UNM.csv")
ss <- read_csv("D:/Data Science/R1/Adsorption/cleaning.csv")
csd <- read_csv("D:/Data Science/R1/Adsorption/cs_d.csv")

csd$Ads <- as.character(csd$Ads)

head(csd)

ss <-  NULL

unmex <- subset(unm, unm$Isotherm == 'Experimental')
unm <- subset(unm, unm$Isotherm != 'Experimental')

cu1 <- subset(cu1, Isotherm != 'Experimental' & cu1$X1 < 1150)
cuex <- subset(cu, Qe != 0)
cuex$Langmuir = "Experimental"

####NEW XRD#####
k = ggplot(xrd, aes(x=xrd$`2Theta`, y = Intensity, fill = Phase, width = 0.4))+
  geom_col(col = 'black') + 
  labs(x = '2 Theta, degrees', y='Intensity') + 
  theme_minimal()+
  theme(
    text = element_text(family = 'Times New Roman', size = 16, colour = 'black'),
    axis.text =  element_text(colour = 'black', size = 16.05),
    legend.title=element_blank(),
    legend.text = element_text(size = 16),
    legend.position=c(0.8,0.7),
    axis.title.x = element_text(margin = margin(t = 12)),
    axis.title.y = element_text(margin = margin(r = 12)))

k
getwd()

#SAVE FILE
ggsave('fig1.wmf', plot = last_plot(), device = 'wmf', dpi = 600)


isotherms <- c('Experimental', 'Langmuir', 'Freundlich', 'Redlich-Peterson', 'Toth', 'Langmuir-Freundlich')
cuex <- subset(cu1, Isotherm == 'Experimental')

####### CODE FOR ISOTHERM modified

library(ggplot2)
k1 = ggplot(data = cu1, aes(x = X1, y = Qe, color = Isotherm))+
geom_line(aes(linetype = Isotherm), size = 1.1) +
geom_point(inherit.aes = FALSE, data = cuex, aes(x = X1, y = Qe, shape = Langmuir), color = 'blue', size = 3) +
labs(x = 'Equilibrium concentration of Cu, mg/l', y='Adsorption, mg/g') + 
xlim(0, 1100)+  
  theme_minimal()  
k1 + scale_color_manual(values=c("green", 'firebrick1', 'black', 'orange', 'black')) +
  theme(
    legend.title=element_blank(), 
    legend.position=c(0.2,0.7),
    legend.text = element_text(size = 16),
    text = element_text(family = 'Times New Roman', size = 16),
    axis.text = element_text(colour = 'black', size = 16.05),
    axis.title = element_text(),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)))

#SAVE FILE
ggsave('fig4.jpg', plot = last_plot(), device = 'jpg', dpi = 600)
ggsave('fig4.wmf', plot = last_plot(), device = 'wmf', dpi = 600)

################

##### CODE FOR ISOTHERM natural
library(dplyr)

un = ggplot(data = unm, aes(x = X1, y = Qe, color = Isotherm))+
  geom_line(aes(linetype = Isotherm), size = 1.1) +
  geom_point(inherit.aes = FALSE, data = unmex, aes(x = X1, y = Qe, shape = Isotherm), color = 'blue', size = 3) +
  #geom_point(inherit.aes = FALSE, data = unmex, aes(x = X1, y = Qe , shape = Isotherm), color = 'white', size = 1) +
  labs(x = 'Equilibrium concentration of Cu, mg/l', y='Adsorption, mg/g') + 
  theme_minimal()  
un + scale_color_manual(values=c("green", 'firebrick1', 'black', 'orange', 'black', 'black')) + 
theme(
  legend.title=element_blank(), 
  legend.position=c(0.15,0.8),
  text = element_text(family = 'Times New Roman', size = 16),
  legend.text = element_text(size = 16),
  axis.text = element_text(colour = 'black', size = 16.05),
  axis.title = element_text(),
  axis.title.x = element_text(margin = margin(t = 15)),
  axis.title.y = element_text(margin = margin(r = 15))
)

#SAVE FILE
ggsave('fig3.wmf', plot = last_plot(), device = 'wmf', units = "in",  dpi = 600)
ggsave('fig3.jpg', plot = last_plot(), device = 'jpg', units = "in",  dpi = 600)


#################

ss1 <- subset(ss, ss$C0 > 80)
ss2 <- subset(ss, ss$C0 < 56.0)

lmnat <- lm(ss1$S~ ss1$C0, data = ss1, ss1$Ads =='nat')
summary(lmnat)

lmmod <- lm(ss1$S~ ss1$C0, data = ss1, ss1$Ads =='mod')
summary(lmmod)
resnat <- rstandard(lmnat)
plot(resnat)
qqline(resnat)

library(MASS)
par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(lmnat)


resst <- rstandard(lmmod)
qqnorm(resst)
qqline(resst)
##### CODE FOR S%
s = ggplot(data = ss1, aes(x = C0, y = S, col = Ads))+
  geom_smooth(method = lm, size = 1.3) +
  geom_point(color = 'black', size =3) +
  geom_point(size =2) +
  labs(x = 'Initial concentration of Cu, mg/l', y='Percent removal') + 
  theme_minimal() +
  theme(
    legend.title=element_blank(), 
    legend.position=c(0.8,0.8),
    legend.text=element_text(size=16),
    text = element_text(family = 'Times New Roman', size = 16),
    axis.text = element_text(colour = 'black', size=16.1),
    axis.title = element_text(),
    axis.title.x = element_text(margin = margin(t=15)),
    axis.title.y = element_text(margin = margin(r=15)))
s

ggsave('fig2.wmf', plot = last_plot(), device = 'wmf', units = "in",  dpi = 600)
ggsave('fig2.jpg', plot = last_plot(), device = 'jpg', units = "in",  dpi = 600)

d = ggplot(data = ss2, aes(x = C0, y = S, col = Ads))+
  geom_line() +
  geom_point() +
  #geom_point(inherit.aes = FALSE, data = unmex, aes(x = X1, y = Qe , shape = Isotherm), color = 'white', size = 1) +
  labs(x = 'Equilibrium concentration of Cu, mg/l', y='Adsorption, mg/g') + 
  theme_minimal()  
d


kd1 = ggplot(data = ss1, aes(x = Ads, y = kd, fill = Ads))+
  geom_boxplot() +
  geom_jitter(alpha = 0.7, col = 'green') +
  labs(x = 'Adsorbent', y='Distribution coefficient, l/g') + 
  theme_minimal()  
kd1


cskd = ggplot(data = csd, aes(x = Ads, y = kd, fill = exp))+
  geom_boxplot() +
  geom_jitter(alpha = 0.7) +
  labs(x = 'Equilibrium concentration of Cu, mg/l', y='Adsorption, mg/g') + 
  theme_minimal()  
cskd





library(ggplot2)
ggplot(data = cu1, aes(x = X1, y = Qe, fill = Isotherm))  +
    geom_smooth(col = 'red') + 
  ggtitle('Adsorption of Cu on bentonite') +
  labs(x = 'Equilibrium concentration of Cu, mg/l', y='Adsorption, mg/g') + 
  theme_minimal()
