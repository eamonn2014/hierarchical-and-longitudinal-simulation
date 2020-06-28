
## this is good...
## need to add a treatment effect starting after baseline
## hybrid approach from my nested app and longitudinal modelling app

rm(list=ls())

set.seed(786)

library(lme4)
library(VCA)
library(ggplot2)
library(MASS)
library(nlme)
library(tidyverse)

p1 <- function(x) {formatC(x, format="f", digits=1)}

# 3 level nested app chunk~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# design
intercept =100 
top = 5 # number of groups at top of hierarchy
# in the app this is seletable...
range1     = c(2, 10) ##app sample these, levels 
range2     = c(5, 10) ##app sample these
replicates = c(3, 10) ##app sample these

x1 <- range1[1]
x2 <- range1[2]
x3 <- range2[1]
x4 <- range2[2]
x5 <- replicates[1]
x6 <- replicates[2]

# SDs
a =20
b =10 
#c =2   not need as we add a random intercept and random slope later
d =2

# this is from my app so the sections below are not necessary for plain R code
# seems that I need to use both c(x1,x2) c(x1:x2) so sample function works correctly

if (x1==x2) {
  
  middle <-  sample(c(x1,x2),   top, replace=TRUE)    # ditto groups in each top level 6
  
} else {
  
  middle <-  sample(c(x1:x2),   top, replace=TRUE)    # ditto groups in each top level 6
}


if (x3==x4) {
  
  lower <-   sample(c(x3,x4),   sum(middle), replace=TRUE )
  
} else {
  
  lower <-   sample(c(x3:x4),   sum(middle), replace=TRUE )
  
}

if (x5==x6) {
  
  replicates <-  sample(c(x5,x6),   sum(lower), replace=TRUE )
  
} else {
  
  replicates <-  sample(c(x5:x6),   sum(lower), replace=TRUE ) 
  
}


N <- sum(replicates)

residual <- d
d <- intercept


# random effects
top.r <-    rnorm(top,          d,                a)    
middle.r <- rnorm(sum(middle),  0,                b)     
#lower.r <-  rnorm(sum(lower),   0,                c)     # not needed as we create this later

# ids
lower.id <- rep(seq_len(sum(lower)), replicates )        
middle.id <- cut(lower.id, c(0,cumsum(lower)),  labels=FALSE)
top.id   <- cut(middle.id, c(0,cumsum(middle)), labels=FALSE)

#---------------------------------------------------------------------------
 ### set number of individuals, this chunk from longitudianl app
n      <- N
beta1  <- -1 # slope
ar.val <- 0.9
sigma  <- residual 
tau0   <-  1.2748
tau1   <-  0.2276
tau01  <- -0.62

### simulate (correlated) random effects for intercepts and slopes
mu  <- c(0,0)
S   <- matrix(c(1, tau01, tau01, 1), nrow=2)
tau <- c(tau0, tau1)
S   <- diag(tau) %*% S %*% diag(tau)
U   <- mvrnorm(n, mu=mu, Sigma=S)

p <- rle(lower.id)$lengths
eij <- unlist(sapply(p, function(x) arima.sim(model=list(ar=ar.val), n=x) * sqrt(1-ar.val^.2) * sigma))
eij <- as.vector(eij)   #new

#--------------------------------------------------------------------------
time = ave(lower.id, lower.id, FUN = seq_along)  # time variable

Data <- data.frame( top=top.id, mid=middle.id, low=lower.id, 
                    
                    time=time,

                    # random noise
                    y2= rnorm( sum(replicates), 
                              top.r[top.id] + 
                              middle.r[middle.id] +
                              U[,1][lower.id] + 
                    ((beta1 +  U[,2][lower.id]) *  time), sigma), 

                    # ar1 nosie
                    y=   top.r[top.id] + 
                              middle.r[middle.id] +
                              U[,1][lower.id] + 
                              ((beta1 +  U[,2][lower.id]) *  time) +
                              eij
)
                              
df <- as.data.frame(Data)

df[1:20,]

df$time <- factor(df$time)

(fit2 <- lmer( y2 ~ time + (1|top) + (1|mid) + (as.numeric(time)|low), data=df))
 
(fit3 <- lmer( y ~ time + (1|top) + (1|mid) + (as.numeric(time)|low), data=df))

(fit.res <-  
  tryCatch(gls(y2 ~ time +0 ,
               correlation=corAR1(form=~ as.numeric(time)|low), 
               weights=varIdent(form=~1|time),
               df, na.action=na.omit) , 
           error=function(e) e))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~plot not model based~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df$VISIT <- df$time
df$value <- df$y2       # ar1 is th ey response
df$variable <- "BIOCHEM.B"
df$ID <- factor(df$low)
df$VISIT=as.numeric(levels(df$VISIT))[df$VISIT]

df_summary <- df %>% # the names of the new data frame and the data frame to be summarised
  group_by(VISIT, variable) %>%                # the grouping variable
  summarise(mean_PL = mean(value, na.rm=TRUE),  # calculates the mean of each group
            sd_PL = sd(value, na.rm=TRUE),      # calculates the sd of each group
            n_PL = length(na.omit(value)),      # calculates the sample size per group
            SE_PL = sd(value, na.rm=TRUE)/sqrt(length(na.omit(value)))) # SE of each group

df_summary1 <- merge(df, df_summary)  # merge stats to dataset

df_summary1$L2SE <- df_summary1$mean_PL - 2*df_summary1$SE_PL
df_summary1$H2SE <- df_summary1$mean_PL + 2*df_summary1$SE_PL


pr1 <- ggplot((df_summary1), aes(x = VISIT, y =value, color = ID)) +
  geom_line( size=.5, alpha=0.2) +
  #scale_color_gradient(low = "blue", high = "red")+
  #scale_color_brewer(palette = "Dark2") +
  stat_summary(geom="line",  fun=mean, colour="black", lwd=0.5) +  # , linetype="dashed"
  stat_summary(geom="point", fun=mean, colour="black") +
  geom_errorbar(data=(df_summary1), 
                aes( ymin=L2SE, ymax=H2SE ), color = "black",
                width=0.05, lwd = 0.05) +
  scale_y_continuous(expand = c(.1,0) ) +
  
  
  
  scale_x_continuous(breaks = c(unique(df$VISIT)),
                     labels = 
                       c(unique(df$VISIT))
  ) +
  
  

  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background=element_rect(fill = "white"),
    panel.background = element_rect(fill = 'black'),
    # Change legend
    legend.position = c(0.6, 0.07),
    legend.direction = "horizontal",
    legend.background = element_rect(fill = "black", color = NA),
    legend.key = element_rect(color = "gray", fill = "black"),
    legend.title = element_text(color = "white"),
    legend.text = element_text(color = "white")
  ) +



EnvStats::stat_n_text(size = 4, y.pos = max(df_summary1$value, na.rm=T)*1.1 , y.expand.factor=0, 
                      angle = 0, hjust = .5, family = "mono", fontface = "plain") + #295 bold
  
  theme(panel.background=element_blank(),
        # axis.text.y=element_blank(),
        # axis.ticks.y=element_blank(),
        # https://stackoverflow.com/questions/46482846/ggplot2-x-axis-extreme-right-tick-label-clipped-after-insetting-legend
        # stop axis being clipped
        plot.title=element_text(), plot.margin = unit(c(5.5,12,5.5,5.5), "pt"),
        legend.text=element_text(size=12),
        legend.title=element_text(size=14),
        legend.position="none",
        axis.text.x  = element_text(size=10),
        axis.text.y  = element_text(size=10),
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        plot.caption=element_text(hjust = 0, size = 7))





print(pr1 + labs(y="Response", x = "Visit") + 
        ggtitle(paste0("Individual responses ",
                       length(unique(df$ID))," patients & arithmetic mean with 95% CI shown in black\nNumber of patient values at each time point") )
)

##############

fit2
fit3
fit.res














































