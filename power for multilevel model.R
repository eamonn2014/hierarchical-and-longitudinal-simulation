library(lme4)

sim1 <- function(bSex=0, bFreq=0, bSF=0, b0=1000, Vsubj=1, Vword=1, Verror=1) {
  Subject <- rep( 1:60, each=50 )
  Word <- rep( 1:50, 60 )
  Sex <- rep(c('M','F'), each=50*30)
  
  #  assume frequency is constant accross word, random from 1-100
  tmp <- sample( 1:100, 50, replace=TRUE )
  Frequency <- tmp[Word]
  
  # random effects per subject
  S.re <- rnorm(60, 0, sqrt(Vsubj))
  
  # random effects per word
  W.re <- rnorm(50, 0, sqrt(Vword))
  
  # epsilons
  eps <- rnorm(50*60, 0, sqrt(Verror))
  
  # put it all together
  ReactionTime <- b0 + bSex*(Sex=='M') + bFreq*Frequency + bSF*(Sex=='M')*Frequency +
    S.re[Subject] + W.re[Word] + eps
  
  # put into a data frame
  mydata <- data.frame( Subject = paste('s',Subject, sep=''), 
                        Word = paste('w', Word, sep=''), Sex=Sex, Frequency=Frequency,
                        ReactionTime = ReactionTime)
  
  # analyze looking at interaction term with LR test
  fit1 <- lmer( ReactionTime ~ (Sex*Frequency) + (1|Subject) + (1|Word), data=mydata)
  fit2 <- lmer( ReactionTime ~ Sex + Frequency + (1|Subject) + (1|Word), data=mydata)
  anova(fit2,fit1)[2,7]
}



pb <- winProgressBar(max=100) # or tkProgressBar or txtProgressbar

setWinProgressBar(pb, 0)
out1 <- replicate( 100, {setWinProgressBar(pb, getWinProgressBar(pb)+1);
  sim1( bSex=10, bFreq=2, bSF=0.25, Vsub=4000, Vword=2500, Verror=10000)})
hist(out1)
mean( out1 < 0.05 )