library(tidyverse)
library(nlme)
options(scipen = 9999)

t_loop <- c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
sims <- 500
outstore <- matrix(, nrow = length(t_loop)*sims, ncol = 4)
simcounter <- 0

for(val in t_loop){
  



for(k in 1:sims){
  simcounter <- simcounter +  1

people <- 100
time <- val
dfstore <- matrix(, nrow = people*time, ncol = 4)
count <- 0
for(i in 1:people){
  
  for(t in 1:time){
    count <- count + 1
    
    if(t == 1){
      dfstore[count, 1] <- i
      dfstore[count, 2] <- t
      dfstore[count, 3] <- rnorm(1, 0, 1)
      dfstore[count, 4] <- rnorm(1, 0, 1)
    }else{
      dfstore[count, 1] <- i
      dfstore[count, 2] <- t
      dfstore[count, 3] <- 0.5*dfstore[count - 1, 3] + rnorm(1, 0, 1)
      dfstore[count, 4] <- 0.5*dfstore[count - 1, 4] + 0.1*dfstore[count - 1, 3] + rnorm(1, 0, 1)
      
    }
    
  }

}
df <- data.frame(dfstore)
names(df) <- c('id', 'time', 'x', 'y')
model <- lme(y ~ x,
             random = ~1 | id,
             data = df)

beffect <- fixed.effects(model)[2]
pval <- summary(model)$tTable[2 , 5]

outstore[simcounter, 1] <- k
outstore[simcounter, 2] <- time
outstore[simcounter, 3] <- beffect
outstore[simcounter, 4] <- pval

}
  
}


dd <- data.frame(outstore)
names(dd) <- c('simulation', 'periods', 'beffect', 'pval')
d <- dd %>% group_by(periods) %>% 
  summarize(
    numsignificant = sum(pval < 0.05),
    meanbeta = mean(beffect)
  )

d <- d %>% mutate(propsignificant = numsignificant / sims)
ggplot(d, aes(x = periods, y = propsignificant)) + 
  geom_bar(stat = "identity", color = "orange") + 
  labs(y = "Proportion Significant",
       x = 'Periods')



d<- d %>% mutate(case = "t-smalleffect")
write.csv(d, "data/tsmall.csv", row.names = F)






