library(tidyverse)
library(nlme)
options(scipen = 9999)

n_loop <- c(50, 100, 150, 200, 250, 300, 350, 400, 450)
sims <- 500
outstore <- matrix(, nrow = length(n_loop)*sims, ncol = 4)
simcounter <- 0

for(val in n_loop){
  



for(k in 1:sims){
  simcounter <- simcounter +  1

people <- val
time <- 10
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
outstore[simcounter, 2] <- people
outstore[simcounter, 3] <- beffect
outstore[simcounter, 4] <- pval

}
  
}


dd <- data.frame(outstore)
names(dd) <- c('simulation', 'N', 'beffect', 'pval')
d <- dd %>% group_by(N) %>% 
  summarize(
    numsignificant = sum(pval < 0.05),
    meanbeta = mean(beffect)
  )

d <- d %>% mutate(propsignificant = numsignificant / sims)
ggplot(d, aes(x = N, y = propsignificant)) + 
  geom_bar(stat = "identity", color = "orange") + 
  labs(y = "Proportion Significant")


d<- d %>% mutate(case = "n-smalleffect")
write.csv(d, "data/nsmall.csv", row.names = F)





