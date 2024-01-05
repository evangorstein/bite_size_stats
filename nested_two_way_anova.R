library(tidyverse)
library(lme4)
library(lmerTest)
library(conflicted)
conflict_prefer("lmer", "lmerTest")

#Load data
df <- data.frame(growth=c(13, 16, 16, 12, 15, 16, 19, 16, 15, 15, 12, 15,
                          19, 19, 20, 22, 23, 18, 16, 18, 19, 20, 21, 21,
                          21, 23, 24, 22, 25, 20, 20, 22, 24, 22, 25, 26),
                 fertilizer=rep(c('A', 'B', 'C'), each=12),
                 tech=factor(rep(1:9, each=4)),
                 tech2 = factor(rep(rep(1:3, each=4), 3)))

#create boxplots to visualize plant growth
ggplot(df, aes(x=tech, y=growth, fill=fertilizer)) +
  geom_boxplot()

contrasts(df$tech2) = contr.sum(3)
mod1 <- lm(growth ~ fertilizer/tech2, data = df)
summary(mod1)
anova(mod1)
#Equivalently, aov(growth ~ fertilizer/tech2, data = df) or 
# aov(growth ~ fertilizer/tech, data = df) or 
# aov(growth ~ fertilizer*tech, data = df) or 
# aov(growth ~ fertilizer + tech, data = df) 

mod2 <- aov(growth ~ fertilizer + Error(tech), data = df)
summary(mod2)

mod2.1 <- lmer(growth ~ fertilizer + (1 | tech), data = df)
summary(mod2.1)
anova(mod2.1)
#as.data.frame(ranef(mod2.1))


gdf <- group_by(df, tech) %>%
  summarise(fertilizer = first(fertilizer), growth = mean(growth))


mod3 <- lm(growth ~ fertilizer, data = gdf)
summary(mod3)

#################3 New data ############################3
scores <- c(25, 29, 14, 11, 11, 6, 22, 18, 17, 20, 5, 2)
school <- factor(c("A", "A", "A", "A", "B", "B", "B", "B", "C", "C", "C", "C"))
teacher <- factor(c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6))
teacher2 <- factor(c(1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2))
df2 <- data.frame(scores, school, teacher, teacher2)

res2 <- lm(scores ~ school + school/teacher2)
anova(res2)
summary(res2)




