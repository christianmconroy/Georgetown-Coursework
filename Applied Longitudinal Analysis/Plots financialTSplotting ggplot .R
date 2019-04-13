macLong <- read.table('macroLong.txt', header = TRUE)
macWide <- read.table('macroWide.txt', header = TRUE)

##GDP (current US$)
GDP <- macLong[which(macLong$Indicator=='GDP (current US$)'),]

require(ggplot2) 
p <- ggplot(data = GDP, aes(x = Time, y = Value, group = Country)) 
group_names <- c('1'='Developed', '0'='Emerging') 
p + geom_line() + facet_grid(. ~ Classification, labeller = as_labeller(group_names)) 
p + geom_line(alpha=.2) + facet_grid(. ~ Classification, labeller = as_labeller(group_names)) + 
  stat_summary(aes(group = Classification), geom = "point", fun.y = mean, shape = 19, size = 1) + 
  stat_summary(aes(group = Classification), geom='line', fun.y = mean, size = 0.8) 

p2 <- ggplot(data = GDP, aes(x = Time, y = Value, color = Classification)) 
p2 + stat_summary(aes(group = Classification), geom='line', fun.y = mean, size = 1) + 
  stat_summary(aes(group = Classification), geom='point', fun.y = mean, size = 3) 


## Market Capitalization (current US$)
mark.cap <- macLong[which(macLong$Indicator==
                            'Market capitalization of listed domestic companies (current US$)'),]
head(mark.cap)

p.mc <- ggplot(data = mark.cap, aes(x = Time, y = Value, group = Country)) 
p.mc + geom_line() + facet_grid(. ~ Classification, labeller = as_labeller(group_names)) 
p.mc + geom_line(alpha=.2) + facet_grid(. ~ Classification, labeller = as_labeller(group_names)) + 
  stat_summary(aes(group = Classification), geom = "point", fun.y = mean, shape = 19, size = 1) + 
  stat_summary(aes(group = Classification), geom='line', fun.y = mean, size = 0.8) 

p2.mc <- ggplot(data = mark.cap, aes(x = Time, y = Value, color = Classification)) 
p2.mc + stat_summary(aes(group = Classification), geom='line', fun.y = mean, size = 1) + 
  stat_summary(aes(group = Classification), geom='point', fun.y = mean, size = 3) 


## Stock Price Volatility
spv <- macLong[which(macLong$Indicator=='Stock price volatility'),]
head(spv)

p.spv <- ggplot(data = spv, aes(x = Time, y = Value, group = Country)) 
p.spv + geom_line() + facet_grid(. ~ Classification, labeller = as_labeller(group_names)) 
p.spv + geom_line(alpha=.2) + facet_grid(. ~ Classification, labeller = as_labeller(group_names)) + 
  stat_summary(aes(group = Classification), geom = "point", fun.y = mean, shape = 19, size = 1) + 
  stat_summary(aes(group = Classification), geom='line', fun.y = mean, size = 0.8) 

p2.spv <- ggplot(data = spv, aes(x = Time, y = Value, color = Classification)) 
p2.spv + stat_summary(aes(group = Classification), geom='line', fun.y = mean, size = 1) + 
  stat_summary(aes(group = Classification), geom='point', fun.y = mean, size = 3) 
