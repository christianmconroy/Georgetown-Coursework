# Stacked Bars

## For Trade Visuals##
library(ggplot2)
library(reshape2)
library(scales)

##### GSP Chart

# Values below are in thousand Euro 
df <- data.frame("Year" = c(2014, 2015, 2016),
                 "Total Imports"= c(35582745, 37822096, 37664155),
                 "GSP Eligible Imports"= c(16987636, 18910730, 18864567),
                 "GSP Preferential Imports"= c(15031426, 16698497, 16622456))
df$EligbutMGN <- eval(df$GSP.Eligible.Imports-df$GSP.Preferential.Imports)
df$MFNNotElig <- eval(df$Total.Imports-df$GSP.Eligible.Imports)
df$urate <- eval(df$GSP.Preferential.Imports/df$GSP.Eligible.Imports)

names(df)
# Melt dataframe for plotting
data.m <- melt(df[c(1,4:6)], id.vars='Year')
data.m$pct <- eval(data.m$value/df$Total.Imports)
data.m$urate <- c(.885, .883, .881, .885, .883, .881, .885, .883, .881)

# plot everything - Horizontal
ggplot(data.m, aes(Year, value)) + geom_col(aes(fill = variable)) + coord_flip() + scale_y_continuous(labels = comma) + labs(y = "Value (thousand EUR)") + scale_fill_discrete(name = "Regime Type", labels=c("GSP Preferential Imports", "Eligible for GSP but MFN Claimed", "MFN Claimed, Not Eligible for GSP")) + geom_text(aes(x = Year, y = value + .25, label = paste0(format(100*pct, digits = 2), '%'), group = variable), position = position_stack(vjust = .5))

# plot everything - Vertical
ggplot(data.m, aes(Year, value)) + geom_col(aes(fill = variable)) + scale_y_continuous(labels = comma) + labs(title = "Indian Union Budget 2018-2019\nChanges in Customs Duty Rates", y = "Value (thousand EUR)") + scale_fill_discrete(name = "Regime Type", labels=c("GSP Preferential Imports", "Eligible for GSP but MFN Claimed", "MFN Claimed, Not Eligible for GSP")) + geom_text(aes(x = Year, y = value + .25, label = paste0(format(100*pct, digits = 2), '%'), group = variable), position = position_stack(vjust = .5)) 



#### Tariff Change Dodge Bar Chart

df2 <- data.frame("Product"= c("Cellphones", "CellParts", "PCBAchar", "Smartwatch", "LCD panels", "Specpartspanels", "silicatelc"),
                  "Current" = c(.15, .075, 0.0, .10, .075, 0.0, 0.0),
                 "2018 Budget"= c(.20, .15, .10, .20, .15, .10, .05))
# Include note that some specified parts and LCD can get up to 10%

# Vertical bar position dodge with % listed in or on top 
df3 <- melt(df2[,c('Product','Current','X2018.Budget')],id.vars = 1)

ggplot(df3 ,aes(x = Product,y = value*100)) + geom_col(aes(fill = variable), position = "dodge") + geom_text(aes(x = Product,y = value *100 +1,label=paste(round(value * 100, 1), "%", sep=""), group=variable), position=position_dodge(width=1), size=4) + scale_fill_discrete(name = "Period", labels=c("Current", "2018 Budget")) + labs(y = "Rate of Duty", x = "Product") + scale_y_continuous(labels = function(x) paste0(x, "%")) + scale_x_discrete(breaks=c("Cellphones", "CellParts", "PCBAchar", "Smartwatch", "LCD panels", "Specpartspanels", "silicatelc"), labels=c("Cellular mobile phones ", "Parts/accessories of \ncellular phones ", "PCBA of charger and moulded \nplastics of charger \nof cellular mobile phones", "Smart watches/wearables", "LCD/LED/OLED panels and parts", "Specified parts for\n LCD/LED TV panels", "Preform/silica for use in \nmanufacture of optical fibres")) +theme(axis.text.x=element_text(angle=45,hjust=1,size=15)) 


