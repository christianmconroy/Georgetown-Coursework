# World Bank Repository, GGPlot colored lines by variable time series 

install.packages("WDI")
install.packages("countrycode")
install.packages("plm")
install.packages("ggplot2")
install.packages("reporttools")
install.packages("stargazer")
install.packages("psData")

require(WDI) 
WDIsearch("gdp growth") 

wb <- WDI(country=c("US", "FR"), indicator=c("NY.GDP.MKTP.KD.ZG"), start=1960, end=2012)

wb <- WDI(country="all", indicator=c("SE.XPD.TOTL.GB.ZS", "GC.DOD.TOTL.GD.ZS",
                                     "BN.CAB.XOKA.GD.ZS", "SP.POP.DPND", "AG.LND.TOTL.K2",
                                     "NY.GDP.MKTP.CD", "NY.GDP.MKTP.PP.CD",
                                     "NY.GDP.MKTP.KD", "NY.GDP.PCAP.CD", "SP.POP.TOTL",
                                     "IT.PRT.NEWS.P3", "IT.RAD.SETS", "NE.IMP.GNFS.ZS",
                                     "NE.EXP.GNFS.ZS", "NE.TRD.GNFS.ZS",
                                     "BX.KLT.DINV.WD.GD.ZS", "BN.KLT.PRVT.GD.ZS",
                                     "NE.CON.GOVT.ZS", "SL.IND.EMPL.ZS", "NV.IND.TOTL.ZS",
                                     "NY.GDP.MKTP.KD.ZG", "NY.GDP.PCAP.KD"),
          start=1960,
          end=2012,
          extra=TRUE)

# Assignment operator "<-" will add a new variable to the dataframe "wb"
wb$debt<-wb$GC.DOD.TOTL.GD.ZS # central government debt as share of gdp
wb$curracct<-wb$BN.CAB.XOKA.GD.ZS # current account balance as share of gdp
wb$dependency<-wb$SP.POP.DPND # dependency-age population as share of total population
wb$land<-wb$AG.LND.TOTL.K2 # total land in square kilometers
wb$pop<-wb$SP.POP.TOTL # total population
wb$radioscap<-(wb$IT.RAD.SETS/wb$pop)*1000 # radios per 1000 people
wb$logradioscap<-log(wb$radioscap + 1) # logarithm of radios per capita, because it's skewed
wb$newspaperscap<-wb$IT.PRT.NEWS.P3 # newspapers in circulation per 1000 people
wb$gdp<-wb$NY.GDP.MKTP.KD # gross domestic product, constant 2000 USD
wb$gdp2<-wb$NY.GDP.MKTP.CD # gross domestic product, current USD, millions
wb$gdp3<-wb$NY.GDP.MKTP.PP.CD # gross domestic product, purchasing-power parity, current
wb$gdpcap<-wb$NY.GDP.PCAP.KD # gdp per capita constant 2000 USD
wb$gdpgrowth<-wb$NY.GDP.MKTP.KD.ZG # change in gross domestic product
wb$imports<-wb$NE.IMP.GNFS.ZS # imports of goods and services as share of gdp
wb$exports<-wb$NE.EXP.GNFS.ZS # exports of goods and services as share of gdp
wb$trade<-wb$NE.TRD.GNFS.ZS # total trade as share of gdp
wb$fdi<-wb$BX.KLT.DINV.WD.GD.ZS # foreign-direct investment as share of gdp
wb$privatecapital<-wb$BN.KLT.PRVT.GD.ZS # private capital flows as share of gdp
wb$spending<-wb$NE.CON.GOVT.ZS # government consumption expenditure as share of gdp
wb$industry<-wb$SL.IND.EMPL.ZS # employment in industry as share of total employment
wb$industry2<-wb$NV.IND.TOTL.ZS # value added in industry as share of gdp

head(wb[,1:7]) # limit to columns 1 through 7

# make a subset of only the region/world aggregate measures
aggregates<-subset(wb, region=="Aggregates")
# make a subset of only global-level aggregate measures
world<-subset(aggregates, country=="World")
# Make a subset of only region aggregates
regions<-subset(aggregates, country=="East Asia & Pacific (all income levels)" |
                  country=="Middle East & North Africa (all income levels)" |
                  country=="Sub-Saharan Africa (all income levels)" |
                  country=="North America" |
                  country=="Latin America & Caribbean (all income levels)" |
                  country=="Europe & Central Asia (all income levels)" |
                  country=="South Asia")
wb<-subset(wb, region!="Aggregates")

# load ggplot2 package, must install.packages("ggplot2") if you haven't previously.
require(ggplot2)
ggplot(world, aes(x = year)) +
  geom_line(aes(y = trade, colour = "Trade")) +
  geom_line(aes(y = spending, colour = "Spending")) +
  scale_colour_discrete(name = "Variables") +
  xlab("Year") +
  ylab("% of GDP") +
  ggtitle("Trade and Government Spending Around the World, 1960-2012")

ggplot(regions, aes(x = year, y = gdpcap, colour=country)) +
  geom_line() +
  scale_colour_discrete(name = "Regions") +
  xlab("Year") +
  ylab("GDP Per Capita") +
  ggtitle("GDP Per Capita by Region, 1960-2012")

ggplot(wb[wb$region=="Europe & Central Asia (all income levels)",], aes(x=year, y=trade)) +
  geom_line(aes(group=country)) +
  facet_wrap(~ country) +
  xlab("Year") +
  ylab("Trade (% of GDP)") +
  ggtitle("Trade Levels in Europe and Central Asia, 1960-2012") +
  theme(axis.text.x = element_text(angle = 75, hjust = 1))

require(psData)
polity <- PolityGet(vars = "polity2")
polity<-subset(polity, year>=1960 & year<=2012)
head(polity) # peek at the dataframe
