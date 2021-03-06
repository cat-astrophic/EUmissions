# This script performs the data analysis for a paper on the effect of EU NOx emissions standards on EU NOx emissions

# Loading libraries

library(stargazer)
library(plotly)

# Declaring the filepath for the data file

filepath <- 'C:/Users/User/Documents/Data/EUmissions.csv'

# Reading in the data

noxdata <- read.csv(filepath)

# Create per capita emissions, natural log of per capita emissions, natural log of GDP per capita,
# initial nox per capita, and the natural log of initial nox per capita

noxdata$Oxides.of.Nitrogen.per.capita <- 1000*noxdata$Oxides.of.Nitrogen / noxdata$Population
noxdata$Ln.Oxides.of.Nitrogen.per.capita <- log(noxdata$Oxides.of.Nitrogen.per.capita)
noxdata$Ln.GDP.per.capita <- log(noxdata$GDP.per.capita)
noxdata$Initial.NOx.per.capita <- 1000*noxdata$Initial.NOx / noxdata$Initial.Population
noxdata$Ln.Initial.NOx.per.capita <- log(noxdata$Initial.NOx.per.capita)

# Create a new dataframe (for visualization purposes when looking at histograms)

vars <- c('Oxides.of.Nitrogen.per.capita', 'Ln.Oxides.of.Nitrogen.per.capita', 'Ln.GDP.per.capita',
          'Renewable.Energy...Pct', 'HD.NOx', 'R.D', 'Urban.Population...Pct', 'Country', 'Year',
          'LD.Diesel.NOx', 'LD.Gas.NOx', 'EU', 'Initial.NOx.per.capita', 'Ln.Initial.NOx.per.capita',
          'GDP.per.capita', 'Growth.Rate', 'Latitude', 'Land.Area', 'Population.Density', 'Energy.Use.per.capita',
          'Fossil.Fuel.Energy.Consumption.Pct', 'Combustible.Renewables...Waste', 'Renewable.Electricity.Output')
noxdata <- noxdata[vars]
noxdata <- subset(noxdata, 2000 <= Year)
noxdata <- subset(noxdata, Year <= 2012)
noxdata <- na.omit(noxdata)

# Create a table of summary statistics for the data and write to file

write.csv(stargazer(noxdata), 'C:/Users/User/Documents/Data/EUmissions/sumtab.txt', row.names = FALSE)

# Regression models

# Baseline model

noxmod1 <- lm(Ln.Oxides.of.Nitrogen.per.capita ~ Ln.Initial.NOx.per.capita + Growth.Rate, data = noxdata)

# Baseline models with ln(initial NOx), ln(GPD per capita), and the different NOx standards

noxmod2 <- lm(Ln.Oxides.of.Nitrogen.per.capita ~ Ln.Initial.NOx.per.capita + Growth.Rate
              + HD.NOx, data = noxdata)

noxmod3 <- lm(Ln.Oxides.of.Nitrogen.per.capita ~ Ln.Initial.NOx.per.capita + Growth.Rate
              + LD.Gas.NOx, data = noxdata)

noxmod4 <- lm(Ln.Oxides.of.Nitrogen.per.capita ~ Ln.Initial.NOx.per.capita + Growth.Rate
              + LD.Diesel.NOx, data = noxdata)

# Baseline models with ln(initial NOx), ln(GPD per capita), and pairs of the NOx standards

noxmod5 <- lm(Ln.Oxides.of.Nitrogen.per.capita ~ Ln.Initial.NOx.per.capita + Growth.Rate
              + HD.NOx + LD.Gas.NOx, data = noxdata)

noxmod6 <- lm(Ln.Oxides.of.Nitrogen.per.capita ~ Ln.Initial.NOx.per.capita + Growth.Rate
              + HD.NOx + LD.Diesel.NOx, data = noxdata)

# Models with no standards but with additional factors that may contribute to $\theta$

noxmod7 <- lm(Ln.Oxides.of.Nitrogen.per.capita ~ Ln.Initial.NOx.per.capita + Growth.Rate
              + Energy.Use.per.capita + R.D + Latitude + Population.Density + Renewable.Electricity.Output
              + Combustible.Renewables...Waste, data = noxdata)

# Models with single standards as a proxy and additional factors that may contribute to $\theta$

noxmod8 <- lm(Ln.Oxides.of.Nitrogen.per.capita ~ Ln.Initial.NOx.per.capita + Growth.Rate + HD.NOx
              + Energy.Use.per.capita + R.D + Latitude + Population.Density + Renewable.Electricity.Output
              + Combustible.Renewables...Waste, data = noxdata)

noxmod9 <- lm(Ln.Oxides.of.Nitrogen.per.capita ~ Ln.Initial.NOx.per.capita + Growth.Rate + LD.Gas.NOx
              + Energy.Use.per.capita + R.D + Latitude + Population.Density + Renewable.Electricity.Output
              + Combustible.Renewables...Waste, data = noxdata)

noxmod10 <- lm(Ln.Oxides.of.Nitrogen.per.capita ~ Ln.Initial.NOx.per.capita + Growth.Rate + LD.Diesel.NOx
               + Energy.Use.per.capita + R.D + Latitude + Population.Density + Renewable.Electricity.Output
               + Combustible.Renewables...Waste, data = noxdata)

# Models with paired standards as a proxy and additional factors that may contribute to $\theta$

noxmod11 <- lm(Ln.Oxides.of.Nitrogen.per.capita ~ Ln.Initial.NOx.per.capita + Growth.Rate + HD.NOx + LD.Gas.NOx
               + Energy.Use.per.capita + R.D + Latitude + Population.Density + Renewable.Electricity.Output
               + Combustible.Renewables...Waste, data = noxdata)

noxmod12 <- lm(Ln.Oxides.of.Nitrogen.per.capita ~ Ln.Initial.NOx.per.capita + Growth.Rate + HD.NOx + LD.Diesel.NOx
               + Energy.Use.per.capita + R.D + Latitude + Population.Density + Renewable.Electricity.Output
               + Combustible.Renewables...Waste, data = noxdata)

# Writing results to file

write.csv(stargazer(noxmod1, noxmod2, noxmod3, noxmod4, noxmod5, noxmod6, type = 'text',
                    se = list(rse1, rse2, rse3, rse4, rse5, rse6)),
          'C:/Users/User/Documents/Data/EUmissions/regression_results_baseline.txt', row.names = FALSE)
write.csv(stargazer(noxmod1, noxmod2, noxmod3, noxmod4, noxmod5, noxmod6,
                    se = list(rse1, rse2, rse3, rse4, rse5, rse6)),
          'C:/Users/User/Documents/Data/EUmissions/regression_results_baseline_tex.txt', row.names = FALSE)
write.csv(stargazer(noxmod7, noxmod8, noxmod9, noxmod10, noxmod11, noxmod12, type = 'text',
                    se = list(rse7, rse8, rse9, rse10, rse11, rse12)),
          'C:/Users/User/Documents/Data/EUmissions/regression_results_fullly_specified.txt', row.names = FALSE)
write.csv(stargazer(noxmod7, noxmod8, noxmod9, noxmod10, noxmod11, noxmod12,
                    se = list(rse7, rse8, rse9, rse10, rse11, rse12)),
          'C:/Users/User/Documents/Data/EUmissions/regression_results_fully_specified_tex.txt', row.names = FALSE)

# Viewing results

stargazer(noxmod1,noxmod2,noxmod3,noxmod4,noxmod5,noxmod6, type = 'text',
          se = list(rse1, rse2, rse3, rse4, rse5, rse6))
stargazer(noxmod7,noxmod8,noxmod9,noxmod10,noxmod11,noxmod12, type = 'text',
          se = list(rse7, rse8, rse9, rse10, rse11, rse12))

# Creating a choropleth for the paper with plotly

df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv')
names(df) <- c('Country', 'Junk', 'Code')
mapdat <- subset(noxdata, Year == 2012)
mapdat <- merge(mapdat, df, by = 'Country')

# Specify map projection/options

l <- list(color = toRGB('grey'), width = 0.5)

geog <- list(showframe = FALSE,
            showcoastlines = FALSE,
            scope = 'europe',
            projection = list(type = 'Mercator'))

# Create the choropleth maps and save to file

init_map <- plot_geo(mapdat) %>%
  add_trace(z = ~Initial.NOx.per.capita, color = ~Initial.NOx.per.capita, colors = 'Greys',
            text = ~Country, locations = ~Code, marker = list(line = l)) %>%
  colorbar(title = '', limits = c(0,2.2)) %>%
  layout(title = 'Initial (1970) per capita NOx emissions in tons',
    geo = geog)

fin_map <- plot_geo(mapdat) %>%
  add_trace(z = ~Oxides.of.Nitrogen.per.capita, color = ~Initial.NOx.per.capita, colors = 'Greys',
            text = ~Country, locations = ~Code, marker = list(line = l)) %>%
  colorbar(title = '', limits = c(0,2.2)) %>%
  layout(title = 'Final (2012) per capita NOx emissions in tons',
    geo = geog)

# Creating additional plots as data viz for the paper

uk_df <- noxdata[which(noxdata$Country == 'United Kingdom'),]
france_df <- noxdata[which(noxdata$Country == 'France'),]
ned_df <- noxdata[which(noxdata$Country == 'Netherlands'),]
fin_df <- noxdata[which(noxdata$Country == 'Finland'),]
austria_df <- noxdata[which(noxdata$Country == 'Austria'),]
years <- c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012)

plot(years, austria_df$Oxides.of.Nitrogen.per.capita,type = 'l', col = 'green', ylim = c(0,1.6),
     xlab = 'Year', ylab = 'per capita NOx Emissions (metric tons CO2e)',
     main = 'Times series of per capita NOx emissions in Europe')
lines(years, fin_df$Oxides.of.Nitrogen.per.capita, col = 'blue', lty = 'dashed')
lines(years, france_df$Oxides.of.Nitrogen.per.capita, col = 'red')
lines(years, ned_df$Oxides.of.Nitrogen.per.capita, col = 'orange', lty = 'dashed')
lines(years, uk_df$Oxides.of.Nitrogen.per.capita, col = 'black', lty = 2)
legend(2000, 0.4, legend = c('Austria', 'Finland', 'France', 'The Netherlands', 'United Kingdom'),
       col = c('green', 'blue', 'red', 'orange', 'black'), lty = c(1,2,1,2,2), cex = 0.8)

dev.copy(png,'C:/Users/User/Documents/Data/EUmissions/time_series.png')
dev.off()

