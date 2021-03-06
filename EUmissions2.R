# This script performs the data analysis for a paper on the effect of EU NOx emissions standards on EU NOx emissions - 2ary data source

# Loading libraries

library(stargazer)
library(plotly)

# Declaring the filepath for the data file

filepath <- 'C:/Users/User/Documents/Data/EUmissions.csv'

# Reading in the data

noxdata <- read.csv(filepath)

# Create per capita emissions, natural log of per capita emissions, natural log of GDP per capita,
# initial nox per capita, and the natural log of initial nox per capita

noxdata$Oxides.of.Nitrogen.per.capita <- 1000*noxdata$NOX / noxdata$Population
noxdata$Ln.Oxides.of.Nitrogen.per.capita <- log(noxdata$Oxides.of.Nitrogen.per.capita)
noxdata$Ln.GDP.per.capita <- log(noxdata$GDP.per.capita)
noxdata$Initial.NOx.per.capita <- 1000*noxdata$NOX.Initial / noxdata$Initial.Population.90
noxdata$Ln.Initial.NOx.per.capita <- log(noxdata$Initial.NOx.per.capita)

# Create a new dataframe (for visualization purposes when looking at histograms)

vars <- c('Oxides.of.Nitrogen.per.capita', 'Ln.Oxides.of.Nitrogen.per.capita', 'Ln.GDP.per.capita',
          'Renewable.Energy...Pct', 'HD.NOx', 'R.D', 'Urban.Population...Pct', 'Country', 'Year',
          'LD.Diesel.NOx', 'LD.Gas.NOx', 'EU', 'Initial.NOx.per.capita', 'Ln.Initial.NOx.per.capita',
          'GDP.per.capita', 'Growth.Rate', 'Latitude', 'Land.Area', 'Population.Density', 'Energy.Use.per.capita',
          'Fossil.Fuel.Energy.Consumption.Pct', 'Combustible.Renewables...Waste', 'Renewable.Electricity.Output')
noxdata <- noxdata[vars]
noxdata <- subset(noxdata, 1990 <= Year)
noxdata <- subset(noxdata, Year <= 2017)
noxdata <- na.omit(noxdata)

# Create a table of summary statistics for the data and write to file

write.csv(stargazer(noxdata), 'C:/Users/User/Documents/Data/EUmissions/sumtab2.txt', row.names = FALSE)

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

# Heteroskedasticity robust standard errors

cov <- vcovHC(noxmod1, type = 'HC0')
rse1 <- sqrt(diag(cov))

cov <- vcovHC(noxmod2, type = 'HC0')
rse2 <- sqrt(diag(cov))

cov <- vcovHC(noxmod3, type = 'HC0')
rse3 <- sqrt(diag(cov))

cov <- vcovHC(noxmod4, type = 'HC0')
rse4 <- sqrt(diag(cov))

cov <- vcovHC(noxmod5, type = 'HC0')
rse5 <- sqrt(diag(cov))

cov <- vcovHC(noxmod6, type = 'HC0')
rse6 <- sqrt(diag(cov))

cov <- vcovHC(noxmod7, type = 'HC0')
rse7 <- sqrt(diag(cov))

cov <- vcovHC(noxmod8, type = 'HC0')
rse8 <- sqrt(diag(cov))

cov <- vcovHC(noxmod9, type = 'HC0')
rse9 <- sqrt(diag(cov))

cov <- vcovHC(noxmod10, type = 'HC0')
rse10 <- sqrt(diag(cov))

cov <- vcovHC(noxmod11, type = 'HC0')
rse11 <- sqrt(diag(cov))

cov <- vcovHC(noxmod12, type = 'HC0')
rse12 <- sqrt(diag(cov))

# Writing results to file

write.csv(stargazer(noxmod1, noxmod2, noxmod3, noxmod4, noxmod5, noxmod6, type = 'text',
                    se = list(rse1, rse2, rse3, rse4, rse5, rse6)),
          'C:/Users/User/Documents/Data/EUmissions/regression_results_baseline2.txt', row.names = FALSE)
write.csv(stargazer(noxmod1, noxmod2, noxmod3, noxmod4, noxmod5, noxmod6,
                    se = list(rse1, rse2, rse3, rse4, rse5, rse6)),
          'C:/Users/User/Documents/Data/EUmissions/regression_results_baseline2_tex.txt', row.names = FALSE)
write.csv(stargazer(noxmod7, noxmod8, noxmod9, noxmod10, noxmod11, noxmod12, type = 'text',
                    se = list(rse7, rse8, rse9, rse10, rse11, rse12)),
          'C:/Users/User/Documents/Data/EUmissions/regression_results_fullly_specified2.txt', row.names = FALSE)
write.csv(stargazer(noxmod7, noxmod8, noxmod9, noxmod10, noxmod11, noxmod12,
                    se = list(rse7, rse8, rse9, rse10, rse11, rse12)),
          'C:/Users/User/Documents/Data/EUmissions/regression_results_fully_specified2_tex.txt', row.names = FALSE)

# Viewing results

stargazer(noxmod1,noxmod2,noxmod3,noxmod4,noxmod5,noxmod6, type = 'text',
          se = list(rse1, rse2, rse3, rse4, rse5, rse6))
stargazer(noxmod7,noxmod8,noxmod9,noxmod10,noxmod11,noxmod12, type = 'text',
          se = list(rse7, rse8, rse9, rse10, rse11, rse12))

