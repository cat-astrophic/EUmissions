# This script performs data prep for a second emissions data source for the EUmissions project

# Importing required modules

import pandas as pd

# Declaring the directory where the data is + will be written :: USER MUST UPDATE THIS!

directory = 'C:/Users/User/Documents/Data/'

# Defining some variables for convenience

a = 'Road transport: Automobile road abrasion'
b = 'Road transport: Automobile tyre and brake wear'
c = 'Road transport: Gasoline evaporation'
d = 'Road transport: Heavy duty vehicles and buses'
e = 'Road transport: Light duty vehicles'
f = 'Road transport: Mopeds & motorcycles'
g = 'Road transport: Passenger cars'

# Reading in the data

print('Reading in the data..........')
data = pd.read_csv(directory + 'NEC_NFR14.csv', sep = '\t')

# Subsetting for non-forecasted NOx data and creating data for total national emissions and road emissions

print('Creating new data sets.......')
data = data[data.Year < 2020]
data = data[data.Pollutant_name == 'NOx']
data1 = data[data.sector_name == 'National total for the entire territory (based on fuel sold)']
data2 = data[data.sector_name.isin([a,b,c,d,e,f,g])].drop(['Country_Code', 'Pollutant_name', 'Format_name', 'Sector_code', 'sector_name', 'parent_sector_code', 'Unit', 'Notations'], axis = 1).fillna(0)
data2['Emissions'] = data2['Emissions'].astype(float)
data2 = data2.groupby(['Country','Year']).sum()

# Writing the results to csv files

print('Writing data to files........')
data1.to_csv(directory + 'EU_NOX_all.csv', index = False)
data2.to_csv(directory + 'EU_NOX_road.csv')

