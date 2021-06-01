import pandas as pd
import numpy as np

#??????
##Make sure to add back the backlog projects
#??????

# read in the individual POFs for each asset category assuming NPI

corridor_npi_link = "C:/Users/TOsosanya/Desktop/NSW/Newdata 2705/Corridors/Corridor results.xlsx"
#corridor_npi = pd.read_excel(corridor_npi_link,sheet_name = "Sheet 1",dtype = {'Asset Number':np.int32})
corridor_npi = pd.read_excel(corridor_npi_link,sheet_name = "Sheet 1",dtype = {'Asset Number':str})
print(corridor_npi.head())
columns_pof = ['Asset Category','Asset Number',	'ObjectID',	'Zone',	'Latitude',	'Longitude',
	'QuantityAsset','Asset_Type','Project Name','CONSTRUCTION_YEAR','CY_STATUS','ARL','LIFE_SPAN',
    'ConOps_Score','POF0','POF1','POF2','POF3','POF4','POF5','POF6','POF7','POF8','POF9','POF10']
corridor_npi_pof = pd.DataFrame(data = corridor_npi, columns = columns_pof) 


# read in the individual POFs (with linear deterioration) for each category assuming Investment from T1

columns_pof_newasset = columns_pof = ['Asset Category','Asset Number',	'ObjectID',	'Zone',	'Latitude',	'Longitude',
	'QuantityAsset','Asset_Type','Project Name','CONSTRUCTION_YEAR','CY_STATUS','ARL','LIFE_SPAN',
    'ConOps_Score']
corridor_npi_pof_newasset = pd.DataFrame(data = corridor_npi, columns = columns_pof_newasset) 

## Run certain tests to find out if na in columns
corridor_npi_pof_newasset['CONSTRUCTION_YEAR'].describe()
print(corridor_npi_pof_newasset['CONSTRUCTION_YEAR'].isna().any())
print(corridor_npi_pof_newasset['LIFE_SPAN'].isna().any())

## back to adding the linear deteriroration
corridor_npi_pof_newasset['POF0']=0
corridor_npi_pof_newasset['POF1']= (2020 + corridor_npi_pof_newasset['CONSTRUCTION_YEAR'])/corridor_npi_pof_newasset['LIFE_SPAN'] 

# write a function that produces a linear det. curve for POF1 - POF10
