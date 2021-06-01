from pandas import pandas as pd

# Clear variables ---------------------------------------------------------------
## del var to delete the variable 
## import sys
## sys.modules[__name__].__dict__.clear()



# Load Data -------------------------------------------------------------------

file = "C:/Users/TOsosanya/Desktop/NSW/Newdata 2705/Pavements/pavement_assets ConOps Calcs.xlsx"
data = pd.read_excel(file)
print(data)

# Data manipulation ------------------------------------------------------------

data2 = pd.DataFrame(data["ConOps_Rating"].value_counts())
data2.columns = ["number"] 
data2['ConOps_Rating'] = data2.index # rename the index which was previously lost
data3 = data2.sort_values("ConOps_Rating")
#data3.columns = ["number2","ConOps_Rating"]


# get the total number of interventions

year_one = data[data["Intervention_Y1"] == 1]
year_one_minimum = pd.DataFrame(year_one["ConOps_Rating"].value_counts())
year_one_minimum.columns = ["minimum"] 
year_one_minimum['ConOps_Rating'] = year_one_minimum.index

# merge the 2 and get the availability summary

data_inner = pd.merge(left = data2,right = year_one_minimum,left_on = 'ConOps_Rating',right_on = 'ConOps_Rating')
data_inner["summarised"] = 100 * round((data_inner["number"]-data_inner["minimum"])/(data_inner["number"]),2)
# data_inner.columns = ["ConOps_Rating","number","minimum","summarised"] # rearrange the columns
print(data_inner.sort_values("ConOps_Rating",ascending = True))






# df_row_reindex = pd.concat([df1, df2], ignore_index=True)
# frames = [data2,data3]
# df_keys = pd.concat(frames,join = "inner", keys=['ConOps_Rating', 'ConOps_Rating'])



# abc = ("abc")
# def upper_guy(abc):
#     for item in abc:
#         return(abc.upper())

# upper_guy(abc)