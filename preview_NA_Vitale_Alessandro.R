#ALESSANDRO VITALE
library("dplyr")

#text from col 83 to 89
#"Normalized information about building where the client lives, What is average 
#(_AVG suffix), modus (_MODE suffix), median (_MEDI suffix) apartment size, 
#common area, living area, age of building, number of elevators, number of entrances, 
#state of the building, number of floor",normalized

#colonna 83:
#LIVINGAPARTMENTS_MEDI I suppose it's the number of room for the living area(?)
#NA------> the client doesn't have a house or he didn't give enough data----> I would remove this column
dataset1=subset(dataset,select=-c(LIVINGAPARTMENTS_MEDI))

#colonna 84:
#LIVINGAPARTMENTS_MEDI I suppose it's the normalized living area
#same treatment of column 83---->drop it
dataset1=subset(dataset1,select=-c(LIVINGAREA_MEDI))

#colonna 85:
#NONLIVINGAPARTMENTS_MEDI same thing as before for the non living area
dataset1=subset(dataset1,select=-c(NONLIVINGAPARTMENTS_MEDI))

#colonna 86:
#NONLIVINGAREA_MEDI
dataset1=subset(dataset1,select=-c(NONLIVINGAREA_MEDI))

#colonna 89:
#TOTALAREA_MODE
dataset1=subset(dataset1,select=-c(TOTALAREA_MODE))

#colonna 92:
#OBS_30_CNT_SOCIAL_CIRCLE
#How many observation of client's social surroundings with observable 30 DPD (days past due) default,
#this can be interesting to analyze, there aren't a lot of NA, I would keep it.
#I would remove the rows for this information

#colonna 93:
#DEF_30_CNT_SOCIAL_CIRCLE,
#How many observation of client's social surroundings defaulted on 30 DPD (days past due) ,
#same thing are before, I would keep this column and remove the rows

#colonna 94:
#OBS_60_CNT_SOCIAL_CIRCLE

#colonna 95:
#DEF_60_CNT_SOCIAL_CIRCLE

#colonna 96:
#DAYS_LAST_PHONE_CHANGE,
#How many days before application did client change phone,
#This also could be usefull, there are very few NA, try to keep this column and remove the rows
#NB there is a world where this NA means that the client doesn't have a phone, in that case 
#it would be better to keep them and put -1 instead

#colonna 117:
#AMT_REQ_CREDIT_BUREAU_HOUR,
#Number of enquiries to Credit Bureau about the client one hour before application,

#from INVESTOPEDIA:
#A credit inquiry is a request by an institution for credit 
#report information from a credit reporting agency.

#this can be usefull, but there are a lot of NA. Looking at the dataset thoso who have NAs here tends to 
#have NA also in all the other rows-> consider removing the rows unless you know why
#some people aren't checked

#colonna 118:
#AMT_REQ_CREDIT_BUREAU_DAY,
#Number of enquiries to Credit Bureau about the client one day before application (excluding one hour before application),

#colonna 119:
#AMT_REQ_CREDIT_BUREAU_WEEK,
#Number of enquiries to Credit Bureau about the client one week before application (excluding one day before application),

#colonna 120:
#AMT_REQ_CREDIT_BUREAU_MON,
#Number of enquiries to Credit Bureau about the client one month before application (excluding one week before application),

#colonna 121:
#AMT_REQ_CREDIT_BUREAU_QRT,
#Number of enquiries to Credit Bureau about the client 3 month before application (excluding one month before application),

#colonna 122:
#AMT_REQ_CREDIT_BUREAU_YEAR,
#Number of enquiries to Credit Bureau about the client one day year (excluding last 3 months before application),






