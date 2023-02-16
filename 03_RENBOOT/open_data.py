import pandas as pd
import pyreadr as pyreadr
import numpy as np


def open():
    df = pyreadr.read_r('dpb.Rdata')['data']
    df['REGION_RATING_CLIENT'] = df['REGION_RATING_CLIENT'].astype("category")
    df['REGION_RATING_CLIENT_W_CITY'] = df['REGION_RATING_CLIENT_W_CITY'].astype("category")
    df.drop(columns=['NAME_CONTRACT_TYPE', 'FLAG_DOCUMENT_2', 'FLAG_DOCUMENT_12', 'FLAG_DOCUMENT_20'], inplace=True)

    #df['TARGET'] = (1-df['TARGET'].to_numpy().astype(np.int32))
    #df['TARGET'] = df['TARGET'].astype("category")
    
#    important_variables = ['TARGET','EXT_SOURCE_2','DAYS_ID_PUBLISH','DAYS_BIRTH','DAYS_REGISTRATION','DAYS_LAST_PHONE_CHANGE','REGION_POPULATION_RELATIVE','AMT_INCOME_TOTAL','AMT_REQ_CREDIT_BUREAU_TOTAL','OCCUPATION_TYPE','OWN_CAR_AGE','AMT_GOODS_PRICE','AMT_ANNUITY','AMT_CREDIT','OBS_30_CNT_SOCIAL_CIRCLE','OBS_60_CNT_SOCIAL_CIRCLE','NAME_FAMILY_STATUS','CNT_FAM_MEMBERS','ORGANIZATION_TYPE','CNT_CHILDREN','NAME_TYPE_SUITE','NAME_HOUSING_TYPE','NAME_INCOME_TYPE','NAME_EDUCATION_TYPE','DEF_30_CNT_SOCIAL_CIRCLE','CODE_GENDER']
    important_variables = ['TARGET','EXT_SOURCE_2','DAYS_ID_PUBLISH','DAYS_BIRTH','DAYS_REGISTRATION','DAYS_LAST_PHONE_CHANGE','REGION_POPULATION_RELATIVE','AMT_INCOME_TOTAL','AMT_REQ_CREDIT_BUREAU_TOTAL','OCCUPATION_TYPE','OWN_CAR_AGE','AMT_GOODS_PRICE','AMT_ANNUITY', 'AMT_CREDIT', 'OBS_30_CNT_SOCIAL_CIRCLE', 'OBS_60_CNT_SOCIAL_CIRCLE', 'NAME_FAMILY_STATUS']
    
    df = df[important_variables]
    
    lista_variabili_numeriche = []
    for col in df.columns:
        if (not isinstance(df[col].unique(), pd.core.arrays.categorical.Categorical)):
            lista_variabili_numeriche = lista_variabili_numeriche + [col]

    dizionario_trasformazioni = {}
    for col in lista_variabili_numeriche:
        dizionario_trasformazioni[col] = {'media': np.mean(df[col]), 'std': np.std(df[col])}
        df[col] = (df[col] - dizionario_trasformazioni[col]['media'])/(dizionario_trasformazioni[col]['std'])

    lista_variabili_categoriche = []
    for col in df.columns:
        if ((not col in lista_variabili_numeriche) and (col != 'TARGET')):
            lista_variabili_categoriche = lista_variabili_categoriche + [col]
        
    for col_name in lista_variabili_categoriche:
        aux = pd.get_dummies(df[col_name], col_name)
        aux.drop(columns=aux.columns[0], inplace=True)
        df = pd.concat([df, aux], axis=1)
        df.drop(columns=col_name, inplace=True)
        
    return {'df': df, 'transformed': dizionario_trasformazioni}


def open_without_normalization():
    df = pyreadr.read_r('dpb.Rdata')['data']
    df['REGION_RATING_CLIENT'] = df['REGION_RATING_CLIENT'].astype("category")
    df['REGION_RATING_CLIENT_W_CITY'] = df['REGION_RATING_CLIENT_W_CITY'].astype("category")
    df.drop(columns=['NAME_CONTRACT_TYPE', 'FLAG_DOCUMENT_2', 'FLAG_DOCUMENT_12', 'FLAG_DOCUMENT_20'], inplace=True)

    #df['TARGET'] = (1-df['TARGET'].to_numpy().astype(np.int32))
    #df['TARGET'] = df['TARGET'].astype("category")
    
#    important_variables = ['TARGET','EXT_SOURCE_2','DAYS_ID_PUBLISH','DAYS_BIRTH','DAYS_REGISTRATION','DAYS_LAST_PHONE_CHANGE','REGION_POPULATION_RELATIVE','AMT_INCOME_TOTAL','AMT_REQ_CREDIT_BUREAU_TOTAL','OCCUPATION_TYPE','OWN_CAR_AGE','AMT_GOODS_PRICE','AMT_ANNUITY','AMT_CREDIT','OBS_30_CNT_SOCIAL_CIRCLE','OBS_60_CNT_SOCIAL_CIRCLE','NAME_FAMILY_STATUS','CNT_FAM_MEMBERS','ORGANIZATION_TYPE','CNT_CHILDREN','NAME_TYPE_SUITE','NAME_HOUSING_TYPE','NAME_INCOME_TYPE','NAME_EDUCATION_TYPE','DEF_30_CNT_SOCIAL_CIRCLE','CODE_GENDER']
    important_variables = ['TARGET','EXT_SOURCE_2','DAYS_ID_PUBLISH','DAYS_BIRTH','DAYS_REGISTRATION','DAYS_LAST_PHONE_CHANGE','REGION_POPULATION_RELATIVE','AMT_INCOME_TOTAL','AMT_REQ_CREDIT_BUREAU_TOTAL','OCCUPATION_TYPE','OWN_CAR_AGE','AMT_GOODS_PRICE','AMT_ANNUITY', 'AMT_CREDIT', 'OBS_30_CNT_SOCIAL_CIRCLE', 'OBS_60_CNT_SOCIAL_CIRCLE', 'NAME_FAMILY_STATUS']
    
    df = df[important_variables]
    
    lista_variabili_numeriche = []
    for col in df.columns:
        if (not isinstance(df[col].unique(), pd.core.arrays.categorical.Categorical)):
            lista_variabili_numeriche = lista_variabili_numeriche + [col]

    lista_variabili_categoriche = []
    for col in df.columns:
        if ((not col in lista_variabili_numeriche) and (col != 'TARGET')):
            lista_variabili_categoriche = lista_variabili_categoriche + [col]
        
    for col_name in lista_variabili_categoriche:
        aux = pd.get_dummies(df[col_name], col_name)
        aux.drop(columns=aux.columns[0], inplace=True)
        df = pd.concat([df, aux], axis=1)
        df.drop(columns=col_name, inplace=True)
        
    return {'df': df}