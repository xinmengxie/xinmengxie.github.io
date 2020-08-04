# -*- coding: utf-8 -*-
"""
Created on Tue Aug  4 05:44:34 2020

@author: xiexi
"""

import numpy as np
from numpy import linalg as la
from sklearn import linear_model
import pandas as pd
import math 
from sklearn.decomposition import PCA
import matplotlib.pyplot as plt
import copy
import itertools
import warnings
from matplotlib import pyplot
import statsmodels.api as sm

# In[] inflation rate
data = pd.read_csv("./CPI_U.csv", index_col = 3)

y = data ['Value']

# Define the p, d and q parameters to take any value between 0 and 2
p = d = q = range(0, 3)

# Generate all different combinations of p, q and q triplets
pdq = list(itertools.product(p, d, q))

# Generate all different combinations of seasonal p, q and q triplets
seasonal_pdq = [(x[0], x[1], x[2], 12) for x in list(itertools.product(p, d, q))]


# In[]
warnings.filterwarnings("ignore") # specify to ignore warning messages

order_list = []
season_order_list = []
AIC = []
BIC = []

for param in pdq:
    for param_seasonal in seasonal_pdq:
        try:
            mod = sm.tsa.statespace.SARIMAX(y,
                                            order=param,
                                            seasonal_order=param_seasonal,
                                            enforce_stationarity=False,
                                            enforce_invertibility=False)

            results = mod.fit()
            
            order_list.append(param)
            season_order_list.append(param_seasonal)
            AIC.append(results.aic)
            BIC.append(results.bic)
            print('ARIMA{}x{}12 - AIC:{} - BIC:{}'.format(param, param_seasonal, results.aic, results.bic))
        except:
            continue


zippedList =  list(zip(order_list, season_order_list, AIC, BIC))
df_AIC_BIC = pd.DataFrame(zippedList, columns = ['order' , 'sensonal', 'AIC', 'BIC']) 
sort_df_AIC = df_AIC_BIC.sort_values(by=['AIC'])
sort_df_BIC = df_AIC_BIC.sort_values(by=['BIC'])


# In[]
mod = sm.tsa.statespace.SARIMAX(y[:"2017 Jan"],
                                order=(1, 1, 1),
                                seasonal_order=(0, 1, 2, 12),
                                enforce_stationarity=False,
                                enforce_invertibility=False)

results = mod.fit(disp = 0)

results.plot_diagnostics(figsize=(15, 12))
pyplot.show()


# In[]
# validating

pred = results.get_prediction(start="2017 Jan", end="2020 Mar", dynamic = False) 
#pred = results.predict(start="2017 Jan", end="2020 Mar")
pred_ci = pred.conf_int()

pred_ci_1 = pred_ci.set_index(y["2017 Jan":].index)


y_forecasted = pd.DataFrame(pred.predicted_mean)

y_forecasted_1 = y_forecasted.set_index(y["2017 Jan":].index)


fig, ax = pyplot.subplots(figsize=(9, 7))
ax.plot(y['2017 Jan':], '-b', label='observed')
ax.plot(y_forecasted_1, '--r', label='forecast')
ax.axis('equal')
ax.fill_between(pred_ci_1.index,
                pred_ci_1.iloc[:, 0],
                pred_ci_1.iloc[:, 1], color='k', alpha=.2)
ax.set_xlabel('Date')
ax.set_ylabel('inflation')
pyplot.xticks(rotation = 60, fontsize=7)
leg = ax.legend()
pyplot.show()

rmse = np.sqrt((np.sum(y['2017 Jan':].values - y_forecasted_1.values) ** 2) / len(y_forecasted_1))

# In[]
mod_full = sm.tsa.statespace.SARIMAX(y, ## all the available inflation rate
                                order=(1, 1, 1),
                                seasonal_order=(0, 1, 2, 12),
                                enforce_stationarity=False,
                                enforce_invertibility=False)

results_full = mod_full.fit(disp = 0)
pred_2025 = results.get_prediction(start="2020 Apr", end="2025 Jul", dynamic = False) 
y_forecasted_2025 = pd.DataFrame(pred_2025.predicted_mean)


# In[] PCA

data = pd.read_csv("./USTREASURY_YIELD.csv")
# drop data if is NA
data = data.dropna(axis=1, how='any')

# Standardise the data in the df into z scores

data_new = data.drop(['Date'],axis = 1)
data_new = data_new/100
print(data_new.mean())
data_new_std = (data_new - data_new.mean()) / data_new.std()
# Create a covariance & correriance matrix 

cov_matrix_array = np.array(np.cov(data_new_std, rowvar=False))
corr_matrix_array = np.array(data_new_std.corr())
pd.DataFrame(corr_matrix_array)

# Perform eigendecomposition

eigenvalues, eigenvectors = np.linalg.eig(corr_matrix_array)

# Put data into a DataFrame and save to excel
df_eigval = pd.DataFrame({"Eigenvalues":eigenvalues})


df_eigvec = pd.DataFrame(eigenvectors)


# Work out explained proportion 
df_eigval["Explained proportion"] = df_eigval["Eigenvalues"] / np.sum(df_eigval["Eigenvalues"])
#Format as percentage
df_eigval.style.format({"Explained proportion": "{:.2%}"})

# Three components plot
principal_components = data_new_std.dot(eigenvectors)

df=df_eigvec[[0,1,2]]
df.columns=['level','slope','curvation']
df.index = data.T.index[1:]
df.plot(figsize=(12,6),title= 'Trend of First Three Component',fontsize=14)
plt.show()

eigvalues = pd.DataFrame(df_eigval['Eigenvalues']).T

# sensitivity of principal components
pc = (eigvalues.head(1)**0.5).values*df_eigvec
pc_3 = pc[[0,1,2]]
pc_3.columns = ['PC1','PC2','PC3']
pc_3.index = data.T.index[1:]
print(pc_3)


Most_recent = pd.DataFrame(data.iloc[2502][1:])/100
print(Most_recent)
Most_recent.columns = ['Yield']
level_std = df['level'].std()
slope_std = df['slope'].std()
curvation_std = df['curvation'].std()
Loss_gain = level_std*pc_3['PC1']+slope_std*pc_3['PC2']+curvation_std*pc_3['PC3']
Most_recent['2020-4-18'] = Most_recent['Yield']+Loss_gain.values
print(Most_recent)
Most_recent.plot(figsize=(12,6),title= 'Trend of First Three Component',fontsize=14)

# In[]  Vt

#Basic data for TIPS
Cpi13 = 232.71797
Cpi15 = 237.14365
Rate = 0.00375/2
RefCpi = y_forecasted_2025
RefCpI = RefCpi["0"].values
print(RefCpi)
#Calculate Adjust Cash flow
AdjPrincipal13 = 100 * RefCpI/Cpi13
AdjPrincipal15 = 100 * RefCpI/Cpi15

print(RefCpI)
AdjCf = np.zeros(11)
AdjCf[0] = (AdjPrincipal13[3] + AdjPrincipal15[3])* Rate
AdjCf[1] = (AdjPrincipal13[9] + AdjPrincipal15[9])* Rate
AdjCf[2] = (AdjPrincipal13[15] + AdjPrincipal15[15])* Rate
AdjCf[3] = (AdjPrincipal13[21] + AdjPrincipal15[21])* Rate
AdjCf[4] = (AdjPrincipal13[27] + AdjPrincipal15[27])* Rate
AdjCf[5] = (AdjPrincipal13[33] + AdjPrincipal15[33])* Rate
AdjCf[6] = (AdjPrincipal13[39] + AdjPrincipal15[39])* Rate + AdjPrincipal13[39]
AdjCf[7] = AdjPrincipal15[45] * Rate
AdjCf[8] = AdjPrincipal15[51] * Rate
AdjCf[9] = AdjPrincipal15[57] * Rate
AdjCf[10] = AdjPrincipal15[63] * Rate + AdjPrincipal15[63]
print(AdjCf)

#Calculate Vt
yieldcurve = (Most_recent['Yield'].T)
spotrate = np.zeros(11)

spotrate[0]=yieldcurve['3 MO']
spotrate[1]=(yieldcurve['6 MO']+yieldcurve['1 YR'])*0.5
spotrate[2]=yieldcurve['1 YR']+(yieldcurve['2 YR']-yieldcurve['1 YR'])*0.25
spotrate[3]=yieldcurve['1 YR']+(yieldcurve['2 YR']-yieldcurve['1 YR'])*0.75
spotrate[4]=yieldcurve['2 YR']+(yieldcurve['3 YR']-yieldcurve['2 YR'])*0.25
spotrate[5]=yieldcurve['2 YR']+(yieldcurve['3 YR']-yieldcurve['2 YR'])*0.75
spotrate[6]=yieldcurve['3 YR']+(yieldcurve['5 YR']-yieldcurve['3 YR'])*0.125
spotrate[7]=yieldcurve['3 YR']+(yieldcurve['5 YR']-yieldcurve['3 YR'])*0.375
spotrate[8]=yieldcurve['3 YR']+(yieldcurve['5 YR']-yieldcurve['3 YR'])*0.625
spotrate[9]=yieldcurve['3 YR']+(yieldcurve['5 YR']-yieldcurve['3 YR'])*0.875
spotrate[10]=yieldcurve['5 YR']+(yieldcurve['7 YR']-yieldcurve['5 YR'])*0.125

Vt = 0
for i in range(0,11):
    Vt += AdjCf[i]/((1+spotrate[i])**((6*i+3)/12))
    i += 1
Vt = copy.deepcopy(Vt)
print(Vt)

# In[] Profit/Loss
pc_3['PC1']
# Get Vt+1 distribution by Monte-Carlo simulation
U = np.zeros(3)
matric=[]
nmc = 10000
PvCf = np.zeros(nmc)
for i in range(nmc):
    spotrate=np.zeros(11)
    U = np.random.normal(loc=0,scale=1,size=3)
    Loss_gain = level_std*pc_3['PC1']*U[0]+slope_std*pc_3['PC2']*U[1]+curvation_std*pc_3['PC3']*U[2]
    Most_recent['2020-4-18'] = Most_recent['Yield']+Loss_gain.values
    # calculate spot rate
    yieldcurve=pd.DataFrame(Most_recent['2020-4-18']).T
    spotrate[0]=yieldcurve['3 MO']
    spotrate[1]=(yieldcurve['6 MO']+yieldcurve['1 YR'])*0.5
    spotrate[2]=yieldcurve['1 YR']+(yieldcurve['2 YR']-yieldcurve['1 YR'])*0.25
    spotrate[3]=yieldcurve['1 YR']+(yieldcurve['2 YR']-yieldcurve['1 YR'])*0.75
    spotrate[4]=yieldcurve['2 YR']+(yieldcurve['3 YR']-yieldcurve['2 YR'])*0.25
    spotrate[5]=yieldcurve['2 YR']+(yieldcurve['3 YR']-yieldcurve['2 YR'])*0.75
    spotrate[6]=yieldcurve['3 YR']+(yieldcurve['5 YR']-yieldcurve['3 YR'])*0.125
    spotrate[7]=yieldcurve['3 YR']+(yieldcurve['5 YR']-yieldcurve['3 YR'])*0.375
    spotrate[8]=yieldcurve['3 YR']+(yieldcurve['5 YR']-yieldcurve['3 YR'])*0.625
    spotrate[9]=yieldcurve['3 YR']+(yieldcurve['5 YR']-yieldcurve['3 YR'])*0.875
    spotrate[10]=yieldcurve['5 YR']+(yieldcurve['7 YR']-yieldcurve['5 YR'])*0.125
    PvCfi = 0
    for j in range(0,11):
        PvCfi += AdjCf[j]/((1+spotrate[j])**((6*j+3)/12))
        j += 1
    PvCf[i] = PvCfi

Vt1 = copy.deepcopy(PvCf)
#filter extreme value
def f(n):
    return n <= 1000
PvCf = list(filter(f,Vt1))

#Get distribution for profit/loss
PLplot = PvCf - Vt
PL = Vt1 - Vt
# In[] VaR&ES

#Plot distribution for PL with 0.05 quantile
def histoQuant(x,nbins,q):
    xquant = np.percentile(x,100*q)
    a,binPoints = np.histogram(x,bins=nbins,density=True)
    i = 0
    while binPoints[i] < xquant:
        i+=1
    bquant = binPoints[i]
    binPoints = binPoints[:-1]
    a1 = a[binPoints>=bquant]
    x1 = binPoints[binPoints>=bquant]
    a2 = a[binPoints<=bquant]
    x2 = binPoints[binPoints<=bquant]
    fig,ax1 = plt.subplots()
    plt.plot(x1,a1)
    ax1.fill_between(x1,0.,a1)
    plt.plot(x2,a2,color='red')
    ax1.fill_between(x2,0.,a2,color='red')
    plt.grid()

histoQuant(PLplot,100,0.05)

#find VaR&ES
VaRPL = []
ESPL = []
pList = [0.05, 0.01, 0.005]

for p in pList:
    PLStar = np.percentile(PL,100.*p)
    VaR = -PLStar
    VaRPL.append(VaR)
    ES = -np.mean(PL[PL<=PLStar])
    ESPL.append(ES)

fmt = "%6.3f: %6.4f, %6.4f"
for i in range(len(VaRPL)):
    print(fmt%(pList[i],VaRPL[i],ESPL[i]))

# In[] Confidence band

#Confidence band for VaR&ES at p = 0.05
nsamp = len(PL)
nboot = 10000
VaRDist = np.zeros(nboot)
ESDist = np.zeros(nboot)

for i in range(nboot):
    PLsim = np.random.choice(PL,size=nsamp)
    PLsimStar = np.percentile(PLsim,100*0.05)
    VaRDist[i] = -PLsimStar
    ESDist[i] = -np.mean(PLsim[PLsim<=PLsimStar])

VaRconfBand = np.percentile(VaRDist,(2.5,97.5))
ESconfBand = np.percentile(ESDist,(2.5,97.5))

fmt = "%6.4f %6.4f %6.4f"
print(fmt%(VaRconfBand[0], VaRPL[0], VaRconfBand[1]))
print(fmt%(ESconfBand[0], ESPL[0], ESconfBand[1]))

















