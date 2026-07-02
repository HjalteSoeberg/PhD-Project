import pandas as pd
import numpy as np
from keras.models import Sequential
from keras.layers import Dense, SimpleRNN
from sklearn.preprocessing import MinMaxScaler
from sklearn.metrics import mean_squared_error

#from __future__ import division
from past.utils import old_div
from sippy import *
from sippy import functionset as fset
from sippy import functionsetSIM as fsetSIM
import matplotlib.pyplot as plt

ts=20


# SISO SS system (n = 2)
A = np.array([[0.89, 0.], [0., 0.45]])
B = np.array([[0.3], [2.5]])
C = np.array([[0.7, 1.]])
D = np.array([[0.0]])


K = np.array([[1],[0.5]])

N=int(10**4)
u=np.random.randn(1,N)  #exogenous input 
v=np.random.randn(1,N)*10 #noise
x=np.zeros((2,N+1))
y=np.zeros((1,N))
time=np.zeros((1,N+1))

for t in range(N):
    x[:,t+1]=np.dot(A,x[:,t])+np.dot(B,u[:,t])+np.dot(K,v[:,t])
    y[:,t]=np.dot(C,x[:,t])+np.dot(D,u[:,t])+v[:,t]
    time[0,t]=t
    
    
T0=int(N/2)

ytrain=y[0,0:T0] #training output
utrain=u[0,0:T0] #training input

#Second half of data for testing
ytest=y[0,T0:y.shape[1]]
utest=u[0,T0:u.shape[1]]

sys_id = system_identification(ytrain, utrain, 'MOESP', SS_fixed_order = 2 )

U=pd.DataFrame({'u':utest, 'y': ytest})

Asim=sys_id.A-np.dot(sys_id.K,sys_id.C) #A-KC
Bsim=sys_id.B-np.dot(sys_id.K,sys_id.D) #B-KD
Bsim=np.concatenate([Bsim,sys_id.K],axis=1) #[B-KD,K]
Dsim=np.concatenate([sys_id.D,np.zeros((1,1))],axis=1)
#Identified systems
xid, yid = fsetSIM.SS_lsim_process_form(Asim, Bsim, sys_id.C, Dsim, np.transpose(U.to_numpy()), sys_id.x0)
xid_2,yid_2=fsetSIM.SS_lsim_innovation_form(sys_id.A,sys_id.B,sys_id.C,sys_id.D,\
                                            sys_id.K,np.reshape(ytest,(1,-1)),\
                                            np.reshape(utest,(1,-1)),sys_id.x0)

print ("mean_squared_error_sysid", mean_squared_error(ytest,np.transpose(yid)))
print ("mean_squared_error_sysid", mean_squared_error(ytest,np.transpose(yid_2)))