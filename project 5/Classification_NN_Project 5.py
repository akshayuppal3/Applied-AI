# This Python 3 environment comes with many helpful analytics libraries installed
# It is defined by the kaggle/python docker image: https://github.com/kaggle/docker-python
# For example, here's several helpful packages to load in 

import numpy as np # linear algebra
import pandas as pd # data processing, CSV file I/O (e.g. pd.read_csv)

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

from subprocess import check_output
print(check_output(["ls", "../input"]).decode("utf8"))

#from sklearn.ensemble import RandomForestClassifier

#keras
from keras.models import Sequential
from keras.layers import Dense
from keras.wrappers.scikit_learn import KerasClassifier
from keras.utils import np_utils

# Any results you write to the current directory are saved as output.

# define baseline model
def baseline_model():
	# create model
    model = Sequential()
    model.add(Dense(8, input_dim=93, activation='relu'))
    model.add(Dense(9, activation='softmax'))
	# Compile model
    model.compile(loss='categorical_crossentropy', optimizer='adam', metrics=['accuracy'])
    return model

dataset = pd.read_csv('../input/train.csv')
X = dataset.iloc[: ,1 :-1].values
Y = dataset.iloc[:, -1].values

from sklearn.preprocessing import Imputer
imputer = Imputer(missing_values = "NaN" , strategy = "mean", axis =0)
imputer = imputer.fit(X[:,:])
X[:,:] = imputer.transform(X[:,:])

from sklearn.preprocessing import LabelEncoder
label_encoder_Y = LabelEncoder()
Y = label_encoder_Y.fit_transform(Y)

from sklearn.cross_validation import train_test_split
X_trn, X_test, Y_trn , Y_test = train_test_split(X,Y, test_size= 0.2, random_state= 0)

#keras
knn = KerasClassifier(build_fn=baseline_model, epochs=200, batch_size=5, verbose=0)
knn.fit(X_trn, Y_trn)

Y_pred = knn.predict(X_test)
dataset_test = pd.read_csv('../input/test.csv')
X_test_out = dataset_test.iloc[:, 1:].values

submission = pd.DataFrame({ "id": dataset_test["id"]})

i = 0

# Create column name based on target values(see sample_submission.csv)
range_of_classes = range(1, 10)
for num in range_of_classes:
    col_name = str("Class_{}".format(num))
    submission[col_name] = Y_pred_out[:,i]
    i = i + 1
submission.to_csv('otto.csv', index=False)   