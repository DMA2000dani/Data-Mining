# -*- coding: utf-8 -*-
"""SVM.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/14TlmAM5HFNLyTgiwgn5kHcOzkwxeU2US
"""

import pandas as pd
from sklearn.model_selection import train_test_split, cross_val_score
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.manifold import TSNE
from sklearn.naive_bayes import BernoulliNB, GaussianNB, CategoricalNB, MultinomialNB
import numpy as np
from sklearn.metrics import  ConfusionMatrixDisplay,\
                  classification_report,  RocCurveDisplay, PrecisionRecallDisplay,\
                    accuracy_score, f1_score, precision_score, recall_score
from sklearn.neighbors import KNeighborsClassifier
from sklearn.preprocessing import MinMaxScaler, StandardScaler
from sklearn.tree import DecisionTreeClassifier
from IPython.core.interactiveshell import InteractiveShell
InteractiveShell.ast_node_interactivity = "all"
from IPython.display import display, HTML
show_html = lambda html: display(HTML(html))
from yellowbrick.classifier.rocauc import roc_auc
!pip install skopt
!pip install scikit-optimize
import skopt
from skopt import BayesSearchCV
from sklearn.ensemble import RandomForestClassifier
from sklearn.svm import SVC

from google.colab import drive
drive.mount('/content/drive')

df = pd.read_csv("drive/MyDrive/MD/Project_2/Data/Cleaned_Dataset.csv", sep=",", encoding="UTF-8")
df

df = df.dropna()

cls = [str(v) for v in df['Credit_Score'].unique()]
cls

df.columns

X = df.loc[:,df.columns !="Credit_Score"]
y = df["Credit_Score"]

scaler=MinMaxScaler()
X=pd.DataFrame(scaler.fit_transform(X))

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

cv=10
iter=40

rbsvc =  SVC(kernel='rbf', max_iter=25000, random_state=0)
scores = cross_val_score(rbsvc,X_train,y_train,cv=10)
print("Mean accuracy: {:.2f}%".format(np.mean(scores)*100))
print("Variance: {:.4f}".format(np.var(scores)))

param = {'C':10**np.linspace(-3,3,101), 'gamma':['scale','auto']}


rbsvc_gs = BayesSearchCV(rbsvc,param,n_iter=iter, cv=cv, n_jobs=-1, refit=True, random_state=0)
rbsvc_gs.fit(X_train, y_train);

show_html(pd.DataFrame(rbsvc_gs.cv_results_).loc[:,['params', 'mean_test_score','rank_test_score']].sort_values(by='rank_test_score').head().to_html())

print(classification_report(rbsvc_gs.predict(X_test), y_test,target_names=cls))

plt.figure(figsize=(8,8));
ConfusionMatrixDisplay.from_estimator(rbsvc_gs, X_test, y_test, display_labels=cls, ax=plt.subplot());