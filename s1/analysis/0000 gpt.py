import pandas as pd
import networkx as nx
import matplotlib.pyplot as plt
from statsmodels.formula.api import ols
import numpy as np


# Load the data
data = pd.read_csv("s1/data/clean.l.sub.csv")

# Initialize a directed graph
G = nx.DiGraph()

# Adding nodes. Each student becomes a node
for id in data['StudyID'].unique():
    G.add_node(id)

# Adding edges for friends
for i, row in data.iterrows():
    G.add_edge(row['StudyID'], row['pn_bf1'])
    G.add_edge(row['StudyID'], row['pn_bf2'])

# Adding edges for self-control exemplars
for i, row in data.iterrows():
    G.add_edge(row['StudyID'], row['pn_scw'])
    G.add_edge(row['StudyID'], row['pn_scw2'])

# Calculating each node's degree (number of connections) and in-degree (incoming connections)
data['degree'] = data['StudyID'].apply(lambda x: G.degree(x))
data['in_degree'] = data['StudyID'].apply(lambda x: G.in_degree(x))

# Basic network visualization
nx.draw(G, with_labels=True)
plt.show()

# Analysis - Calculate correlation between self-control and academic performance
self_control_corr = data['scw_s'].corr(data['coregpa'])
print(f'Correlation between self-control and GPA: {self_control_corr}')

# Analysis - Calculate correlation between degree and self-control
degree_self_control_corr = data['degree'].corr(data['scw_s'])
print(f'Correlation between number of connections and self-control: {degree_self_control_corr}')

# Analysis - Calculate correlation between in-degree and self-control
in_degree_self_control_corr = data['in_degree'].corr(data['scw_s'])
print(f'Correlation between number of incoming connections and self-control: {in_degree_self_control_corr}')


# Try two
# Load the data
data = pd.read_csv("s1/data/clean.l.sub.csv")

# Initialize a directed graph
G = nx.DiGraph()

# Add nodes and edges to the graph
for i, row in data.iterrows():
    G.add_node(row['StudyID'], attr_dict=row.to_dict())
    G.add_edge(row['StudyID'], row['pn_bf1'])
    G.add_edge(row['StudyID'], row['pn_bf2'])
    G.add_edge(row['StudyID'], row['pn_scw'])
    G.add_edge(row['StudyID'], row['pn_scw2'])

# Calculate mean GPA and teacher rating within each student's ego network (both friends and exemplars)
data['ego_net_avg_gpa'] = np.nan
data['ego_net_avg_scw_tr'] = np.nan
for node in G.nodes:
    ego_net = nx.ego_graph(G, node)
    egos = [n for n in ego_net.nodes if n != node]
    if egos:
        data.loc[data['StudyID'] == node, 'ego_net_avg_gpa'] = data.loc[data['StudyID'].isin(egos), 'coregpa'].mean()
        data.loc[data['StudyID'] == node, 'ego_net_avg_scw_tr'] = data.loc[data['StudyID'].isin(egos), 'avg_scw_tr'].mean()


# Run an OLS regression controlling for demographics and objective indicators of self-control
regression_formula = "scw_s ~ ego_net_avg_gpa + ego_net_avg_scw_tr + female + eth + ell + sped + frpl + age"
model = ols(regression_formula, data).fit()
print(model.summary())