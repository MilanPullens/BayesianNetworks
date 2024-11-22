import pandas as pd
import matplotlib.pyplot as plt

topMilan = pd.read_csv('top200Milan.csv')#.head(100)
topPip = pd.read_csv('top200Pip.csv')#.head(100)
topManon = pd.read_csv('top200Manon.csv')#.head(100)

for df in [topMilan, topPip, topManon]:
    df["release_date"] = df["release_date"].astype(str).apply(lambda x: x[:4]).astype(int)
    df["explicit"] = df["explicit"].astype(int)

interesting = topMilan[topMilan['instrumentalness'] < 0.005][['artist','name', 'valence', 'danceability', 'energy', 'instrumentalness']]

print(interesting)

attributes = ['popularity', 'tempo', 'valence', 'danceability', 'energy', 'acousticness', 'liveness', 'explicit', 'loudness', 'instrumentalness', 'duration_ms', 'release_date']


num_attributes = len(attributes)
cols = 4  
rows = (num_attributes + cols - 1) // cols  

fig, axes = plt.subplots(rows, cols, figsize=(9, 6), constrained_layout=True)
axes = axes.flatten() 

# for i, attribute in enumerate(attributes):
#     axes[i].hist(topDF[attribute], bins=15, color='blue', alpha=0.7)
#     axes[i].set_title(attribute.capitalize()) 

# for j in range(i + 1, len(axes)):
#     fig.delaxes(axes[j])

dataframes = [topMilan, topPip, topManon]
names = ['Milan', 'Pip', 'Manon']

colors = ['blue', 'green', 'red']

for i, attribute in enumerate(attributes):
    for df, label, color in zip(dataframes, ["Listener_0", "Listener_1", "Listener_2"], colors):
        axes[i].hist(df[attribute], bins=15, alpha=0.3, label=label, color=color)

    axes[i].set_title(attribute.capitalize())

    axes[i].legend()

plt.show()

combisHad = []

for i, name1 in enumerate(names):
    for j, name2 in enumerate(names):
        if name1 != name2 and not set([name1,name2]) in combisHad:
            combisHad.append(set([name1,name2]))
            print(f"Common numbers {name1} and {name2}:")
            for id in set(dataframes[i]["id"]):
                if id in set(dataframes[j]["id"]):
                    sharedRow = dataframes[i].loc[dataframes[i]['id'] == id]
                    print(f"{sharedRow['artist'].to_string(index=False)} - {sharedRow['name'].to_string(index=False)}")
            print()