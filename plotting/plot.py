# from pdb import set_trace
import os
import glob
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

sns.set_palette('pastel')
folder = os.path.join(os.getcwd(), 'results')
csvs = glob.glob(os.path.join(folder, '*.csv'))
result = lambda file: os.path.join(folder, file)
def save_ax(ax, name):
    fig = ax.figure
    fig.savefig(result(f'{name}.png'))
    plt.close(fig)

for csv in csvs:

    df = pd.read_csv(csv, dialect='unix')

    var_name = 'dataset'
    id_var = 'epoch'
    value_name = 'loss'
    value_map = {'lossTrain': 'train', 'lossValid': 'validation'}
    ax = sns.lineplot(
        x=id_var,
        y=value_name,
        hue=var_name,
        data=df.rename(columns=value_map).melt(var_name=var_name, value_name=value_name, id_vars=[id_var], value_vars=value_map.values())
    )
    fname = csv.replace('.csv', f'-{value_name}')
    save_ax(ax, fname)

    metrics = ['accValid']
    for metric in metrics:
        ax = sns.lineplot(
            x=id_var,
            y=metric,
            data=df
        )
        fname = csv.replace('.csv', f'-{metric}')
        save_ax(ax, fname)
