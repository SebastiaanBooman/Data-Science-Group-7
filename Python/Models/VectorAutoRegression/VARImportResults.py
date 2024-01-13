from VARExportResults import ExportVARResults
from VARDataClasses import VARExportClass
import numpy as np
import matplotlib.pyplot as plt
import math

class VARImportResults:
    def load_and_plot_var_res(path: str, stat_to_plot: str, title: str, ylab: str, xlab_lambda, percentage:bool, log:bool):
        res = ExportVARResults.load(path)

        res = VARExportClass(**res)
        filename = path.replace(".json", "")
        VARImportResults.plot(res, stat_to_plot, title, ylab, xlab_lambda, percentage, filename, log)
    
    def plot(obj: VARExportClass, stat_to_plot: str, title: str, ylab: str, xlab_lambda, percentage, filename: str, log:bool):
        folds = [i for i in range(len(obj.dev_status_results[0]["fold_results"]))]
        dev_statuses = [xlab_lambda(ds) for ds in obj.dev_status_results]

        total_fold_res = {}
        
        for fold_i in range(len(folds)):
            i_fold_res = []
            for dev_status_res in obj.dev_status_results:
                total_country_amount = dev_status_res["country_amount"]
                for fold_res in dev_status_res["fold_results"]:
                    if fold_res["fold_ita"] == (fold_i + 1):
                        if percentage:                               
                            i_fold_res.append(
                                round((fold_res[stat_to_plot]/total_country_amount) * 100, 1) if type(fold_res[stat_to_plot]) in (float, int) else 0 
                            )
                        else:
                            i_fold_res.append(
                                round(fold_res[stat_to_plot], 3) if type(fold_res[stat_to_plot]) in (float, int) else 0
                            )
                
            total_fold_res[f"Fold {fold_i + 1}"] = i_fold_res 

        x = np.arange(len(dev_statuses))  # the label locations
        width = 0.20  # the width of the bars
        multiplier = 0

        fig, ax = plt.subplots(layout='constrained', figsize=(10,6))

        for attribute, measurement in total_fold_res.items():
            offset = width * multiplier
            if log:
                measurement_log = map(lambda x: round(math.log10(x+1), 3), measurement)
                measurement_log = list(measurement_log)
                rects = ax.bar(x + offset, measurement_log, width, label=attribute)
            else:
                rects = ax.bar(x + offset, measurement, width, label=attribute)
            ax.bar_label(rects, padding=3)
            multiplier += 1

        if log:
            title = title + ' (Log10)'
            ylab = ylab + ' (Log10)'

        ax.spines[['right', 'top']].set_visible(False)
        ax.set_ylabel(ylab)
        ax.set_title(title, weight='bold')
        ax.set_xticks(x + width, dev_statuses)
        ax.legend(loc='upper left', ncols=1)
        ax.set_ylim(0, 0.2)
        #plt.show()

        plt.savefig(f"{filename} + {stat_to_plot}_percentage.png", dpi=150)

if __name__ == "__main__":
    log:bool = True
    percentage:bool = True

    # Parameter tuning results export
    """
    VARImportResults.load_and_plot_var_res(
        "./VAR dev status results.json",
        "mean_rmse",
        'VAR RMSE by fold per development status',
        "RMSE",
        lambda x: x["development_status"], 
        False,
        log
    )"""
    VARImportResults.load_and_plot_var_res(
        "./VAR dev status results.json",
        "data_amount",
        '% VAR country amount by fold per development status',
        "% Data amount",
        lambda x: f"{x['development_status']}\n(total country amount: {x['country_amount']})", 
        percentage,
        False
    )

"""


    # Baseline model results export
    VARImportResults.load_and_plot_var_res(
        "./Baseline_VAR dev status results.json",
        "mean_rmse",
        'VAR RMSE by fold per development status',
        "RMSE",
        lambda x: x["development_status"], 
        False,
        log
    )
    VARImportResults.load_and_plot_var_res(
        "./Baseline_VAR dev status results.json",
        "data_amount",
        'VAR country amount by fold per development status',
        "Data amount",
        lambda x: f"{x['development_status']}\n(total country amount: {x['country_amount']})",
        percentage,
        False
    )
"""