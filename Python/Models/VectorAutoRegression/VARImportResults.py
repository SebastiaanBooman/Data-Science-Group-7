from VARExportResults import ExportVARResults
from VARDataClasses import VARExportClass
import numpy as np
import matplotlib.pyplot as plt
import math

class VARImportResults:
    def load_and_plot_var_res(path: str, stat_to_plot: str, title: str, ylab: str, xlab_lambda, log:bool):
        res = ExportVARResults.load(path)

        res = VARExportClass(**res)
        filename = path.replace(".json", "")
        VARImportResults.plot(res, stat_to_plot, title, ylab, xlab_lambda, filename, log)
    
    def plot(obj: VARExportClass, stat_to_plot: str, title: str, ylab: str, xlab_lambda, filename: str, log:bool):
        folds = [i for i in range(len(obj.dev_status_results[0]["fold_results"]))]
        dev_statuses = [xlab_lambda(ds) for ds in obj.dev_status_results]

        total_fold_res = {}
        
        for fold_i in range(len(folds)):
            i_fold_rmse = []
            for dev_status_res in obj.dev_status_results:
                for fold_res in dev_status_res["fold_results"]:
                    if fold_res["fold_ita"] == (fold_i + 1):
                        i_fold_rmse.append(round(fold_res[stat_to_plot], 3) if type(fold_res[stat_to_plot]) in (float, int) else 0)
                
            total_fold_res[f"Fold {fold_i + 1}"] = i_fold_rmse #[dev_status["fold_results"] for dev_status in obj.dev_status_results if dev_status["fold_results"]["fold_ita"] == i]

        x = np.arange(len(dev_statuses))  # the label locations
        width = 0.20  # the width of the bars
        multiplier = 0

        fig, ax = plt.subplots(layout='constrained', figsize=(10,6))

        for attribute, measurement in total_fold_res.items():
            offset = width * multiplier
            if log:
                measurement_log = map(lambda x: math.log10(x+1), measurement)
                measurement_log = list(measurement_log)
                rects = ax.bar(x + offset, measurement_log, width, label=attribute)
            else:
                rects = ax.bar(x + offset, measurement, width, label=attribute)
            ax.bar_label(rects, padding=3)
            multiplier += 1

        # Add some text for labels, title and custom x-axis tick labels, etc.
        if log:
            title = title + ' (Log10)'
            ylab = ylab + ' (Log10)'

        ax.spines[['right', 'top']].set_visible(False)
        ax.set_ylabel(ylab)
        ax.set_title(title, weight='bold')
        ax.set_xticks(x + width, dev_statuses)
        ax.legend(loc='upper left', ncols=1)
        #ax.set_ylim(0, 250)
        #plt.show()

        plt.savefig(f"{filename} + {stat_to_plot}.png", dpi=150)

if __name__ == "__main__":
    log:bool = True

    VARImportResults.load_and_plot_var_res("./VAR dev status results.json", "mean_rmse", 'VAR RMSE by fold per development status', "RMSE", lambda x: x["development_status"], log)
    VARImportResults.load_and_plot_var_res("./VAR dev status results.json", "data_amount", 'VAR country amount by fold per development status', "Data amount", lambda x: f"{x['development_status']}\n(total country amount: {x['country_amount']})", False)

    VARImportResults.load_and_plot_var_res("./Baseline_VAR dev status results.json", "mean_rmse", 'VAR RMSE by fold per development status', "RMSE", lambda x: x["development_status"], log)
    VARImportResults.load_and_plot_var_res("./Baseline_VAR dev status results.json", "data_amount", 'VAR country amount by fold per development status', "Data amount", lambda x: f"{x['development_status']}\n(total country amount: {x['country_amount']})", False)