from VARExportResults import ExportVARResults
from VARDataClasses import VARExportClass
import math

class ResultByFoldDevStatus:
    def open_results_get_fold_by_dev_status(path: str, fold: int, dev_status:str) -> {}:
        """Retrieves the result for a single dev status in a single fold"""

        res = ExportVARResults.load(path)
        res = VARExportClass(**res)

        res_obj = {}
        res_obj["development status"] = dev_status

        for dev_status_res in res.dev_status_results:
            print(dev_status_res)
            if dev_status != dev_status_res["development_status"]:
                pass

            for fold_res in dev_status_res["fold_results"]:
                if fold_res["fold_ita"] == fold:
                    res_obj["res"] = fold_res
                    return res_obj
    
    def plot_res_list(res_list: list, export_path: str) -> None:
        dev_status_list = [rs["development status"] for rs in res_list]
        rmse_list = [math.log10(rs["res"]["mean_rmse"] + 1 ) for rs in res_list]
        print(rmse_list)

        ExportVARResults.plot_simple_bar(
            dev_status_list, rmse_list, "Development status", "Mean VAR RMSE (log10)", f"Mean VAR RMSE by Development status for fold {fold} (log10)", export_path)
            
if __name__ == "__main__":
    fold = 4
    debug_path = "/Python/Models/VectorAutoRegression"
    res_list = []
    #res_list.append(ResultByFoldDevStatus.open_results_get_fold_by_dev_status(f"{debug_path}/Developed_VAR dev status results.json", fold,  "Developed region"))
    #res_list.append(ResultByFoldDevStatus.open_results_get_fold_by_dev_status(f"{debug_path}/Developing_VAR dev status results.json", fold,  "Developing region"))

    res_list.append(ResultByFoldDevStatus.open_results_get_fold_by_dev_status("./Developing_VAR dev status results.json", fold,  "Developing region"))
    res_list.append(ResultByFoldDevStatus.open_results_get_fold_by_dev_status("./Least developed_VAR dev status results.json", fold,  "Least developed region"))
    res_list.append(ResultByFoldDevStatus.open_results_get_fold_by_dev_status("./Emerging_VAR dev status results.json", fold,  "Emerging region"))
    res_list.append(ResultByFoldDevStatus.open_results_get_fold_by_dev_status("./Developed_VAR dev status results.json", fold,  "Developed region"))
    ResultByFoldDevStatus.plot_res_list(res_list, "./test_plot.png")



