source("R/calculateAccMetricsAvg.R")
source("R/calculateStepsAvg.R")

# Counts_vert_vs_res ------------------------------------------------------
# Calculate average counts by speed
calculateAccMetricsAvg("/Volumes/LV_HD/Accelerometry/PAI_ACC/projects/ECSS_2019/counts_back/output_30secROI")
calculateAccMetricsAvg("/Volumes/LV_HD/Accelerometry/PAI_ACC/projects/ECSS_2019/counts_hip/output_30secROI")


# PAI_steps ------------------------------------------------------------
# Calculate average steps by speed
calculateStepsAvg("/Volumes/LV_HD/Accelerometry/PAI_ACC/projects/ECSS_2019/steps_hip/output_30secROI")