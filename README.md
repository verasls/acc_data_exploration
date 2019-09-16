# Exploratory analysis of accelerometry data

Code and data analysis reports for exploratory analysis of some accelerometry aspects

The repository is organized as follows:

```
.
├── code/ 								
│   ├── functions/			# R code with function definitions
│   └── scripts/			# R code with data analysis scripts
│       └── aux/			# R code with some auxiliary scripts to process data
├── .gitignore	
├── LICENSE
├── README.md 							
├── acc_data_exploration.Rproj
├── *.Rmd 				# Data analysis reports Rmd files
```

A quick briefing about the analysis can be seen below, with links to the html version of the reports.

[EE_regressions_comparison](https://lveras.com/reports/report_EE_regressions_comparison.html): develop an equation to predict energy expenditure in class II-III obese people, determine its prediction accuracy and compare it with those equations developed for non-obese people

[GRF_ACC_misplacement](https://lveras.com/reports/report_GRF_ACC_misplacement.html): compare the effect of correct/incorrect acceleromter placement at waist line on peak ground reaction force prediction accuracy

[METs_comparison](https://lveras.com/reports/report_METs_comparison.html): compare the moderate-to-vigorous physical activity intensity classification obtained from cut-points based on individually determined (based on individual rest metablic rate) and standard (3.5 ml O<sub>2</sub><sup>.</sup>kg<sup>-1</sup>min<sup>-1</sup> ) METs

[PAI_steps](https://lveras.com/reports/report_PAI_steps.html): develop cut-points to classify moderate and vigorous physical activityin severely obese patients based on step counts data

[EE_actigraph_filtering](https://lveras.com/reports/report_EE_actigraph_filtering.html): compare raw accelerometer metrics output and energy expenditure prediction accuracy between accelerometers with filtered and unfiltered data