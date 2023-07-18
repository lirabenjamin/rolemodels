# 0 Master

# Settings for analysis
outcomes = c('scw_s')
reclean_data = T # Set to T to reclean raw data
rerun = T # Set to T to rerun analyses
eachtime = F # Set to T to run models separately in each time point
coregpa_cut =  50
# For within person analyses
# Keep students who have at least k measures
k = 2


#Settings for plots
bw = T # Set to T for BW images
grayscale = c("gray40","gray90") # Grayscale colors for the bars
gray_errorbars = c("gray60","gray60") #Grayscale colors for the errorbars
ci_width = .1 # Width of errorbars in pictures
main_plot_h = 4*1 # Height of main plots
main_plot_w = 7*1 # Widdth of main plots

# Scripts to run
source("s1/analysis/0.1 Custom Functions.R")
source("s1/analysis/1. Clean Data.R")
source("s1/analysis/1.1 Interrater Reliability Teachers.R")
source("s1/analysis/1.1 GPA models.R")
source("s1/analysis/1.1 Reliability.R")
source("s1/analysis/2. Make datasets.R")
source("s1/analysis/3. Run models.R")
source("s1/analysis/4. Main Plots.R")
source("s1/analysis/4.1 Main Tables.R")
source("s1/analysis/5. Supplement Plots.R")

# Generate Reports
rmarkdown::render("Reports/Make presentation.Rmd",output_dir = "Reports/")
rmarkdown::render("Reports/results_plot.Rmd",output_dir = "Reports/")



models_plot %>% filter(model == "Average.Demographics",peer_type==  "Friends",term == "near")
