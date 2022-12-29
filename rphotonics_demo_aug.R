library(data.table)
library(slider)
library(tictoc)
source("07_22//rphotonics_beta_aug.R")

# DATA.TABLES USES COPIES OF ITSELF WHEN REFERNCED, THIS MAKES THE VAR NAMES DYNAMIC
# https://stackoverflow.com/questions/10225098/understanding-exactly-when-a-data-table-is-a-reference-to-vs-a-copy-of-another

###################

# Rphotonics Demo #

###################

##############
# Waveguides #
##############

get_demo_data <- function(directory) {
  dfwg <- map_files_to_df(directory)

  dfwg$dims <- get_number_from_string(input_string = dfwg$dims)

  lambda_filter <- seq(1490, 1590.02, 0.06*70)

  dfwg <- dfwg %>% filter(lambda %in% lambda_filter)

  return(dfwg)
}

###################
# Ring Resonators #
###################

# Colnames should be "lambda", "power" after this point
raw_data <- get_test_data("C:\\Users\\qc677770\\Downloads\\DATA", 2, struct_type = "res")

# Ring Circumfrance (um -> nm)
inner_ring_radius <- 13.58 / 2
outer_ring_radius <- 14.44 / 2
ring_width <- outer_ring_radius - inner_ring_radius
middle_ring_radius <- inner_ring_radius + ring_width / 2
ring_length <- 2 * pi * middle_ring_radius * 1000
print(ring_length)

final_data <- rr_anlaysis(input_data = raw_data, labels = c("lambda", "power"),
            ring_perimeter = ring_length)

lor_data <- final_data$lorenz_data
lor_peaks <- final_data$lorenz_peaks
lor_specs <- final_data$ring_specs

print(lorenzian_fit_plot(lor_data[[2]], lor_peaks, 2))
#print(final_data)
#print(extract_peaks_visual(raw_data, minpeakdistance = 1000))
#export(lorenzian_fit_plot(lor_data[[2]], lor_peaks, 2), file = "test4.png")

#data <- cut_back_method(get_demo_data("07_22\\GOTHICS"),"x_y_die", list("lambda", "loss1", "dims"))

#print(plot_waveguide_data(data))


# Experimental
#  mw_copy <- copy(input_data_copy)
#
#  mw_sm_copy <- copy(mw_copy)
#  smoothed_col <- "power_sub"
#  mw_sm <- smooth_data(mw_copy, y_label = smoothed_col, span = 0.02)
#  mw_sm_copy[, (smoothed_col) := mw_sm]

#  mw_copy[, power_sub := 10**(power_sub / 10)]
#  mw_copy[, power_sub := power_sub / max(power_sub)]

#  mw_sm_copy[, power_sub := 10**(power_sub/10)]
#  mw_sm_copy[, power_sub := power_sub / max(power_sub)]

#  print(generate_loss_plot(data = mw_copy, y_name = "power_sub"))
#  print(generate_loss_plot(data = mw_sm_copy, y_name = "power_sub"))
  ###########################################