
PDF_OUTFILE = paste(dir_var$pinn_dir1, thresh_file, sep='/')


legend_txt_bg_col <- c('Empty'                     = 'red',
                       'Pinning Error'             = 'black',
                       'Morethan Plate Median'     = '#660066',
                       'Lessthan Plate Median'     = 'green',
                       'Morethan 90% Plate Median' = 'cyan',
                       'Lessthan 25% Plate Median' = 'yellow',
                       'Excluded Colonies'         = 'blue')

plot_platemap(plateformat = 384,
              plate_data = a,
              data_flow = 'across',
              legend_txt_bg_col = legend_txt_bg_col)

plot_platemap(plateformat = 384,
              plate_data = data.frame(colonyarea$data_subtypes,
                                      stringsAsFactors = FALSE),
              data_flow = 'across',
              legend_txt_bg_col = legend_txt_bg_col)

plot_platemap(plateformat = 384,
              plate_data = across_data,
              data_flow = 'across',
              legend_txt_bg_col = legend_txt_bg_col)

plot_platemap(plateformat = 384,
              plate_data = down_data,
              data_flow = 'down',
              legend_txt_bg_col = legend_txt_bg_col)

plot_platemap(plateformat = 384,
              plate_data = down_data2,
              data_flow = 'down',
              legend_txt_bg_col = legend_txt_bg_col)

plateformat = 1536
plate_data = data.frame(convert_384_to_1536_data(colonyarea$data_subtypes,
                                                 is_plate_config = FALSE,
                                                 in_data_flow = 'down',
                                                 out_data_flow = 'down'),
                        stringsAsFactors = FALSE)

plate_data <- convert_down_across(1536, plate_data$data_subtypes, FALSE, 'down', 'across')
plot_platemap(plateformat,
              plate_data = plate_data,
              data_flow = 'down',
              legend_txt_bg_col = legend_txt_bg_col,
              symbol_size = rep(0.4, plateformat))

plateformat = 96
plate_data = data.frame(plate_data[c(1:96),], stringsAsFactors = FALSE)

plot_platemap(plateformat,
              plate_data = plate_data,
              data_flow = 'down',
              legend_txt_bg_col = legend_txt_bg_col,
              symbol_size = rep(0.4, plateformat))

data_1536 <- data.frame(simulated_data_1536(colonyarea$data_subtypes),
                        stringsAsFactors = FALSE)





