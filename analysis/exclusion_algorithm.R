exclusion_algorithm <- function(plateformat,
                                data_area,
                                empty_indices,
                                is_neighborful = FALSE,
                                is_save = FALSE)
{
  # setup directories
  dir_var <- dir_setup(is_neighborful)

  if(is_neighborful){
    # neighborful algorithm
    for(percent_median_thresh in c(25)){
      for(param1 in 1:8){
        for(param2 in 1:8){
          excluded_colonies <- c()

          thresh_file <- paste('neighborful_excluded_colonies_A_H_',
                               percent_median_thresh, 'percent_',
                               param1, '_', param2,
                               '.rds', sep='')
          excluded_filepath <- paste(dir_var$pinn_dir3, thresh_file, sep='/')

          # call neighborful_algorithm
          neighborful_algorithm(plateformat           = plateformat,
                                colony_area_raw_data  = data_area,
                                empty_indices         = empty_indices,
                                excluded_colonies     = excluded_colonies,
                                percent_median_thresh = percent_median_thresh,
                                param1_thresh         = param1,
                                param2_thresh         = param2,
                                is_save               = is_save,
                                excluded_file         = excluded_filepath)
        }
      }
    }
  }

  # neighborless algorithm
  excluded_colonies <- c()

  excluded_filepath <- paste(dir_var$pinn_dir8,
                             'neighborless_excluded_colonies.rds',
                             sep='/')

  # call neighborless_algorithm
  neighborless_algorithm(plateformat          = plateformat,
                         colony_area_raw_data = data_area,
                         empty_indices        = empty_indices,
                         excluded_colonies    = excluded_colonies,
                         is_save              = is_save,
                         excluded_file        = excluded_filepath)
  return(TRUE)
}
