dir_setup <- function(is_neighborful)
{

  # setup directories
  old_dir <- getwd()

  new_dir <- basename(tempdir())

  new_dir <- paste(old_dir, new_dir, sep='/')

  dir_var <- list(old_dir = old_dir, new_dir = new_dir)

  pinn_dir1 <- 'results/neighborful/platemaps'
  pinn_dir1 <- paste(new_dir, pinn_dir1, sep='/')

  pinn_dir2 <- 'results/predictive_measures/raw_count'
  pinn_dir2 <- paste(new_dir, pinn_dir2, sep='/')

  pinn_dir3 <- 'data/neighborful/excluded_colonies'
  pinn_dir3 <- paste(new_dir, pinn_dir3, sep='/')

  pinn_dir4 <- 'data/neighborful/predictive_measures/sens_spec_ppv_npv_prev'
  pinn_dir4 <- paste(new_dir, pinn_dir4, sep='/')

  pinn_dir5 <- 'data/neighborful/predictive_measures/raw_count'
  pinn_dir5 <- paste(new_dir, pinn_dir5, sep='/')

  pinn_dir6 <- 'results/neighborless/platemaps'
  pinn_dir6 <- paste(new_dir, pinn_dir6, sep='/')

  pinn_dir7 <- 'results/predictive_measures/sens_spec_ppv_npv_prev'
  pinn_dir7 <- paste(new_dir, pinn_dir7, sep='/')

  pinn_dir8 <- 'data/neighborless/excluded_colonies'
  pinn_dir8 <- paste(new_dir, pinn_dir8, sep='/')

  pinn_dir9 <- 'data/neighborless/predictive_measures/sens_spec_ppv_npv_prev'
  pinn_dir9 <- paste(new_dir, pinn_dir9, sep='/')

  pinn_dir10 <- 'data/neighborless/predictive_measures/raw_count'
  pinn_dir10 <- paste(new_dir, pinn_dir10, sep='/')

  dir_var <- c(dir_var,
               pinn_dir1 = pinn_dir1,
               pinn_dir2 = pinn_dir2,
               pinn_dir3 = pinn_dir3,
               pinn_dir4 = pinn_dir4,
               pinn_dir5 = pinn_dir5,
               pinn_dir6 = pinn_dir6,
               pinn_dir7 = pinn_dir7,
               pinn_dir8 = pinn_dir8,
               pinn_dir9 = pinn_dir9,
               pinn_dir10 = pinn_dir10)

  if(is_neighborful){
    dir_counter_all <- 3:12
  } else {
    dir_counter_all <- 8:12
  }

  for(dir_counter in dir_counter_all){
    dir_path <- dir_var[[dir_counter]]

    if (! file_test('-d', dir_path)) {
      dir.create(dir_path, showWarnings = FALSE, recursive = TRUE)
    }
  }

  return(dir_var)
}
