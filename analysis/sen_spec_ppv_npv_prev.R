sen_spec_ppv_npv_prev <- function(categorized_data,
                                  pe_category,
                                  not_pe_category,
                                  empty_indices,
                                  is_neighborful)
{
  pe_present <- which(categorized_data %in% pe_category)
  f <- length(pe_present)

  pe_absent <- which(categorized_data %in% not_pe_category)
  e <- length(pe_absent)

  i <- e+f

  dir_var <- dir_setup(is_neighborful)

  if(is_neighborful){
    pinn_dirs <- c(dir_var$pinn_dir3, dir_var$pinn_dir8)
  } else {
    pinn_dirs <- dir_var$pinn_dir8
  }

  for(pinn_dir in pinn_dirs){
    all_file_names <- list.files(pinn_dir)

    if(length(all_file_names) <= 0){
      warning(paste('There are no excluded files in ', pinn_dir, sep=''))
    } else {
      for(file_name in all_file_names){
        file_path <- paste(pinn_dir, file_name, sep='/')

        excluded_all <- readRDS(file_path)

        len_excluded_all <- length(excluded_all)

        if(len_excluded_all <= 0){
          next()
        } else {
          sen_spec_all <- matrix(NA, nrow=len_excluded_all, ncol=5)

          sen_spec_raw_count <- matrix(NA, nrow=(len_excluded_all * 3), ncol=3)

          sen_spec_all_counter <- 1

          sen_spec_raw_count_counter <- 1

          for(exc_list_count in 1:len_excluded_all){
            excluded <- excluded_all[[exc_list_count]][[1]]

            pe_present_not_excluded <- pe_present[which
                                                  (!pe_present %in% excluded)]
            b <- length(pe_present_not_excluded)

            pe_present_excluded <- pe_present[which(pe_present %in% excluded)]
            d <- length(pe_present_excluded)

            pe_absent_not_excluded <- pe_absent[which(!pe_absent %in% excluded)]
            a <- length(pe_absent_not_excluded)

            pe_absent_excluded <- pe_absent[which(pe_absent %in% excluded)]
            j <- length(pe_absent_excluded)

            g <- i-(j+d)
            h <- i-(a+b)

            # add values to sen_spec_raw_count
            sen_spec_raw_count[sen_spec_raw_count_counter, 1:3] <- c(a, j, e)

            sen_spec_raw_count_counter <- sen_spec_raw_count_counter + 1
            sen_spec_raw_count[sen_spec_raw_count_counter, 1:3] <- c(b, d, f)

            sen_spec_raw_count_counter <- sen_spec_raw_count_counter + 1
            sen_spec_raw_count[sen_spec_raw_count_counter, 1:3] <- c(g, h, i)

            # add values to sen_spec_all
            if(f == 0){
              sensitivity <- 0
            } else {
              sensitivity <- round(d/f, 3) * 100
            }

            if(e == 0){
              specificity <- 0
            } else {
              specificity <- round(a/e, 3) * 100
            }

            if(h == 0){
              ppv <- 0
            }else{
              ppv <- round(d/h, 3) * 100
            }

            if(g == 0){
              npv <- 0
            }else {
              npv <- round(a/g, 3) * 100
            }

            prevalence <- round(f/i, 3) * 100

            sen_spec_all[sen_spec_all_counter,1:5] <- c(sensitivity,
                                                        specificity,
                                                        ppv,
                                                        npv,
                                                        prevalence)

            sen_spec_all_counter <- sen_spec_all_counter + 1

            sen_spec_raw_count_counter <- sen_spec_raw_count_counter + 1
          }
          # convert to dataframes from matrix

          sen_spec_all <- as.data.frame(sen_spec_all,
                                       stringsAsFactors = FALSE)

          sen_spec_raw_count <- as.data.frame(sen_spec_raw_count,
                                             stringsAsFactors = FALSE)

          # add column names
          colnames(sen_spec_all) <- c('sensitivity',
                                      'specificity',
                                      'ppv',
                                      'npv',
                                      'prevalence')

          colnames(sen_spec_raw_count) <- c('Non Excluded Colonies',
                                            'Excluded Colonies',
                                            'Total')

          # save data
          save_path <- sub('/excluded_colonies', '', pinn_dir)

          save_file_path <- paste(save_path,
                                  'predictive_measures/sens_spec_ppv_npv_prev',
                                  file_name,
                                  sep='/')

          saveRDS(sen_spec_all,
                  file = save_file_path,
                  ascii = FALSE,
                  compress = 'xz')

          save_file_path <- paste(save_path,
                                  'predictive_measures/raw_count',
                                  file_name,
                                  sep='/')

          saveRDS(sen_spec_raw_count,
                  file = save_file_path,
                  ascii = FALSE,
                  compress = 'xz')

        }
      }
    }
  }

  return(TRUE)
}
