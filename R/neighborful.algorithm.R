neighborful.algorithm <- function(plate,
                                  excluded_colonies,
                                  data_files_path,
                                  common_algorithm_functions_path,
                                  PDF_OUTFILE,
                                  R_LIST_OUTFILE,
                                  simulated_data_template_infile,
                                  simulated_data_outfile,
                                  percent_median_exclusion_threshold,
                                  neighbor_length_threshold,
                                  param2_threshold,
                                  is_draw)
{
  total_colonies_index <- 1:plate

  # 1. create simulated data and platemap
  # convert 1536 format to 384 format and get indices
  indices_1536_to_384 <- index.1536.to.384()

  # get dataframe of gene deletion library
  bioneer_samples <- bioneer.samples(simulated_data_template_infile)

  bioneer_samples_384 <- bioneer_samples$Orf

  bioneer_samples_1536 <- data.384.to.1536(indices_1536_to_384, bioneer_samples_384)

  empty_spots_1536 <- which(bioneer_samples_1536 %in% 'empty')

  neighborful_excluded_colonies_ls_A <- list()
  neighborful_excluded_colonies_ls_B <- list()
  neighborful_excluded_colonies_ls_C <- list()
  neighborful_excluded_colonies_ls_D <- list()
  neighborful_excluded_colonies_ls_E <- list()
  neighborful_excluded_colonies_ls_F <- list()
  neighborful_excluded_colonies_ls_G <- list()
  neighborful_excluded_colonies_ls_H <- list()

  # median threshold for a given plate
  median_exclusion_threshold <- (100 / percent_median_exclusion_threshold)

  # plate median
  plate_median <- 600

  plate_median_threshold <- plate_median / median_exclusion_threshold

  # template data - simulation and platemap
  template.data(simulated_data_template_infile, simulated_data_outfile, plate_median, bioneer_samples_384, indices_1536_to_384, data.384.to.1536)

  file_number <- 1

  if(is_draw){
    simulated.data.platemap(infile = basename(simulated_data_template_infile), bioneer_samples_1536, file_number, PDF_OUTFILE, excluded_colonies)
  }
  # 2. Apply neighbor based algorithm to simulated data

  for (infile in list.files(data_files_path)){
    plate_index <- as.integer(sub('.*(?=..$)', '', infile, perl=T))

    file_name_path = paste(data_files_path, infile, sep = '')

    colony_area_raw_data <- read.table(file = file_name_path,
                                       header = FALSE,
                                       sep = "\t",
                                       stringsAsFactors = FALSE)

    #middle colonies
    is_middle = TRUE

    colony_indices <- total_colonies_index[-(not.middle.indices(plate))]

    excluded_coloniesA <- excluded.coloniesA(plate,
                                             colony_indices,
                                             colony_area_raw_data,
                                             empty_spots_1536,
                                             excluded_colonies,
                                             plate_median_threshold,
                                             neighbor_length_threshold,
                                             param2_threshold,
                                             is_middle)

    excluded_coloniesB <- excluded.coloniesB(plate,
                                             colony_indices,
                                             colony_area_raw_data,
                                             excluded_colonies,
                                             plate_median_threshold,
                                             neighbor_length_threshold,
                                             param2_threshold,
                                             is_middle)

    excluded_coloniesC <- excluded.coloniesC(plate,
                                             colony_indices,
                                             colony_area_raw_data,
                                             empty_spots_1536,
                                             excluded_colonies,
                                             plate_median_threshold,
                                             neighbor_length_threshold,
                                             is_middle)

    excluded_coloniesD <- excluded.coloniesD(plate,
                                             colony_indices,
                                             colony_area_raw_data,
                                             excluded_colonies,
                                             plate_median_threshold,
                                             neighbor_length_threshold,
                                             is_middle)

    excluded_coloniesE <- excluded.coloniesE(plate,
                                             colony_indices,
                                             colony_area_raw_data,
                                             empty_spots_1536,
                                             excluded_colonies,
                                             plate_median_threshold,
                                             neighbor_length_threshold,
                                             param2_threshold,
                                             is_middle)

    excluded_coloniesF <- excluded.coloniesF(plate,
                                             colony_indices,
                                             colony_area_raw_data,
                                             empty_spots_1536,
                                             excluded_colonies,
                                             plate_median_threshold,
                                             neighbor_length_threshold,
                                             param2_threshold,
                                             is_middle)

    excluded_coloniesG <- excluded.coloniesG(plate,
                                             colony_indices,
                                             colony_area_raw_data,
                                             empty_spots_1536,
                                             excluded_colonies,
                                             plate_median_threshold,
                                             neighbor_length_threshold,
                                             is_middle)

    excluded_coloniesH <- excluded.coloniesH(plate,
                                             colony_indices,
                                             colony_area_raw_data,
                                             empty_spots_1536,
                                             excluded_colonies,
                                             plate_median_threshold,
                                             neighbor_length_threshold,
                                             is_middle)

    #peripheral colonies

    is_middle = FALSE

    colony_indices = not.middle.indices(plate)

    excluded_coloniesA <- excluded.coloniesA(plate,
                                             colony_indices,
                                             colony_area_raw_data,
                                             empty_spots_1536,
                                             excluded_coloniesA,
                                             plate_median_threshold,
                                             neighbor_length_threshold,
                                             param2_threshold,
                                             is_middle)

    excluded_coloniesB <- excluded.coloniesB(plate,
                                             colony_indices,
                                             colony_area_raw_data,
                                             excluded_coloniesB,
                                             plate_median_threshold,
                                             neighbor_length_threshold,
                                             param2_threshold,
                                             is_middle)

    excluded_coloniesC <- excluded.coloniesC(plate,
                                             colony_indices,
                                             colony_area_raw_data,
                                             empty_spots_1536,
                                             excluded_coloniesC,
                                             plate_median_threshold,
                                             neighbor_length_threshold,
                                             is_middle)

    excluded_coloniesD <- excluded.coloniesD(plate,
                                             colony_indices,
                                             colony_area_raw_data,
                                             excluded_coloniesD,
                                             plate_median_threshold,
                                             neighbor_length_threshold,
                                             is_middle)

    excluded_coloniesE <- excluded.coloniesE(plate,
                                             colony_indices,
                                             colony_area_raw_data,
                                             empty_spots_1536,
                                             excluded_coloniesE,
                                             plate_median_threshold,
                                             neighbor_length_threshold,
                                             param2_threshold,
                                             is_middle)

    excluded_coloniesF <- excluded.coloniesF(plate,
                                             colony_indices,
                                             colony_area_raw_data,
                                             empty_spots_1536,
                                             excluded_coloniesF,
                                             plate_median_threshold,
                                             neighbor_length_threshold,
                                             param2_threshold,
                                             is_middle)

    excluded_coloniesG <- excluded.coloniesG(plate,
                                             colony_indices,
                                             colony_area_raw_data,
                                             empty_spots_1536,
                                             excluded_coloniesG,
                                             plate_median_threshold,
                                             neighbor_length_threshold,
                                             is_middle)

    excluded_coloniesH <- excluded.coloniesH(plate,
                                             colony_indices,
                                             colony_area_raw_data,
                                             empty_spots_1536,
                                             excluded_coloniesH,
                                             plate_median_threshold,
                                             neighbor_length_threshold,
                                             is_middle)


    #send excluded ones as a list to the parent list
    neighborful_excluded_colonies_ls_A[[file_number]] <- list(excluded_coloniesA)
    names(neighborful_excluded_colonies_ls_A)[file_number] <- infile

    neighborful_excluded_colonies_ls_B[[file_number]] <- list(excluded_coloniesB)
    names(neighborful_excluded_colonies_ls_B)[file_number] <- infile

    neighborful_excluded_colonies_ls_C[[file_number]] <- list(excluded_coloniesC)
    names(neighborful_excluded_colonies_ls_C)[file_number] <- infile

    neighborful_excluded_colonies_ls_D[[file_number]] <- list(excluded_coloniesD)
    names(neighborful_excluded_colonies_ls_D)[file_number] <- infile

    neighborful_excluded_colonies_ls_E[[file_number]] <- list(excluded_coloniesE)
    names(neighborful_excluded_colonies_ls_E)[file_number] <- infile

    neighborful_excluded_colonies_ls_F[[file_number]] <- list(excluded_coloniesF)
    names(neighborful_excluded_colonies_ls_F)[file_number] <- infile

    neighborful_excluded_colonies_ls_G[[file_number]] <- list(excluded_coloniesG)
    names(neighborful_excluded_colonies_ls_G)[file_number] <- infile

    neighborful_excluded_colonies_ls_H[[file_number]] <- list(excluded_coloniesH)
    names(neighborful_excluded_colonies_ls_H)[file_number] <- infile

    if(is_draw){
      # Draw plate map
      infile_orig <- infile

      variations <- c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H')

      variation_counter <- 1

      for(i in variations){
        infile <- paste(infile_orig, '_variation-', i, sep = '')

        excluded_colonies <- eval(parse(text = paste('excluded_colonies',
                                                     i,
                                                     sep = '')))
        plot.platemap(infile,
                      bioneer_samples_1536,
                      variation_count,
                      PDF_OUTFILE,
                      excluded_colonies)

        variation_counter <- variation_counter + 1
      }

    }
    file_number <- file_number + 1
  }

  #save list "neighborful_excluded_colonies_ls" as RData file
  save(neighborful_excluded_colonies_ls_A, file = paste(R_LIST_OUTFILE, '_excluded_colonies_A', '.RData', sep = ''), ascii = FALSE)

  save(neighborful_excluded_colonies_ls_B, file = paste(R_LIST_OUTFILE, '_excluded_colonies_B', '.RData', sep = ''), ascii = FALSE)

  save(neighborful_excluded_colonies_ls_C, file = paste(R_LIST_OUTFILE, '_excluded_colonies_C', '.RData', sep = ''), ascii = FALSE)

  save(neighborful_excluded_colonies_ls_D, file = paste(R_LIST_OUTFILE, '_excluded_colonies_D', '.RData', sep = ''), ascii = FALSE)

  save(neighborful_excluded_colonies_ls_E, file = paste(R_LIST_OUTFILE, '_excluded_colonies_E', '.RData', sep = ''), ascii = FALSE)

  save(neighborful_excluded_colonies_ls_F, file = paste(R_LIST_OUTFILE, '_excluded_colonies_F', '.RData', sep = ''), ascii = FALSE)

  save(neighborful_excluded_colonies_ls_G, file = paste(R_LIST_OUTFILE, '_excluded_colonies_G', '.RData', sep = ''), ascii = FALSE)

  save(neighborful_excluded_colonies_ls_H, file = paste(R_LIST_OUTFILE, '_excluded_colonies_H', '.RData', sep = ''), ascii = FALSE)

}
