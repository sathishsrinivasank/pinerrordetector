# workmap for exclusion based algorithm

# 1. load pinerrordetector package and source all function scripts for this analysis
require('pinerrordetector')

setwd('c:/Users/sathish/Desktop/pinerrordetector/analysis')
source('./dir_setup.R')
source('./exclusion_algorithm.R')
source('./sen_spec_ppv_npv_prev.R')
source('./get_gtree_table.R')
source('./sen_spec_ppv_npv_prev_table_draw.R')
source('./sens_spec_raw_count_table_draw.R')
source('./xyplot_pred_measures.R')
source('./draw_platemap.R')

# 2. get excluded_colonies
plateformat <- 1536

data_subtypes_384 <- colonyarea$data_subtypes

in_data_flow = "across"

out_data_flow = "down"

data_area <- simulated_data_1536(data_384 = data_subtypes_384,
                                 in_data_flow = in_data_flow,
                                 out_data_flow = out_data_flow,
                                 is_plate_coords = FALSE)

empty_indices <- as.numeric(which(convert_small_to_large(plate_from = 384,
                                                         plate_to = 1536,
                                                         data_from = data_subtypes_384,
                                                         in_data_flow = in_data_flow,
                                                         out_data_flow = out_data_flow,
                                                         is_plate_coords = FALSE)$y %in% 'Empty'))
is_neighborful <- TRUE
is_save <- TRUE

if(exclusion_algorithm(plateformat    = plateformat,
                       data_area      = data_area$y,
                       empty_indices  = empty_indices,
                       is_neighborful = is_neighborful,
                       is_save        = is_save)){

  # 3. get predictive measures
  vec1 <- categorize_data(data_area$y, empty_indices)
  
  pe_category <- "Pinning Error"
  not_pe_category <- c("Empty" , "Lessthan 25% Plate Median")
  
  if(sen_spec_ppv_npv_prev(categorized_data = vec1,
                           pe_category      = pe_category,
                           not_pe_category  = not_pe_category,
                           empty_indices    = empty_indices,
                           is_neighborful   = is_neighborful)){

    # 4. draw tables of predictive measures
    row_names_titles <- c('Neighborful Algorithm A',
                          'Neighborful Algorithm B',
                          'Neighborful Algorithm C',
                          'Neighborful Algorithm D',
                          'Neighborful Algorithm E',
                          'Neighborful Algorithm F',
                          'Neighborful Algorithm G',
                          'Neighborful Algorithm H',
                          'Neighborless Algorithm')
    require(grid)
    require(gridExtra)
    require(gtable)

      if(sens_spec_raw_count_table_draw(row_names_titles = row_names_titles,
                                        is_neighborful = is_neighborful)){
      if(sen_spec_ppv_npv_prev_table_draw(row_names_titles = row_names_titles,
                                          is_neighborful = is_neighborful)){
        # 5. draw lattice xyplot of predictive measures
        require(lattice)
        require(latticeExtra)

        if(xyplot_pred_measures(is_neighborful = is_neighborful)){
        # 6. draw platemaps
          legend_txt_bg_col <- c('Empty'                     = 'red',
                                 'Pinning Error'             = 'black',
                                 'Morethan Plate Median'     = '#660066',
                                 'Lessthan Plate Median'     = 'green',
                                 'Morethan 90% Plate Median' = 'cyan',
                                 'Lessthan 25% Plate Median' = 'yellow',
                                 'Excluded Colonies'         = 'blue')

          draw_platemap(plateformat       = plateformat,
                        categorized_data  = vec1,
                        legend_txt_bg_col = legend_txt_bg_col,
                        data_flow         = out_data_flow,
                        symbol_size       = rep(0.4, plateformat),
                        is_neighborful    = is_neighborful)

        }
      }
    }
  }
}




