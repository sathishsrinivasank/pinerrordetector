# workmap for exclusion based algorithm

# 1. load pinerrordetector package and source all function scripts for this analysis
# require('pinerrordetector')

setwd('c:/Users/sathish/Desktop/pinerrordetector/analysis')
source('./dir_setup.R')
source('./categorize_data.R')
source('./exclusion_algorithm.R')
source('./sen_spec_ppv_npv_prev.R')
source('./get_gtree_table.R')
source('./sen_spec_ppv_npv_prev_table_draw.R')
source('./sens_spec_raw_count_table_draw.R')
source('./xyplot_pred_measures.R')
source('./draw_platemap.R')

# 2. get excluded_colonies
plateformat <- 1536

colonyarea <- read.table('C:\\Users\\sathish\\Desktop\\pinerrordetector\\data-raw\\colonyarea2.txt',
                         sep='\t',
                         header=TRUE,
                         stringsAsFactors = FALSE)

data_area <- simulated_data_1536(colonyarea$data_subtypes)

empty_indices <- which(convert_384_to_1536_data(colonyarea$data_subtypes)
                       %in% 'Empty')
is_neighborful <- TRUE

is_save <- TRUE

if(exclusion_algorithm(plateformat    = plateformat,
                       data_area      = data_area,
                       empty_indices  = empty_indices,
                       is_neighborful = is_neighborful,
                       is_save        = is_save)){

  # 3. get predictive measures
  pe_category <- 'pe'

  not_pe_category <- c('empty', 'lessig')

  categorized_data <- categorize_data(data_area, empty_indices)

  if(sen_spec_ppv_npv_prev(categorized_data = categorized_data,
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

    if(sens_spec_raw_count_table_draw(row_names_titles = row_names_titles,
                                   is_neighborful = is_neighborful)){
      if(sen_spec_ppv_npv_prev_table_draw(row_names_titles = row_names_titles,
                                       is_neighborful = is_neighborful)){
        # 5. draw lattice xyplot of predictive measures
        require(lattice)
        require(latticeExtra)

        if(xyplot_pred_measures(is_neighborful = is_neighborful)){
          # 6. draw platemaps
          categorized_data[which(categorized_data %in% 'empty')
                           ] <- 'Empty'

          categorized_data[which(categorized_data %in% 'pe')
                           ]   <- 'Pinning Error'

          categorized_data[which(categorized_data %in% 'more')
                           ] <- 'Morethan Plate Median'

          categorized_data[which(categorized_data %in% 'less')
                           ] <- 'Lessthan Plate Median'

          categorized_data[which(categorized_data %in% 'moresig')
                           ] <- 'Morethan 90% Plate Median'

          categorized_data[which(categorized_data %in% 'lessig')
                           ] <- 'Lessthan 25% Plate Median'

          legend_txt_bg_col <- c('Empty'                     = 'red',
                                 'Pinning Error'             = 'black',
                                 'Morethan Plate Median'     = '#660066',
                                 'Lessthan Plate Median'     = 'green',
                                 'Morethan 90% Plate Median' = 'cyan',
                                 'Lessthan 25% Plate Median' = 'yellow',
                                 'Excluded Colonies'         = 'blue')

          draw_platemap(plateformat       = plateformat,
                        categorized_data  = categorized_data,
                        legend_txt_bg_col = legend_txt_bg_col,
                        data_flow         = 'down',
                        symbol_size       = rep(0.4, plateformat),
                        is_neighborful    = is_neighborful)

        }
      }
    }
  }
}




