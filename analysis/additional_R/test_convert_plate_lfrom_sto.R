# unit testing convert_plate_lfrom_sto
plate_from <- 384
plate_to <- 96
# base_data <- rnorm(plate_from)
base_data <- 1:plate_from

# across->down
data_1 <- convert_plate_lfrom_sto(plate_from      = plate_from,
                                  plate_to        = plate_to,
                                  data_from       = base_data,
                                  is_plate_coords = TRUE,
                                  in_data_flow    = 'down',
                                  out_data_flow   = 'across')

data_2 <- convert_plate_lfrom_sto(plate_from      = plate_from,
                                  plate_to        = plate_to,
                                  data_from       = base_data,
                                  is_plate_coords = TRUE,
                                  in_data_flow    = 'down',
                                  out_data_flow   = 'down')

result <- comp_2_df(data_1[,1:3], data_2[,1:3])
# result[which(result[,3] == 'pass'), ]
result[which(result[,3] == 'fail'), ]

result <- comp_2_df(data_1[,c(1,2,4)], data_2[,c(1,2,4)])
# result[which(result[,3] == 'pass'), ]
result[which(result[,3] == 'fail'), ]

result <- comp_2_df(data_1[,c(1,2,5)], data_2[,c(1,2,5)])
# result[which(result[,3] == 'pass'), ]
result[which(result[,3] == 'fail'), ]

result <- comp_2_df(data_1[,c(1,2,6)], data_2[,c(1,2,6)])
# result[which(result[,3] == 'pass'), ]
result[which(result[,3] == 'fail'), ]
