test_that('sdsdfas',
          {
            expect_equal(nchar('ca'), 2)
          })
# unit testing convert_down_across
plateformat <- 96
base_data <- 1:plateformat
# across->down
data_1 <- convert_down_across(plateformat = plateformat,
                              data_from = base_data,
                              is_plate_coords = TRUE,
                              in_data_flow = 'across',
                              out_data_flow = 'down')

data_2 <- convert_down_across(plateformat = plateformat,
                              data_from = data_1[,3],
                              is_plate_coords = TRUE,
                              in_data_flow = 'down',
                              out_data_flow = 'across')

result <- comp_2_df(data_1, data_2)
# result[which(result[,3] == 'pass'), ]
result[which(result[,3] == 'fail'), ]

# down->across
data_1 <- convert_down_across(plateformat = plateformat,
                              data_from = base_data,
                              is_plate_coords = TRUE,
                              in_data_flow = 'down',
                              out_data_flow = 'across')

data_2 <- convert_down_across(plateformat = plateformat,
                              data_from = data_1[,3],
                              is_plate_coords = TRUE,
                              in_data_flow = 'across',
                              out_data_flow = 'down')

result <- comp_2_df(data_1, data_2)
# result[which(result[,3] == 'pass'), ]
result[which(result[,3] == 'fail'), ]

# down->down
data_1 <- convert_down_across(plateformat = plateformat,
                              data_from = base_data,
                              is_plate_coords = TRUE,
                              in_data_flow = 'down',
                              out_data_flow = 'down')

data_2 <- convert_down_across(plateformat = plateformat,
                              data_from = data_1[,3],
                              is_plate_coords = TRUE,
                              in_data_flow = 'down',
                              out_data_flow = 'down')
data_2[,3] <- 1:plateformat

result <- comp_2_df(data_1, data_2)
# result[which(result[,3] == 'pass'), ]
result[which(result[,3] == 'fail'), ]

# across->across
data_1 <- convert_down_across(plateformat = plateformat,
                              data_from = base_data,
                              is_plate_coords = TRUE,
                              in_data_flow = 'across',
                              out_data_flow = 'across')

data_2 <- convert_down_across(plateformat = plateformat,
                              data_from = data_1[,3],
                              is_plate_coords = TRUE,
                              in_data_flow = 'across',
                              out_data_flow = 'across')

data_2[,3] <- 1:plateformat

result <- comp_2_df(data_1, data_2)
# result[which(result[,3] == 'pass'), ]
result[which(result[,3] == 'fail'), ]
