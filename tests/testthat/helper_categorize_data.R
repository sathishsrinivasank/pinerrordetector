k11 <- colonyarea$data_subtypes

k12 <- convert_small_to_large(plate_from = 384,
                              plate_to = 1536,
                              data_from = k11,
                              in_data_flow = 'across',
                              out_data_flow = "across",
                              is_plate_coords = FALSE)

k13 <- simulated_data_1536(data_384 = k11,
                           in_data_flow = "across",
                           out_data_flow = "across",
                           is_plate_coords = FALSE)

k14 <- which(convert_small_to_large(plate_from = 384,
                                    plate_to = 1536,
                                    data_from = k11,
                                    in_data_flow = 'across',
                                    out_data_flow = "across",
                                    is_plate_coords = FALSE)$y %in% 'Empty')

k15 <- categorize_data(data_area = k13$y, empty_indices = k14)






