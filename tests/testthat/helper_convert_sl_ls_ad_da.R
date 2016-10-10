# convert_small_to_large  | convert_large_to_small | convert_down_across
i01 <- plate_coords(plate_to = 1536,
                    data_from = data.frame(y = 1:1536),
                    data_format = "across")

i02 <- plate_coords(plate_to = 1536,
                    data_from = data.frame(y = 1:1536),
                    data_format = "down")

# across to across
i11 <- convert_large_to_small(plate_from = 1536,
                              plate_to = 384,
                              data_from = data.frame(y = 1:1536),
                              in_data_flow = 'across',
                              out_data_flow = 'across',
                              is_plate_coords = FALSE)

i11 <- convert_small_to_large(plate_from = 384,
                              plate_to = 1536,
                              data_from = i11,
                              in_data_flow = 'across',
                              out_data_flow = 'across',
                              is_plate_coords = TRUE)

# down to down
i21 <- convert_large_to_small(plate_from = 1536,
                              plate_to = 384,
                              data_from = data.frame(y = 1:1536),
                              in_data_flow = 'down',
                              out_data_flow = 'down',
                              is_plate_coords = FALSE)

i21 <- convert_small_to_large(plate_from = 384,
                              plate_to = 1536,
                              data_from = i21,
                              in_data_flow = 'down',
                              out_data_flow = 'down',
                              is_plate_coords = TRUE)


# across to down
i31 <- convert_large_to_small(plate_from = 1536,
                              plate_to = 384,
                              data_from = data.frame(y = 1:1536),
                              in_data_flow = 'across',
                              out_data_flow = 'down',
                              is_plate_coords = FALSE)


i31 <- convert_down_across(plateformat = 384,
                           data_from = i31,
                           is_plate_coords = FALSE,
                           in_data_flow = 'down',
                           out_data_flow = 'across')


i31 <- convert_small_to_large(plate_from = 384,
                              plate_to = 1536,
                              data_from = i31,
                              in_data_flow = 'across',
                              out_data_flow = 'across',
                              is_plate_coords = TRUE)


# down to across
i41 <- convert_large_to_small(plate_from = 1536,
                              plate_to = 384,
                              data_from = data.frame(y = 1:1536),
                              in_data_flow = 'down',
                              out_data_flow = 'across',
                              is_plate_coords = FALSE)


i41 <- convert_down_across(plateformat = 384,
                           data_from = i41,
                           is_plate_coords = FALSE,
                           in_data_flow = 'across',
                           out_data_flow = 'down')

i41 <- convert_small_to_large(plate_from = 384,
                              plate_to = 1536,
                              data_from = i41,
                              in_data_flow = 'down',
                              out_data_flow = 'down',
                              is_plate_coords = TRUE)



