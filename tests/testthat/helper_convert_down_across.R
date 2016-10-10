# Data for 'convert_down_across'

# across to down
a1 <- convert_down_across(plateformat = 384,
                          data_from = 1:384,
                          is_plate_coords = TRUE,
                          in_data_flow = 'across',
                          out_data_flow = 'down')

a2 <- structure(list(row = c("I", "J", "K", "L", "M", "N", "O", "P", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J",
                             "K", "L", "M", "N", "O", "P", "A", "B"),
                     column = c(2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
                                3L, 3L, 4L, 4L),
                     y = c(194L, 218L, 242L, 266L, 290L, 314L, 338L, 362L, 3L, 27L, 51L,75L, 99L, 123L, 147L, 171L,
                           195L, 219L, 243L, 267L, 291L, 315L,339L, 363L, 4L, 28L)),
                .Names = c("row", "column", "y"),
                row.names = c(194L, 218L, 242L, 266L, 290L, 314L, 338L, 362L, 3L, 27L, 51L, 75L, 99L, 123L, 147L, 171L,
                              195L, 219L, 243L, 267L, 291L, 315L, 339L, 363L, 4L, 28L),
                class = "data.frame")

# vector and data frame formats of data_from
a11  <- convert_down_across(plateformat = 384,
                            data_from = 1:384,
                            is_plate_coords = TRUE,
                            in_data_flow = 'across',
                            out_data_flow = 'down')

a12 <- convert_down_across(plateformat = 384,
                           data_from = data.frame(y = 1:384),
                           is_plate_coords = TRUE,
                           in_data_flow = 'across',
                           out_data_flow = 'down')


# down to across
b1 <- convert_down_across(plateformat = 384,
                          data_from = data.frame(y = 1:384),
                          is_plate_coords = TRUE,
                          in_data_flow = 'down',
                          out_data_flow = 'across')

b2 <- structure(list(row = c("B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B",
                             "B", "B", "B", "B", "B", "B", "C", "C"),
                     column = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L,
                                20L, 21L, 22L, 23L, 24L, 1L, 2L),
                     y = c(2L, 18L, 34L, 50L, 66L, 82L, 98L, 114L, 130L, 146L, 162L, 178L, 194L, 210L, 226L, 242L,
                           258L, 274L, 290L, 306L, 322L, 338L, 354L, 370L, 3L, 19L)),
                .Names = c("row", "column", "y"),
                row.names = c(2L, 18L, 34L, 50L, 66L, 82L, 98L, 114L, 130L, 146L, 162L, 178L, 194L, 210L, 226L, 242L,
                              258L, 274L, 290L, 306L, 322L, 338L, 354L, 370L, 3L, 19L),
                class = "data.frame")

# across to across
c1 <- convert_down_across(plateformat = 384,
                          data_from = 1:384,
                          is_plate_coords = TRUE,
                          in_data_flow = 'across',
                          out_data_flow = 'across')

c2 <- structure(list(row = c("B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B",
                             "B", "B", "B", "B", "B", "B", "B", "C", "C"),
                     column = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L,
                                19L, 20L, 21L, 22L, 23L, 24L, 1L, 2L),
                     y = 25:50),
                .Names = c("row", "column", "y"),
                row.names = 25:50,
                class = "data.frame")

# No Plate Coordinates
d1 <- convert_down_across(plateformat = 384,
                          data_from = 1:384,
                          is_plate_coords = FALSE,
                          in_data_flow = 'down',
                          out_data_flow = 'across')

d2 <- structure(list(y = c(2L, 18L, 34L, 50L, 66L, 82L, 98L, 114L, 130L, 146L, 162L, 178L, 194L, 210L, 226L, 242L,
                           258L, 274L, 290L, 306L, 322L, 338L, 354L, 370L, 3L, 19L)),
                .Names = "y",
                row.names = c(2L, 18L, 34L, 50L, 66L, 82L, 98L, 114L, 130L, 146L, 162L, 178L, 194L, 210L, 226L,
                              242L, 258L, 274L, 290L, 306L, 322L, 338L, 354L, 370L, 3L, 19L),
                class = "data.frame")

