# across
e1 <- plate_coords(96, 1:96, "across")
e2 <- structure(list(row = c("C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C",
                             "D", "D", "D", "D", "D", "D", "D", "D", "D", "D", "D", "D",
                             "E", "E"),
                     column = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 1L,
                                2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 1L, 2L),
                     y = 25:50), .Names = c("row", "column", "y"),
                row.names = 25:50,
                class = "data.frame")


# down
f1 <- plate_coords(96, data.frame(y = 1:96), "down")
f2 <- structure(list(row = c("A", "B", "C", "D", "E", "F", "G", "H",
                             "A", "B", "C", "D", "E", "F", "G", "H",
                             "A", "B", "C", "D", "E", "F", "G", "H",
                             "A", "B"),
                     column = c(4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 5L,
                                5L, 5L, 5L, 5L, 5L, 5L, 5L, 6L, 6L,
                                6L, 6L, 6L, 6L, 6L, 6L, 7L, 7L),
                     y = 25:50),
                .Names = c("row", "column", "y"),
                row.names = 25:50,
                class = "data.frame")

