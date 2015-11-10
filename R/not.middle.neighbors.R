not.middle.neighbors <- function(plate, colony)
{
  # Top left corner: Eg: 1
  if (colony == 1) {
    neighbors_selected_colony = topleft.corner.neighbors(plate)
    # bottom left corner: Eg: 32
  } else if (colony == plate.nrow(plate)) {
    neighbors_selected_colony = bottomleft.corner.neighbors(plate)
    # top right corner: Eg: 1505
  } else if (colony == (plate - plate.nrow(plate) + 1)) {
    neighbors_selected_colony = topright.corner.neighbors(plate)
    # bottom right corner: Eg: 1536
  } else if (colony == plate) {
    neighbors_selected_colony = bottomright.corner.neighbors(plate)
    # top line  neigbours
  } else if (colony %in% top.line.indices(plate)) {
    neighbors_selected_colony = top.line.neighbors(plate, colony)
    # bottom line  neigbours
  } else if (colony %in% bottom.line.indices(plate)) {
    neighbors_selected_colony = bottom.line.neighbors(plate, colony)
    # left line  neigbours
  } else if (colony %in% left.line.indices(plate)) {
    neighbors_selected_colony = left.line.neighbors(plate, colony)
    # right line neigbours
  } else if (colony %in% right.line.indices(plate)) {
    neighbors_selected_colony = right.line.neighbors(plate, colony)
  }
  return(neighbors_selected_colony)
}
