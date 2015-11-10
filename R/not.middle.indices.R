# @method not.middle.indices(plate)
# @param plate int type of plate format (Eg: 1536)
# @return numeric vector indices of colonies other than middle colonies in the plate
not.middle.indices <-  function(plate)
{
  return(sort(c((right.line.indices(plate)),
                (left.line.indices(plate)),
                (top.line.indices(plate)),
                (bottom.line.indices(plate)),
                1,
                (plate.nrow(plate)),
                (plate - (plate.nrow(plate)) + 1),
                plate)))
}
