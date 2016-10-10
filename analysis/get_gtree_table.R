get_gtree_table <- function(data_sens_spec, row_names, title_text)
{
  row.names(data_sens_spec) <- row_names

  table = tableGrob(data_sens_spec)

  title = textGrob(title_text,
                   gp = gpar(fontsize=20))

  padding <- unit(8,"mm")
  
  table <- gtable_add_rows(x = table, 
                           heights = grobHeight(title) + padding,
                           pos = 0)
  table <- gtable_add_grob(x = table,
                           grobs = title, 
                           t = 1,
                           l = 2,
                           b = 0.5,
                           r = ncol(table))
  
  return(gTree(children=gList(table)))
}
