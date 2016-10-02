get_gtree_table <- function(data_sens_spec, row_names, title_text, footnote_text='')
{
  row.names(data_sens_spec) <- row_names

  tgrob = tableGrob(data_sens_spec)

  h = grobHeight(tgrob)

  w = grobWidth(tgrob)

  title = textGrob(title_text,
                   y = (unit(0.5,"npc") + (0.5 * h)),
                   vjust = 0,
                   gp = gpar(fontsize=20))

  footnote <- textGrob(footnote_text,
                       x = (unit(0.5,"npc") - (0.5 * w)),
                       y = (unit(0.5,"npc") - (0.5 * h)),
                       vjust = 1,
                       hjust = 0,
                       gp = gpar( fontface="italic"))

  return(gTree(children=gList(tgrob, title, footnote)))
}
