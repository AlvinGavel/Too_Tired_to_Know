# Modified version of code originally from https://stackoverflow.com/a/45614547/11890275, shared under CC BY-SA 4.0

GeomSplitViolin <- ggproto("GeomSplitViolin",
                           GeomViolin, 
                           draw_group = function(self, data, ...) {
   data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
   grp <- data[1, "group"]
   newdata <- plyr::arrange(transform(data,
                                      x = if (grp %% 2 == 1) xminv else xmaxv),
                            if (grp %% 2 == 1) y else -y)
   newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
   newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
                             
   ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
})

geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., 
                              draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, 
                              show.legend = NA, inherit.aes = TRUE) {
   layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, 
      position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
      params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}