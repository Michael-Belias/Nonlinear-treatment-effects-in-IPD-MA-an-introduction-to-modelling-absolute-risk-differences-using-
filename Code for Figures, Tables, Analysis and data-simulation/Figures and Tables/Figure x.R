#### Figure 3






grid.arrange(arrangeGrob(point.wise.DF.RCS.HT.plot, point.wise.DF.BS.HT.plot,point.wise.DF.PS.HT.plot,point.wise.DF.SS.HT.plot),
             bottom = textGrob(label = expression(paste("BMI ", (Kg/m^2))), rot = 0, vjust = 0,hjust= 0.25,gp = gpar(fontsize=32)),
             legend,ncol = 1, heights = c(5,1),left = textGrob(label = "Mortality risk", rot = 90, vjust = 1,hjust= 0,gp = gpar(fontsize=32)))


