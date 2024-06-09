# carruthersetal2024-masoko-vision-eggspots
Script used to perform colour calibration and anal fin colour measurements in photographs from A. calliptera 'masoko' 

**instructions -> work in progress**

```
test.calib = calibrateLab (roidir="~/Desktop/cielab_masoko_colourcalibration/D31J03/roidir", 
                           photodir="~/Desktop/cielab_masoko_colourcalibration/D31J03", 
                           sampleList="~/Desktop/cielab_masoko_colourcalibration/D31J03/photo_list.txt", 
                           truefile= "~/Desktop/cielab_masoko_colourcalibration/CTrax_lab.txt", 
                           colornames=1:24, 
                           plotFit="~/Desktop/cielab_masoko_colourcalibration/D31J03/D31J03")

write.table(test.calib[[1]], file = "~/Desktop/cielab_masoko_colourcalibration/D31J03/D31J03_parameters.txt", sep = "\t" , row.names = FALSE)
write.table(test.calib[[2]], file = "~/Desktop/cielab_masoko_colourcalibration/D31J03/D31J03_error_AFTERcorrection.txt", sep = "\t", row.names = FALSE )
write.table(test.calib[[3]], file = "~/Desktop/cielab_masoko_colourcalibration/D31J03/D31J03_error_BEFOREcorrection.txt", sep = "\t" , row.names = FALSE)
write.table(test.calib[[2]], file = "~/Desktop/cielab_masoko_colourcalibration/D31J03/D31J03_error_pvalues.txt", sep = "\t", row.names = FALSE )

D31J03_control = roiLab(pngimage="~/Desktop/cielab_masoko_colourcalibration/D31J03/D31J03_control.png", 
                        sampleid = "D31J03", calibrate=test.calib[[1]])
write.table(D31J03_control, file = "~/Desktop/cielab_masoko_colourcalibration/D31J03/D31J03_control.txt", sep = "\t", row.names = FALSE )


D31J03_spot = roiLab(pngimage="~/Desktop/cielab_masoko_colourcalibration/D31J03/D31J03_spot.png", 
                        sampleid = "D31J03", calibrate=test.calib[[1]])
write.table(D31J03_spot, file = "~/Desktop/cielab_masoko_colourcalibration/D31J03/D31J03_spot.txt", sep = "\t", row.names = FALSE )


D31J03_black = roiLab(pngimage="~/Desktop/cielab_masoko_colourcalibration/D31J03/D31J03_black.png", 
                        sampleid = "D31J03", calibrate=test.calib[[1]])
write.table(D31J03_black, file = "~/Desktop/cielab_masoko_colourcalibration/D31J03/D31J03_black.txt", sep = "\t", row.names = FALSE )


D31J03_red = roiLab(pngimage="~/Desktop/cielab_masoko_colourcalibration/D31J03/D31J03_red.png", 
                     sampleid = "D31J03", calibrate=test.calib[[1]])
write.table(D31J03_red, file = "~/Desktop/cielab_masoko_colourcalibration/D31J03/D31J03_red.txt", sep = "\t", row.names = FALSE ) ```
