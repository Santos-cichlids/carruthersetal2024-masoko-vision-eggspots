# carruthersetal2024-masoko-vision-eggspots

This repository contains the scripts used to perform colour calibration and anal fin colour measurements in photographs from A. calliptera 'masoko'. In the example_dataset you will find examples of input files that you can use with the code below. 

In this repository you can also find the bootstrap code used to account for the batch effects seen in egg-spot and control colours (see methods).

**Below is the code to run the colour calibration and colour measurements**

```
#This step runs the colour calibration
test.calib = calibrateLab (roidir="~/Desktop/cielab_masoko_colourcalibration/D31B09/roidir", 
                           photodir="~/Desktop/cielab_masoko_colourcalibration/D31B09", 
                           sampleList="~/Desktop/cielab_masoko_colourcalibration/D31B09/photo_list.txt", 
                           truefile= "~/Desktop/cielab_masoko_colourcalibration/CTrax_lab.txt", 
                           colornames=1:24, 
                           plotFit="~/Desktop/cielab_masoko_colourcalibration/D31B09/D31B09")

write.table(test.calib[[1]], file = "~/Desktop/cielab_masoko_colourcalibration/D31B09/D31B09_parameters.txt", sep = "\t" , row.names = FALSE)
write.table(test.calib[[2]], file = "~/Desktop/cielab_masoko_colourcalibration/D31B09/D31B09_error_AFTERcorrection.txt", sep = "\t", row.names = FALSE )
write.table(test.calib[[3]], file = "~/Desktop/cielab_masoko_colourcalibration/D31B09/D31B09_error_BEFOREcorrection.txt", sep = "\t" , row.names = FALSE)
write.table(test.calib[[2]], file = "~/Desktop/cielab_masoko_colourcalibration/D31B09/D31B09_error_pvalues.txt", sep = "\t", row.names = FALSE )

#This step measures CIELAB values for the control colour
D31B09_control = roiLab(pngimage="~/Desktop/cielab_masoko_colourcalibration/D31B09/D31B09_control.png", 
                        sampleid = "D31B09", calibrate=test.calib[[1]])
write.table(D31B09_control, file = "~/Desktop/cielab_masoko_colourcalibration/D31B09/D31B09_control.txt", sep = "\t", row.names = FALSE )

#This step measures CIELAB values for the egg-spots
D31B09_spot = roiLab(pngimage="~/Desktop/cielab_masoko_colourcalibration/D31B09/D31B09_spot.png", 
                        sampleid = "D31B09", calibrate=test.calib[[1]])
write.table(D31B09_spot, file = "~/Desktop/cielab_masoko_colourcalibration/D31B09/D31B09_spot.txt", sep = "\t", row.names = FALSE )

#This step measures CIELAB values for the proximal black regions of the anal fin
D31B09_black = roiLab(pngimage="~/Desktop/cielab_masoko_colourcalibration/D31B09/D31B09_black.png", 
                        sampleid = "D31B09", calibrate=test.calib[[1]])
write.table(D31B09_black, file = "~/Desktop/cielab_masoko_colourcalibration/D31B09/D31B09_black.txt", sep = "\t", row.names = FALSE )

#This step measures CIELAB values for the distal red rim of the anal fin
D31B09_red = roiLab(pngimage="~/Desktop/cielab_masoko_colourcalibration/D31B09/D31B09_red.png", 
                     sampleid = "D31B09", calibrate=test.calib[[1]])
write.table(D31B09_red, file = "~/Desktop/cielab_masoko_colourcalibration/D31B09/D31B09_red.txt", sep = "\t", row.names = FALSE ) ```
