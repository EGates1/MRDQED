# MRDQED
A Magnetic Resonance Data Quality Evaluation Dashboard

This shiny dashboard is meant to help facilitate fast, efficient review of MRI data sets and associated annotations. Data sets containing standard MR sequences and a small number of annotations can be easily loaded via a csv file interface.

## Setup
To use the app, you need to supply a comma separated value (csv) file of file paths to the images in NifTI format. Save this file as appdata.csv in the top level (same as `app.R`). File paths are assumed to be relative to the same directory. appdata.csv at minimum needs a column labeled ptid (subject id).

Optimally, include image data columns  T1, T2, T1CE, FLAIR, and FUNC. FUNCtional data gives to option to provide ADC or CBV in addition to the four standard image contrasts.

For annotations, supply columns MASK (for example the brain), SEG for substructure segmentation, and ROI2 for an optional second segmentation file.

Any other columns in appdata.csv will be available to view in tabular form.

For example, the first two rows of appdata.csv for the MICCAI Brain Tumor Segmentation Challenge will look like:

>ptid,type,Age,Survival,ResectionStatus,T1,T2,T1CE,FLAIR,MASK,SEG,ROI2
>Brats18_2013_20_1,HGG,NA,NA,NA,sampledata/Brats18_2013_20_1_t1.nii.gz,sampledata/Brats18_2013_20_1_t2.nii.gz,sampledata/Brats18_2013_20_1_t1ce.nii.gz,sampledata/Brats18_2013_20_1_flair.nii.gz,sampledata/Brats18_2013_20_1_mask.nii.gz,sampledata/Brats18_2013_20_1_seg.nii.gz,sampledata/Brats18_2013_20_1_csf.nii.gz

After supplying the paths, pre-rendered pngs can be made by calling

`source("generate_pics.R")` from within R or `Rscript generate_pics.R` from command line. The pngs will save to the [pngs/](pngs) folder. The advantage of pre-rendering pngs is faster loading time since the full 3D data doesn't need to be loaded for every case. A disadvantage is that if the underlying image data changes, those changes will NOT be reflected in the dashboard until new pngs are made.

Once pngs are rendered, launch the app using `shiny::runApp()`


### Dependencies
R version 3.5.1 or newer should run all the necessary packages. The following R packages are needed:
```
library(shiny)
library(DT)
library(ggplot2)
library(png)
library(grid)
library(gridExtra)
library(oro.nifti)
```