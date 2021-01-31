# Rendering pngs may take a while when done in series (loop)
# You may want to call the individual functions in separate sessions/jobs
# to speed up the process.

datadir = '.'
pngbasedir = file.path(datadir,"pngs/")
dataset <- read.csv(file.path(datadir,"appdata.csv"),stringsAsFactors = F)

source("png_shots.R")
for(ii in 1:nrow(dataset)){
  ptid = dataset$ptid[ii]
  images = intersect(c("T1","T2","T1CE","FLAIR","FUNC"),names(dataset))
  masks = intersect(c("MASK","SEG","ROI2"),names(dataset))
  for(img in images){
    ROI_vis(dataset[ii,img], masklist=unlist(dataset[ii,masks]),
            pngname=file.path(pngbasedir,sprintf("%s_%s.png",ptid,img)),
            tol=0.01, zmax=NULL)
    for(msk in masks){
      max_shot(image=dataset[ii,img],mask=dataset[ii,msk],
               pngname = file.path(pngbasedir,sprintf("%s_%s_%s.png",ptid,img,msk)))
    }
  }
}
