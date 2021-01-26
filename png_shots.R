library(oro.nifti)

############################################################################
## image_wcrosshair() save an image of the slice through the given point
## and indicates the point with a crosshair.
############################################################################

image_wcrosshair <- function(x, max_xyz,
                             plane=c("axial","sagittal","coronal"),
                             col.crosshair="green", hotmax=NA_real_, ...){
  X <- nrow(x)
  Y <- ncol(x)
  Z <- nsli(x)
  col=gray(0:64/64)
  zlim <- c(x@"cal_min", x@"cal_max")
  if(!is.na(hotmax)){
    col = hotmetal()
    zlim <- c(0,hotmax)
  }
  breaks <- c(min(x, zlim, na.rm=TRUE),
              seq(min(zlim, na.rm=TRUE), max(zlim, na.rm=TRUE),
                  length=length(col)-1),
              max(x, zlim, na.rm=TRUE))
  par(mfrow=c(1,1), oma=rep(0,4), mar=rep(0,4), bg="black")
  switch(plane[1],
         "axial" = {
           aspect <- x@pixdim[3] / x@pixdim[2]
           graphics::image(1:X, 1:Y, x[,,max_xyz[3]], col=col, breaks=breaks,
                           asp=aspect, axes=FALSE, xlab="", ylab="")
           abline(h=max_xyz[2], v=max_xyz[1], col=col.crosshair, asp=aspect, lwd=2)
         },
         "sagittal" = {
           aspect <- x@pixdim[4] / x@pixdim[3]
           graphics::image(1:Y, 1:Z, x[max_xyz[1],,], col=col, breaks=breaks,
                           asp=aspect, axes=FALSE, xlab="", ylab="")
           abline(h=max_xyz[3], v=max_xyz[2], col=col.crosshair, asp=aspect, lwd=2)
         },
         "coronal" = {
           aspect <- x@pixdim[4] / x@pixdim[2]
           graphics::image(1:X, 1:Z, x[,max_xyz[2],], col=col, breaks=breaks,
                           asp=aspect, axes=FALSE, xlab="", ylab="")
           abline(h=max_xyz[3], v=max_xyz[1], col=col.crosshair, asp=aspect, lwd=2)
         })
}

############################################################################
## max_shot() finds the maximum intensity within the mask and plots 3-planes
## hotmask uses a hot colormap instead of grayscale
############################################################################

max_shot <- function(image, mask, pngname, hotmax=NA_real_){
  if(!file.exists(image)){
    stop("Input image not found, cannot make pngs")
  } else if(!file.exists(mask)){
    stop("Input mask not found, cannot make pngs")
  }
  image_nii_reor = readNIfTI(image, reorient=TRUE)
  mask_nii_reor = readNIfTI(mask, reorient=TRUE)
  
  # use the first max, not sure what to do if ties
  max_ind = which(mask_nii_reor !=0 &
                    image_nii_reor==max(image_nii_reor[mask_nii_reor!=0]),
                  arr.ind=T)[1,]
  
  for(pl in c("axial","sagittal","coronal")){
    # TODO: make png name an arg
    # plot cross-sections at maximum intensity
    
    outpngname = sub(".png$", paste0("_",substr(pl,1,3),".png",sep=""), pngname)
    print(paste("saving image:",outpngname))
    pixwidth = max(dim(image_nii_reor))
    png(filename=outpngname,width = pixwidth, height = pixwidth,units="px")
    image_wcrosshair(image_nii_reor,max_xyz=c(max_ind),plane=pl,hotmax=hotmax)
    dev.off()
  }
}

############################################################################
## ROI_vis() aggregates several integer-valued masks and overlay's them on a
## on an image, taking snapshots at the planes of maximum area.
## doing all them at once allows consistent color
## specifying a tolerance plots densities (histograms) as well.
############################################################################
ROI_vis <- function(image, masklist, pngname, tol=0, zmax=NULL){
  if(!file.exists(image)){
    stop("Input image not found, cannot make pngs")
  } else if(!all(sapply(masklist,file.exists))){
    stop("None of the mask files exist, cannot make pngs")
  }
  
  print(paste("Reading: ",image))
  img = readNIfTI(image, reorient = TRUE)
  
  print(paste("Reading: ",masklist))
  maskniis = lapply(masklist, readNIfTI, reorient=TRUE )
  names(maskniis) <- masklist
  
  # make a list of the labels in each mask to aggregate
  labs = lapply(maskniis, function(n) unique(n[n!=0]))
  print("Labels from each mask:")
  print(labs)

  # make a combined mask file to plot, if a label has already been used, find the next unused label
  labframe <- data.frame(mask=rep(unname(masklist),times=sapply(labs,length)),
                         original_label = unlist(labs),
                         new_label = unlist(labs))
  
  # to remove duplicate labels, move the second appearance to the next unoccupied integer
  # the number of duplicated rows will be the same as the total rows - unique
  dupidx <- duplicated(labframe$new_label)
  labframe[dupidx,"new_label"] <- setdiff(1:nrow(labframe),unique(labframe$original_label))[1:sum(dupidx)]
  
  print("Original labels and new labels on plots:")
  print(labframe)
  # make a single mask nii and combine the labels
  mask_combined = maskniis[[1]]
  mask_combined[1:length(mask_combined)] = 0
  densities = vector(nrow(labframe),mode="list")
  mode_values = numeric(nrow(labframe))
  didx = 0 # need to keep track of which density since the loops are not row indices.
  
  # use two loops to avoid re-loading image data
  for(nii in intersect(masklist, unique(labframe$mask))){
    labframe_sub = labframe[labframe$mask==nii,]
    nii_sub = maskniis[[nii]]
    for(jjj in 1:nrow(labframe_sub)){
      oldlab = labframe_sub$original_label[jjj]
      newlab = labframe_sub$new_label[jjj]
      print(c(oldlab,newlab))
      # bidx = boundary_idx(nii_sub,r=args$r,label=oldlab)
      # mask_combined[bidx] = newlab
      # set mask to new labels as well
      nii_sub[maskniis[[nii]]==oldlab] = newlab
      
      # calculate density estimates pixels if activated
      if(tol != 0){
        didx = didx + 1
        pixs = img[nii_sub==newlab]
        if(!is.null(zmax) & length(pixs) > 1){
          d1 = density(pixs, bw="nrd", from=0, to=zmax)
        } else if(length(pixs) > 1) {
          d1 = density(pixs, bw="nrd")
        } else {
          d1 = list(x=rep(pixs,2), y=c(0,1)) # HACK if only 1 pixel, density will fail
        }
        d1_mode = d1$x[order(d1$y,decreasing=TRUE)][1]
        densities[[didx]]   = d1
        mode_values[[didx]] = d1_mode
      }
    }
    if(any(labframe_sub$new_label != labframe_sub$original_label)){
      maskniis[[nii]] = nii_sub
    }
  }
  # rainbow(6) colors: red, yellow, green, light blue, dark blue, magenta
  plotcols = rainbow(nrow(labframe), alpha=0.2)
  linecols = rainbow(nrow(labframe), alpha=1)
  
  # plot individual shots at max area
  mask_shot <- function(image_nii,mask_nii,plane){
    max_zval = switch(pl,
                      "axial"={
                        which.max(sapply(1:dim(mask_nii)[3], function(x) sum(mask_nii[,,x]!=0)))
                      },
                      "sagittal"={
                        which.max(sapply(1:dim(mask_nii)[1], function(x) sum(mask_nii[x,,]!=0)))
                      },
                      "coronal"={
                        which.max(sapply(1:dim(mask_nii)[2], function(x) sum(mask_nii[,x,]!=0)))
                      })
    slice_overlay(x=img, y=mask_nii,
                  plane=pl, z=max_zval,
                  col.y=plotcols, zlim.y = range(labframe$new_label))
  }
  
  # save plots
  for(jjj in 1:length(masklist)){
    mask_nii = maskniis[[jjj]]
    mask_nii[1,1,1] = max(labframe$new_label) # for correct colors
    for(pl in c("axial","sagittal","coronal")){
      png(sub(".png$",paste0("_",pl,jjj,"maskshot.png"),pngname), width=512, height=512, units="px")
      mask_shot(image_nii=img,mask_nii=mask_nii,plane=pl)
      dev.off()
    }  
  }
  
  # plot combined densities
  if(tol != 0){
    png(sub(".png$","_density.png",pngname),width=512,height=512, units="px")
    par(cex=2.5, mar=c(2,0.1,0.1,0.1), bg="gray")
    
    # initialze with plot(NULL) then sequentially add densities
    plot(NULL,
         xlim=range(unlist(lapply(densities, function(dd) dd$x ))),
         ylim=c(0,1),
         xlab="Image intensity",
         ylab="")
    
    for(iii in 1:length(densities)){
      dd = densities[[iii]]
      line_color = linecols[iii]
      lines(x=dd$x,
            y=dd$y/max(dd$y),
            type="l",
            lwd=3,
            col=linecols[iii])
      abline(v=c(1+tol,1-tol)*mode_values[iii], lty=2, col=line_color)
    }
    dev.off()  
  }
}

