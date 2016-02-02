###############################################################################
# Plotting Functions
###############################################################################
#' Add Alpha Level to a Passed Color
#'
#' This function takes a color produced by 'rgb' in hex format and adds an alpha value.
#' @param color Expected to be a color produced by rgb or compatible
#' @param alpha Expected to be a value between 0-1
#' @keywords alpha
#' @export
#' @examples
#' red = rgb(1,0,0);
#' red_alpha = add_alpha_to_color(red,0.5);

add_alpha_to_color <- function(color,alpha) {
	color_vector = col2rgb(color)/255;
	
	#Deal with the case when alpha is specified on an 8-bit scale vs 0-1
	if (alpha > 1) {
		alpha = alpha/255;
	}
	
	color_plus_alpha = rgb(t(color_vector),alpha=alpha);
}

#' Add A Color Underneath a Set of X-Y Coordinates
#'
#' This function adds shading underneath a line, extending down to the x-axis
#' @param x X-coordindates of the line
#' @param y X-coordindates of the line
#' @param color Color of the shading underneath the line
#' @keywords line plot
#' @export
#' @examples
#' plot(0:5,0:5,type='l',col=rgb(1,0,0))
#' add_under_color(0:5,0:5,rgb(1,0,0,0.5));

add_under_color <- function(x,y,color) {
	poly_coords = list();
	poly_coords$x = c(min(x),x,max(x));
	poly_coords$y = c(0,y,0);

	polygon(poly_coords$x,poly_coords$y,col=color,border=NA);
}

#' Draws A Line with Shading Underneath
#'
#' This function adds shading underneath a line, extending down to the x-axis
#' @param x X-coordindates of the line
#' @param y X-coordindates of the line
#' @param color Color of the shading underneath the line
#' @param undercolor Optional: if specified, the shading underneath the line will be this color, if unspecified it defaults to the same color as the line, with alpha level of 0.25
#' @param ... Optional: Any additional parameters will be passed on to the lines function
#' @keywords line plot
#' @export
#' @examples
#' lines_with_undershading(0:5,0:5,col=rgb(1,0,0))

lines_with_undershading <- function(x,y,color,undercolor=NA,...) {
	lines(x,y,col=color,...);

	if (is.na(undercolor)) {
		undercolor = add_alpha_to_color(color,0.25);
	}

	add_under_color(x,y,undercolor)
}

#' Draws A Line with Confidence Interval Shading
#'
#' This function draws a line with confidence interval shading
#' @param x X-coordindates of the line
#' @param y Y-coordindates of the line
#' @param top_interval Y-coordindates of the top of the interval
#' @param bottom_interval Y-coordindates of the bottom of the interval
#' @param color Optional: Change the color of the plotted points/CI shading
#' @param ... Optional: Any additional parameters will be passed on to the lines function
#' @keywords confidence interval plot
#' @export
#' @examples
#' plotLinesWithConfInt(1:5,1:5,c(1:5)+1,c(1:5)-1,rgb(0.5,0.5,0.5,0.5),typ='l')

plotLinesWithConfInt <- function(x,y,top_interval,bottom_interval,color=NA,...) {
	if (is.na(color)) {
		color = rgb(0,0,0);
	}

	plot(x,y,ylim = c(min(bottom_interval),max(top_interval)*1.08),
		 col=color,typ='l',...);
	
	shade_color = add_alpha_to_color(color,0.25);

	polygon(c(x,rev(x)),c(top_interval,rev(bottom_interval)),col=shade_color,
			border=NA);
}

#' Draws A Line with Confidence Interval Shading From a Collection of Data
#'
#' This function draws a line with confidence interval shading, assuming all data in one column should be used to build the confidence interval
#' @param mat Matrix of data
#' @param x Optional: X-coordindates of the line
#' @param color Optional: Change the color of the plotted points/CI shading
#' @param ... Optional: Any additional parameters will be passed on to the plotLinesWithConfInt function
#' @keywords confidence interval plot
#' @export
#' @examples
#' plotLinesMatWithConfInt(...)

plotLinesMatWithConfInt <- function(mat,x_coords=NA,color=NA,...) {
	means = colMeans(mat,na.rm=T);

	top_int = apply(mat,2,function(x) t.test(x,conf.level=0.9)$conf.int[2])
	bottom_int = apply(mat,2,function(x) t.test(x,conf.level=0.9)$conf.int[1])
	
	if (is.na(x_coords)) {
		x_coords = 1:dim(mat)[2]
	}

	plotLinesWithConfInt(x_coords,means,top_int,bottom_int,color=color,...)
}

#' Draw a Barplot with Confidence Intervals
#' 
#' This function draws a bar plot with confidence intervals from a provided list
#' containing your data.
#' @param data A list containing your data sets
#' @param label_names Optional: Labels for your data sets, if not provided the 
#'   names of the data parameter will be used
#' @param padj Optional: Shift the position of the bar labels, negative numbers 
#'   go up while positive numbers go down, defaults to NA
#' @param conf.int Optional: The confidence interval percentile, defaults to 
#'   0.95
#' @param add.N.count Optional: Add the number of observations to the bar label
#'   names
#' @param ... Optional: Any additional parameters will be passed on to the 
#'   barplot function
#' @keywords confidence interval plot
#' @export
#' @examples
#' data = list(A = seq(1,5),B=seq(1,10));
#' plotBarplotWithConfInt(data)

plotBarplotWithConfInt <- function(data,label_names = NA,padj= NA,conf.int = 0.95,
                                   add.N.count = FALSE,...) {
  library(Hmisc);

  if (! is.character(label_names[1]) & is.na(label_names[1])) {
    label_names = names(data)
  }
  
  if (add.N.count) {
    for (i in seq(1,length(data))) {
      label_names[i] = paste0(label_names[i], 
                              sprintf(' (n=%d)',length(na.omit(data[[i]]))));
    }
  }
    
  top_int = c();
  bottom_int = c();
  means = c();
	
  if (class(data) == "list") {
    for (exp_name in names(data)) {
      this_set = data[[exp_name]];
      if (length(this_set) == 1) {
        top_int = c(top_int,NA);
        bottom_int = c(bottom_int,NA);
      } else {
        conf_int = t.test(this_set,conf.level=conf.int)$conf.int;
        top_int = c(top_int,conf_int[2]);
        bottom_int = c(bottom_int,conf_int[1]);
      }
      
      means = c(means,mean(this_set,na.rm=T));
    }
  } else {
    top_int = apply(data,2,function(x) t.test(x,conf.level=conf.int)$conf.int[2])
    bottom_int = apply(data,2,function(x) t.test(x,conf.level=conf.int)$conf.int[1])
    
    means = colMeans(data,na.rm=T);
  }

  bar_mids = barplot(means,
                     names=label_names,axisnames=F,lwd=3,
					 ylim=c(0,max(top_int)*1.05),...);

  #lwd = -1 makes the axes bars disappear
  axis(1,labels = label_names,at=bar_mids,line = 1,padj=padj,lwd=-1);
  
  errbar(bar_mids,means,top_int,bottom_int,add=T,cex=0.0001,lwd=2);
  
  return(bar_mids)
}

#' Apply a better set of ploting settings 
#' 
#' This function draws a bar plot with confidence intervals from a provided list
#' containing your data.
#' @export

applyBetterParSettings <- function() {
  #bty - remove box around plot
  #mgp - move labels closer to axes
  #mar - reduce margins around plot, format (bottom,left,top,right)
  
  par(bty='n', mgp=c(1.5,0.5,0), mar=c(3,3,0,0));
}

###############################################################################
# Outside Utilities
###############################################################################

#' Convert a SVG image to PNG using imagemagick
#'
#' This function takes in an svg file name and converts that image to a png. 
#' This function assumes that imagemagick has been installed and that "convert"
#' is available at the command line. This function also assumes that the svg file
#' has the string '.svg' present at the end of the of the svg file
#' @param svg.file.name: The location of the svg file
#' @param im.width Optional: Specifies the output width of the png file, defaults
#' to 1000
#' @keywords SVG PNG convert
#' @export
#' @examples
#' svg.file = 'test.svg';
#' svg(svg.file)
#' plot(1:10)
#' graphics.off()
#' convertSVGtoPNG(svg.file)

convertSVGtoPNG <- function(svg.file.name,im.width=1000) {
  
  #Note: R regexp requires that the escape "\" also be escaped in regexp
  png.file.name = sub("\\.svg","\\.png",svg.file.name)
  
  #convert options
  #  density: sets the number of pixels per inch sampled from the svg, 300 seems good
  #  trim: remove any all white columns/rows from the image
  #  resize: set the size of the output image
  convert_cmd = sprintf('convert -density 300 %s -flatten -trim -resize %dx %s',svg.file.name,im.width,png.file.name)
  
  system(convert_cmd)
}
