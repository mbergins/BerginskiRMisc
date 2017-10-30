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
#' This function draws a line with confidence interval shading, assuming all 
#' data in one column should be used to build the confidence interval
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
  
  top_int = apply(mat,2,function(x) t.test(x,conf.level=0.95)$conf.int[2])
  bottom_int = apply(mat,2,function(x) t.test(x,conf.level=0.95)$conf.int[1])
  
  if (is.na(x_coords)) {
    x_coords = 1:dim(mat)[2]
  }
  
  plotLinesWithConfInt(x_coords,means,top_int,bottom_int,color=color,...)
}

#' Make a Time-lapse ggplot2 Object with Confidence Intervals
#' 
#' This function builds a ggplot2 object that plots the mean of a provided set 
#' of data and the corresponding confidence interval that data at each time 
#' point. The input format is expected to be a list of matrices where each 
#' matrix represents a set of time series. Each of the matrices in the list are 
#' formated so that each row is a single time point and each column is a single 
#' experiment.
#' 
#' The confidence interval is calculated using a t.test and indicated on the 
#' plot as a shaded area that matches the color of the mean line.
#' 
#' @param inputData A list of matrices containing your data sets
#' @param time.interval Optional: The amount of time between each sample,
#'   defaults to 1
#' @param x.tick.position Optional: The location of the x axis numeric
#'   positions, if not specified, defaults to time.interval
#' @param conf.int Optional: Percentage size of the confidence interval plot,
#'   defaults to 95\%
#' @param xlabel Optional: If specified, will be passed to xlab
#' @param ylabel Optional: If specified, will be passed to ylab
#' @param legend.labels Optional: A mapping between the names of the inputData 
#'   list and the desired legend titles
#' @keywords confidence interval plot
#' @export
#' @examples
#' data = list(A = matrix(1:10,nrow=10,ncol=10) + rnorm(100), 
#'             B = matrix(1:10,nrow=10,ncol=10) + rnorm(100) + 5,
#'             C = matrix(1:10,nrow=10,ncol=10) + rnorm(100,sd=5));
#' thisPlot = buildTimelapsePlotWithConfInt(data);
#' thisPlot

buildTimelapsePlotWithConfInt <- 
  function(inputData, x.tick.position = NA, time.interval = 1, conf.int = 0.95,
           xlabel = NA, ylabel = NA,legend.labels = NA) {
    
  require(ggplot2)
  
  #This list will be converted to a data frame after determining the mean and
  #upper/lower confidence intervals
  summaryData = list();
  for (expType in names(inputData)) {
    summaryData[[paste0(expType,"Mean")]] = rowMeans(inputData[[expType]],na.rm=T)
    
    if (mean(summaryData[[paste0(expType,"Mean")]]))
      summaryData[[paste0(expType,"Upper")]] = apply(inputData[[expType]],1,
                                                     function(x) t.test(x,conf.level=conf.int)$conf.int[2])
    
    summaryData[[paste0(expType,"Lower")]] = apply(inputData[[expType]],1,
                                                   function(x) t.test(x,conf.level=conf.int)$conf.int[1])
  }
  summaryData = as.data.frame(summaryData);
  if (!is.na(x.tick.position[1])) {
    summaryData$time = x.tick.position;
  } else {
    summaryData$time = seq(along.with = summaryData[,1],by = time.interval,from = 0);  
  }

  #Use the names in the input data to specify the lines and corresponding
  confIntPlot = ggplot(data=summaryData,aes_string(x="time"));
  
  for (expType in names(inputData)) {
    color.string = shQuote(expType)
    if (! is.na(legend.labels[1])) {
      color.string = shQuote(legend.labels[[expType]])
    }
    
    confIntPlot <- confIntPlot + 
      geom_line(aes_string(y=paste0(expType,"Mean"),
                           color=color.string), 
                size=1.5) +
      geom_ribbon(aes_string(ymin=paste0(expType,"Upper"), 
                             ymax=paste0(expType,"Lower"), 
                             fill=color.string), 
                  alpha=0.2);
  }

  confIntPlot = confIntPlot + scale_colour_brewer("",type="qual",palette = 2) + 
    scale_fill_brewer("",type="qual",palette = 2)
  
  if (! is.na(xlabel)) {
    confIntPlot = confIntPlot + xlab(xlabel);
  }
  
  if (! is.na(ylabel)) {
    confIntPlot = confIntPlot + ylab(ylabel);
  }
  
  return(confIntPlot)
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

#' Make a boxplot with some small improvements
#'
#' @param data: A list containing your data sets 
#' @param add.N.count: Whether or not to add the N counts to the data labels, defaults to True
#' @param label.names: Specifies the label names for the data sets, defaults to the result of names(data)
#' @param ... Optional: Any additional parameters will be passed on to the plotLinesWithConfInt function
#' @keywords fancy boxplot
#' @export
#' @examples
#' boxplotFancy(list(A=rnorm(10),B=rnorm(10)))

boxplotFancy <- function(data,add.N.count=T,label.names=NA,...) {
  if (is.na(label.names[1])) {
    label.names = names(data)
  }
  
  if (add.N.count) {
    for (i in seq(1,length(data))) {
      label.names[i] = paste0(label.names[i], 
                              sprintf('\n(n=%d)',length(na.omit(data[[i]]))));
    }
  }
  
  if (add.N.count) {
    boxplot(data,names=label.names,axis=F,xaxt='n',...)
    axis(1,at=1:length(label.names),tick=F,labels=label.names)
  } else {
    boxplot(data,names=label.names,...)
  }
}

#' Apply a better set of ploting settings
#' 
#' This function sets sets the par settings to something I think is a bit more 
#' reasonable for base R graphics. Any of these settings can be overriden by
#' passing in different values
#' 
#' The settings and explanation are:
#' 
#' -bty='n': remove the box around the entire plot
#' 
#' -mgp=c(1.5,0.5,0): reduce distance of the axis title from the axis (1.5), 
#' reduce distance of tick mark labels from the ticks (0.5)
#' 
#' -mar=c(3,3,0,0): reduce the margins around the (bottom,left,top,right) of the
#' plot
#' @export

applyBetterParSettings <- function(...) {
  #bty - remove box around plot
  #mgp - move labels closer to axes
  #mar - reduce margins around plot, format (bottom,left,top,right)
  
  par(bty='n', mgp=c(1.5,0.5,0), mar=c(3,3,0,0),...);
}

#' Add a column to a tidy data set with the count of observations appended
#' 
#' This function returns a tidy data set with one addition column which contains
#' a variable name with the counts appended. As an example, suppose you had 10 
#' observations of category (column named your_category) 'A' and 12 observations
#' of category 'B'. The output will add this additional column:
#' your_category.n 
#' <chr> 
#' A\n(n=10) 
#' B\n(n=12)
#' 
#' This is most useful for making a new categorical variable name to pass to a 
#' ggplot bar/box plot.
#' @param dataSet: A tidy data set
#' @param countField: A column to use for counting
#' @param addNewline Optional: Add a newline between the category name and the
#'   variable count, defaults to true
#' @export
addNCountColumn <- function(dataSet,countField,addNewline = T) {
  library(tidyverse);
  
  countFieldenQuo <- enquo(countField)
  
  countSummary = dataSet %>% group_by(!!countFieldenQuo) %>% summarize(count = n())
  nCountStrings = c()
  for (rowNum in 1:dim(countSummary)[1]) {
    thisCategoryRow = countSummary[rowNum,]
    nCountStrings = c(nCountStrings,
                    paste0(thisCategoryRow[1],'\n(n=',thisCategoryRow[2],')'))
  }
  countSummary[[paste0(quo_name(countFieldenQuo),'.n')]] = nCountStrings
  countSummary$count <- NULL

  dataSet = left_join(dataSet,countSummary)
  
  return(dataSet)  
}

#' Define a ggplot2 theme for myself
#' 
#' This function returns a theme that can be directly used in ggplot2 plots.
#' @examples
#' qplot(1:10,1:10) + theme_berginski()
#' @export
theme_berginski <- function() {
  library(ggplot2);
  
  return(theme(panel.grid = element_blank(), 
               panel.background = element_blank(), 
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(), 
               axis.ticks = element_line(color='black'),
               axis.text = element_text(color='black'),
               axis.title.x=element_text(margin=margin(1.5,0,0,0)),
               axis.title.y=element_text(margin=margin(0,1.5,0,0)),
               legend.title=element_blank()))
  
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
#' @param debug Optional: Print out imagemagick convert command
#' @keywords SVG PNG convert
#' @export
#' @examples
#' svg.file = 'test.svg';
#' svg(svg.file)
#' plot(1:10)
#' graphics.off()
#' convertSVGtoPNG(svg.file)

convertSVGtoPNG <- function(svg.file.name,im.width=1000,debug=F) {
  
  #Note: R regexp requires that the escape "\" also be escaped in regexp
  png.file.name = sub("\\.svg","\\.png",svg.file.name)
  
  #convert options
  #  density: sets the number of pixels per inch sampled from the svg, 300 seems good
  #  trim: remove any all white columns/rows from the image
  #  resize: set the size of the output image
  convert_cmd = sprintf('convert -density 300 "%s" -flatten -trim -resize %dx "%s"',
                        svg.file.name,im.width,png.file.name)
  if (debug) {
    print(convert_cmd)  
  }
  system(convert_cmd)
}

#' Convert a SVG image to different image type using imagemagick
#' 
#' This function takes in an svg file name and converts that image to a
#' different format using imagemagick. This function assumes that imagemagick
#' has been installed and that "convert" is available at the command line. This
#' function also assumes that the svg file has the string '.svg' present at the
#' end of the of the svg file
#' @param svg.file.name: The location of the svg file
#' @param target Optional: The extension of the output file, defaults to .jpg
#' @param im.width Optional: Specifies the output width of the output file,
#'   defaults to 1000
#' @param debug Optional: Print out imagemagick convert command
#' @keywords SVG convert
#' @export
#' @examples
#' svg.file = 'test.svg';
#' svg(svg.file)
#' plot(1:10)
#' graphics.off()
#' convertSVGtoTarget(svg.file,target="jpg")
convertSVGtoTarget <- function(svg.file.name,im.width=1000,target="jpg",debug=F) {
  
  #Note: R regexp requires that the escape "\" also be escaped in regexp
  output.file.name = sub("\\.svg",paste0("\\.",target),svg.file.name)
  
  #convert options
  #  density: sets the number of pixels per inch sampled from the svg, 300 seems good
  #  trim: remove any all white columns/rows from the image
  #  resize: set the size of the output image
  convert_cmd = sprintf('convert -density 300 "%s" -flatten -trim -resize %dx "%s"',
                        svg.file.name,im.width,output.file.name)
  if (debug) {
    print(convert_cmd)  
  }
  system(convert_cmd)
}


#' Trim the whitespace around an image using Imagemagick
#' 
#' This function takes a filename for an image, which will then be processed using the trim command in imagemagick to remove the surrounding whitespace
#' @param file.name: The location of the file
#' @param debug Optional: Print out imagemagick convert command
#' @keywords trim
#' @export
#' @examples
#' file.name = 'test.png';
#' svg(file.name)
#' plot(1:10)
#' graphics.off()
#' convertSVGtoTarget(file.name)
trimImage <- function(file.name,debug=F) {
  
  convert_cmd = sprintf('convert "%s" -trim "%s"',
                        file.name,file.name)
  if (debug) {
    print(convert_cmd)
  }
  system(convert_cmd)
}
