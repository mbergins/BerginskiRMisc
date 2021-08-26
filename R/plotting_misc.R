###############################################################################
# Plotting Functions
###############################################################################
#' Add a column to a tidy data set with the count of observations appended
#' 
#' This function returns a tidy data set with one addition column which contains
#' a variable name with the counts appended. As an example, suppose you had 10 
#' observations of category (column named your_category) 'A' and 12 observations
#' of category 'B'. The output will add this additional column:
#' your_category.
#' <chr> 
#' A\\n(n=10) 
#' B\\n(n=12)
#' 
#' This is most useful for making a new categorical variable name to pass to a 
#' ggplot bar/box plot.
#' @param dataSet: A tidy data set
#' @param countField: A column to use for counting
#' @param addNewline Optional: Add a newline between the category name and the variable count, defaults to true
#' @export
addNCountColumn <- function(dataSet,countField,addNewline = T) {

  countFieldenQuo <- enquo(countField)
  countSummary = dataSet %>% group_by(!!countFieldenQuo) %>% summarise(count = n())
  
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
               axis.title.y=element_text(margin=margin(0,1.5,0,0))))
  
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
#' \dontrun{
#' svg.file = 'test.svg';
#' svg(svg.file)
#' plot(1:10)
#' graphics.off()
#' convertSVGtoPNG(svg.file)
#' }

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
#' \dontrun{
#' svg.file = 'test.svg';
#' svg(svg.file)
#' plot(1:10)
#' graphics.off()
#' convertSVGtoTarget(svg.file,target="jpg")
#' }
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
#' \dontrun{
#' file.name = 'test.png';
#' svg(file.name)
#' plot(1:10)
#' graphics.off()
#' trimImage(file.name)
#' }
trimImage <- function(file.name,debug=F) {
  
  if (nchar(Sys.which("convert")) == 0) {
    warning("Imagemagick not installed or not avaible when called through trimImage function.")
    return()
  }
  
  convert_cmd = sprintf('convert "%s" -trim "%s"',
                        file.name,file.name)
  if (debug) {
    print(convert_cmd)
  }
  system(convert_cmd)
}
