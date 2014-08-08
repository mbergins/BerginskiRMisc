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
