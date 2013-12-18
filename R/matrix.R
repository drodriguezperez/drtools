##
##  Matrix
##
##  Created by Daniel Rodríguez Pérez on 18/12/2013.
##
##  Copyright (c) 2013 Daniel Rodríguez Pérez.
##
##  This program is free software: you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation, either version 3 of the License, or
##  (at your option) any later version.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  You should have received a copy of the GNU General Public License
##  along with this program.  If not, see <http://www.gnu.org/licenses/>
##

.validateMatrix <- function(x) {
  if (is.data.frame(x)) 
    x <- as.matrix(x)
  
  if (!is.array(x) || length(dn <- dim(x)) < 2L) 
    stop("'x' must be an array of at least two dimensions")
}

#' Form Row and Column Min and Max
#' 
#' Form row and column min and max for numeric arrays.
#' 
#' @param x an array of two or more dimensions, containing numeric values
#' @param na.rm logical. Should missing values (including NaN) be omitted from the calculations?
#'
#' @rdname colMin
#' @export colMin
#' @aliases colMin
colMin <- function(x, na.rm = FALSE) {
  .validateMatrix(x)  
  result <- apply(x, 2, min, na.rm = na.rm)
  return(result)
}

#' @rdname colMin
#' @export rowMin
#' @aliases rowMin
rowMin <- function(x, na.rm = FALSE) {
  .validateMatrix(x)  
  result <- apply(x, 1, min, na.rm = na.rm)
  return(result)
}
