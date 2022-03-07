#' @title package_check
#' @description package_check takes in a vector of package names as characters.
#' It checks if they are installed on the user's system, and prompts the user before installing.
#' It then loads all packages specified in the input.
#'
#' @author Zain Ahmad
#'
#' @param packages a vector of packages names
#'
#' @return none
#' 
#' @examples package_check(package_vec)
#' 
#' 
#' @export
#'
#' 


package_check <- function(packages){

  for (x in packages){ 
    # if package is installed, will be loaded
    if (!require(x, character.only = TRUE)){ 
      
      # if not, user will be prompted to install package
      user.prompt <- askYesNo("You do not have this package installed. Do you want to install it?",
                              default = FALSE)
      # if user responds yes
      if(user.prompt){
        # package will be installed
        install.packages(x, dependencies = TRUE)
        # installed package will be loaded
        library(x, character.only = TRUE)
        }
      
      # if user cancels, function gives error and stops
      else if(is.na(user.prompt)){
        stop("Package_check aborted.")
      } 
      
      # if user responds no
      else{
         # user is given warning
         warning(paste("Package", x, "not installed"))
         next} 
      }
  }
}