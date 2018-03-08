
###################################################
#  Define a function to check prediction format   #
###################################################
ValidateSubmission <- function(files.path, number.of.rows, number.of.columns, correct.header, correct.column.1){
  for (i in 1:length(files.path)) {
    infile.path=files.path[i]
    cat('checking file', infile.path, '\n')
    #check if file exists
    if (!file.exists(infile.path)) {
      stop("File ", infile," does not exist!")
    }
    #check permission to read file
    if (file.access(infile.path, mode = 4) < 0) {
      stop("No permission to read file ", infile.path, " !")
    }
    #check file is readable
    CheckReadableFile <- function(x) { tryCatch(read.table(x, sep='\t', header = TRUE, na.strings=''), error = function(e) {cat("Can not read", x, " column element missing, read following error message for more informations",'\n') ; read.table(x)})  }
    CheckReadableFile(infile.path)
    #read files
    pred.table <- read.table(infile.path, sep='\t', header = TRUE, na.strings='') #file with submission data
    #check file has not empty cells
    if (any(grepl('NA', pred.table) == TRUE)){
      stop('submission file ', infile.path, ' has a blank cell in column ',match('TRUE',grepl('NA', pred.table)))
    }
    #check file has correct number of lines
    if (number.of.rows != nrow(pred.table)) { 
      stop('submission file ', infile.path, ' has not a correct number of lines')
    }
    #check number of fields of each line is correct
    if (number.of.columns != ncol(pred.table)) {
      stop ('submission file ', infile.path,' has not a correct number of columns')
    }
    #check if header is valid
    if (any(correct.header != colnames(pred.table))) {
      stop ('submission file', infile.path, ' has not a correct header') # check all element of header are correct
    } 
    #check each column contains valid character (for example: in some columns only numbers are accepted, etc....)
    #check if column 1 contains valid character
    if ( any(as.character(correct.column.1) !=  as.character(pred.table[, 1]))) {       # escape sequences \d, \s and \w represent any decimal digit, space character and ‘word’ character, * means zero or one match,  + means preceding item will match one or more times
      stop("First column of ", infile.path," must contain valid alleles! Check row ", which(as.character(correct.column.1) !=  as.character(pred.table[, 1])))
    }
    #check if column 2 is numerical value or star
    if (any(regexpr('[*]|\\d+', pred.table[, 2]) != 1)) {
      stop("Second column of ", infile.path," must be numeric or \"*\"!")
    } else if (any(grepl('[*]', pred.table[, 2]) == grepl('\\d+', pred.table[, 2])) == TRUE) { # check both * and digits are not present in column 2
      stop("Second column of ", infile.path, " must be numeric or \"*\", not both!")
    }
    #check if column 3 is numerical value or star
    if (any(regexpr('[*]|\\d+', pred.table[, 3]) != 1)) {
      stop("Third column of ", infile.path, " must be numeric or \"*\"!")
    } else if (any(grepl('[*]', pred.table[, 3]) == grepl('\\d+', pred.table[, 3])) == TRUE) { # check both * and digits are not present in column 3
      stop("Third column of ", infile.path, " must be numeric or \"*\", not both!")
    }
    cat('File format check passed!', '\n')
  }
}


