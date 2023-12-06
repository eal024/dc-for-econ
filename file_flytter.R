

filer <- list.files(getwd(), pattern = "exam_")

fra <- getwd()
til <- paste0( getwd(), "/exams")

filer[1]

file.remove( filer[1], from = fra, to = til )
