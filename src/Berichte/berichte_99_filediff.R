# welche berichte sind neu dazugekommen rausfinden und nur neue in ordner lassen. ziel davon: dateistruktur der cleanings etc beibehalten.

# old <- list.files("C:\\Users\\zoot\\OneDrive - Universitaet Bern\\Other Projects\\viamia_analysis\\data\\Berichte\\0421\\BL")
# new <- list.files("C:\\Users\\zoot\\OneDrive - Universitaet Bern\\Other Projects\\viamia_analysis\\data\\Berichte\\0721\\BL")
# shared <- intersect(new, old)
# # delete shared files from new folder
# dir <- file.path("C:\\Users\\zoot\\OneDrive - Universitaet Bern\\Other Projects\\viamia_analysis\\data\\Berichte\\0721\\BL")
# file.remove(paste0(dir,"\\", shared))

# function 
remove_shared_files <- function(old_dir, new_dir) {
  old <- list.files(old_dir)
  new <- list.files(new_dir)
  shared <- intersect(new, old)
  
  # skip if none to be removed
  if(length(shared) == 0) {
    cat("Keine gemeinsamen Dateien")
  }
  else {
    # delete shared files from new folder
    dir <- file.path(new_dir)
    
    file.remove(paste0(dir,"\\", shared))
    cat("Anzahl Dateien gelöscht:", length(shared))
  }

}

kantone <- list.dirs("C:\\Users\\zoot\\OneDrive - Universitaet Bern\\Other Projects\\viamia_analysis\\data\\Berichte\\0721\\", full.names = FALSE, recursive = FALSE)
new_dir_stem <- "C:\\Users\\zoot\\OneDrive - Universitaet Bern\\Other Projects\\viamia_analysis\\data\\Berichte\\0721\\"
old_dir_stem <- "C:\\Users\\zoot\\OneDrive - Universitaet Bern\\Other Projects\\viamia_analysis\\data\\Berichte\\0421\\"


# remove_shared_files(paste0(old_dir_stem, kantone[1]),
#                     paste0(new_dir_stem, kantone[1]))

purrr::map(kantone, function(k) {
  remove_shared_files(paste0(old_dir_stem, k),
                      paste0(new_dir_stem, k))
})
