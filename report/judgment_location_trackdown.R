library(googledrive)
library(trackdown)

googledrive::drive_auth(email = "stepanpaulik@gmail.com")
googledrive::drive_empty_trash()

# UPLOAD RMARKDOWN
# trackdown::upload_file(file = "report/judgment_location_article.Rmd",
#                        gpath = "judgment_location/report", 
#                        hide_code = TRUE)

trackdown::update_file(file = "report/judgment_location_article.Rmd",
                       gpath = "judgment_location/report", 
                       hide_code = TRUE)

# googledrive::drive_put(media = "report/judgment_location_article.pdf",
#                           path = "judgment_location/report/judgment_location_article.pdf")

googledrive::drive_update(media = "report/judgment_location_article.pdf",
                          file = "judgment_location/report/judgment_location_article.pdf")

# Download the file
trackdown::download_file(file = "report/judgment_location_article.Rmd",
                         gpath = "judgment_location/report")
