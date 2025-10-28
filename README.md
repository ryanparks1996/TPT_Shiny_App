# TPT_Shiny_App
Shiny app built in R to generate simple plot(s) and summary statistics based on user inputs.

Every week my company sends out an updated file containing all the weekly TPT (Tickets per Tap) averages across the company. TPT is a critical metric that is closely monitored, as it directly impacts merchandise costs. As of Fall 2025, the currently desired TPT is 3.0. A store's average does include rides that don't pay tickets, so the games that do pay tickets will have an average ever so slightly above 3. Only one- to two tenths.

This app was built in R using the Shiny package. In addition to the app.R file, there is a helpers.r file that contains function(s) to assist the app. Also included will be an updated version of the previously mentioned file (sent out weekly). To run this program remotely, you must first download the .xlsx file. The current build of the app has a file uploader integrated and will not work without first uploading the spreadsheet.

This project has been a great learning tool for using the shiny package and in thinking critically about design choices. I still have a ways to go in making the application look attractive, but this has been a great stepping stone towards that goal. To a lesser extent, this project has allowed me to further practice the ggplot2 R package and its grammar of graphics.

As of 10/28/25, the checkbox for selecting monthly averages does not actually do anything. Most of the code is there in the helpers.R file, but getting this fully functional will take a bit longer, and I need to spend my very limited time on other projects. I will come back to this in the near future.
