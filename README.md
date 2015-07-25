# Deal_Scraper
Deal scraping and analytics pipeline for cheap shit to post on amazon

First Version Upload:

-Deal tracking is working with a small memory leak (most likely due to warnings)
-gmail account authorization needs to be done manually (soon to be a clickable function on shiny page)
-Historical price tracking also not yet implemented in UI but base functionality to digitize graph lines is present in global file
-Deal files are written in realtime to output csv files in the project folder. We need to add these to gitignore or remove them when pushing code.