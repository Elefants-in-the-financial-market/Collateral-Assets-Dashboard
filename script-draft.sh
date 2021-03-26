
call always at 7pm cet --> there data for next day is published
do that in the beginning https://stackoverflow.com/questions/6565357/git-push-requires-username-and-password

Resources: 
# R -e 'rmarkdown::render("test.Rmd", output_file="filename")' # https://rmarkdown.rstudio.com/lesson-13.html 
https://www.dssw.co.uk/blog/2011-05-22-how-to-run-a-shell-script-every-day-on-a-mac/
https://stackoverflow.com/questions/36854193/scheduling-a-terminal-command-or-script-file-to-run-daily-at-a-specific-time-mac
#mv index.html dates/${collateral_date}.html

collateral_date=$(date +%m-%d-%Y -d "$DATE + $i day")

echo "Getting data, running PACTA and compiling page for ${collateral_date}"

# Run PACTA-analysis here and save plots in plots/



# Generate site

# Clean old working site and generate new one.
R -e 'rmarkdown::clean_site(preview = FALSE)' 
R -e 'rmarkdown::render_site()' 

# Commit to Github

#git add .
git add _site/
git add plots/

git commit -m"Data for ${collateral_date}"
if [ -n "$(git status - porcelain)" ];
then
	echo "IT IS CLEAN"
else
 git status
 echo "Pushing data to remote server!!!"
	git push -u origin master
fi
