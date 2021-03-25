# Collateral-Assets-Dashboard

Providing an overview of the climate-compatibility of the [collateral assets database of the ecb](https://www.ecb.europa.eu/paym/coll/assets/html/index.en.html).


File-structure: 

Folders:
- `src/`: contains all R-files to generate the analysis and plots
- `pages/`: contains Rmd-files for website-subpages eg. old-reports,...
- `plot/`: contains current plots for this day
- `_site/`: automatically generated when `rmarkdown::render_site()` is called. Contains exportable and viewable html

Files: 
- `footer.html`: page-footer
- `header.html`: page-header
- `index.Rmd`: Rmd-source-file for index.html, containing main report
- `_site.yml`: site-specifications for `rmarkdown::render_site()`
- `script-draft.sh`: to be renamed. Shell-script automating everything
- `README.md`: readme for Github


