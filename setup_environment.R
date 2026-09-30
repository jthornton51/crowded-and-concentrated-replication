# Run from the package root after installing R and renv.
if(!requireNamespace('renv',quietly=TRUE))stop('Install renv first: install.packages("renv", repos="https://cloud.r-project.org")')
dir.create('r-library',showWarnings=FALSE)
renv::restore(project=getwd(),library=file.path(getwd(),'r-library'),prompt=FALSE)
cat('Then run: python run_replication.py --r-library r-library\n')
