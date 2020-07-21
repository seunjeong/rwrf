# rwrf

Some useful functions for WRF modeling and post-run analysis.

# Just for the record

When using RStudio, do the following:

1) create a new project for "package" (then we have man, R, NAMESPACE etc.)

2) create a repository from github Web

3) then do the following


> …or create a new repository on the command line

> echo "# rwrf" >> README.md

> git init

> git add README.md

> git commit -m "first commit"

> git remote add origin https://github.com/seunjeong/rwrf.git

> git push -u origin master
                
or

> …or push an existing repository from the command line

>git remote add origin https://github.com/seunjeong/rwrf.git

>git push -u origin master

If you see a Warning message (shown below) from RStudio, then delete the NAMESPACE file and do the following:

> devtools::document()

Warning: The existing 'NAMESPACE' file was not generated by roxygen2, and will not be overwritten.

