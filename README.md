# Your Complicated Family app

This repository is for modifying the apps for 3-person families.

To change any of the formula parameters, open the file `helpers/score-parameters.R` and make the necessary adjustments there.

By default, the report PDF is freely downloadable. If the report needs to be password-protected, you can create a file named `reportpassword.txt` that contains the password, and then the report will only be available after inputting the correct password.

To enable development mode, set a variable `DEV <- TRUE` in your environment. In development mode you will see a few of the results tables in real-time.

In UI.R I have hashtaged-out the function that writes two of the output files to the user screen, the z scores and the observed scores (relationship_x).  The family table with names and ages can still be observed by user.  The buttons for downloading all files are still there and they work.

