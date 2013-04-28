set loc en

::msgcat::mcset $loc UpgradeTool

::msgcat::mcset $loc err-invalid-dir \
"The directory you entered is not a valid directory.\n\n    %s"

::msgcat::mcset $loc err-same-dir \
"The old and new directories are the same."

::msgcat::mcset $loc err-weird-dir \
"UpgradeTool cannot determine the game version from the directory\
name \"%s\". The directory name should look like \"AngbandTk-123r4\".\
You can determine the version of AngbandTk you are playing by\
looking on the startup screen."

::msgcat::mcset $loc err-diff \
"UpgradeTool cannot upgrade from %s to %s."

::msgcat::mcset $loc err-swap \
"UpgradeTool cannot downgrade from a new version to an old version."

::msgcat::mcset $loc err-unsupported \
"UpgradeTool does not support version %s of %s."

::msgcat::mcset $loc err-missing-dir \
"UpgradeTool cannot find the following directory:\n\n    %s\n\n\
Please make sure %s was installed properly."

::msgcat::mcset $loc err-open-log \
"The following error occured while attempting to open\
the file \"%s\" for writing:\n\n%s"

::msgcat::mcset $loc msg-complete \
"UpgradeTool has finished. You should quit UpgradeTool now and try the\
new version of %s. If there were problems with the upgrade, send\
an e-mail to omnibandtk@shaw.ca. Be sure to attach the file %s\
to your message."

