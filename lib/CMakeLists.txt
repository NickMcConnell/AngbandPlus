INSTALL(DIRECTORY 
  apex
  cmov
  data
  dngn
  edit
  file
  help
  info
  note
  pref
  save
  user
  xtra
  DESTINATION ${DEFAULT_PATH}
  PATTERN "delete.me" EXCLUDE
  )

ADD_SUBDIRECTORY (mods)
