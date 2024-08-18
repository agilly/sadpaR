VERBOSE=F
DEBUG=F
## LANGUAGE SETTINGS
language=ini::read.ini("sadpar.ini")$app_config$language
cli::cli_inform("Using language {language}")
appLang=config::get(file="lang.yml", config=language)
