
#
#' @export
getTranslator <- function(id, lang = 'pt') {
  path <- system.file("i18n", paste0(id, ".json"), package="rshinystatistics")
  i18n <- shiny.i18n::Translator$new(translation_json_path = path)
  i18n$set_translation_language(lang)
  return(i18n$t)
}
