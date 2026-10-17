# icesASD 0.1.0

* New writeASDxml() writes the catch scenarios XML file to upload at
  https://asd.ices.dk, matching the file produced by the ICES catch
  scenarios Excel template, with correct XML escaping and UTF-8 encoding.
* New asdInfo(), asdColumns(), asdRows() and asdNotes() create the parts of
  the table. Column and row IDs follow the Excel template numbering.
* New checkASD() checks codes against the ICES vocabularies (via icesVocab),
  the layout of the table, and that footnote symbols match the notes.
  writeASDxml() runs it before writing.
* Optional CatchScenarioVariables (ICES StockDataType codes) for each column.
* New vignette: "Creating a catch scenarios upload file".
* createASDxml() is deprecated.
