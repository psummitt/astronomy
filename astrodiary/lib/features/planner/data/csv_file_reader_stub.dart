/// Web stand-in for `csv_file_reader.dart`.
///
/// Returns null so `CsvHandler` uses the bytes `file_picker` already loaded,
/// which is the only option in a browser.
Future<String?> readPickedTextFile(String path) async => null;