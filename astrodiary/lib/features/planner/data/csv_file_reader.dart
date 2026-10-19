import 'dart:io';

/// Reads a file picked from disk on platforms that have `dart:io`.
///
/// The web build gets `csv_file_reader_stub.dart` instead, which returns null
/// so the caller falls back to the bytes that `file_picker` already provides.
Future<String?> readPickedTextFile(String path) async {
  final file = File(path);
  if (!await file.exists()) return null;
  return file.readAsString();
}