import 'dart:convert';

import 'package:csv/csv.dart';
import 'package:file_picker/file_picker.dart';
import 'package:flutter/foundation.dart';
// Keeps `dart:io` out of the web build; the web stub returns null.
import 'csv_file_reader_stub.dart' if (dart.library.io) 'csv_file_reader.dart';
import '../domain/planner_observation.dart';

class CsvHandler {
  static Future<bool> exportToCsv(List<PlannerObservation> observations) async {
    final rows = <List<dynamic>>[
      PlannerObservation.csvHeaders,
      ...observations.map((obs) => obs.toCsvRow()),
    ];
    final csv = Csv();
    final csvData = csv.encode(rows);

    final result = await FilePicker.saveFile(
      dialogTitle: 'Save your observation plan',
      fileName: 'astrodiary_plan.csv',
      bytes: utf8.encode(csvData),
      type: FileType.custom,
      allowedExtensions: ['csv'],
    );
    return result != null;
  }

  static Future<List<PlannerObservation>?> importFromCsv() async {
    final result = await FilePicker.pickFiles(
      type: FileType.custom,
      allowedExtensions: ['csv'],
    );

    if (result.isEmpty) {
      return null;
    }

    final file = result.first;

    // Prefer a direct disk read on platforms that have one; otherwise decode
    // the bytes file_picker already loaded (the only option in a browser).
    String? content;
    if (file.path != null) {
      content = await readPickedTextFile(file.path!);
    }
    content ??= utf8.decode(await file.readAsBytes());

    final csv = Csv();
    final rows = csv.decode(content);
    if (rows.length <= 1) return [];

    final observations = <PlannerObservation>[];
    for (var i = 1; i < rows.length; i++) {
      try {
        if (rows[i].isNotEmpty) {
          observations.add(PlannerObservation.fromCsvRow(rows[i]));
        }
      } catch (e) {
        debugPrint('Error parsing CSV row $i: $e');
      }
    }
    return observations;
  }
}
