import 'dart:convert';
import 'dart:io';
import 'package:csv/csv.dart';
import 'package:file_picker/file_picker.dart';
import 'package:flutter/foundation.dart';
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
    String content;
    if (file.path != null) {
      final diskFile = File(file.path!);
      content = await diskFile.readAsString();
    } else {
      final bytes = await file.readAsBytes();
      content = utf8.decode(bytes);
    }

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
