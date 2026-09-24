import 'dart:convert';
import 'dart:io';
import 'package:csv/csv.dart';
import 'package:file_picker/file_picker.dart';
import 'package:flutter/foundation.dart';
import '../domain/planner_observation.dart';

class CsvHandler {
  static Future<bool> exportToCsv(List<PlannerObservation> observations) async {
    List<List<dynamic>> rows = [];
    rows.add(PlannerObservation.csvHeaders);
    for (var obs in observations) {
      rows.add(obs.toCsvRow());
    }

    String csvData = const ListToCsvConverter().convert(rows);

    if (kIsWeb) {
      final result = await FilePicker.platform.saveFile(
        fileName: 'astrolog_plan.csv',
        bytes: utf8.encode(csvData),
      );
      return result != null;
    } else {
      String? outputFile = await FilePicker.platform.saveFile(
        dialogTitle: 'Save your observation plan',
        fileName: 'astrolog_plan.csv',
        type: FileType.custom,
        allowedExtensions: ['csv'],
      );

      if (outputFile != null) {
        final file = File(outputFile);
        await file.writeAsString(csvData);
        return true;
      }
    }
    return false;
  }

  static Future<List<PlannerObservation>?> importFromCsv() async {
    FilePickerResult? result = await FilePicker.platform.pickFiles(
      type: FileType.custom,
      allowedExtensions: ['csv'],
      withData: true,
    );

    if (result != null) {
      String content;
      if (kIsWeb) {
        content = utf8.decode(result.files.first.bytes!);
      } else {
        final file = File(result.files.first.path!);
        content = await file.readAsString();
      }

      List<List<dynamic>> rows = const CsvToListConverter().convert(content);
      if (rows.length <= 1) return [];

      List<PlannerObservation> observations = [];
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
    return null;
  }
}
