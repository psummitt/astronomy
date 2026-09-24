import 'package:intl/intl.dart';

class PlannerObservation {
  final int? id;
  final String targetName;
  final DateTime dateTime;
  final String ra;
  final String dec;
  final String notes;

  PlannerObservation({
    this.id,
    required this.targetName,
    required this.dateTime,
    required this.ra,
    required this.dec,
    required this.notes,
  });

  Map<String, dynamic> toMap() {
    return {
      'id': id,
      'targetName': targetName,
      'dateTime': dateTime.toIso8601String(),
      'ra': ra,
      'dec': dec,
      'notes': notes,
    };
  }

  factory PlannerObservation.fromMap(Map<String, dynamic> map) {
    return PlannerObservation(
      id: map['id'],
      targetName: map['targetName'] ?? '',
      dateTime: DateTime.tryParse(map['dateTime'] ?? '') ?? DateTime.now(),
      ra: map['ra'] ?? '',
      dec: map['dec'] ?? '',
      notes: map['notes'] ?? '',
    );
  }

  List<dynamic> toCsvRow() {
    final dateFormat = DateFormat('yyyy-MM-dd HH:mm');
    return [
      targetName,
      dateFormat.format(dateTime),
      ra,
      dec,
      notes,
    ];
  }

  factory PlannerObservation.fromCsvRow(List<dynamic> row) {
    final dateFormat = DateFormat('yyyy-MM-dd HH:mm');
    return PlannerObservation(
      targetName: row.isNotEmpty ? row[0].toString() : '',
      dateTime: row.length > 1 ? (DateTime.tryParse(row[1].toString()) ?? dateFormat.parse(row[1].toString())) : DateTime.now(),
      ra: row.length > 2 ? row[2].toString() : '',
      dec: row.length > 3 ? row[3].toString() : '',
      notes: row.length > 4 ? row[4].toString() : '',
    );
  }

  static List<String> get csvHeaders => [
    'Target Name',
    'Date Time (yyyy-MM-dd HH:mm)',
    'RA',
    'Dec',
    'Notes'
  ];
}
