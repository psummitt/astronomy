import '../../../core/utils/firestore_timestamp_converter.dart';

class Target {
  final String name;
  final String? objectType;
  final String? catalogId;

  Target({required this.name, this.objectType, this.catalogId});

  Map<String, dynamic> toJson() => {
        'name': name,
        if (objectType != null) 'objectType': objectType,
        if (catalogId != null) 'catalogId': catalogId,
      };

  factory Target.fromJson(Map<String, dynamic> json) => Target(
        name: json['name'] ?? '',
        objectType: json['objectType'],
        catalogId: json['catalogId'],
      );
}

class Instrument {
  final String name;
  final String? type;
  final double? apertureMm;
  final double? focalLengthMm;

  Instrument({
    required this.name,
    this.type,
    this.apertureMm,
    this.focalLengthMm,
  });

  Map<String, dynamic> toJson() => {
        'name': name,
        if (type != null) 'type': type,
        if (apertureMm != null) 'apertureMm': apertureMm,
        if (focalLengthMm != null) 'focalLengthMm': focalLengthMm,
      };

  factory Instrument.fromJson(Map<String, dynamic> json) => Instrument(
        name: json['name'] ?? '',
        type: json['type'],
        apertureMm: (json['apertureMm'] as num?)?.toDouble(),
        focalLengthMm: (json['focalLengthMm'] as num?)?.toDouble(),
      );
}

class Weather {
  final String conditions;
  final double? temperatureC;
  final double? humidityPercent;
  final String? seeing;
  final String? transparency;

  Weather({
    required this.conditions,
    this.temperatureC,
    this.humidityPercent,
    this.seeing,
    this.transparency,
  });

  Map<String, dynamic> toJson() => {
        'conditions': conditions,
        if (temperatureC != null) 'temperatureC': temperatureC,
        if (humidityPercent != null) 'humidityPercent': humidityPercent,
        if (seeing != null) 'seeing': seeing,
        if (transparency != null) 'transparency': transparency,
      };

  factory Weather.fromJson(Map<String, dynamic> json) => Weather(
        conditions: json['conditions'] ?? '',
        temperatureC: (json['temperatureC'] as num?)?.toDouble(),
        humidityPercent: (json['humidityPercent'] as num?)?.toDouble(),
        seeing: json['seeing'],
        transparency: json['transparency'],
      );
}

class ObservationLog {
  final String id;
  final String userId;
  final DateTime observationDate;
  final DateTime startTime;
  final DateTime? endTime;
  final Weather weather;
  final Target target;
  final Instrument instrument;
  final List<String> software;
  final String notes;
  final DateTime? createdAt;
  final DateTime? updatedAt;

  ObservationLog({
    required this.id,
    required this.userId,
    required this.observationDate,
    required this.startTime,
    this.endTime,
    required this.weather,
    required this.target,
    required this.instrument,
    this.software = const [],
    required this.notes,
    this.createdAt,
    this.updatedAt,
  });

  Map<String, dynamic> toJson() => {
        'id': id,
        'userId': userId,
        'observationDate':
            FirestoreTimestampConverter.toJson(observationDate),
        'startTime': FirestoreTimestampConverter.toJson(startTime),
        if (endTime != null)
          'endTime': NullableFirestoreTimestampConverter.toJson(endTime),
        'weather': weather.toJson(),
        'target': target.toJson(),
        'instrument': instrument.toJson(),
        'software': software,
        'notes': notes,
        if (createdAt != null)
          'createdAt': NullableFirestoreTimestampConverter.toJson(createdAt),
        if (updatedAt != null)
          'updatedAt': NullableFirestoreTimestampConverter.toJson(updatedAt),
      };

  factory ObservationLog.fromJson(Map<String, dynamic> json, String docId) {
    return ObservationLog(
      id: docId,
      userId: json['userId'] ?? '',
      observationDate: FirestoreTimestampConverter.fromJson(
          json['observationDate']),
      startTime: FirestoreTimestampConverter.fromJson(json['startTime']),
      endTime: NullableFirestoreTimestampConverter.fromJson(json['endTime']),
      weather: Weather.fromJson(json['weather'] ?? {}),
      target: Target.fromJson(json['target'] ?? {}),
      instrument: Instrument.fromJson(json['instrument'] ?? {}),
      software: (json['software'] as List<dynamic>?)
              ?.map((e) => e.toString())
              .toList() ??
          [],
      notes: json['notes'] ?? '',
      createdAt:
          NullableFirestoreTimestampConverter.fromJson(json['createdAt']),
      updatedAt:
          NullableFirestoreTimestampConverter.fromJson(json['updatedAt']),
    );
  }
}
