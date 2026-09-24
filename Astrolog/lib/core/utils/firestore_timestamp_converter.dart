import 'package:cloud_firestore/cloud_firestore.dart';

class FirestoreTimestampConverter {
  const FirestoreTimestampConverter();

  static DateTime fromJson(dynamic json) {
    if (json is Timestamp) {
      return json.toDate();
    } else if (json is String) {
      return DateTime.parse(json);
    } else if (json is int) {
      return DateTime.fromMillisecondsSinceEpoch(json);
    }
    return DateTime.now();
  }

  static dynamic toJson(DateTime date) {
    return Timestamp.fromDate(date);
  }
}

class NullableFirestoreTimestampConverter {
  const NullableFirestoreTimestampConverter();

  static DateTime? fromJson(dynamic json) {
    if (json == null) return null;
    return FirestoreTimestampConverter.fromJson(json);
  }

  static dynamic toJson(DateTime? date) {
    if (date == null) return null;
    return FirestoreTimestampConverter.toJson(date);
  }
}
