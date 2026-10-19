import 'database_backend.dart';

/// No-op backend used on the web, where the planner persists to
/// SharedPreferences instead of SQLite.
///
/// Exists so the web build never compiles `dart:io`, `dart:ffi` or
/// `sqflite_common_ffi`.
class StubPlannerDatabaseBackend implements PlannerDatabaseBackend {
  @override
  Future<int> insertObservation(Map<String, Object?> values) async => 0;

  @override
  Future<List<Map<String, Object?>>> listObservations() async => const [];

  @override
  Future<int> deleteObservation(int id) async => 0;

  @override
  Future<int> clearObservations() async => 0;
}

/// Selected by the conditional import in `database_helper.dart`.
PlannerDatabaseBackend createPlannerDatabaseBackend() =>
    StubPlannerDatabaseBackend();