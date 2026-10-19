/// Storage abstraction for planner observations.
///
/// The planner persists to SQLite on desktop/mobile but to SharedPreferences
/// on the web (see `plan_provider.dart`). Keeping the SQLite types behind this
/// interface lets the web build skip `dart:io`, `dart:ffi` and
/// `sqflite_common_ffi` entirely, instead of shipping them behind a runtime
/// `kIsWeb` check. In debug mode every dependency is a separate module the
/// browser must fetch before the app boots, so trimming this matters.
abstract class PlannerDatabaseBackend {
  /// Inserts one observation. [values] may contain a null `id` so the
  /// database assigns one.
  Future<int> insertObservation(Map<String, Object?> values);

  /// All observations, oldest first.
  Future<List<Map<String, Object?>>> listObservations();

  /// Deletes the observation with [id]; returns the number of rows removed.
  Future<int> deleteObservation(int id);

  /// Removes every observation; returns the number of rows removed.
  Future<int> clearObservations();
}