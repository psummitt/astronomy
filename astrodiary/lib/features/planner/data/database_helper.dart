import 'database_backend.dart';
// Resolves to the SQLite implementation only where `dart:io` exists, so the
// web build never compiles dart:ffi / sqflite / sqlite3.
import 'database_backend_stub.dart'
    if (dart.library.io) 'database_backend_io.dart';
import '../domain/planner_observation.dart';

class DatabaseHelper {
  static final DatabaseHelper _instance = DatabaseHelper._internal();
  factory DatabaseHelper() => _instance;
  DatabaseHelper._internal();

  final PlannerDatabaseBackend _backend = createPlannerDatabaseBackend();

  Future<int> insertObservation(PlannerObservation observation) async {
    return _backend.insertObservation(observation.toMap());
  }

  Future<List<PlannerObservation>> getObservations() async {
    final List<Map<String, Object?>> maps = await _backend.listObservations();
    return List.generate(
      maps.length,
      (int i) => PlannerObservation.fromMap(maps[i]),
    );
  }

  Future<int> deleteObservation(int id) async {
    return _backend.deleteObservation(id);
  }

  Future<void> clearAll() async {
    await _backend.clearObservations();
  }
}