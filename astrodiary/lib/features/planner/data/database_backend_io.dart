import 'dart:io' show Platform;

import 'package:path/path.dart' as p;
import 'package:sqflite_common_ffi/sqflite_ffi.dart';

import 'database_backend.dart';

/// SQLite backend for platforms with `dart:io`.
class IoPlannerDatabaseBackend implements PlannerDatabaseBackend {
  Database? _database;

  Future<Database> get _db async {
    if (_database != null) return _database!;
    _database = await _open();
    return _database!;
  }

  Future<Database> _open() async {
    if (Platform.isWindows || Platform.isLinux) {
      sqfliteFfiInit();
      databaseFactory = databaseFactoryFfi;
    }

    final String dbPath = await getDatabasesPath();
    return openDatabase(
      p.join(dbPath, 'astrodiary_planner.db'),
      version: 1,
      onCreate: (Database db, int version) async {
        await db.execute('''
          CREATE TABLE planner_observations (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            targetName TEXT,
            dateTime TEXT,
            ra TEXT,
            dec TEXT,
            notes TEXT
          )
        ''');
      },
    );
  }

  @override
  Future<int> insertObservation(Map<String, Object?> values) async {
    return (await _db).insert('planner_observations', values);
  }

  @override
  Future<List<Map<String, Object?>>> listObservations() async {
    return (await _db).query(
      'planner_observations',
      orderBy: 'dateTime ASC',
    );
  }

  @override
  Future<int> deleteObservation(int id) async {
    return (await _db).delete(
      'planner_observations',
      where: 'id = ?',
      whereArgs: [id],
    );
  }

  @override
  Future<int> clearObservations() async {
    return (await _db).delete('planner_observations');
  }
}

/// Selected by the conditional import in `database_helper.dart`.
PlannerDatabaseBackend createPlannerDatabaseBackend() =>
    IoPlannerDatabaseBackend();