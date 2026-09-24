import 'dart:async';
import 'dart:io' show Platform;
import 'package:flutter/foundation.dart';
import 'package:path/path.dart';
import 'package:sqflite_common_ffi/sqflite_ffi.dart';
import '../domain/planner_observation.dart';

class DatabaseHelper {
  static final DatabaseHelper _instance = DatabaseHelper._internal();
  factory DatabaseHelper() => _instance;
  DatabaseHelper._internal();

  Database? _database;

  Future<Database?> get database async {
    if (kIsWeb) return null;
    if (_database != null) return _database!;
    _database = await _initDatabase();
    return _database;
  }

  Future<Database?> _initDatabase() async {
    if (kIsWeb) {
      return null;
    }

    if (Platform.isWindows || Platform.isLinux) {
      sqfliteFfiInit();
      databaseFactory = databaseFactoryFfi;
    }

    final dbPath = await getDatabasesPath();
    final path = join(dbPath, 'astrolog_planner.db');

    return await openDatabase(
      path,
      version: 1,
      onCreate: _onCreate,
    );
  }

  Future _onCreate(Database db, int version) async {
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
  }

  Future<int> insertObservation(PlannerObservation observation) async {
    final db = await database;
    if (db == null) return 0;
    return await db.insert('planner_observations', observation.toMap());
  }

  Future<List<PlannerObservation>> getObservations() async {
    final db = await database;
    if (db == null) return [];
    final List<Map<String, dynamic>> maps =
        await db.query('planner_observations', orderBy: 'dateTime ASC');
    return List.generate(maps.length, (i) => PlannerObservation.fromMap(maps[i]));
  }

  Future<int> deleteObservation(int id) async {
    final db = await database;
    if (db == null) return 0;
    return await db.delete('planner_observations', where: 'id = ?', whereArgs: [id]);
  }

  Future<void> clearAll() async {
    final db = await database;
    if (db == null) return;
    await db.delete('planner_observations');
  }
}
