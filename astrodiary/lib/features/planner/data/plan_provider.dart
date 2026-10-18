import 'dart:convert';
import 'package:flutter/foundation.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:shared_preferences/shared_preferences.dart';
import '../domain/planner_observation.dart';
import 'database_helper.dart';

class PlanNotifier extends Notifier<List<PlannerObservation>> {
  final DatabaseHelper _dbHelper = DatabaseHelper();

  /// Monotonic counter backing [_nextWebId].
  int _webIdCursor = 0;

  @override
  List<PlannerObservation> build() {
    loadObservations();
    return [];
  }

  Future<void> loadObservations() async {
    if (kIsWeb) {
      final prefs = await SharedPreferences.getInstance();
      final String? data = prefs.getString('astrodiary_planner_observations');
      if (data != null) {
        final List<dynamic> decoded = jsonDecode(data);
        state = decoded.map((item) => PlannerObservation.fromMap(item)).toList();
      }
    } else {
      final list = await _dbHelper.getObservations();
      state = list;
    }
  }

  Future<void> addObservation(PlannerObservation observation) async {
    if (kIsWeb) {
      final newObs = PlannerObservation(
        id: _nextWebId(),
        targetName: observation.targetName,
        dateTime: observation.dateTime,
        ra: observation.ra,
        dec: observation.dec,
        notes: observation.notes,
      );
      state = _sorted([...state, newObs]);
      await _saveWeb();
    } else {
      await _dbHelper.insertObservation(observation);
      await loadObservations();
    }
  }

  Future<void> deleteObservation(int? id) async {
    if (id == null) return;
    if (kIsWeb) {
      state = state.where((obs) => obs.id != id).toList();
      await _saveWeb();
    } else {
      await _dbHelper.deleteObservation(id);
      await loadObservations();
    }
  }

  Future<void> importObservations(List<PlannerObservation> newObservations) async {
    if (kIsWeb) {
      // CSV rows carry no id. Every entry needs one, otherwise it is
      // undeletable (deleteObservation ignores null ids).
      final withIds = newObservations
          .map((obs) => PlannerObservation(
                id: _nextWebId(),
                targetName: obs.targetName,
                dateTime: obs.dateTime,
                ra: obs.ra,
                dec: obs.dec,
                notes: obs.notes,
              ))
          .toList();
      state = _sorted([...state, ...withIds]);
      await _saveWeb();
    } else {
      for (var obs in newObservations) {
        await _dbHelper.insertObservation(obs);
      }
      await loadObservations();
    }
  }

  /// Mirrors the native `ORDER BY dateTime ASC` so both platforms agree.
  List<PlannerObservation> _sorted(List<PlannerObservation> items) {
    final sorted = [...items];
    sorted.sort((a, b) => a.dateTime.compareTo(b.dateTime));
    return sorted;
  }

  /// Generates an id that is not already used. The cursor advances on every
  /// call so a batch (e.g. a CSV import) gets distinct ids even though `state`
  /// is not updated until the batch is complete.
  int _nextWebId() {
    final used = state.map((obs) => obs.id ?? -1).toSet();
    if (_webIdCursor == 0) {
      _webIdCursor = DateTime.now().millisecondsSinceEpoch;
    }
    while (used.contains(_webIdCursor)) {
      _webIdCursor++;
    }
    return _webIdCursor++;
  }

  Future<void> _saveWeb() async {
    final prefs = await SharedPreferences.getInstance();
    final String data = jsonEncode(state.map((obs) => obs.toMap()).toList());
    await prefs.setString('astrodiary_planner_observations', data);
  }
}

final planProvider =
    NotifierProvider<PlanNotifier, List<PlannerObservation>>(PlanNotifier.new);
