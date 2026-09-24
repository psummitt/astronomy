import 'dart:convert';
import 'package:flutter/foundation.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:shared_preferences/shared_preferences.dart';
import '../domain/planner_observation.dart';
import 'database_helper.dart';

class PlanNotifier extends StateNotifier<List<PlannerObservation>> {
  PlanNotifier() : super([]) {
    loadObservations();
  }

  final DatabaseHelper _dbHelper = DatabaseHelper();

  Future<void> loadObservations() async {
    if (kIsWeb) {
      final prefs = await SharedPreferences.getInstance();
      final String? data = prefs.getString('planner_observations');
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
        id: DateTime.now().millisecondsSinceEpoch,
        targetName: observation.targetName,
        dateTime: observation.dateTime,
        ra: observation.ra,
        dec: observation.dec,
        notes: observation.notes,
      );
      state = [...state, newObs];
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
      state = [...state, ...newObservations];
      await _saveWeb();
    } else {
      for (var obs in newObservations) {
        await _dbHelper.insertObservation(obs);
      }
      await loadObservations();
    }
  }

  Future<void> _saveWeb() async {
    final prefs = await SharedPreferences.getInstance();
    final String data = jsonEncode(state.map((obs) => obs.toMap()).toList());
    await prefs.setString('planner_observations', data);
  }
}

final planProvider =
    StateNotifierProvider<PlanNotifier, List<PlannerObservation>>((ref) {
  return PlanNotifier();
});
