import 'package:astrodiary/features/planner/data/plan_provider.dart';
import 'package:astrodiary/features/planner/domain/planner_observation.dart';
import 'package:flutter/foundation.dart' show kIsWeb;
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:shared_preferences/shared_preferences.dart';

// CSV rows carry no id. On web the planner persists to SharedPreferences and
// has no database to assign one, so imported plans used to get `id == null`,
// which made them undeletable.
//
// Web-only (the native path uses SQLite). Run with:
//   flutter test --platform chrome
void main() {
  TestWidgetsFlutterBinding.ensureInitialized();

  test('CSV import on web assigns distinct ids and stays deletable',
      () async {
    expect(kIsWeb, isTrue,
        reason: 'this test targets the web persistence path');
    SharedPreferences.setMockInitialValues({});
    final container = ProviderContainer();
    addTearDown(container.dispose);

    final notifier = container.read(planProvider.notifier);
    await notifier.importObservations([
      PlannerObservation.fromCsvRow(
          ['M31', '2026-01-01 20:00', '00h 42m', '+41d', 'galaxy']),
      PlannerObservation.fromCsvRow(
          ['M42', '2026-01-02 21:00', '05h 35m', '-05d', 'nebula']),
      PlannerObservation.fromCsvRow(
          ['M13', '2026-01-03 22:00', '16h 41m', '+36d', 'cluster']),
    ]);

    final imported = container.read(planProvider);
    expect(imported.length, 3);
    expect(imported.every((o) => o.id != null), isTrue,
        reason: 'every imported plan needs an id');
    expect(imported.map((o) => o.id).toSet().length, 3,
        reason: 'ids must be distinct');

    // Chronological, matching the native `ORDER BY dateTime ASC`.
    expect(imported.map((o) => o.targetName).toList(),
        ['M31', 'M42', 'M13']);

    // Each imported entry must actually be removable.
    final firstId = imported.first.id;
    await notifier.deleteObservation(firstId);
    final afterDelete = container.read(planProvider);
    expect(afterDelete.length, 2);
    expect(afterDelete.every((o) => o.id != firstId), isTrue);
  }, testOn: 'chrome');

  test('web planner survives a reload with ids intact', () async {
    expect(kIsWeb, isTrue,
        reason: 'this test targets the web persistence path');
    SharedPreferences.setMockInitialValues({});
    final container = ProviderContainer();
    addTearDown(container.dispose);

    await container.read(planProvider.notifier).importObservations([
      PlannerObservation.fromCsvRow(
          ['Jupiter', '2026-02-01 21:00', '05h 00m', '+20d', 'planet']),
    ]);
    final persistedId = container.read(planProvider).single.id;

    // Re-read from SharedPreferences, as a page reload would.
    final reloaded = ProviderContainer();
    addTearDown(reloaded.dispose);
    await reloaded.read(planProvider.notifier).loadObservations();

    final restored = reloaded.read(planProvider);
    expect(restored.single.id, persistedId);

    // Still deletable after reload.
    await reloaded.read(planProvider.notifier).deleteObservation(restored.single.id);
    expect(reloaded.read(planProvider), isEmpty);
  }, testOn: 'chrome');
}