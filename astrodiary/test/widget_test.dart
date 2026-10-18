import 'package:astrodiary/app/app.dart';
import 'package:astrodiary/app/router.dart';
import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:flutter_test/flutter_test.dart';

void main() {
  testWidgets('AstroDiary app loads with the astronomy shell', (tester) async {
    tester.view.physicalSize = const Size(1280, 1600);
    tester.view.devicePixelRatio = 1.0;
    addTearDown(() {
      tester.view.resetPhysicalSize();
      tester.view.resetDevicePixelRatio();
    });

    await tester.pumpWidget(
      const ProviderScope(
        child: AstroDiaryApp(),
      ),
    );

    expect(find.byType(MaterialApp), findsOneWidget);
  });

  // Deep links (browser refresh on /logbook/detail/:id, or a shared link)
  // arrive without `state.extra`. The route must redirect instead of throwing.
  testWidgets('deep link to a log without state.extra falls back to logbook',
      (tester) async {
    tester.view.physicalSize = const Size(1280, 1600);
    tester.view.devicePixelRatio = 1.0;
    addTearDown(() {
      tester.view.resetPhysicalSize();
      tester.view.resetDevicePixelRatio();
    });

    await tester.pumpWidget(
      const ProviderScope(
        child: AstroDiaryApp(),
      ),
    );
    await tester.pumpAndSettle();

    router.go('/logbook/detail/does-not-exist');
    await tester.pumpAndSettle();

    expect(tester.takeException(), isNull);
    expect(find.text('Observation Logbook'), findsOneWidget);
  });
}