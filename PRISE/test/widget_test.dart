import 'package:flutter/material.dart';
import 'package:flutter_test/flutter_test.dart';

import 'package:prise/main.dart';

void main() {
  testWidgets('shows the calculator and accessible navigation', (tester) async {
    await tester.pumpWidget(const PriseApp());
    await tester.pumpAndSettle();

    expect(find.text('Find Mercury or Venus near the Sun'), findsOneWidget);
    expect(find.text('Update estimate'), findsOneWidget);
    expect(find.text('Instructions'), findsOneWidget);
    expect(find.text('About'), findsOneWidget);
  });

  testWidgets('changing the body updates the estimate immediately', (
    tester,
  ) async {
    await tester.pumpWidget(const PriseApp());
    await tester.pumpAndSettle();

    final initialDetail = tester
        .widget<Text>(find.byKey(const ValueKey('result-event-detail')))
        .data;
    await tester.tap(find.byType(DropdownButtonFormField<String>));
    await tester.pumpAndSettle();
    await tester.tap(find.text('Venus').last);
    await tester.pumpAndSettle();

    expect(
      tester.widget<Text>(find.byKey(const ValueKey('result-heading'))).data,
      startsWith('Venus on '),
    );
    expect(
      tester.widget<Text>(find.byKey(const ValueKey('result-event-detail'))).data,
      isNot(initialDetail),
    );
  });

  testWidgets('changing the date updates the estimate immediately', (
    tester,
  ) async {
    await tester.pumpWidget(const PriseApp());
    await tester.pumpAndSettle();
    final initialDate = DateTime.now();
    final targetDate = DateTime(initialDate.year, initialDate.month + 1, 1);
    final initialDetail = tester
      .widget<Text>(find.byKey(const ValueKey('result-event-detail')))
      .data;

    final dateLabel =
      '${initialDate.year}-${initialDate.month.toString().padLeft(2, '0')}-${initialDate.day.toString().padLeft(2, '0')}';
    await tester.tap(
      find.ancestor(
        of: find.text(dateLabel),
        matching: find.byType(InkWell),
      ),
    );
    await tester.pumpAndSettle();
    await tester.tap(find.byIcon(Icons.chevron_right));
    await tester.pumpAndSettle();
    await tester.tap(find.text('1').last);
    await tester.pumpAndSettle();
    await tester.tap(find.text('OK'));
    await tester.pumpAndSettle();

    expect(
      tester.widget<Text>(find.byKey(const ValueKey('result-heading'))).data,
      startsWith(
        'Mercury on ${targetDate.year}-${targetDate.month.toString().padLeft(2, '0')}-01',
      ),
    );
    expect(
      tester.widget<Text>(find.byKey(const ValueKey('result-event-detail'))).data,
      isNot(initialDetail),
    );
  });
}
