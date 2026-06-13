import 'package:flutter_test/flutter_test.dart';
import 'package:easter/easter_algorithms.dart';
import 'package:easter/main.dart';

void main() {
  group('Easter Algorithm Tests', () {
    test('Standard Gregorian algorithm checks', () {
      expect(standardEaster(1974), EasterDate(year: 1974, monthName: 'APRIL', month: 4, day: 14));
      expect(standardEaster(1984), EasterDate(year: 1984, monthName: 'APRIL', month: 4, day: 22));
      expect(standardEaster(1994), EasterDate(year: 1994, monthName: 'APRIL', month: 4, day: 3));
      expect(standardEaster(2026), EasterDate(year: 2026, monthName: 'APRIL', month: 4, day: 5));
      expect(standardEaster(2038), EasterDate(year: 2038, monthName: 'APRIL', month: 4, day: 25));
    });

    test('Quirky Burgess floated algorithm checks (including 1902 mismatch & overrides)', () {
      // 1902 has a mismatch: Standard is March 30, Burgess is March 23
      expect(burgessEaster(1902), EasterDate(year: 1902, monthName: 'MARCH', month: 3, day: 23));
      
      // 1974, 1984, and 1994 have manual overrides to match standard dates
      expect(burgessEaster(1974), EasterDate(year: 1974, monthName: 'APRIL', month: 4, day: 14));
      expect(burgessEaster(1984), EasterDate(year: 1984, monthName: 'APRIL', month: 4, day: 29));
      expect(burgessEaster(1994), EasterDate(year: 1994, monthName: 'APRIL', month: 4, day: 3));
      
      // 2021 has a mismatch: Standard is April 4, Burgess is March 28
      expect(burgessEaster(2021), EasterDate(year: 2021, monthName: 'MARCH', month: 3, day: 28));
      
      // 2025 has a mismatch: Standard is April 20, Burgess is April 13
      expect(burgessEaster(2025), EasterDate(year: 2025, monthName: 'APRIL', month: 4, day: 13));
    });

    test('Simplified integer Burgess algorithm matches Standard Gregorian 100% of the time', () {
      for (int year = 1900; year <= 2099; year++) {
        final std = standardEaster(year);
        final sim = simplifiedIntegerEaster(year);
        expect(sim, std, reason: 'Simplified integer algorithm mismatch for year $year');
      }
    });
  });

  group('Widget Rendering Tests', () {
    testWidgets('App renders MainNavigationShell with Modern Calculator view initially', (WidgetTester tester) async {
      await tester.pumpWidget(const EasterApp());
      
      // Verify shell app bar title
      expect(find.text('Easter Computus 1982'), findsOneWidget);
      
      // Verify Dashboard button exists and is active
      expect(find.text('Analysis Dashboard'), findsOneWidget);
      expect(find.text('Retro BASIC Terminal'), findsOneWidget);
      
      // Verify Modern Calculator elements render
      expect(find.text('Select Calendar Year'), findsOneWidget);
      expect(find.text('STANDARD GREGORIAN'), findsOneWidget);
      expect(find.text('ORIGINAL BASIC'), findsOneWidget);
      expect(find.text('Analysis of the Algorithm Bug'), findsOneWidget);
    });

    testWidgets('App can switch to Retro Terminal view', (WidgetTester tester) async {
      await tester.pumpWidget(const EasterApp());
      
      // Switch to Retro Terminal
      await tester.tap(find.text('Retro BASIC Terminal'));
      await tester.pumpAndSettle();
      
      // Verify terminal view renders and is waiting for year input
      expect(find.byType(RetroTerminal), findsOneWidget);
      expect(find.textContaining('YEAR REQUIRED'), findsOneWidget);
    });
  });
}
