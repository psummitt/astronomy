import 'package:flutter_test/flutter_test.dart';
import 'package:eclipse/logic/modern_calculator.dart';
import 'package:eclipse/logic/eclipse_event.dart';

import 'package:flutter/foundation.dart';

void main() {
  test('ModernCalculator 2024 check includes Penumbral eclipse', () {
    final calculator = ModernCalculator();
    final events = calculator.calculateEclipses(2024);
    
    debugPrint('--- Modern 2024 Search Results ---');
    for (var e in events) {
      debugPrint('${e.date} Type: ${e.type} UmbralMag: ${e.magnitude} PenumbralMag: ${e.penumbralMagnitude}');
    }

    // 2024 Mar 25 is a Penumbral Lunar Eclipse
    expect(events.any((e) => e.date.year == 2024 && e.date.month == 3 && e.type == EclipseType.penumbral), true);
  });

  test('ModernCalculator 2026 check all eclipses', () {
    final calculator = ModernCalculator();
    final events = calculator.calculateEclipses(2026);
    
    debugPrint('--- Modern 2026 Search Results ---');
    for (var e in events) {
      debugPrint('${e.date} Type: ${e.type} Mag: ${e.magnitude}');
    }
    
    // 2026 Mar 03 Total, 2026 Aug 28 Partial
    expect(events.any((e) => e.type == EclipseType.total), true);
    expect(events.any((e) => e.type == EclipseType.partial), true);
  });
}
