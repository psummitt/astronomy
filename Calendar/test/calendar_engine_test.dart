import 'package:flutter_test/flutter_test.dart';
import 'package:calendar_app/utils/calendar_engine.dart';

void main() {
  group('CalendarEngine tests', () {
    test('Leap Year calculations', () {
      expect(CalendarEngine.isLeapYear(2000), isTrue); // divisible by 400
      expect(CalendarEngine.isLeapYear(1900), isFalse); // divisible by 100 but not 400
      expect(CalendarEngine.isLeapYear(2024), isTrue); // divisible by 4
      expect(CalendarEngine.isLeapYear(2025), isFalse); // not divisible by 4
      expect(CalendarEngine.isLeapYear(1700), isFalse); // before 1752 limit
    });

    test('Days in Month', () {
      expect(CalendarEngine.getDaysInMonth(2026, 1), equals(31)); // Jan
      expect(CalendarEngine.getDaysInMonth(2026, 2), equals(28)); // Feb non-leap
      expect(CalendarEngine.getDaysInMonth(2024, 2), equals(29)); // Feb leap
      expect(CalendarEngine.getDaysInMonth(2026, 4), equals(30)); // Apr
      expect(CalendarEngine.getDaysInMonth(2026, 12), equals(31)); // Dec
    });

    test('Day of Year & Days Remaining', () {
      final dateNonLeapStart = DateTime(2026, 1, 1);
      final dateNonLeapEnd = DateTime(2026, 12, 31);
      final dateLeapMid = DateTime(2024, 3, 1); // March 1st in leap year is day 61 (31+29+1)

      expect(CalendarEngine.getDayOfYear(dateNonLeapStart), equals(1));
      expect(CalendarEngine.getDaysRemainingInYear(dateNonLeapStart), equals(364));

      expect(CalendarEngine.getDayOfYear(dateNonLeapEnd), equals(365));
      expect(CalendarEngine.getDaysRemainingInYear(dateNonLeapEnd), equals(0));

      expect(CalendarEngine.getDayOfYear(dateLeapMid), equals(61));
      expect(CalendarEngine.getDaysRemainingInYear(dateLeapMid), equals(305));
    });

    test('Gregorian Easter calculations', () {
      // Reference Easter Sundays:
      // 2026: April 5
      // 2027: March 28
      // 2030: April 21
      expect(CalendarEngine.calculateEaster(2026), equals(DateTime(2026, 4, 5)));
      expect(CalendarEngine.calculateEaster(2027), equals(DateTime(2027, 3, 28)));
      expect(CalendarEngine.calculateEaster(2030), equals(DateTime(2030, 4, 21)));
    });

    test('Find N-th Weekday', () {
      // 1st Monday of Nov 2026 is Nov 2
      // 4th Thursday of Nov 2026 is Nov 26 (Thanksgiving)
      // Last Monday of May 2026 is May 25 (Memorial Day)
      expect(CalendarEngine.findNthWeekday(2026, 11, DateTime.monday, 1), equals(DateTime(2026, 11, 2)));
      expect(CalendarEngine.findNthWeekday(2026, 11, DateTime.thursday, 4), equals(DateTime(2026, 11, 26)));
      expect(CalendarEngine.findNthWeekday(2026, 5, DateTime.monday, -1), equals(DateTime(2026, 5, 25)));
    });

    test('Calculate Standard Holidays', () {
      final list2026 = CalendarEngine.calculateStandardHolidays(2026);
      
      // Verify New Year's Day (Jan 1)
      final newYear = list2026.firstWhere((h) => h.id == 'std_new_year');
      expect(newYear.month, equals(1));
      expect(newYear.day, equals(1));

      // Verify Thanksgiving (Nov 26, 2026)
      final thanksgiving = list2026.firstWhere((h) => h.id == 'std_thanksgiving');
      expect(thanksgiving.month, equals(11));
      expect(thanksgiving.day, equals(26));

      // Verify Easter (Apr 5, 2026)
      final easter = list2026.firstWhere((h) => h.id == 'std_easter');
      expect(easter.month, equals(4));
      expect(easter.day, equals(5));

      // Verify Good Friday (Apr 3, 2026)
      final goodFriday = list2026.firstWhere((h) => h.id == 'std_good_friday');
      expect(goodFriday.month, equals(4));
      expect(goodFriday.day, equals(3));
    });
  });
}
