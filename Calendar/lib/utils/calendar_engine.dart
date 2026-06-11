import '../models/holiday.dart';

class CalendarEngine {
  /// Check if a year is a leap year (Gregorian calendar rules)
  static bool isLeapYear(int year) {
    if (year < 1752) return false;
    return (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0);
  }

  /// Get total days in a month for a specific year
  static int getDaysInMonth(int year, int month) {
    if (month < 1 || month > 12) return 0;
    const days = [0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
    if (month == 2 && isLeapYear(year)) {
      return 29;
    }
    return days[month];
  }

  /// Get the day of the year (1-365 or 1-366)
  static int getDayOfYear(DateTime date) {
    final year = date.year;
    int dayOfYear = 0;
    for (int m = 1; m < date.month; m++) {
      dayOfYear += getDaysInMonth(year, m);
    }
    dayOfYear += date.day;
    return dayOfYear;
  }

  /// Get the days remaining in the year
  static int getDaysRemainingInYear(DateTime date) {
    final totalDays = isLeapYear(date.year) ? 366 : 365;
    return totalDays - getDayOfYear(date);
  }

  /// Helper to find the N-th occurrence of a weekday in a month.
  /// [n] can be 1, 2, 3, 4.
  /// If [n] is -1, it returns the LAST occurrence of that weekday in the month.
  /// [weekday] corresponds to [DateTime.monday] (1) through [DateTime.sunday] (7).
  static DateTime findNthWeekday(int year, int month, int weekday, int n) {
    if (n > 0) {
      DateTime first = DateTime(year, month, 1);
      int offset = weekday - first.weekday;
      if (offset < 0) offset += 7;
      return DateTime(year, month, 1 + offset + (n - 1) * 7);
    } else {
      // Last weekday of the month
      int daysInMonth = getDaysInMonth(year, month);
      DateTime last = DateTime(year, month, daysInMonth);
      int offset = last.weekday - weekday;
      if (offset < 0) offset += 7;
      return DateTime(year, month, daysInMonth - offset);
    }
  }

  /// Meeus/Jones/Butcher algorithm to calculate Gregorian Easter
  static DateTime calculateEaster(int year) {
    int a = year % 19;
    int b = year ~/ 100;
    int c = year % 100;
    int d = b ~/ 4;
    int e = b % 4;
    int f = (b + 8) ~/ 25;
    int g = (b - f + 1) ~/ 3;
    int h = (19 * a + b - d - g + 15) % 30;
    int i = c ~/ 4;
    int k = c % 4;
    int l = (32 + 2 * e + 2 * i - h - k) % 7;
    int m = (a + 11 * h + 22 * l) ~/ 451;
    int month = (h + l - 7 * m + 114) ~/ 31;
    int day = ((h + l - 7 * m + 114) % 31) + 1;
    return DateTime(year, month, day);
  }

  /// Calculate the list of standard holidays for a given year.
  /// These match the holidays from the original BASIC program.
  static List<Holiday> calculateStandardHolidays(int year) {
    List<Holiday> holidays = [];

    // Easter
    DateTime easter = calculateEaster(year);

    // 1. New Year's Day (Jan 1)
    holidays.add(Holiday(id: 'std_new_year', name: "New Year's Day", month: 1, day: 1));

    // 2. Lincoln's Birthday (Feb 12)
    holidays.add(Holiday(id: 'std_lincoln', name: "Lincoln's Birthday", month: 2, day: 12));

    // 3. Washington's Birthday (3rd Monday in Feb)
    DateTime washington = findNthWeekday(year, 2, DateTime.monday, 3);
    holidays.add(Holiday(id: 'std_washington', name: "Washington's Birthday", month: 2, day: washington.day));

    // 4. Valentine's Day (Feb 14)
    holidays.add(Holiday(id: 'std_valentine', name: "Valentine's Day", month: 2, day: 14));

    // 5. Ash Wednesday (Easter - 46 days)
    DateTime ashWednesday = easter.subtract(const Duration(days: 46));
    holidays.add(Holiday(id: 'std_ash_wednesday', name: "Ash Wednesday", month: ashWednesday.month, day: ashWednesday.day));

    // 6. St. Patrick's Day (Mar 17)
    holidays.add(Holiday(id: 'std_st_patrick', name: "St. Patrick's Day", month: 3, day: 17));

    // 7. Good Friday (Easter - 2 days)
    DateTime goodFriday = easter.subtract(const Duration(days: 2));
    holidays.add(Holiday(id: 'std_good_friday', name: "Good Friday", month: goodFriday.month, day: goodFriday.day));

    // 8. Easter Sunday
    holidays.add(Holiday(id: 'std_easter', name: "Easter Sunday", month: easter.month, day: easter.day));

    // 9. Mother's Day (2nd Sunday in May)
    DateTime mothersDay = findNthWeekday(year, 5, DateTime.sunday, 2);
    holidays.add(Holiday(id: 'std_mothers_day', name: "Mother's Day", month: 5, day: mothersDay.day));

    // 10. Memorial Day (Last Monday in May)
    DateTime memorialDay = findNthWeekday(year, 5, DateTime.monday, -1);
    holidays.add(Holiday(id: 'std_memorial_day', name: "Memorial Day", month: 5, day: memorialDay.day));

    // 11. Independence Day (July 4)
    holidays.add(Holiday(id: 'std_independence', name: "Independence Day", month: 7, day: 4));

    // 12. Labor Day (1st Monday in Sep)
    DateTime laborDay = findNthWeekday(year, 9, DateTime.monday, 1);
    holidays.add(Holiday(id: 'std_labor', name: "Labor Day", month: 9, day: laborDay.day));

    // 13. Columbus Day (2nd Monday in Oct)
    DateTime columbusDay = findNthWeekday(year, 10, DateTime.monday, 2);
    holidays.add(Holiday(id: 'std_columbus', name: "Columbus Day", month: 10, day: columbusDay.day));

    // 14. Election Day (Tuesday after 1st Monday in Nov)
    DateTime firstMondayNov = findNthWeekday(year, 11, DateTime.monday, 1);
    DateTime electionDay = firstMondayNov.add(const Duration(days: 1));
    holidays.add(Holiday(id: 'std_election', name: "Election Day", month: 11, day: electionDay.day));

    // 15. Veteran's Day (Nov 11)
    holidays.add(Holiday(id: 'std_veterans', name: "Veteran's Day", month: 11, day: 11));

    // 16. Thanksgiving (4th Thursday in Nov)
    DateTime thanksgiving = findNthWeekday(year, 11, DateTime.thursday, 4);
    holidays.add(Holiday(id: 'std_thanksgiving', name: "Thanksgiving", month: 11, day: thanksgiving.day));

    // 17. Christmas Day (Dec 25)
    holidays.add(Holiday(id: 'std_christmas', name: "Christmas Day", month: 12, day: 25));

    // 18. Boxing Day (Dec 26)
    holidays.add(Holiday(id: 'std_boxing', name: "Boxing Day", month: 12, day: 26));

    return holidays;
  }

  static const List<String> monthNames = [
    "",
    "JANUARY",
    "FEBRUARY",
    "MARCH",
    "APRIL",
    "MAY",
    "JUNE",
    "JULY",
    "AUGUST",
    "SEPTEMBER",
    "OCTOBER",
    "NOVEMBER",
    "DECEMBER"
  ];

  static const List<String> shortMonthNames = [
    "",
    "JAN",
    "FEB",
    "MAR",
    "APR",
    "MAY",
    "JUN",
    "JUL",
    "AUG",
    "SEP",
    "OCT",
    "NOV",
    "DEC"
  ];

  static const List<String> weekdayNames = [
    "MONDAY",
    "TUESDAY",
    "WEDNESDAY",
    "THURSDAY",
    "FRIDAY",
    "SATURDAY",
    "SUNDAY"
  ];

  static const List<String> shortWeekdayLabels = [
    "S",
    "M",
    "T",
    "W",
    "T",
    "F",
    "S"
  ];
}
