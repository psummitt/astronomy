class EasterDate {
  final int year;
  final String monthName; // "MARCH" or "APRIL"
  final int month; // 3 or 4
  final int day;

  EasterDate({
    required this.year,
    required this.monthName,
    required this.month,
    required this.day,
  });

  DateTime toDateTime() => DateTime(year, month, day);

  @override
  String toString() => '$monthName $day';

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is EasterDate &&
          runtimeType == other.runtimeType &&
          year == other.year &&
          monthName == other.monthName &&
          month == other.month &&
          day == other.day;

  @override
  int get hashCode =>
      year.hashCode ^ monthName.hashCode ^ month.hashCode ^ day.hashCode;
}

/// The standard, correct Gregorian Easter algorithm (Meeus/Jones/Butcher).
/// Valid for all Gregorian years.
EasterDate standardEaster(int year) {
  final int a = year % 19;
  final int b = year ~/ 100;
  final int c = year % 100;
  final int d = b ~/ 4;
  final int e = b % 4;
  final int f = (b + 8) ~/ 25;
  final int g = (b - f + 1) ~/ 3;
  final int h = (19 * a + b - d - g + 15) % 30;
  final int i = c ~/ 4;
  final int k = c % 4;
  final int l = (32 + 2 * e + 2 * i - h - k) % 7;
  final int m = (a + 11 * h + 22 * l) ~/ 451;
  final int month = (h + l - 7 * m + 114) ~/ 31;
  final int day = ((h + l - 7 * m + 114) % 31) + 1;
  final String monthName = (month == 4) ? 'APRIL' : 'MARCH';
  return EasterDate(year: year, monthName: monthName, month: month, day: day);
}

/// The quirky floated algorithm matching the exact calculations in Easter.bas.
/// Includes floating-point representation quirks and manual overrides for 1974, 1984, and 1994.
EasterDate burgessEaster(int year) {
  final double y = year.toDouble();
  final double n = y - 1900.0;
  double a = n / 19.0;
  a = 19.0 * (a - a.floorToDouble());
  final double b = ((7.0 * a + 1.0) / 19.0).floorToDouble();
  double m = (11.0 * a + 4.00001 - b) / 29.0;
  final double x = m - m.floorToDouble();
  
  // BASIC line 240: IF X = 1 THEN GOTO 270 (setting M = 0)
  if ((x - 1.0).abs() < 1e-9) {
    m = 0.0;
  } else {
    m = 29.0 * x;
  }
  
  final double q = (n / 4.0).floorToDouble();
  double w = (n + q + 31.0 - m) / 7.0;
  w = 7.0 * (w - w.floorToDouble());
  final double wInt = w.floorToDouble();
  double de = (25.0 - m - wInt).floorToDouble();

  String monthName;
  if (de > 0.0) {
    monthName = 'APRIL';
  } else if (de < 0.0) {
    monthName = 'MARCH';
  } else {
    // BASIC line 390: IF DE = 0 THEN PRINT "MARCH 31"
    return EasterDate(year: year, monthName: 'MARCH', month: 3, day: 31);
  }

  // BASIC line 400: IF DE < - 9 THEN DE = DE + 9: GOTO 400
  while (de < -9.0) {
    de += 9.0;
  }

  double d = 0.0;
  if (de < 0.0) {
    d = 31.0 - de.abs();
  } else if (de > 0.0) {
    d = de;
  }

  // BASIC lines 430 & 440 manual overrides
  if (year == 1974 || year == 1984) {
    d += 7.0;
  } else if (year == 1994) {
    d = d + 7.0 - 31.0;
    monthName = 'APRIL';
  }

  return EasterDate(
    year: year,
    monthName: monthName,
    month: (monthName == 'APRIL') ? 4 : 3,
    day: d.round(),
  );
}

/// The integer arithmetic version of Burgess's algorithm, demonstrating that 
/// the underlying logic is 100% correct if executed with proper integer divisions.
/// Valid from 1900 to 2099 without any manual overrides.
EasterDate simplifiedIntegerEaster(int year) {
  final int n = year - 1900;
  final int a = n % 19;
  final int b = (7 * a + 1) ~/ 19;
  final int m = (11 * a + 4 - b) % 29;
  final int q = n ~/ 4;
  final int w = (n + q + 31 - m) % 7;
  final int de = 25 - m - w;

  if (de > 0) {
    return EasterDate(year: year, monthName: 'APRIL', month: 4, day: de);
  } else {
    return EasterDate(year: year, monthName: 'MARCH', month: 3, day: 31 + de);
  }
}
