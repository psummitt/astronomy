import 'dart:math';

/// Calculation mode for the moon's position.
enum MoonCalculationMode {
  /// The original simplified algorithm from the 1982 RADEM.BAS program.
  original1982,

  /// A modern, more accurate algorithm based on standard astronomical approximations.
  modern,
}

/// A result object containing the moon's position.
class MoonPosition {
  final DateTime universalTime;
  final double rightAscension; // Hours (0-24)
  final double declination;    // Degrees (-90 to 90)

  MoonPosition({
    required this.universalTime,
    required this.rightAscension,
    required this.declination,
  });

  /// Formatted RA string (e.g., "14h 15m 00s" or "14.25h")
  String get raFormatted {
    int h = rightAscension.floor();
    double mFull = (rightAscension - h) * 60.0;
    int m = mFull.floor();
    int s = ((mFull - m) * 60.0).round();
    if (s >= 60) {
      s = 0;
      m += 1;
    }
    if (m >= 60) {
      m = 0;
      h = (h + 1) % 24;
    }
    return '${h.toString().padLeft(2, '0')}h ${m.toString().padLeft(2, '0')}m ${s.toString().padLeft(2, '0')}s';
  }

  /// Formatted Declination string (e.g., "+12° 30' 00"")
  String get decFormatted {
    double absDec = declination.abs();
    int d = absDec.floor();
    double mFull = (absDec - d) * 60.0;
    int m = mFull.floor();
    int s = ((mFull - m) * 60.0).round();
    if (s >= 60) {
      s = 0;
      m += 1;
    }
    String sign = declination >= 0 ? '+' : '-';
    return '$sign${d.toString().padLeft(2, '0')}° ${m.toString().padLeft(2, '0')}\' ${s.toString().padLeft(2, '0')}"';
  }

  String get raString => rightAscension.toStringAsFixed(2);
  String get decString => declination.toStringAsFixed(2);
}

/// Calculator for Moon RA and Declination ported from the RADEM algorithm.
class MoonCalculator {
  static const double _degToRad = pi / 180.0;
  static const double _radToDeg = 180.0 / pi;

  /// Calculates Moon position using Universal Time directly.
  static MoonPosition calculateForDateTime(DateTime dateTime, {MoonCalculationMode mode = MoonCalculationMode.modern}) {
    DateTime ut = dateTime.toUtc();
    if (mode == MoonCalculationMode.original1982) {
      double localHours = ut.hour + (ut.minute / 60.0);
      return _calculateOriginal(ut, localHours);
    } else {
      return _calculateModern(ut);
    }
  }

  /// Original RADEM 1982 calculation.
  static MoonPosition _calculateOriginal(DateTime ut, double utHours) {
    int y = ut.year;
    int m = ut.month;
    double d = ut.day + (ut.hour / 24.0) + (ut.minute / 1440.0);

    double dg = 365.0 * y + d + ((m - 1.0) * 31.0);
    if (m >= 3) {
      dg = dg - (m + 0.4 + 2.3).floor() + (y / 4.0).floor() - ((0.75) * ((y / 100.0).floor() + 1.0)).floor();
    } else {
      dg = dg + ((y - 1.0) / 4.0).floor() - ((0.75) + ((y - 1.0) / 100.0 + 1.0).floor()).floor();
    }

    double nm = dg - 715875.0 - 0.5;

    const double lz = 311.1687;
    const double le = 178.699;
    const double lp = 255.7433;

    double pg = 0.111404 * nm + lp;
    pg = pg % 360.0;
    if (pg < 0) pg += 360.0;

    double lmd = lz + 360.0 * nm / 27.321582;
    pg = lmd - pg;
    double dr = 6.2886 * sin(_degToRad * pg);
    lmd = lmd + dr;
    lmd = lmd % 360.0;
    if (lmd < 0) lmd += 360.0;

    double rm = lmd / 15.0;

    double al = le - nm + 0.052954;
    al = al % 360.0;
    if (al < 0) al += 360.0;
    al = lmd - al;
    if (al < 0) al += 360.0;

    double he = 5.1454 * sin(_degToRad * (al + 1.0));
    double dm = he + 23.1444 * sin(_degToRad * (lmd + 1.0));

    return MoonPosition(
      universalTime: ut,
      rightAscension: rm,
      declination: dm,
    );
  }

  /// Modern Meeus-based simplified algorithm for moon position from RADEM.
  static MoonPosition _calculateModern(DateTime ut) {
    double jd = _getJulianDate(ut);
    double d = jd - 2451545.0;

    double l = (218.316 + 13.176396 * d) % 360.0;
    double m = (134.963 + 13.064993 * d) % 360.0;
    double f = (93.272 + 13.229350 * d) % 360.0;

    double longDeg = l + 6.289 * sin(_degToRad * m);
    double latDeg = 5.128 * sin(_degToRad * f);

    double eps = 23.439 - 0.0000004 * d;

    double raRad = atan2(
      sin(_degToRad * longDeg) * cos(_degToRad * eps) - tan(_degToRad * latDeg) * sin(_degToRad * eps),
      cos(_degToRad * longDeg)
    );
    double decRad = asin(
      sin(_degToRad * latDeg) * cos(_degToRad * eps) + cos(_degToRad * latDeg) * sin(_degToRad * eps) * sin(_degToRad * longDeg)
    );

    double ra = raRad * _radToDeg / 15.0;
    if (ra < 0) ra += 24.0;
    double dec = decRad * _radToDeg;

    return MoonPosition(
      universalTime: ut,
      rightAscension: ra,
      declination: dec,
    );
  }

  static double _getJulianDate(DateTime date) {
    int y = date.year;
    int m = date.month;
    double d = date.day + (date.hour / 24.0) + (date.minute / 1440.0) + (date.second / 86400.0);

    if (m <= 2) {
      y--;
      m += 12;
    }

    double a = (y / 100).floorToDouble();
    double b = 2 - a + (a / 4).floorToDouble();

    return (365.25 * (y + 4716)).floorToDouble() + (30.6001 * (m + 1)).floorToDouble() + d + b - 1524.5;
  }
}
