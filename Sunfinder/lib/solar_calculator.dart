import 'dart:math';

class SolarResult {
  final double azimuth;
  final double altitude;
  final String dateString;
  final double hour;

  SolarResult({
    required this.azimuth,
    required this.altitude,
    required this.dateString,
    required this.hour,
  });
}

class SunriseSunsetResult {
  final String dateString;
  final String sunrise;
  final double sunriseAzimuth;
  final String sunset;
  final double sunsetAzimuth;
  final double noonAltitude;
  final double noonAzimuth;

  SunriseSunsetResult({
    required this.dateString,
    required this.sunrise,
    required this.sunriseAzimuth,
    required this.sunset,
    required this.sunsetAzimuth,
    required this.noonAltitude,
    required this.noonAzimuth,
  });
}

class SolarCalculator {
  static const double dr = 0.0174533; // Degrees to Radians
  static const double rd = 57.2958;   // Radians to Degrees

  static int getDayOfYear(int month, int day, bool isLeapYear) {
    final daysInMonth = [31, isLeapYear ? 29 : 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
    int doy = 0;
    for (int i = 0; i < month - 1; i++) {
      doy += daysInMonth[i];
    }
    return doy + day;
  }

  static String getDateString(int doy, bool isLeapYear) {
    final daysInMonth = [31, isLeapYear ? 29 : 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
    final monthNames = ["JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JLY", "AUG", "SEP", "OCT", "NOV", "DEC"];
    
    int remaining = doy;
    for (int i = 0; i < 12; i++) {
      if (remaining <= daysInMonth[i]) {
        return "${monthNames[i]} $remaining";
      }
      remaining -= daysInMonth[i];
    }
    return "UNKNOWN";
  }

  static List<SolarResult> calculatePosition({
    required double latitude,
    required double longitude,
    required double standardMeridian,
    required double magneticVariation,
    required bool isLeapYear,
    required int startDoy,
    required int endDoy,
    required double startHour,
    required double endHour,
    required double hourIncrement,
  }) {
    List<SolarResult> results = [];
    final da = isLeapYear ? 366.0 : 365.24232;
    final k = 360.0 / da;
    final la = latitude * dr;
    
    final mx = longitude >= 0 ? longitude - standardMeridian : standardMeridian - longitude;
    final mc = mx / 15.0;
    final mr = mx * dr;

    for (int n = startDoy; n <= endDoy; n++) {
      // Lines 1500-1590: Mean Longitude and Equation of Time
      final l1 = (279.575 + (k * n)) * dr;
      final g1 = (356.967 + (k * n)) * dr;
      final ld = l1 + (1.916 * sin(g1) + 0.02 * sin(2 * g1)) * dr;
      final dl = 0.39782 * sin(ld);
      final zLatSun = atan(dl / sqrt(-dl * dl + 1));
      
      final el = -104.7 * sin(l1) + 596.2 * sin(2 * l1) + 4.3 * sin(3 * l1) - 12.7 * sin(4 * l1) - 429.3 * cos(l1) - 2 * cos(2 * l1) + 19.3 * cos(3 * l1);
      final et = -el / 3600.0;
      final ed = et * 15.0;
      final er = ed * dr;

      final bb = 1.5708 - la; // 90 deg - your lat

      for (double s = startHour; s <= endHour; s += hourIncrement) {
        final aa = 1.5708 - zLatSun;
        final c = s < 12 
            ? (12 - s) * 15 * dr + er + mr 
            : (12 - s) * 15 * dr + er + mr; // Logic in BAS line 860 has a typo 'SR' vs 'ER', using ER

        final e = (bb - aa) / 2.0;
        final f = (bb + aa) / 2.0;
        final g = c / 2.0;

        final x = cos(e) / (cos(f) * tan(g));
        final y = sin(e) / (sin(f) * tan(g));
        final xx = atan(x) * 2.0;
        final yy = atan(y) * 2.0;
        final b = (xx + yy) / 2.0;
        final a = xx - b;
        final l = (b + a) / 2.0;
        final m = (b - a) / 2.0;
        
        final zz = (tan(e) * sin(l)) / sin(m);
        final cc = 2 * atan(zz);

        double altitude = 90.0 - (cc * rd).roundToDouble();
        if (cc < 0) altitude = 180 - altitude;

        double azimuth = (a * rd + magneticVariation).roundToDouble();
        if (cc < 0 && a < 0) azimuth = 180 + azimuth;
        if (s > 12 && azimuth < 180 && azimuth >= 0) azimuth += 180;
        if (azimuth < 0) azimuth += 360;
        if (azimuth >= 360) azimuth -= 360;

        // Special case for noon (BAS lines 1040-1060)
        if (s == 12.0) {
          if (latitude - zLatSun * rd > 0) {
             azimuth = (180 + magneticVariation - mx - ed).roundToDouble();
             altitude = (90 + cos(mr + er) * (latitude - zLatSun * rd)).roundToDouble();
          } else {
             azimuth = (360 + magneticVariation - mx - ed).roundToDouble();
             altitude = (90 * cos(mr + er) - (zLatSun * rd - latitude)).roundToDouble();
          }
          if (azimuth >= 360) azimuth -= 360;
        }

        results.add(SolarResult(
          azimuth: azimuth,
          altitude: altitude,
          dateString: getDateString(n, isLeapYear),
          hour: s,
        ));
      }
    }
    return results;
  }

  static List<SunriseSunsetResult> calculateSunriseSunset({
    required double latitude,
    required double longitude,
    required double standardMeridian,
    required double magneticVariation,
    required bool isLeapYear,
    required int startDoy,
    required int endDoy,
    required int dayIncrement,
  }) {
    List<SunriseSunsetResult> results = [];
    final da = isLeapYear ? 366.0 : 365.24232;
    final k = 360.0 / da;
    final la = latitude * dr;
    
    final mx = longitude >= 0 ? longitude - standardMeridian : standardMeridian - longitude;
    final mc = mx / 15.0;
    final mr = mx * dr;

    for (int n = startDoy; n <= endDoy; n += dayIncrement) {
      // Lines 1500-1590
      final l1 = (279.575 + (k * n)) * dr;
      final g1 = (356.967 + (k * n)) * dr;
      final ld = l1 + (1.916 * sin(g1) + 0.02 * sin(2 * g1)) * dr;
      final dl = 0.39782 * sin(ld);
      final zLatSun = atan(dl / sqrt(-dl * dl + 1));
      
      final el = -104.7 * sin(l1) + 596.2 * sin(2 * l1) + 4.3 * sin(3 * l1) - 12.7 * sin(4 * l1) - 429.3 * cos(l1) - 2 * cos(2 * l1) + 19.3 * cos(3 * l1);
      final et = -el / 3600.0;
      final ed = et * 15.0;
      final er = ed * dr;

      const cc = 1.5708; // 90 deg
      final aa = cc - zLatSun;
      final bb = cc - la;
      final ss = (aa + bb + cc) / 2.0;

      // Handle potential square root of negative (BAS line 1230-1240)
      double tr = 0;
      try {
        tr = sqrt((sin(ss - aa) * sin(ss - bb) * sin(ss - cc)) / sin(ss));
      } catch (e) {
        // "CAN'T DETERMINE" case
        continue;
      }

      final c1 = tr / sin(ss - cc);
      final cAngle = 2 * atan(c1) * rd;
      final azimuth = 2 * rd * atan(tr / sin(ss - aa));
      final ch = cAngle / 15.0;

      final su = 12.0 - 0.056 + et + mc - ch; // Sunrise
      final sd = 12.0 + 0.056 + et + mc + ch; // Sunset

      String formatTime(double time) {
        int hour = time.floor();
        int minute = ((time - hour) * 60).round();
        if (minute == 60) {
          minute = 0;
          hour++;
        }
        return "${hour.toString().padLeft(2, '0')}:${minute.toString().padLeft(2, '0')}";
      }

      double noonAz;
      double noonAlt;
      if (latitude - zLatSun * rd < 0) {
        noonAz = (360 - ed + magneticVariation - mx).roundToDouble();
        noonAlt = (90.0 * cos(mr + er) - (zLatSun * rd - latitude)).roundToDouble();
      } else {
        noonAz = (180 - ed + magneticVariation - mx).roundToDouble();
        noonAlt = (90.0 * cos(mr + er) - (latitude - zLatSun * rd)).roundToDouble();
      }
      if (noonAz >= 360) noonAz -= 360;
      if (noonAz < 0) noonAz += 360;

      results.add(SunriseSunsetResult(
        dateString: getDateString(n, isLeapYear),
        sunrise: formatTime(su),
        sunriseAzimuth: (azimuth + magneticVariation).roundToDouble(),
        sunset: formatTime(sd),
        sunsetAzimuth: (360 - azimuth + magneticVariation).roundToDouble(),
        noonAltitude: noonAlt,
        noonAzimuth: noonAz,
      ));
    }
    return results;
  }
}
