import 'dart:math' as math;
import 'base_calculator.dart';
import 'eclipse_event.dart';

class ModernCalculator extends BaseCalculator {
  @override
  String get name => "Modern (Meeus)";

  double rad(double deg) => deg * math.pi / 180;

  @override
  List<EclipseEvent> calculateEclipses(int startYear, {int count = 10}) {
    List<EclipseEvent> results = [];
    
    // kValue: Lunations since J2000.0 (New Moon 2000-01-06)
    // Full Moons occur at kValue = integer + 0.5.
    double kValue = (startYear - 2000) * 12.3685;
    kValue = kValue.floorToDouble() - 1.5; // Start early to ensure full year coverage

    int safetyCounter = 0;
    while (safetyCounter < 100) {
      safetyCounter++;
      double tVal = kValue / 1236.85;
      double t2Val = tVal * tVal;
      double t3Val = t2Val * tVal;
      double t4Val = t3Val * tVal;

      // Mean time of full moon (Julian Day Ephemeris)
      double jde = 2451550.09766 + 29.530588861 * kValue
          + 0.00015437 * t2Val - 0.000000150 * t3Val + 0.00000000073 * t4Val;

      // Fundamental arguments (Meeus Astronomical Algorithms Ch 47)
      double jdCent = (jde - 2451545.0) / 36525.0;
      double jc2 = jdCent * jdCent;
      double jc3 = jc2 * jdCent;
      double jc4 = jc3 * jdCent;

      // Sun's Mean Anomaly (M)
      double m = rad(357.52911 + 35999.05029 * jdCent - 0.0001537 * jc2 + jc3 / 24490000);
      // Moon's Mean Anomaly (M')
      double mp = rad(134.96340 + 477198.86751 * jdCent + 0.0087414 * jc2 + jc3 / 69699 - jc4 / 14712000);
      // Moon's Argument of Latitude (F)
      double f = rad(93.27210 + 483202.01752 * jdCent - 0.0036539 * jc2 - jc3 / 3526000 + jc4 / 863310000);
      // Mean Elongation (D)
      double d = rad(297.85019 + 445267.11140 * jdCent - 0.0018819 * jc2 + jc3 / 545868 - jc4 / 113065000);

      // Correction for True Full Moon (Meeus Ch 49 simplified)
      double trueCorrection = -0.40720 * math.sin(mp)
          + 0.17241 * math.sin(m)
          + 0.01608 * math.sin(2 * mp)
          + 0.01011 * math.sin(2 * d - mp);
      jde += trueCorrection;

      // Gamma formula (Empirical Lunar Eclipse Shadow Distance in Earth Radii)
      double gamma = 5.37 * math.sin(f)
          + 0.2067 * math.sin(f + 2 * d)
          + 0.0074 * math.sin(f - 2 * d)
          + 0.0113 * math.sin(f + mp)
          - 0.0093 * math.sin(f - mp)
          + 0.0055 * math.sin(f + m)
          - 0.0055 * math.sin(f - m);

      double absGamma = gamma.abs();
      DateTime date = jdToDateTime(jde);

      if (date.year == startYear) {
        if (absGamma < 1.58) {
          // Linear Magnitude formulas for mean Moon/Earth geometry
          double umbralMag = (1.0128 - absGamma) / 0.5450;
          double penumbralMag = (1.5573 - absGamma) / 0.5450;
          
          EclipseType type;
          if (umbralMag >= 1.0) {
            type = EclipseType.total;
          } else if (umbralMag > 0) {
            type = EclipseType.partial;
          } else {
            type = EclipseType.penumbral;
          }

          if (penumbralMag > 0) {
            final coords = _calculateSubLunarPoint(jde);
            results.add(EclipseEvent(
              date: date,
              magnitude: umbralMag > 0 ? umbralMag : 0,
              penumbralMagnitude: penumbralMag,
              type: type,
              engineName: name,
              bestLocation: _formatLocation(coords[0], coords[1]),
              latitude: coords[0],
              longitude: coords[1],
            ));
          }
        }
      } else if (date.year > startYear) {
        break;
      }
      
      kValue += 1.0;
    }

    return results;
  }

  List<double> _calculateSubLunarPoint(double jd) {
    double t = (jd - 2451545.0) / 36525.0;
    
    // Moon's mean elements (simplified)
    double lp = 218.316 + 481267.881 * t; // Mean longitude
    double mp = 134.963 + 477198.867 * t; // Moon mean anomaly
    double f = 93.272 + 483202.017 * t;   // Argument of latitude
    
    // Geocentric longitude and latitude (simplified)
    double lambda = lp + 6.289 * math.sin(rad(mp)) - 1.274 * math.sin(rad(mp - 2 * (lp - (280.466 + 36000.770 * t)))); 
    double beta = 5.128 * math.sin(rad(f));
    
    // Obliquity of the ecliptic
    double epsilon = 23.439 - 0.013 * t;
    
    // Convert to RA and Dec
    double sinDelta = math.sin(rad(beta)) * math.cos(rad(epsilon)) + 
                      math.cos(rad(beta)) * math.sin(rad(epsilon)) * math.sin(rad(lambda));
    double dec = math.asin(sinDelta) * 180 / math.pi;
    
    double y = math.sin(rad(lambda)) * math.cos(rad(epsilon)) - math.tan(rad(beta)) * math.sin(rad(epsilon));
    double x = math.cos(rad(lambda));
    double ra = math.atan2(y, x) * 180 / math.pi;
    
    // Greenwich Sidereal Time
    double gmst = 280.4606 + 360.985647 * (jd - 2451545.0);
    gmst = gmst % 360;
    if (gmst < 0) gmst += 360;
    
    // Sub-lunar point
    double lat = dec;
    double lon = ra - gmst;
    lon = lon % 360;
    if (lon > 180) lon -= 360;
    if (lon < -180) lon += 360;
    
    return [lat, lon];
  }

  String _formatLocation(double lat, double lon) {
    String latDir = lat >= 0 ? "N" : "S";
    String lonDir = lon >= 0 ? "E" : "W";
    return "${lat.abs().toStringAsFixed(1)}°$latDir, ${lon.abs().toStringAsFixed(1)}°$lonDir";
  }

  DateTime jdToDateTime(double jd) {
    jd += 0.5;
    int z = jd.floor();
    double f = jd - z;
    int a;
    if (z < 2299161) {
      a = z;
    } else {
      int alpha = ((z - 1867216.25) / 36524.25).floor();
      a = z + 1 + alpha - (alpha / 4).floor();
    }
    int b = a + 1524;
    int c = ((b - 122.1) / 365.25).floor();
    int d = (365.25 * c).floor();
    int e = ((b - d) / 30.6001).floor();

    double day = b - d - (30.6001 * e).floor() + f;
    int month = (e < 14) ? e - 1 : e - 13;
    int year = (month > 2) ? c - 4716 : c - 4715;

    int hours = ((day - day.floor()) * 24).floor();
    int minutes = ((((day - day.floor()) * 24) - hours) * 60).floor();

    return DateTime.utc(year, month, day.floor(), hours, minutes);
  }
}
