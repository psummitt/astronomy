import 'dart:math';
import 'package:astronomia/sidereal.dart' as sidereal;
import 'package:astronomia/coord.dart' as coord;
import 'package:astronomia/julian.dart' as julian;
import '../models/star_data.dart';

class AstronomyService {
  static double toRad(double deg) => deg * pi / 180;
  static double toDeg(double rad) => rad * 180 / pi;

  static StarData calculateAltAz(StarData star, double lat, double lon) {
    final now = DateTime.now().toUtc();
    
    // Calculate Julian Day
    final jd = julian.calendarGregorianToJD(
      now.year, 
      now.month, 
      now.day + (now.hour + (now.minute + now.second / 60) / 60) / 24.0
    );

    // RA and Dec in radians
    final raRad = toRad(star.ra);
    final decRad = toRad(star.dec);
    final latRad = toRad(lat);
    final lonRad = toRad(lon);

    // Calculate Greenwich Mean Sidereal Time
    final gmst = sidereal.mean(jd);

    // Local Sidereal Time
    final lst = gmst + lonRad;

    // Hour Angle
    final h = lst - raRad;

    // Convert to Horizontal
    // eqToHz(ra, dec, phi, psi, st)
    // phi: latitude, psi: longitude (positive West), st: Greenwich sidereal time
    final result = coord.eqToHz(raRad, decRad, latRad, -lonRad, gmst);

    // Normalize Azimuth: Meeus uses South-based (0=S, 90=W, 180=N, 270=E)
    // Standard is North-based (0=N, 90=E, 180=S, 270=W)
    double azimuthDeg = toDeg(result.az);
    double normalizedAzimuth = (azimuthDeg + 180) % 360;

    return star.copyWith(
      altitude: toDeg(result.alt),
      azimuth: normalizedAzimuth,
    );
  }
}
