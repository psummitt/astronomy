import 'dart:math' as math;
import '../models/orbit_elements.dart';
import '../models/solar_coordinates.dart';
import '../models/calculation_result.dart';

enum Precision { single, double }

class OrbitEngine {
  final Precision precision;

  OrbitEngine({this.precision = Precision.double});

  double get _pi => precision == Precision.single ? 3.14159 : math.pi;

  double _rad(double deg) => deg * _pi / 180.0;
  double _deg(double rad) => rad * 180.0 / _pi;

  // Gaussian constants helpers
  double get _cosEps => precision == Precision.single ? 0.917437 : math.cos(_rad(23.4392911));
  double get _sinEps => precision == Precision.single ? 0.397881 : math.sin(_rad(23.4392911));

  List<CalculationResult> calculate(OrbitElements elements, List<SolarCoordinates> solarList) {
    List<CalculationResult> results = [];

    final u = _rad(elements.inclination);
    final v = _rad(elements.argumentOfPerihelion);
    final w = _rad(elements.longitudeOfAscendingNode);
    final ep = elements.eccentricity;
    final a = elements.a;
    final t0 = elements.referenceDate;
    final m0 = _rad(elements.meanAnomaly);

    // Line 1510-1550: Gaussian Constants
    final sinW = math.sin(w);
    final cosW = math.cos(w);
    final sinV = math.sin(v);
    final cosV = math.cos(v);
    final cosU = math.cos(u);
    final sinU = math.sin(u);

    final a1 = sinW * sinV;
    final b1 = cosW * sinV;
    final c1 = sinU * sinV;
    final a2 = sinW * cosV;
    final b2 = cosW * cosV;
    final c2 = sinU * cosV;

    final d1 = a2 + b1 * cosU;
    final d2 = -a1 + b2 * cosU;

    final px = b2 - a1 * cosU;
    final py = d1 * _cosEps - c1 * _sinEps;
    final pz = d1 * _sinEps + c1 * _cosEps;

    final qx = -b1 - a2 * cosU;
    final qy = d2 * _cosEps - c2 * _sinEps;
    final qz = d2 * _sinEps + c2 * _cosEps;

    int jp = 3; // Nearly Parabolic
    if (ep == 1.0) jp = 1; // Parabolic
    if (ep <= 0.75) jp = 2; // Elliptic

    if (a <= 0) return [];

    for (var solar in solarList) {
      if (solar.date.trim().isEmpty || solar.date.toUpperCase().startsWith("DONE")) continue;

      double t = solar.julianDate;
      double pa = 0;
      double qa = 0;

      if (jp == 1) {
        // Parabolic (Line 500)
        double m = (t - t0) / math.pow(a, 1.5);
        double c = 0.0364912 * m;
        double cs = math.sqrt(c * c / 4.0 + 1.0);
        double c1_val = c / 2.0 - cs;
        double c2_val = c / 2.0 + cs;
        double s = _sgn(c1_val) * math.pow(c1_val.abs(), 1.0 / 3.0) +
                   _sgn(c2_val) * math.pow(c2_val.abs(), 1.0 / 3.0);
        pa = a * (1.0 - s * s);
        qa = 2.0 * a * s;
      } else if (jp == 2) {
        // Elliptic (Line 600)
        double n0 = _rad(0.985608 / math.pow(a, 1.5));
        // Using + based on the logic that M0 is mean anomaly at T0
        double m = m0 + n0 * (t - t0);
        double e1 = m;
        double e = m;
        for (int iter = 0; iter < 100; iter++) {
          e = m + ep * math.sin(e1);
          if (e1 != 0 && (1.0 - e / e1).abs() < 0.0001) break;
          e1 = e;
        }
        pa = a * (math.cos(e) - ep);
        qa = a * math.sqrt(1.0 - ep * ep) * math.sin(e);
      } else {
        // Nearly Parabolic (Line 700)
        double e_val = (1.0 - ep) / (1.0 + ep);
        double f = 1.0 - (0.399375 - 0.198691 * e_val) * e_val;
        double d_val = 1.0 - (0.0284851 - (0.0186341 - 0.001917 * e_val) * e_val) * e_val;
        double c = f * math.sqrt((1.0 + ep) / 2.0) / math.pow(a, 1.5);
        double b = ep * d_val;
        double m = c * (t - t0);
        
        double t1 = 0.0364912 * m;
        double t3_val = math.sqrt(t1 * t1 / 4.0 + 1.0);
        double x0 = t1 / 2.0 - t3_val;
        double t2 = _sgn(x0) * math.pow(x0.abs(), 1.0 / 3.0) + math.pow((t1 / 2.0 + t3_val), 1.0 / 3.0);
        double n = b * t2 * t2;
        double s = 1.0 + 0.431919 * n;
        double p = n + e_val;
        double h = 1.0;
        if (p >= 0.22) {
          h = 1.0 - 3e-6 * (p - 0.21);
        }
        double t3_final = t2 * s * h;
        double t4 = t3_final * t3_final * e_val + 1.0;
        pa = a * (1.0 - t3_final * t3_final) / t4;
        qa = 2.0 * a * t3_final / t4;
      }

      double x = pa * px + qa * qx;
      double y = pa * py + qa * qy;
      double z = pa * pz + qa * qz;
      double r_sun = math.sqrt(x * x + y * y + z * z);

      double xi = x + solar.x;
      double eta = y + solar.y;
      double zeta = z + solar.z;
      double d_earth = math.sqrt(xi * xi + eta * eta + zeta * zeta);

      double ratio = (d_earth == 0) ? 0 : (zeta / d_earth).clamp(-1.0, 1.0);
      double dc = math.asin(ratio);
      double dc_deg = _deg(dc);
      
      double ra = math.atan2(eta, xi);
      double ra_deg = _deg(ra);
      if (ra_deg < 0) ra_deg += 360.0;
      double ra_hours = ra_deg / 15.0;

      int hr = ra_hours.isNaN ? 0 : ra_hours.floor();
      double mn = ra_hours.isNaN ? 0 : (ra_hours - hr) * 60.0;

      int id = _sgn(dc_deg).toInt();
      double dc_abs = dc_deg.abs();
      int dg = dc_abs.isNaN ? 0 : id * dc_abs.floor().toInt();
      double dm = dc_abs.isNaN ? 0 : (dc_abs - dc_abs.floor()) * 60.0;

      results.add(CalculationResult(
        date: solar.date,
        raHours: hr,
        raMinutes: mn,
        decDegrees: dg,
        decMinutes: dm,
        distanceEarth: d_earth,
        distanceSun: r_sun,
      ));
    }

    return results;
  }

  double _sgn(double x) {
    if (x > 0) return 1.0;
    if (x < 0) return -1.0;
    return 0.0;
  }
}
