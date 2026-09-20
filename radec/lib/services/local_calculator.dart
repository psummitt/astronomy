import 'dart:math';
import '../models/planet_data.dart';

class LocalCalculator {
  static const double radPerDeg = pi / 180.0;
  static const double degPerRad = 180.0 / pi;

  // Planetary data for Epoch 1960
  // Index 0: Mercury, 1: Venus, 2: Earth, 3: Mars, 4: Jupiter, 5: Saturn, 6: Uranus, 7: Neptune, 8: Pluto
  // Fields: 0: Mean motion, 1: L0, 2: e_term, 3: w, 4: a, 5: dr, 6: r_w, 7: i_term, 8: node
  static const List<List<double>> _pd = [
    // Mercury
    [0.071422, 3.8484, 0.388301, 1.34041, 0.3871, 0.07974, 2.73514, 0.122173, 0.836013],
    // Venus
    [0.027962, 3.02812, 0.013195, 2.28638, 0.7233, 0.00506, 3.85017, 0.059341, 1.33168],
    // Earth
    [0.017202, 1.74022, 0.032044, 1.78547, 1.0, 0.017, 3.33929, 0.0, 0.0],
    // Mars
    [0.009146, 4.51234, 0.175301, 5.85209, 1.5237, 0.141704, 1.04656, 0.03142, 0.858702],
    // Jupiter
    [0.00145, 4.53364, 0.090478, 0.23911, 5.2028, 0.249374, 1.76188, 0.01972, 1.74533],
    // Saturn
    [0.000584, 4.89884, 0.105558, 1.61094, 9.5385, 0.534156, 3.1257, 0.043633, 1.977458],
    // Uranus
    [0.000205, 2.46615, 0.088593, 2.96706, 19.182, 0.901554, 4.49084, 0.01396, 1.28805],
    // Neptune
    [0.000104, 3.78556, 0.016965, 0.773181, 30.06, 0.27054, 2.33498, 0.031416, 2.29162],
    // Pluto
    [0.000069, 3.16948, 0.471239, 3.91303, 39.44, 9.86, 5.23114, 0.300197, 1.91812],
  ];

  static const List<String> _names = [
    'Mercury', 'Venus', 'Earth', 'Mars', 'Jupiter', 'Saturn', 'Uranus', 'Neptune', 'Pluto'
  ];

  static List<PlanetData> calculate(DateTime date) {
    // Porting the days from Epoch 1960 logic
    // BASIC code: NI = DG - 715875
    // DG = 365 * Y + D + ((M - 1) * 31)
    // Adjustments for leap years and months
    
    // We can use Dart's DateTime difference to get days since 1960-01-01
    final epoch = DateTime(1960, 1, 1);
    final diff = date.difference(epoch).inDays;
    final double ni = diff.toDouble();

    List<double> heliLongs = List.filled(9, 0.0);
    List<double> sunDistances = List.filled(9, 0.0);
    List<double> eclipticDistances = List.filled(9, 0.0);

    for (int j = 0; j < 9; j++) {
      double a = ni * _pd[j][0] + _pd[j][1];
      a = a % (2 * pi);
      if (a < 0) {
        a += 2 * pi;
      }

      double c = _pd[j][2] * sin(a - _pd[j][3]);
      a += c;
      a = a % (2 * pi);
      if (a < 0) {
        a += 2 * pi;
      }

      heliLongs[j] = a;
      sunDistances[j] = _pd[j][4] + _pd[j][5] * sin(a - _pd[j][6]);
      eclipticDistances[j] = _pd[j][7] * sin(a - _pd[j][8]);
    }

    List<PlanetData> results = [];
    final double earthA = heliLongs[2];
    final double earthD = sunDistances[2];

    for (int i = 0; i < 9; i++) {
      if (i == 2) {
        continue; // Skip Earth
      }

      double z = earthA - heliLongs[i];
      if (z.abs() > pi) {
        if (z < 0) {
          z += 2 * pi;
        } else {
          z -= 2 * pi;
        }
      }

      // Distance from Earth Q
      double q = sqrt(pow(sunDistances[i], 2) + pow(earthD, 2) - 2 * sunDistances[i] * earthD * cos(z));

      // Angular distance from Sun X
      // P = (D(I) + D(3) + Q)/2
      // X = 2 * FN ACO( SQR(((P*(P-D(I)))/(D(3)*Q))))
      double p = (sunDistances[i] + earthD + q) / 2;
      double val = sqrt((p * (p - sunDistances[i])) / (earthD * q));
      // Clamp for acos
      if (val > 1.0) val = 1.0;
      if (val < -1.0) val = -1.0;
      double x = 2 * acos(val);

      double r; // RA
      double v; // Dec

      if (z < 0) {
        r = (earthA + pi - x) * degPerRad / 15.0;
        v = sin(earthA + pi - x) * 23.44194 + eclipticDistances[i] * degPerRad;
      } else {
        r = (earthA + pi + x) * degPerRad / 15.0;
        v = sin(earthA + pi + x) * 23.44194 + eclipticDistances[i] * degPerRad;
      }

      while (r >= 24) {
        r -= 24;
      }
      while (r < 0) {
        r += 24;
      }

      results.add(PlanetData(
        name: _names[i],
        rightAscension: r,
        declination: v,
        distanceToSun: sunDistances[i],
        distanceToEarth: q,
      ));
    }

    return results;
  }
}
