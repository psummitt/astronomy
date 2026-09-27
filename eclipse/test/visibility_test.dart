import 'dart:math' as math;
import 'package:flutter_test/flutter_test.dart';

bool isVisible(double userLat, double userLon, double moonLat, double moonLon) {
  double rad(double deg) => deg * math.pi / 180;
  double phi1 = rad(userLat);
  double phi2 = rad(moonLat);
  double deltaLambda = rad(moonLon - userLon);
  double cosTheta = math.sin(phi1) * math.sin(phi2) + 
                    math.cos(phi1) * math.cos(phi2) * math.cos(deltaLambda);
  return cosTheta > 0;
}

void main() {
  test('Visibility logic: Overlapping locations', () {
    // Observer at sub-lunar point
    expect(isVisible(0, 0, 0, 0), true);
    expect(isVisible(51.5, -0.1, 51.5, -0.1), true);
  });

  test('Visibility logic: Antipodal locations', () {
    // Observer on opposite side of Earth
    expect(isVisible(0, 0, 0, 180), false);
    expect(isVisible(90, 0, -90, 0), false);
  });

  test('Visibility logic: Horizon limits', () {
    // 90 degrees away should be roughly the limit
    expect(isVisible(0, 0, 0, 89.9), true);
    expect(isVisible(0, 0, 0, 90.1), false);
  });
}
