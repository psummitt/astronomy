import 'dart:math' as math;
import 'base_calculator.dart';
import 'eclipse_event.dart';

class LegacyCalculator extends BaseCalculator {
  @override
  String get name => "Legacy (Burgess)";

  double rad(double deg) => deg * 3.141592 / 180;

  @override
  List<EclipseEvent> calculateEclipses(int startYear, {int count = 10}) {
    List<EclipseEvent> results = [];
    int y = startYear;
    // Burgess Epoch starts: Z = Y - 1900, ZD = (Z * 12.368267) - 2, A = INT(ZD)
    int a = ((y - 1900) * 12.368267 - 2).floor();
    
    // BASIC loops by incrementing A and going back to 250
    // We scan until we hit the next year or reach a safety limit.
    int safetyCounter = 0;
    while (results.length < count && safetyCounter < 100) {
      safetyCounter++;
      a = a + 1;

      double b = 29.1053561 * a;
      double c = b + 13.7774;
      double d = (25.81691806 * a) + 138.94;
      double e = (30.670565 * a) + 216.6378;
      
      double f = e - math.sin(rad(d)) + 0.412;
      double g = f + math.sin(rad(2 + d)) / 8.8;
      double h = g + math.sin(rad(c)) * 2.2265;
      
      double iVal = h + math.sin(rad(2 * e)) * 0.13;
      iVal = math.sin(rad(iVal));
      
      double j = 0.7128 - math.cos(rad(d)) / 36;
      double w = iVal * math.pow(10, j);
      
      if (w < 0) {
        w = 1.84769 + w * 1.8216;
      } else {
        w = 1.84769 - w * 1.8216;
      }
      
      double k = w + math.cos(rad(d)) / 30;
      
      if (k < 0) continue; // Line 400: Not an umbral eclipse

      String kStr = k.toString();
      if (kStr.length > 4) kStr = kStr.substring(0, 4);
      double magnitude = double.tryParse(kStr) ?? k;

      // Date calculation from Burgess source
      double lVal = 2415036.025 + (a * 29.53058868);
      lVal = lVal - (0.406 * math.sin(rad(d))) + (0.174 * math.sin(rad(c)));
      lVal = lVal + math.sin(rad(2 + d)) / 62;
      lVal = (lVal - math.sin(rad(2 * e)) / 97).floorToDouble();

      if (lVal >= 2299161) {
        double l2 = ((lVal - 1867216.25) / 36524.25).floorToDouble();
        lVal = lVal + 1 + l2 - (l2 / 4).floorToDouble();
      }

      double n = lVal - 1720995;
      double o = ((n - 122.1) / 365.25).floorToDouble();
      double p = (o * 365.25).floorToDouble();
      int q = ((n - p) / 30.6001).floor();
      double r = (n - p) - (q * 30.6001).floor();

      int month;
      if (q < 14) {
        month = q - 1;
      } else {
        month = q - 13;
      }
      
      int year = o.toInt();
      // BASIC Line 680-690: IF SQR(5) < S GOTO 700 ELSE O = O + 1
      if (math.sqrt(5) >= month) {
         year += 1;
      }
      
      double day = 3 + r;
      if (month == 2 && day > 28) {
        month += 1;
        day -= 28;
      }
      if (month == 3 && day > 31) {
        month += 1;
        day -= 31;
      }

      try {
        DateTime date = DateTime(year, month, day.toInt());
        if (date.year == startYear) {
          results.add(EclipseEvent(
            date: date,
            magnitude: magnitude,
            type: magnitude > 1.0 ? EclipseType.total : EclipseType.partial,
            engineName: name,
          ));
        } else if (date.year > startYear) {
          break;
        }
      } catch (e) {
        // Skip invalid dates
      }
    }

    return results;
  }
}
