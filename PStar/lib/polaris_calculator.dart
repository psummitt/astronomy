
class PolarisResult {
  final double decimalHours;
  final int hours;
  final int minutes;

  PolarisResult({
    required this.decimalHours,
    required this.hours,
    required this.minutes,
  });

  @override
  String toString() => '${decimalHours.toStringAsFixed(3)} HRS OR $hours HR $minutes MI';
}

enum PolarisEventType {
  westElongationNext,
  westElongationPrevious,
  eastElongationNext,
  eastElongationPrevious,
  lowerTransitNext,
  upperTransitNext,
}

class PolarisCalculator {
  static PolarisResult calculate({
    required int year,
    required int month,
    required int day,
    required double longitude,
    required PolarisEventType eventType,
  }) {
    bool isLeapYear = (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0);
    
    List<int> monthDays = [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334];
    int d2 = monthDays[month - 1];
    int d = day + d2;
    if (isLeapYear && month > 2) {
      d += 1;
    }

    double g = (year - 1980).toDouble();
    double yc = -0.01638889 * g;
    double gst = 6.65422 + yc;
    double dg = d * 0.0657096;
    double gt = gst + dg;

    while (gt > 24) {
      gt -= 24;
    }
    while (gt < 0) {
      gt += 24;
    }

    double cf = 0.065556 + longitude / 360.0;
    gt = gt + gt * cf;
    double ha = 2.183333 - gt;
    while (ha < 0) {
      ha += 24;
    }

    double mt = ha;
    // double qd = 5.98362; // Not used in the branch logic but present in code
    double qe = 5.933333;
    double qw = 5.933333;
    double ql = 11.967222;

    double resultIn;
    switch (eventType) {
      case PolarisEventType.westElongationNext:
        resultIn = mt + qw;
        break;
      case PolarisEventType.westElongationPrevious:
        resultIn = mt - ql - qe;
        break;
      case PolarisEventType.eastElongationNext:
        resultIn = mt + ql + qw;
        break;
      case PolarisEventType.eastElongationPrevious:
        resultIn = mt - qw;
        break;
      case PolarisEventType.lowerTransitNext:
        resultIn = mt + ql;
        break;
      case PolarisEventType.upperTransitNext:
        resultIn = ha;
        break;
    }

    // GOSUB 1280 (Normalize 0-24)
    while (resultIn > 24) {
      resultIn -= 24;
    }
    while (resultIn < 0) {
      resultIn += 24;
    }

    double decimalHoursOriginal = resultIn;

    // GOSUB 1190 (Convert to HR, MIN)
    double adjustedIn = resultIn + resultIn * 0.00273043;
    int hm = adjustedIn.toInt();
    double m1 = 60 * (adjustedIn - adjustedIn.toInt());
    int m2 = m1.toInt();

    return PolarisResult(
      decimalHours: adjustedIn,
      hours: hm,
      minutes: m2,
    );
  }
}
