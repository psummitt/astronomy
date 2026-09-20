class CalculationResult {
  final String date;
  final int raHours;
  final double raMinutes;
  final int decDegrees;
  final double decMinutes;
  final double distanceEarth;
  final double distanceSun;

  CalculationResult({
    required this.date,
    required this.raHours,
    required this.raMinutes,
    required this.decDegrees,
    required this.decMinutes,
    required this.distanceEarth,
    required this.distanceSun,
  });
}
