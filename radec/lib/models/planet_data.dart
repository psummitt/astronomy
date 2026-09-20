class PlanetData {
  final String name;
  final double rightAscension; // Hours
  final double declination; // Degrees
  final double distanceToSun; // AU
  final double distanceToEarth; // AU

  PlanetData({
    required this.name,
    required this.rightAscension,
    required this.declination,
    required this.distanceToSun,
    required this.distanceToEarth,
  });

  @override
  String toString() {
    return '$name: RA ${rightAscension.toStringAsFixed(2)}h, Dec ${declination.toStringAsFixed(2)}°, Sun ${distanceToSun.toStringAsFixed(2)} AU, Earth ${distanceToEarth.toStringAsFixed(2)} AU';
  }
}
