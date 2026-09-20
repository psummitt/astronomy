class StarData {
  final String name;
  final double ra; // Right Ascension in degrees
  final double dec; // Declination in degrees
  final double? magnitude;
  final double? altitude;
  final double? azimuth;

  StarData({
    required this.name,
    required this.ra,
    required this.dec,
    this.magnitude,
    this.altitude,
    this.azimuth,
  });

  factory StarData.fromSesame(String name, double ra, double dec, double? mag) {
    return StarData(
      name: name,
      ra: ra,
      dec: dec,
      magnitude: mag,
    );
  }

  StarData copyWith({
    double? altitude,
    double? azimuth,
  }) {
    return StarData(
      name: name,
      ra: ra,
      dec: dec,
      magnitude: magnitude,
      altitude: altitude ?? this.altitude,
      azimuth: azimuth ?? this.azimuth,
    );
  }
}
