class OrbitElements {
  String name;
  double inclination; // U
  double argumentOfPerihelion; // V
  double longitudeOfAscendingNode; // W
  double referenceDate; // T0 (JD)
  double eccentricity; // EP
  double a; // Perihelion distance or Semi-major axis
  double meanAnomaly; // M0 (for elliptic)

  OrbitElements({
    this.name = '',
    this.inclination = 0,
    this.argumentOfPerihelion = 0,
    this.longitudeOfAscendingNode = 0,
    this.referenceDate = 0,
    this.eccentricity = 0,
    this.a = 0,
    this.meanAnomaly = 0,
  });
}
